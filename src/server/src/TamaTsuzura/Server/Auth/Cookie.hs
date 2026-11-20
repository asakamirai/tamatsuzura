{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.Auth.Cookie
    ( -- * 認証設定・ポリシー型
      RegistrationPolicy(..)
    , CookieSecureMode(..)
    , AuthConfig(..)
    , defaultAuthConfig
      -- * セッション Cookie 定数・判定
    , sessionCookieName
    , shouldSetSecureCookie
      -- * Cookie ヘッダ解析・生成
    , parseCookieHeader
    , extractSessionTokenFromHeaders
    , extractSessionTokenFromCookieValue
    , buildSessionCookieHeader
    , buildClearSessionCookieHeader
    ) where

import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BS8
import qualified Data.Char                      as Char
import           Data.Maybe                     (listToMaybe, mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as Txt
import qualified Data.Text.Encoding             as TxtEnc
import qualified Network.HTTP.Types             as Http
import           TamaTsuzura.Server.Auth.Crypto
    ( PasswordHashParams
    , SessionToken
    , defaultPasswordHashParams
    , sessionTokenRawText
    )
import           TamaTsuzura.Server.Storage.Auth
    ( SessionTtlSeconds
    , defaultSessionTtlSeconds
    , sessionTtlSecondsValue
    )

-- | 新規ユーザー登録 (@POST /api/auth/register@) の許可ポリシー。
-- 将来のセルフホスト向けクローズド運用・招待制へ拡張しやすいよう設定層として分離する。
data RegistrationPolicy
    = RegistrationOpen
      -- ^ 誰でもブラウザから新規アカウント登録可能 (既定値)
    | RegistrationClosed
      -- ^ 新規アカウント登録を無効化 (HTTP 403 Forbidden)
    deriving (Eq, Show)

-- | セッション Cookie の @Secure@ 属性付与方針。
data CookieSecureMode
    = CookieSecureAuto
      -- ^ HTTPS リクエスト時のみ @Secure@ 属性を付与 (既定値)
    | CookieSecureAlways
      -- ^ 常に @Secure@ 属性を付与 (本番 HTTPS 強制運用向け)
    | CookieSecureNever
      -- ^ @Secure@ 属性を付与しない (ローカル HTTP 開発専用)
    deriving (Eq, Show)

-- | 認証・セッション・CSRF・登録ポリシーを束ねるサーバ認証設定。
data AuthConfig = AuthConfig
    { registrationPolicy :: !RegistrationPolicy
      -- ^ 新規ユーザー登録の許可ポリシー
    , sessionTtl         :: !SessionTtlSeconds
      -- ^ SQLite セッションおよび Cookie @Max-Age@ の有効期間
    , cookieSecureMode   :: !CookieSecureMode
      -- ^ Cookie @Secure@ 属性の付与モード
    , passwordHashParams :: !PasswordHashParams
      -- ^ Argon2id パスワードハッシュ化コストパラメータ
    , trustProxyHeaders  :: !Bool
      -- ^ リバースプロキシの @X-Forwarded-Proto@ / @X-Forwarded-Host@ を信頼するか (安全側既定値: 'False')
    } deriving (Eq, Show)

-- | 本番環境向けの安全な既定認証設定。
defaultAuthConfig :: AuthConfig
defaultAuthConfig = AuthConfig
    { registrationPolicy = RegistrationOpen
    , sessionTtl         = defaultSessionTtlSeconds
    , cookieSecureMode   = CookieSecureAuto
    , passwordHashParams = defaultPasswordHashParams
    , trustProxyHeaders  = False
    }

-- | セッション Cookie の名称 (@tamatsuzura_session@)。
sessionCookieName :: ByteString
sessionCookieName = "tamatsuzura_session"

-- | 'CookieSecureMode' とリクエストの HTTPS 判定から、@Secure@ 属性を付与すべきかを返す。
shouldSetSecureCookie :: CookieSecureMode -> Bool -> Bool
shouldSetSecureCookie CookieSecureAuto   isHttps = isHttps
shouldSetSecureCookie CookieSecureAlways _       = True
shouldSetSecureCookie CookieSecureNever  _       = False

-- | RFC 6265 形式の @Cookie@ ヘッダ値 (@name1=val1; name2=val2@) を名前・値の組一覧へ解析する。
-- 空要素や @=@ を含まない不正ペアは無視し、前後の ASCII 空白および外側の二重引用符を除去する。
parseCookieHeader :: ByteString -> [(ByteString, ByteString)]
parseCookieHeader raw =
    mapMaybe parsePair (BS8.split ';' raw)
  where
    parsePair :: ByteString -> Maybe (ByteString, ByteString)
    parsePair segment =
        let (rawName, rest) = BS8.break (== '=') segment
        in  case BS8.uncons rest of
                Nothing        -> Nothing
                Just (_, val0) ->
                    let name :: ByteString
                        name = trimAsciiSpace rawName

                        val :: ByteString
                        val = stripOptionalQuotes (trimAsciiSpace val0)
                    in  if BS.null name
                            then Nothing
                            else Just (name, val)

    stripOptionalQuotes :: ByteString -> ByteString
    stripOptionalQuotes bs
        | BS.length bs >= 2
            && BS8.index bs 0 == '"'
            && BS8.index bs (BS.length bs - 1) == '"' =
            BS.take (BS.length bs - 2) (BS.drop 1 bs)
        | otherwise = bs

-- | HTTP リクエストヘッダ一覧から最初の @tamatsuzura_session@ Cookie 値を抽出する。
-- HTTP/2 等で複数の @Cookie@ ヘッダが分割送信された場合にも対応する。
extractSessionTokenFromHeaders :: Http.RequestHeaders -> Maybe Text
extractSessionTokenFromHeaders headers =
    listToMaybe (mapMaybe extractFromCookieHeader headers)
  where
    extractFromCookieHeader :: Http.Header -> Maybe Text
    extractFromCookieHeader (headerName, headerVal)
        | headerName == Http.hCookie = extractSessionTokenFromCookieValue headerVal
        | otherwise                  = Nothing

-- | 単一の @Cookie@ ヘッダ値から @tamatsuzura_session@ の生トークン文字列を抽出する。
-- 非 UTF-8 バイト列を含む場合は安全に 'Nothing' を返す。
extractSessionTokenFromCookieValue :: ByteString -> Maybe Text
extractSessionTokenFromCookieValue rawHeader =
    case lookup sessionCookieName (parseCookieHeader rawHeader) of
        Nothing -> Nothing
        Just val -> case TxtEnc.decodeUtf8' val of
            Left _    -> Nothing
            Right txt -> Just txt

-- | ログイン・新規登録成功時に返却する @Set-Cookie@ ヘッダ値を生成する。
-- @HttpOnly@, @SameSite=Lax@, @Path=/@, @Max-Age=<ttl>@ を必ず付与し、
-- HTTPS 判定および 'CookieSecureMode' に応じて @Secure@ を付与する。
buildSessionCookieHeader ::
       CookieSecureMode
    -> Bool
    -> SessionTtlSeconds
    -> SessionToken
    -> Text
buildSessionCookieHeader secureMode isHttps ttl rawToken =
    Txt.intercalate "; " $
        [ TxtEnc.decodeLatin1 sessionCookieName <> "=" <> sessionTokenRawText rawToken
        , "Path=/"
        , "HttpOnly"
        , "SameSite=Lax"
        , "Max-Age=" <> Txt.pack (show (sessionTtlSecondsValue ttl))
        ] <> secureAttrs
  where
    secureAttrs :: [Text]
    secureAttrs
        | shouldSetSecureCookie secureMode isHttps = ["Secure"]
        | otherwise                                = []

-- | ログアウト時にセッション Cookie を即時削除するための @Set-Cookie@ ヘッダ値を生成する。
-- ブラウザ互換性のため @Max-Age=0@ と @Expires=Thu, 01 Jan 1970 00:00:00 GMT@ の双方を設定する。
buildClearSessionCookieHeader :: CookieSecureMode -> Bool -> Text
buildClearSessionCookieHeader secureMode isHttps =
    Txt.intercalate "; " $
        [ TxtEnc.decodeLatin1 sessionCookieName <> "="
        , "Path=/"
        , "HttpOnly"
        , "SameSite=Lax"
        , "Max-Age=0"
        , "Expires=Thu, 01 Jan 1970 00:00:00 GMT"
        ] <> secureAttrs
  where
    secureAttrs :: [Text]
    secureAttrs
        | shouldSetSecureCookie secureMode isHttps = ["Secure"]
        | otherwise                                = []

trimAsciiSpace :: ByteString -> ByteString
trimAsciiSpace = BS8.dropWhileEnd isAsciiSpace . BS8.dropWhile isAsciiSpace
  where
    isAsciiSpace :: Char -> Bool
    isAsciiSpace ch = ch == ' ' || ch == '\t' || (Char.isAscii ch && Char.isSpace ch)
