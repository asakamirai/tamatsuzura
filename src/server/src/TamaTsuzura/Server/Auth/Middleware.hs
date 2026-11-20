{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module TamaTsuzura.Server.Auth.Middleware
    ( -- * 定数
      csrfHeaderName
      -- * HTTPS・Cookie・キャッシュ・Origin 検証ミドルウェア
    , isRequestHttps
    , normalizeCookieHeadersMiddleware
    , normalizeRequestSecurityMiddleware
    , noStoreCacheMiddleware
    , validateRequestOrigin
    , originCheckMiddleware
      -- * Servant Cookie セッション＋CSRF 認証ハンドラ
    , sessionAuthHandler
    , csrfSessionAuthHandler
    ) where

import           Control.Applicative              ((<|>))
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.Aeson                       as Aeson
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BS8
import qualified Data.Char                        as Char
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as TxtEnc
import qualified Data.Text.IO                     as TxtIO
import qualified Network.HTTP.Types               as Http
import qualified Network.Wai                      as Wai
import           Servant                          (Handler)
import qualified Servant
import           Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import qualified System.IO                        as Sys
import           TamaTsuzura.Common.Api           (ApiError(..), ApiErrorCode(..))
import           TamaTsuzura.Common.Auth          (CsrfToken, mkCsrfToken)
import           TamaTsuzura.Common.Text          (ToText(..))
import           TamaTsuzura.Server.Api
    ( CsrfVerifiedSession(..)
    , SessionAuthContext(..)
    , apiErrorToServerError
    , jsonContentType
    )
import           TamaTsuzura.Server.Auth.Cookie   (extractSessionTokenFromHeaders)
import           TamaTsuzura.Server.Auth.Crypto
    ( SessionToken
    , mkSessionToken
    , verifyCsrfToken
    )
import           TamaTsuzura.Server.Storage
    ( StorageConfig
    , StorageError(..)
    , storageErrorToApiError
    )
import qualified TamaTsuzura.Server.Storage       as Storage
import           TamaTsuzura.Server.Storage.Auth
    ( AuthenticatedSession(..)
    , StoredSession(..)
    , lookupValidSession
    )
import           Text.Read                        (readMaybe)

-- | 状態変更リクエストに付与する CSRF トークン HTTP ヘッダ名 (@X-CSRF-Token@)。
csrfHeaderName :: Http.HeaderName
csrfHeaderName = "X-CSRF-Token"

-- | リクエストが HTTPS 経由であるかを判定する。
-- @trustProxyHeaders@ が 'True' の場合に限り @X-Forwarded-Proto@ の先頭要素も参照する。
isRequestHttps :: Bool -> Wai.Request -> Bool
isRequestHttps trustProxyHeaders req
    | Wai.isSecure req  = True
    | trustProxyHeaders =
        case firstForwardedValue "X-Forwarded-Proto" (Wai.requestHeaders req) of
            Just proto -> asciiLowerBS proto == "https"
            Nothing    -> False
    | otherwise         = False

-- | HTTP/2 (RFC 9113 §8.2.3) 等で複数の @Cookie@ ヘッダへ分割送信されたリクエストを
-- @"; "@ で単一の @Cookie@ ヘッダへ結合するミドルウェア。
-- Servant の @Header "Cookie"@ 抽出 (先頭 1 件参照) を含むすべての経路で
-- 全 Cookie を漏れなく解釈できるようにする。
normalizeCookieHeadersMiddleware :: Wai.Middleware
normalizeCookieHeadersMiddleware app req =
    case lookupAll Http.hCookie origHeaders of
        _ : _ : _ ->
            let combined :: ByteString
                combined = BS.intercalate "; " (lookupAll Http.hCookie origHeaders)

                nonCookieHeaders :: Http.RequestHeaders
                nonCookieHeaders = filter (\ (name, _) -> name /= Http.hCookie) origHeaders
            in  app req { Wai.requestHeaders = (Http.hCookie, combined) : nonCookieHeaders }
        _ -> app req
  where
    origHeaders :: Http.RequestHeaders
    origHeaders = Wai.requestHeaders req

-- | リバースプロキシ設定を考慮した HTTPS 判定結果を 'Wai.isSecure' へ正規化するミドルウェア。
-- Servant の 'Servant.IsSecure' 結合子が Warp / CGI / プロキシ配下のいずれでも
-- 一貫した HTTPS 判定を得られるようにする。
normalizeRequestSecurityMiddleware :: Bool -> Wai.Middleware
normalizeRequestSecurityMiddleware trustProxyHeaders app req =
    app req { Wai.isSecure = isRequestHttps trustProxyHeaders req }

-- | すべての API レスポンスへ @Cache-Control: no-store@ を付与するミドルウェア。
-- セッション Cookie・CSRF トークンおよび認証済みユーザーの個人文書が中間プロキシや
-- ブラウザキャッシュへ残留するのを防ぐ。
noStoreCacheMiddleware :: Wai.Middleware
noStoreCacheMiddleware app req respond =
    app req $ \ res ->
        let headers :: Http.ResponseHeaders
            headers = Wai.responseHeaders res
        in  case lookup Http.hCacheControl headers of
                Just _  -> respond res
                Nothing -> respond (Wai.mapResponseHeaders (("Cache-Control", "no-store") :) res)

-- | 状態変更メソッド (@POST@ / @PUT@ / @DELETE@ / @PATCH@ 等) に対する
-- @Origin@ / @Referer@ / @Sec-Fetch-Site@ 検証を行う純粋関数。
-- ブラウザからのクロスサイトリクエスト (ログイン CSRF・状態変更 CSRF) を早期遮断する一方で、
-- @Origin@・@Referer@・@Sec-Fetch-Site@ を持たない非ブラウザクライアント (@curl@ や CGI テスト等) は
-- 後続のセッション＋CSRF トークン認証へ通す。
validateRequestOrigin :: Bool -> Wai.Request -> Either ApiError ()
validateRequestOrigin trustProxyHeaders req
    | isSafeHttpMethod (Wai.requestMethod req) = Right ()
    | otherwise = do
        checkFetchSite headers
        case lookupAll "Origin" headers of
            [] -> checkOptionalReferer headers
            [rawOrigin] -> do
                originVal <- parseOriginHeader rawOrigin
                targetVal <- resolveTargetOrigin trustProxyHeaders req
                if originVal == targetVal
                    then Right ()
                    else Left (forbiddenError "Request Origin does not match target origin")
            _ -> Left (forbiddenError "Multiple Origin headers are not allowed")
  where
    headers :: Http.RequestHeaders
    headers = Wai.requestHeaders req

    checkFetchSite :: Http.RequestHeaders -> Either ApiError ()
    checkFetchSite hdrs = case lookupAll "Sec-Fetch-Site" hdrs of
        [] -> Right ()
        [rawVal] ->
            let lowered :: ByteString
                lowered = asciiLowerBS (trimAsciiBS rawVal)
            in  if lowered == "same-origin" || lowered == "none"
                    then Right ()
                    else Left (forbiddenError "Cross-site or same-site request blocked by Fetch Metadata")
        _ -> Left (forbiddenError "Multiple Sec-Fetch-Site headers are not allowed")

    checkOptionalReferer :: Http.RequestHeaders -> Either ApiError ()
    checkOptionalReferer hdrs = case lookupAll Http.hReferer hdrs of
        [] -> Right ()
        [rawRef] -> do
            refOrigin <- parseRefererOrigin rawRef
            targetVal <- resolveTargetOrigin trustProxyHeaders req
            if refOrigin == targetVal
                then Right ()
                else Left (forbiddenError "Request Referer origin does not match target origin")
        _ -> Left (forbiddenError "Multiple Referer headers are not allowed")

-- | 'validateRequestOrigin' を WAI ミドルウェアとして適用し、不一致時は即座に
-- HTTP 403 Forbidden の構造化 JSON 'ApiError' を返す。
originCheckMiddleware :: Bool -> Wai.Middleware
originCheckMiddleware trustProxyHeaders app req respond =
    case validateRequestOrigin trustProxyHeaders req of
        Right ()     -> app req respond
        Left apiErr  ->
            respond $
                Wai.responseLBS
                    Http.status403
                    [(Http.hContentType, jsonContentType)]
                    (Aeson.encode apiErr)

-- | 読み取り専用エンドポイント向けの Servant Cookie セッション認証ハンドラ。
-- SQLite の読み取り専用トランザクション ('lookupValidSession') のみを用い、
-- 認証済み GET リクエストで書き込みロックを取得しない。
sessionAuthHandler :: StorageConfig -> AuthHandler Wai.Request SessionAuthContext
sessionAuthHandler storageCfg = mkAuthHandler $ \ req -> do
    authSess <- authenticateSessionRequest storageCfg req
    pure SessionAuthContext
        { authenticated = authSess
        }

-- | 状態変更エンドポイント向けの Servant Cookie セッション＋CSRF トークン認証ハンドラ。
-- 先にセッション Cookie を検証 (失敗時 401) し、続いて @X-CSRF-Token@ ヘッダを
-- 定数時間比較 ('verifyCsrfToken') で照合 (失敗時 403) する。
csrfSessionAuthHandler :: StorageConfig -> AuthHandler Wai.Request CsrfVerifiedSession
csrfSessionAuthHandler storageCfg = mkAuthHandler $ \ req -> do
    authSess <- authenticateSessionRequest storageCfg req
    providedCsrf <- extractAndValidateCsrfHeader (Wai.requestHeaders req)
    if verifyCsrfToken authSess.session.csrfToken providedCsrf
        then pure CsrfVerifiedSession
            { authenticated = authSess
            }
        else Servant.throwError (apiErrorToServerError invalidCsrfError)

authenticateSessionRequest :: StorageConfig -> Wai.Request -> Handler AuthenticatedSession
authenticateSessionRequest storageCfg req = do
    rawToken <- extractValidSessionToken (Wai.requestHeaders req)
    eLookup <- liftIO (Storage.withStorage storageCfg (`lookupValidSession` rawToken))
    case eLookup of
        Right (Just authSess) -> pure authSess
        Right Nothing         -> Servant.throwError (apiErrorToServerError unauthorizedError)
        Left storageErr       -> do
            logIfInternalError storageErr
            Servant.throwError (apiErrorToServerError (storageErrorToApiError storageErr))

extractValidSessionToken :: Http.RequestHeaders -> Handler SessionToken
extractValidSessionToken headers =
    case extractSessionTokenFromHeaders headers of
        Nothing -> Servant.throwError (apiErrorToServerError unauthorizedError)
        Just rawText -> case mkSessionToken rawText of
            Left _      -> Servant.throwError (apiErrorToServerError unauthorizedError)
            Right token -> pure token

extractAndValidateCsrfHeader :: Http.RequestHeaders -> Handler CsrfToken
extractAndValidateCsrfHeader headers =
    case lookupAll csrfHeaderName headers of
        [rawBytes] ->
            case TxtEnc.decodeUtf8' (trimAsciiBS rawBytes) of
                Left _ -> Servant.throwError (apiErrorToServerError invalidCsrfError)
                Right tokenText -> case mkCsrfToken tokenText of
                    Left _      -> Servant.throwError (apiErrorToServerError invalidCsrfError)
                    Right token -> pure token
        _ -> Servant.throwError (apiErrorToServerError invalidCsrfError)

data NormalizedOrigin = NormalizedOrigin
    { scheme :: !ByteString
    , host   :: !ByteString
    , port   :: !Int
    } deriving (Eq, Show)

resolveTargetOrigin :: Bool -> Wai.Request -> Either ApiError NormalizedOrigin
resolveTargetOrigin trustProxyHeaders req = do
    let schemeVal :: ByteString
        schemeVal = if isRequestHttps trustProxyHeaders req then "https" else "http"

        mDirectHost :: Maybe ByteString
        mDirectHost =
            Wai.requestHeaderHost req <|> lookup Http.hHost (Wai.requestHeaders req)

        mHostRaw :: Maybe ByteString
        mHostRaw =
            if trustProxyHeaders
                then firstForwardedValue "X-Forwarded-Host" (Wai.requestHeaders req) <|> mDirectHost
                else mDirectHost
    case mHostRaw of
        Nothing      -> Left (forbiddenError "Missing Host header for origin verification")
        Just hostRaw -> parseAuthorityOrigin schemeVal (trimAsciiBS hostRaw)

parseOriginHeader :: ByteString -> Either ApiError NormalizedOrigin
parseOriginHeader raw =
    let trimmed :: ByteString
        trimmed = trimAsciiBS raw
    in  if asciiLowerBS trimmed == "null"
            then Left (forbiddenError "Opaque Origin \"null\" is not allowed")
            else parseSchemeAndAuthority trimmed

parseRefererOrigin :: ByteString -> Either ApiError NormalizedOrigin
parseRefererOrigin raw =
    let trimmed :: ByteString
        trimmed = trimAsciiBS raw
    in  case splitScheme trimmed of
            Nothing -> Left (forbiddenError "Malformed Referer header")
            Just (schemeVal, rest) ->
                let authority :: ByteString
                    authority = BS8.takeWhile (\ ch -> ch /= '/' && ch /= '?' && ch /= '#') rest
                in  parseAuthorityOrigin schemeVal authority

parseSchemeAndAuthority :: ByteString -> Either ApiError NormalizedOrigin
parseSchemeAndAuthority raw =
    case splitScheme raw of
        Nothing -> Left (forbiddenError "Malformed Origin header")
        Just (schemeVal, authority)
            | BS8.any (\ ch -> ch == '/' || ch == '?' || ch == '#') authority ->
                Left (forbiddenError "Origin header must not contain a path or query")
            | otherwise ->
                parseAuthorityOrigin schemeVal authority

splitScheme :: ByteString -> Maybe (ByteString, ByteString)
splitScheme raw =
    let (schemeRaw, rest) = BS.breakSubstring "://" raw
    in  if BS.null rest
            then Nothing
            else
                let lowered :: ByteString
                    lowered = asciiLowerBS schemeRaw
                in  if lowered == "http" || lowered == "https"
                        then Just (lowered, BS.drop 3 rest)
                        else Nothing

parseAuthorityOrigin :: ByteString -> ByteString -> Either ApiError NormalizedOrigin
parseAuthorityOrigin schemeVal authority
    | BS.null authority || BS8.elem '@' authority || BS8.any Char.isSpace authority =
        Left (forbiddenError "Malformed host authority in origin verification")
    | otherwise =
        case BS8.uncons authority of
            Just ('[', _) -> parseIpv6Authority schemeVal authority
            _             -> parseHostPortAuthority schemeVal authority

parseIpv6Authority :: ByteString -> ByteString -> Either ApiError NormalizedOrigin
parseIpv6Authority schemeVal authority =
    let (hostInside, rest) = BS8.break (== ']') authority
    in  case BS8.uncons rest of
            Nothing -> Left (forbiddenError "Unterminated IPv6 literal in host")
            Just (']', afterBracket) ->
                let hostLit :: ByteString
                    hostLit = asciiLowerBS (hostInside <> "]")
                in  if BS.length hostLit <= 2
                        then Left (forbiddenError "Empty IPv6 literal in host")
                        else case BS8.uncons afterBracket of
                            Nothing -> Right NormalizedOrigin
                                { scheme = schemeVal
                                , host   = hostLit
                                , port   = defaultPortForScheme schemeVal
                                }
                            Just (':', portRaw) -> do
                                portVal <- parseStrictPort portRaw
                                Right NormalizedOrigin
                                    { scheme = schemeVal
                                    , host   = hostLit
                                    , port   = portVal
                                    }
                            _ -> Left (forbiddenError "Malformed port suffix after IPv6 host")
            _ -> Left (forbiddenError "Malformed IPv6 host")

parseHostPortAuthority :: ByteString -> ByteString -> Either ApiError NormalizedOrigin
parseHostPortAuthority schemeVal authority =
    let (hostRaw, portPart) = BS8.break (== ':') authority
        loweredHost :: ByteString
        loweredHost = asciiLowerBS hostRaw
    in  if BS.null loweredHost
            then Left (forbiddenError "Empty host in origin verification")
            else case BS8.uncons portPart of
                Nothing -> Right NormalizedOrigin
                    { scheme = schemeVal
                    , host   = loweredHost
                    , port   = defaultPortForScheme schemeVal
                    }
                Just (':', portRaw) -> do
                    portVal <- parseStrictPort portRaw
                    Right NormalizedOrigin
                        { scheme = schemeVal
                        , host   = loweredHost
                        , port   = portVal
                        }
                _ -> Left (forbiddenError "Malformed host port")

parseStrictPort :: ByteString -> Either ApiError Int
parseStrictPort raw
    | BS.null raw || not (BS8.all Char.isDigit raw) =
        Left (forbiddenError "Malformed port in origin verification")
    | otherwise =
        case readMaybe (BS8.unpack raw) :: Maybe Integer of
            Just n | n >= 1 && n <= 65535 -> Right (fromInteger n)
            _                             -> Left (forbiddenError "Port out of range in origin verification")

defaultPortForScheme :: ByteString -> Int
defaultPortForScheme "https" = 443
defaultPortForScheme _       = 80

isSafeHttpMethod :: Http.Method -> Bool
isSafeHttpMethod method =
    method == Http.methodGet
        || method == Http.methodHead
        || method == Http.methodOptions
        || method == Http.methodTrace

firstForwardedValue :: Http.HeaderName -> Http.RequestHeaders -> Maybe ByteString
firstForwardedValue hdrName headers =
    case lookup hdrName headers of
        Nothing -> Nothing
        Just rawVal ->
            let firstEntry :: ByteString
                firstEntry = trimAsciiBS (BS8.takeWhile (/= ',') rawVal)
            in  if BS.null firstEntry
                    then Nothing
                    else Just firstEntry

lookupAll :: forall k v. Eq k => k -> [(k, v)] -> [v]
lookupAll key = foldr collect []
  where
    collect :: (k, v) -> [v] -> [v]
    collect (k, v) acc
        | k == key  = v : acc
        | otherwise = acc

asciiLowerBS :: ByteString -> ByteString
asciiLowerBS = BS8.map toLowerAscii
  where
    toLowerAscii :: Char -> Char
    toLowerAscii ch
        | Char.isAsciiUpper ch = Char.toLower ch
        | otherwise            = ch

trimAsciiBS :: ByteString -> ByteString
trimAsciiBS = BS8.dropWhileEnd isAsciiSpace . BS8.dropWhile isAsciiSpace
  where
    isAsciiSpace :: Char -> Bool
    isAsciiSpace ch = ch == ' ' || ch == '\t' || (Char.isAscii ch && Char.isSpace ch)

unauthorizedError :: ApiError
unauthorizedError = ApiError
    { code    = ErrUnauthorized
    , message = "Authentication required"
    , field   = Nothing
    }

invalidCsrfError :: ApiError
invalidCsrfError = ApiError
    { code    = ErrForbidden
    , message = "Invalid or missing CSRF token"
    , field   = Nothing
    }

forbiddenError :: Text -> ApiError
forbiddenError msg = ApiError
    { code    = ErrForbidden
    , message = msg
    , field   = Nothing
    }

logIfInternalError :: StorageError -> Handler ()
logIfInternalError storageErr = case storageErr of
    UnsupportedSchemaVersion{} -> logStderr storageErr
    CorruptedData{}            -> logStderr storageErr
    SqliteOperationError{}     -> logStderr storageErr
    _                          -> pure ()
  where
    logStderr :: StorageError -> Handler ()
    logStderr err =
        -- CGI 環境では stdout が HTTP レスポンス本体となるため、診断ログは必ず stderr へ出力する
        liftIO (TxtIO.hPutStrLn Sys.stderr ("[tamatsuzura] " <> toText err))
