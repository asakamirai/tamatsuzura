{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module TamaTsuzura.Common.Auth
    ( -- * ユーザー識別子・アカウント属性型
      UserId
    , mkUserId
    , Username
    , mkUsername
    , DisplayName
    , mkDisplayName
    , Password
    , mkPassword
    , validateNewPassword
    , passwordCharLength
    , passwordByteLength
    , unsafePasswordText
    , CsrfToken
    , mkCsrfToken
      -- * 認証 API DTO
    , UserProfile(..)
    , RegisterRequest(..)
    , validateRegisterRequest
    , LoginRequest(..)
    , AuthSessionResponse(..)
    ) where

import           Data.Aeson                    (FromJSON(..), ToJSON(..))
import qualified Data.Aeson                    as Aeson
import           Data.Bits                     (xor, (.|.))
import qualified Data.Char                     as Char
import           Data.Text                     (Text)
import qualified Data.Text                     as Txt
import qualified Data.Text.Foreign             as TxtForeign
import           Data.Time.Clock               (UTCTime)
import           TamaTsuzura.Common.Text       (ToText(..))
import           TamaTsuzura.Common.Validation
    ( ValidationError(..)
    , maxCsrfTokenLength
    , maxDisplayNameLength
    , maxPasswordBytes
    , maxUserIdLength
    , maxUsernameLength
    , minCsrfTokenLength
    , minPasswordLength
    , minUsernameLength
    )

-- | ユーザーアカウントを一意に識別する ID。
-- URL パスや外部キーとして安全に扱える ASCII 英数字・ハイフン・アンダースコア (1〜64 文字) のみを許可し、
-- 不正な識別子文字列の混入を防ぐためコンストラクタを非公開とする。
newtype UserId = UserId Text
    deriving (Eq, Ord, ToText)

instance Show UserId where
    show = Txt.unpack . toText

instance ToJSON UserId where
    toJSON = toJSON . toText

instance FromJSON UserId where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkUserId raw)

-- | 文字列を検証して 'UserId' を構築するスマートコンストラクタ。
mkUserId :: Text -> Either ValidationError UserId
mkUserId raw
    | Txt.null raw                   = Left EmptyUserId
    | len > maxUserIdLength          = Left (UserIdTooLong len maxUserIdLength)
    | not (Txt.all isUrlSafeAsc raw) = Left (InvalidUserIdChar raw)
    | otherwise                      = Right (UserId raw)
  where
    len :: Int
    len = Txt.length raw

-- | ログイン・アカウント識別に用いる正規化済みユーザー名。
-- モバイル入力時の意図しない先頭大文字化や前後空白を吸収しつつ、大文字・小文字を区別しない
-- 一意性を型と DB の双方で保証するため、構築時に前後空白除去＋ASCII 小文字正規化を行う。
-- 書式は ASCII 英数字で始まる 3〜32 文字の ASCII 小文字英数字・@_@・@-@ のみを許可する。
newtype Username = Username Text
    deriving (Eq, Ord, ToText)

instance Show Username where
    show = Txt.unpack . toText

instance ToJSON Username where
    toJSON = toJSON . toText

instance FromJSON Username where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkUsername raw)

-- | 入力文字列を検証・小文字正規化して 'Username' を構築するスマートコンストラクタ。
mkUsername :: Text -> Either ValidationError Username
mkUsername raw
    | Txt.any Char.isControl raw                 = Left (InvalidUsernameFormat raw)
    | len < minUsernameLength                    = Left (UsernameTooShort len minUsernameLength)
    | len > maxUsernameLength                    = Left (UsernameTooLong len maxUsernameLength)
    | not (isValidNormalizedUsername normalized) = Left (InvalidUsernameFormat raw)
    | otherwise                                  = Right (Username normalized)
  where
    trimmed :: Text
    trimmed = Txt.strip raw

    normalized :: Text
    normalized = Txt.map toLowerAscii trimmed

    len :: Int
    len = Txt.length normalized

    toLowerAscii :: Char -> Char
    toLowerAscii ch
        | Char.isAsciiUpper ch = Char.toLower ch
        | otherwise            = ch

    isValidNormalizedUsername :: Text -> Bool
    isValidNormalizedUsername candidate = case Txt.uncons candidate of
        Nothing         -> False
        Just (hd, rest) ->
            isAsciiLowerOrDigit hd && Txt.all isUsernameBodyChar rest

    isAsciiLowerOrDigit :: Char -> Bool
    isAsciiLowerOrDigit ch = Char.isAsciiLower ch || Char.isDigit ch

    isUsernameBodyChar :: Char -> Bool
    isUsernameBodyChar ch = isAsciiLowerOrDigit ch || ch == '_' || ch == '-'

-- | UI 表示用の任意のアカウント表示名 (日本語・絵文字等の UTF-8 文字を許容)。
-- 未設定状態は @Maybe DisplayName@ の 'Nothing' で表現するため、本型自体は常に
-- 前後空白除去後に空でなく制御文字を含まない 1〜'maxDisplayNameLength' 文字であることを保証する。
newtype DisplayName = DisplayName Text
    deriving (Eq, Ord, ToText)

instance Show DisplayName where
    show = Txt.unpack . toText

instance ToJSON DisplayName where
    toJSON = toJSON . toText

instance FromJSON DisplayName where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkDisplayName raw)

-- | 制御文字を排除し前後空白を除去して 'DisplayName' を構築するスマートコンストラクタ。
mkDisplayName :: Text -> Either ValidationError DisplayName
mkDisplayName raw
    | Txt.any Char.isControl raw = Left DisplayNameContainsControlChar
    | Txt.null trimmed           = Left EmptyDisplayName
    | len > maxDisplayNameLength = Left (DisplayNameTooLong len maxDisplayNameLength)
    | otherwise                  = Right (DisplayName trimmed)
  where
    trimmed :: Text
    trimmed = Txt.strip raw

    len :: Int
    len = Txt.length trimmed

-- | 認証リクエスト境界で扱う生パスワードの不透明ラッパ。
-- ログやエラー出力への偶発的な平文漏洩を防ぐため 'ToText' インスタンスを持たず、
-- 'Show' は常に @\"<redacted>\"@ を返す。
-- 最小文字数ポリシー ('minPasswordLength') は新規登録時の 'validateNewPassword' で強制し、
-- ログイン時の 'FromJSON' / 'mkPassword' では将来のポリシー引き上げ後方互換と
-- DoS 防止 (最大 'maxPasswordBytes' バイト・NUL 禁止・空文字禁止) のみを検証する。
newtype Password = Password Text
    deriving (Eq)

instance Show Password where
    show _ = "<redacted>"

instance ToJSON Password where
    toJSON (Password raw) = toJSON raw

instance FromJSON Password where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkPassword raw)

-- | 生パスワードの基本安全性 (空文字禁止・NUL 禁止・UTF-8 バイト長上限) を検証して 'Password' を構築する。
-- パスワード前後の空白は有意な文字として保持し、トリムしない。
mkPassword :: Text -> Either ValidationError Password
mkPassword raw
    | Txt.any (== '\0') raw      = Left PasswordContainsNullChar
    | Txt.null raw               = Left EmptyPassword
    | byteLen > maxPasswordBytes = Left (PasswordTooLong byteLen maxPasswordBytes)
    | otherwise                  = Right (Password raw)
  where
    byteLen :: Int
    byteLen = TxtForeign.lengthWord8 raw

-- | 新規アカウント登録時のパスワード強度要件 (最小 'minPasswordLength' 文字) を検証する。
validateNewPassword :: Password -> Either ValidationError Password
validateNewPassword pw@(Password raw)
    | charLen < minPasswordLength = Left (PasswordTooShort charLen minPasswordLength)
    | otherwise                   = Right pw
  where
    charLen :: Int
    charLen = Txt.length raw

-- | パスワードの Unicode 文字数を返す。
passwordCharLength :: Password -> Int
passwordCharLength (Password raw) = Txt.length raw

-- | パスワードの UTF-8 バイト長を O(1) で返す。
passwordByteLength :: Password -> Int
passwordByteLength (Password raw) = TxtForeign.lengthWord8 raw

-- | 暗号ハッシュ化などの境界処理専用に生パスワードの 'Text' を取り出す。
-- ログ出力・エラーメッセージ生成などの一般用途では絶対に使用しないこと。
unsafePasswordText :: Password -> Text
unsafePasswordText (Password raw) = raw

-- | セッションに紐づく CSRF 対策トークン (Synchronizer Token Pattern)。
-- HTTP ヘッダ (@X-CSRF-Token@) で安全に転送できるよう、URL セーフな ASCII 文字
-- (@[A-Za-z0-9_-]@, 'minCsrfTokenLength'〜'maxCsrfTokenLength' 文字) のみを許可する。
-- セッション紐づきの秘密値であるため 'Show' は伏せ字 (@\"<CsrfToken>\"@) とし、
-- 'Eq' は短絡評価を避けた全文字 XOR 畳み込みによる定数時間比較を行う。
newtype CsrfToken = CsrfToken Text
    deriving (ToText)

instance Eq CsrfToken where
    CsrfToken a == CsrfToken b = constantTimeAsciiEq a b

instance Show CsrfToken where
    show _ = "<CsrfToken>"

instance ToJSON CsrfToken where
    toJSON = toJSON . toText

instance FromJSON CsrfToken where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkCsrfToken raw)

-- | 文字列を検証して 'CsrfToken' を構築するスマートコンストラクタ。
mkCsrfToken :: Text -> Either ValidationError CsrfToken
mkCsrfToken raw
    | len < minCsrfTokenLength || len > maxCsrfTokenLength = Left InvalidCsrfToken
    | not (Txt.all isUrlSafeAsc raw)                       = Left InvalidCsrfToken
    | otherwise                                            = Right (CsrfToken raw)
  where
    len :: Int
    len = Txt.length raw

-- | クライアントへ公開可能なユーザープロフィール DTO。
-- パスワードハッシュ等の機密情報は型定義自体に含めず、シリアライズ時の誤露出を静的に排除する。
data UserProfile = UserProfile
    { id          :: !UserId
      -- ^ ユーザーの一意な識別子
    , username    :: !Username
      -- ^ 正規化済みログインユーザー名
    , displayName :: !(Maybe DisplayName)
      -- ^ 任意の表示名 (未設定時は 'Nothing')
    , createdAt   :: !UTCTime
      -- ^ アカウント作成日時 (UTC)
    } deriving (Eq, Show)

instance ToText UserProfile where
    toText profile = case profile.displayName of
        Nothing   -> toText profile.username
        Just name -> toText name <> " (" <> toText profile.username <> ")"

instance ToJSON UserProfile where
    toJSON profile = Aeson.object
        [ "id"           Aeson..= profile.id
        , "username"     Aeson..= profile.username
        , "display_name" Aeson..= profile.displayName
        , "created_at"   Aeson..= profile.createdAt
        ]

instance FromJSON UserProfile where
    parseJSON = Aeson.withObject "UserProfile" $ \ obj ->
        UserProfile
            <$> obj Aeson..: "id"
            <*> obj Aeson..: "username"
            <*> obj Aeson..: "display_name"
            <*> obj Aeson..: "created_at"

-- | 新規ユーザー登録 API (@POST /api/auth/register@) のリクエスト DTO。
data RegisterRequest = RegisterRequest
    { username    :: !Username
      -- ^ 希望するユーザー名 (小文字正規化済み)
    , displayName :: !(Maybe DisplayName)
      -- ^ 任意の表示名 (省略時または @null@ は未設定)
    , password    :: !Password
      -- ^ 登録するパスワード ('validateRegisterRequest' で最小長を検証する)
    } deriving (Eq, Show)

instance ToJSON RegisterRequest where
    toJSON req = Aeson.object
        [ "username"     Aeson..= req.username
        , "display_name" Aeson..= req.displayName
        , "password"     Aeson..= req.password
        ]

instance FromJSON RegisterRequest where
    parseJSON = Aeson.withObject "RegisterRequest" $ \ obj ->
        RegisterRequest
            <$> obj Aeson..:  "username"
            <*> obj Aeson..:? "display_name"
            <*> obj Aeson..:  "password"

-- | 新規ユーザー登録リクエストの追加ドメイン制約 (パスワード最小文字数) を検証する。
validateRegisterRequest :: RegisterRequest -> Either ValidationError RegisterRequest
validateRegisterRequest req = do
    _ <- validateNewPassword req.password
    Right req

-- | ログイン API (@POST /api/auth/login@) のリクエスト DTO。
data LoginRequest = LoginRequest
    { username :: !Username
      -- ^ ログインユーザー名 (小文字正規化済み)
    , password :: !Password
      -- ^ 入力パスワード
    } deriving (Eq, Show)

instance ToJSON LoginRequest where
    toJSON req = Aeson.object
        [ "username" Aeson..= req.username
        , "password" Aeson..= req.password
        ]

instance FromJSON LoginRequest where
    parseJSON = Aeson.withObject "LoginRequest" $ \ obj ->
        LoginRequest
            <$> obj Aeson..: "username"
            <*> obj Aeson..: "password"

-- | 登録・ログイン・現在ユーザー取得 API の共通認証セッション応答 DTO。
-- セッション本体は @HttpOnly@ Cookie で転送し、状態変更 API 用の @csrf_token@ と
-- 公開プロフィールだけをレスポンス JSON としてクライアントへ渡す。
data AuthSessionResponse = AuthSessionResponse
    { user      :: !UserProfile
      -- ^ 認証済みユーザーの公開プロフィール
    , csrfToken :: !CsrfToken
      -- ^ 状態変更リクエストの @X-CSRF-Token@ ヘッダに添付する CSRF トークン
    } deriving (Eq, Show)

instance ToText AuthSessionResponse where
    toText res = "AuthSessionResponse(" <> toText res.user <> ")"

instance ToJSON AuthSessionResponse where
    toJSON res = Aeson.object
        [ "user"       Aeson..= res.user
        , "csrf_token" Aeson..= res.csrfToken
        ]

instance FromJSON AuthSessionResponse where
    parseJSON = Aeson.withObject "AuthSessionResponse" $ \ obj ->
        AuthSessionResponse
            <$> obj Aeson..: "user"
            <*> obj Aeson..: "csrf_token"

isUrlSafeAsc :: Char -> Bool
isUrlSafeAsc ch =
    (Char.isAscii ch && Char.isAlphaNum ch) || ch == '-' || ch == '_'

constantTimeAsciiEq :: Text -> Text -> Bool
constantTimeAsciiEq a b =
    Txt.length a == Txt.length b
        && foldl' accumulateDiff 0 (Txt.zip a b) == 0
  where
    accumulateDiff :: Int -> (Char, Char) -> Int
    accumulateDiff acc (x, y) = acc .|. (Char.ord x `xor` Char.ord y)
