{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.Storage.Auth
    ( -- * セッション有効期間設定
      SessionTtlSeconds
    , minSessionTtlSeconds
    , maxSessionTtlSeconds
    , defaultSessionTtlSeconds
    , mkSessionTtlSeconds
    , sessionTtlSecondsValue
    , sessionTtlDiffTime
      -- * 永続化ドメインレコード
    , StoredUser(..)
    , StoredSession(..)
    , AuthenticatedSession(..)
      -- * ユーザー永続化操作
    , createUser
    , createUserWith
    , getUserById
    , getUserByUsername
      -- * セッション永続化操作 (CGI 完全互換 SQLite バックエンド)
    , createSession
    , createSessionWith
    , lookupValidSession
    , lookupValidSessionAt
    , touchSessionAt
    , deleteSession
    , deleteSessionByHash
    , deleteUserSessions
    , purgeExpiredSessions
    , purgeExpiredSessionsAt
    ) where

import           Data.Bifunctor                    (first)
import           Data.Text                         (Text)
import qualified Data.Text                         as Txt
import           Data.Time.Clock                   (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import qualified Data.UUID                         as UUID
import qualified Data.UUID.V4                      as UUID4
import           Database.SQLite.Simple            (Only(..))
import qualified Database.SQLite.Simple            as SQLite
import           TamaTsuzura.Common.Auth
    ( CsrfToken
    , DisplayName
    , UserId
    , UserProfile(..)
    , Username
    , mkCsrfToken
    , mkDisplayName
    , mkUserId
    , mkUsername
    )
import           TamaTsuzura.Common.Text           (ToText(..))
import           TamaTsuzura.Common.Validation     (ValidationError)
import           TamaTsuzura.Server.Auth.Crypto
    ( PasswordHash
    , SessionToken
    , SessionTokenHash
    , generateCsrfToken
    , generateSessionToken
    , hashSessionToken
    , mkSessionTokenHash
    , parsePasswordHash
    , passwordHashToText
    )
import           TamaTsuzura.Server.Storage.Schema
    ( StorageHandle(..)
    , catchSqliteErrors
    , formatSqliteUtcTime
    , parseSqliteUtcTime
    , truncateToMicros
    , withImmediateTransaction
    , withReadTransaction
    )
import           TamaTsuzura.Server.Storage.Types  (StorageError(..))

-- | セッションの有効期間 (秒、'minSessionTtlSeconds'〜'maxSessionTtlSeconds')。
-- 0 以下や極端に長いセッション寿命による不正状態を型レベルで防ぐためコンストラクタを非公開とする。
newtype SessionTtlSeconds = SessionTtlSeconds
    { unWrap :: Int
    } deriving (Eq, Ord)

instance ToText SessionTtlSeconds where
    toText ttl = Txt.pack (show ttl.unWrap) <> "s"

instance Show SessionTtlSeconds where
    show = Txt.unpack . toText

-- | セッション有効期間の下限 (60 秒 = 1 分)。
minSessionTtlSeconds :: Int
minSessionTtlSeconds = 60

-- | セッション有効期間の上限 (365 日)。
maxSessionTtlSeconds :: Int
maxSessionTtlSeconds = 365 * 24 * 60 * 60

-- | 既定のセッション有効期間 (7 日 = 604,800 秒)。
defaultSessionTtlSeconds :: SessionTtlSeconds
defaultSessionTtlSeconds = SessionTtlSeconds (7 * 24 * 60 * 60)

-- | 許容範囲内であることを検証して 'SessionTtlSeconds' を構築する。
mkSessionTtlSeconds :: Int -> Either Text SessionTtlSeconds
mkSessionTtlSeconds secs
    | secs < minSessionTtlSeconds || secs > maxSessionTtlSeconds =
        Left
            ( "Session TTL must be between "
                <> Txt.pack (show minSessionTtlSeconds)
                <> " and "
                <> Txt.pack (show maxSessionTtlSeconds)
                <> " seconds, got "
                <> Txt.pack (show secs)
            )
    | otherwise = Right (SessionTtlSeconds secs)

-- | 検証済み 'SessionTtlSeconds' から秒数を取り出す。
sessionTtlSecondsValue :: SessionTtlSeconds -> Int
sessionTtlSecondsValue ttl = ttl.unWrap

-- | 検証済み 'SessionTtlSeconds' を 'NominalDiffTime' へ変換する。
sessionTtlDiffTime :: SessionTtlSeconds -> NominalDiffTime
sessionTtlDiffTime ttl = fromIntegral ttl.unWrap

-- | SQLite @users@ テーブルに保存されたユーザーレコード。
-- 認証検証に必要な 'PasswordHash' を含むため、HTTP レスポンスには必ず @profile@ のみを渡すこと。
data StoredUser = StoredUser
    { profile      :: !UserProfile
      -- ^ クライアントへ公開可能なユーザープロフィール
    , passwordHash :: !PasswordHash
      -- ^ PHC 形式 Argon2id パスワードハッシュ
    , updatedAt    :: !UTCTime
      -- ^ 最終更新日時 (UTC)
    } deriving (Eq, Show)

-- | SQLite @sessions@ テーブルに保存されたセッションレコード。
-- 生セッショントークンは保持せず、SHA-256 ダイジェスト ('SessionTokenHash') のみを保持する。
data StoredSession = StoredSession
    { tokenHash      :: !SessionTokenHash
      -- ^ 生セッショントークンの SHA-256 ダイジェスト (主キー)
    , userId         :: !UserId
      -- ^ 所有ユーザー ID
    , csrfToken      :: !CsrfToken
      -- ^ このセッションに紐づく CSRF 対策トークン
    , createdAt      :: !UTCTime
      -- ^ セッション作成日時 (UTC)
    , expiresAt      :: !UTCTime
      -- ^ セッション失効日時 (UTC、@expiresAt <= now@ で失効とみなす)
    , lastAccessedAt :: !UTCTime
      -- ^ 最終アクセス日時 (UTC)
    } deriving (Eq, Show)

-- | セッション検証に成功した際に返却する認証済みコンテキスト。
data AuthenticatedSession = AuthenticatedSession
    { session :: !StoredSession
      -- ^ 有効なセッションレコード
    , user    :: !UserProfile
      -- ^ セッション所有者の公開プロフィール
    } deriving (Eq, Show)

type UserRow = (Text, Text, Maybe Text, Text, Text, Text)

type SessionJoinRow = (Text, Text, Text, Text, Text, Text, Text, Maybe Text, Text)

-- | 新しい UUIDv4 の 'UserId' と現在時刻を用いてユーザーを新規作成する。
createUser ::
       StorageHandle
    -> Username
    -> Maybe DisplayName
    -> PasswordHash
    -> IO (Either StorageError StoredUser)
createUser handle uname mDisplayName pwHash = do
    rawUuid <- UUID4.nextRandom
    now <- getCurrentTime
    case mkUserId (UUID.toText rawUuid) of
        Left err  -> pure (Left (StorageValidationError err))
        Right uid -> createUserWith handle uid now uname mDisplayName pwHash

-- | 明示的に指定した 'UserId' と作成日時を用いてユーザーを新規作成する。
-- 重複 ID は 'UserIdAlreadyExists'、重複ユーザー名 (小文字正規化済み) は 'UsernameAlreadyExists' を返す。
createUserWith ::
       StorageHandle
    -> UserId
    -> UTCTime
    -> Username
    -> Maybe DisplayName
    -> PasswordHash
    -> IO (Either StorageError StoredUser)
createUserWith handle uid now uname mDisplayName pwHash =
    catchSqliteErrors $
        withImmediateTransaction handle.connection $ do
            idTaken <- userIdExists handle.connection uid
            if idTaken
                then pure (Left (UserIdAlreadyExists uid))
                else do
                    nameTaken <- usernameExists handle.connection uname
                    if nameTaken
                        then pure (Left (UsernameAlreadyExists uname))
                        else do
                            let timestamp :: UTCTime
                                timestamp = truncateToMicros now

                                timestampText :: Text
                                timestampText = formatSqliteUtcTime timestamp

                                stored :: StoredUser
                                stored = StoredUser
                                    { profile      = UserProfile
                                        { id          = uid
                                        , username    = uname
                                        , displayName = mDisplayName
                                        , createdAt   = timestamp
                                        }
                                    , passwordHash = pwHash
                                    , updatedAt    = timestamp
                                    }
                            SQLite.execute
                                handle.connection
                                "INSERT INTO users\
                                \ (id, username, display_name, password_hash, created_at, updated_at)\
                                \ VALUES (?, ?, ?, ?, ?, ?)"
                                ( toText uid
                                , toText uname
                                , fmap toText mDisplayName
                                , passwordHashToText pwHash
                                , timestampText
                                , timestampText
                                )
                            pure (Right stored)

-- | ユーザー ID でユーザーを検索する。存在しない場合は @Right Nothing@ を返す。
getUserById ::
       StorageHandle
    -> UserId
    -> IO (Either StorageError (Maybe StoredUser))
getUserById handle uid =
    catchSqliteErrors $
        withReadTransaction handle.connection $ do
            rows <- SQLite.query
                handle.connection
                "SELECT id, username, display_name, password_hash, created_at, updated_at\
                \ FROM users WHERE id = ?"
                (Only (toText uid))
                :: IO [UserRow]
            pure (decodeOptionalUserRow ("id=" <> toText uid) rows)

-- | 正規化済みユーザー名でユーザーを検索する。存在しない場合は @Right Nothing@ を返す。
getUserByUsername ::
       StorageHandle
    -> Username
    -> IO (Either StorageError (Maybe StoredUser))
getUserByUsername handle uname =
    catchSqliteErrors $
        withReadTransaction handle.connection $ do
            rows <- SQLite.query
                handle.connection
                "SELECT id, username, display_name, password_hash, created_at, updated_at\
                \ FROM users WHERE username = ?"
                (Only (toText uname))
                :: IO [UserRow]
            pure (decodeOptionalUserRow ("username=" <> toText uname) rows)

-- | 暗号乱数による生 'SessionToken' と 'CsrfToken' を発行し、
-- トークンの SHA-256 ダイジェストを SQLite へ永続化する。
-- 返却される生 'SessionToken' は @HttpOnly@ Cookie 設定にのみ使用し、DB・ログへは保存しない。
createSession ::
       StorageHandle
    -> UserId
    -> SessionTtlSeconds
    -> IO (Either StorageError (SessionToken, StoredSession))
createSession handle uid ttl = do
    rawToken <- generateSessionToken
    eCsrf <- generateCsrfToken
    case eCsrf of
        Left err ->
            pure (Left (CorruptedData ("Failed to generate CSRF token: " <> toText err)))
        Right csrf -> do
            now <- getCurrentTime
            let tokenHashVal :: SessionTokenHash
                tokenHashVal = hashSessionToken rawToken

                expiresAtVal :: UTCTime
                expiresAtVal = addUTCTime (sessionTtlDiffTime ttl) now
            eStored <- createSessionWith handle tokenHashVal uid csrf now expiresAtVal
            pure $ case eStored of
                Left err     -> Left err
                Right stored -> Right (rawToken, stored)

-- | 明示的なトークンハッシュ・CSRF トークン・作成/失効日時を指定してセッションを永続化する。
-- 対象ユーザーが存在しない場合は外部キーエラーではなく 'UserNotFound' を返す。
-- 失効日時が作成日時以前の場合は不変条件違反として 'CorruptedData' を返し、不正なセッション永続化を防ぐ。
createSessionWith ::
       StorageHandle
    -> SessionTokenHash
    -> UserId
    -> CsrfToken
    -> UTCTime
    -> UTCTime
    -> IO (Either StorageError StoredSession)
createSessionWith handle tokenHashVal uid csrf now expiresAtRaw =
    let created :: UTCTime
        created = truncateToMicros now

        expires :: UTCTime
        expires = truncateToMicros expiresAtRaw
    in  if expires <= created
            then pure (Left (CorruptedData "Session expiration timestamp must be strictly after creation timestamp"))
            else catchSqliteErrors $
                withImmediateTransaction handle.connection $ do
                    exists <- userIdExists handle.connection uid
                    if not exists
                        then pure (Left (UserNotFound uid))
                        else do
                            let createdText :: Text
                                createdText = formatSqliteUtcTime created

                                expiresText :: Text
                                expiresText = formatSqliteUtcTime expires

                                stored :: StoredSession
                                stored = StoredSession
                                    { tokenHash      = tokenHashVal
                                    , userId         = uid
                                    , csrfToken      = csrf
                                    , createdAt      = created
                                    , expiresAt      = expires
                                    , lastAccessedAt = created
                                    }
                            SQLite.execute
                                handle.connection
                                "INSERT INTO sessions\
                                \ (token_hash, user_id, csrf_token, created_at, expires_at, last_accessed_at)\
                                \ VALUES (?, ?, ?, ?, ?, ?)"
                                ( toText tokenHashVal
                                , toText uid
                                , toText csrf
                                , createdText
                                , expiresText
                                , createdText
                                )
                            pure (Right stored)

-- | Cookie から受け取った生 'SessionToken' を現在時刻で照合し、有効なセッションとユーザーを返す。
-- 読み取り専用トランザクションのみを使用し、認証済み GET リクエストで SQLite 書き込みロックを取得しない。
lookupValidSession ::
       StorageHandle
    -> SessionToken
    -> IO (Either StorageError (Maybe AuthenticatedSession))
lookupValidSession handle rawToken = do
    now <- getCurrentTime
    lookupValidSessionAt handle (hashSessionToken rawToken) now

-- | 指定した基準時刻 @now@ において有効 (@expires_at > now@) なセッションと所有ユーザーを取得する。
-- 未存在または失効済みの場合は @Right Nothing@ を返す。
lookupValidSessionAt ::
       StorageHandle
    -> SessionTokenHash
    -> UTCTime
    -> IO (Either StorageError (Maybe AuthenticatedSession))
lookupValidSessionAt handle tokenHashVal now =
    catchSqliteErrors $
        withReadTransaction handle.connection $ do
            let nowText :: Text
                nowText = formatSqliteUtcTime now
            rows <- SQLite.query
                handle.connection
                "SELECT s.token_hash, s.user_id, s.csrf_token, s.created_at, s.expires_at, s.last_accessed_at,\
                \ u.username, u.display_name, u.created_at\
                \ FROM sessions s\
                \ INNER JOIN users u ON u.id = s.user_id\
                \ WHERE s.token_hash = ? AND s.expires_at > ?"
                (toText tokenHashVal, nowText)
                :: IO [SessionJoinRow]
            pure $ case rows of
                []    -> Right Nothing
                [row] -> Just <$> decodeSessionJoinRow row
                _     -> Left (CorruptedData ("Duplicate primary key for session: " <> toText tokenHashVal))

-- | 有効なセッションの @last_accessed_at@ (および必要に応じて延長後の @expires_at@) を更新する。
-- セッションが存在しないか既に失効済み (@expires_at <= now@) の場合は @Right False@ を返す。
touchSessionAt ::
       StorageHandle
    -> SessionTokenHash
    -> UTCTime
    -> Maybe UTCTime
    -> IO (Either StorageError Bool)
touchSessionAt handle tokenHashVal now mNewExpiresAt =
    let accessed :: UTCTime
        accessed = truncateToMicros now

        mNextExpires :: Maybe UTCTime
        mNextExpires = fmap truncateToMicros mNewExpiresAt
    in  case mNextExpires of
            Just nextExp
                | nextExp <= accessed ->
                    pure (Left (CorruptedData "Extended session expiration timestamp must be strictly after access timestamp"))
            _ ->
                catchSqliteErrors $
                    withImmediateTransaction handle.connection $ do
                        let accessedText :: Text
                            accessedText = formatSqliteUtcTime accessed
                        case mNextExpires of
                            Nothing ->
                                SQLite.execute
                                    handle.connection
                                    "UPDATE sessions SET last_accessed_at = ?\
                                    \ WHERE token_hash = ? AND expires_at > ?"
                                    (accessedText, toText tokenHashVal, accessedText)
                            Just nextExp ->
                                SQLite.execute
                                    handle.connection
                                    "UPDATE sessions SET last_accessed_at = ?, expires_at = ?\
                                    \ WHERE token_hash = ? AND expires_at > ?"
                                    (accessedText, formatSqliteUtcTime nextExp, toText tokenHashVal, accessedText)
                        updatedRows <- SQLite.changes handle.connection
                        pure (Right (updatedRows > 0))

-- | 生の 'SessionToken' に対応するセッションを冪等に削除 (ログアウト) する。
deleteSession ::
       StorageHandle
    -> SessionToken
    -> IO (Either StorageError ())
deleteSession handle rawToken =
    deleteSessionByHash handle (hashSessionToken rawToken)

-- | トークンハッシュに対応するセッションを冪等に削除する。
-- 対象行が既に存在しない場合でもエラーにせず @Right ()@ を返す。
deleteSessionByHash ::
       StorageHandle
    -> SessionTokenHash
    -> IO (Either StorageError ())
deleteSessionByHash handle tokenHashVal =
    catchSqliteErrors $
        withImmediateTransaction handle.connection $ do
            SQLite.execute
                handle.connection
                "DELETE FROM sessions WHERE token_hash = ?"
                (Only (toText tokenHashVal))
            pure (Right ())

-- | 指定ユーザーに属するすべてのセッションを一括失効させ、削除件数を返す。
deleteUserSessions ::
       StorageHandle
    -> UserId
    -> IO (Either StorageError Int)
deleteUserSessions handle uid =
    catchSqliteErrors $
        withImmediateTransaction handle.connection $ do
            SQLite.execute
                handle.connection
                "DELETE FROM sessions WHERE user_id = ?"
                (Only (toText uid))
            deleted <- SQLite.changes handle.connection
            pure (Right deleted)

-- | 現在時刻時点で失効済み (@expires_at <= now@) のセッションを一括削除し、削除件数を返す。
purgeExpiredSessions :: StorageHandle -> IO (Either StorageError Int)
purgeExpiredSessions handle = do
    now <- getCurrentTime
    purgeExpiredSessionsAt handle now

-- | 指定時刻 @now@ 時点で失効済み (@expires_at <= now@) のセッションを一括削除し、削除件数を返す。
-- CGI 環境では常駐バッチを持たないため、ログイン・登録時などの書き込み経路から適宜呼び出す設計とする。
purgeExpiredSessionsAt ::
       StorageHandle
    -> UTCTime
    -> IO (Either StorageError Int)
purgeExpiredSessionsAt handle now =
    catchSqliteErrors $
        withImmediateTransaction handle.connection $ do
            SQLite.execute
                handle.connection
                "DELETE FROM sessions WHERE expires_at <= ?"
                (Only (formatSqliteUtcTime now))
            deleted <- SQLite.changes handle.connection
            pure (Right deleted)

userIdExists :: SQLite.Connection -> UserId -> IO Bool
userIdExists conn uid = do
    rows <- SQLite.query
        conn
        "SELECT 1 FROM users WHERE id = ? LIMIT 1"
        (Only (toText uid))
        :: IO [Only Int]
    pure (not (null rows))

usernameExists :: SQLite.Connection -> Username -> IO Bool
usernameExists conn uname = do
    rows <- SQLite.query
        conn
        "SELECT 1 FROM users WHERE username = ? LIMIT 1"
        (Only (toText uname))
        :: IO [Only Int]
    pure (not (null rows))

decodeOptionalUserRow :: Text -> [UserRow] -> Either StorageError (Maybe StoredUser)
decodeOptionalUserRow _ []          = Right Nothing
decodeOptionalUserRow _ [row]       = Just <$> decodeUserRow row
decodeOptionalUserRow keyDesc _     = Left (CorruptedData ("Duplicate user row for " <> keyDesc))

decodeUserRow :: UserRow -> Either StorageError StoredUser
decodeUserRow (rawId, rawUsername, mRawDisplayName, rawPwHash, rawCreatedAt, rawUpdatedAt) = do
    uid          <- mapValidationToCorrupted "users.id" (mkUserId rawId)
    uname        <- decodeCanonicalUsername rawUsername
    mDisplayName <- traverse (mapValidationToCorrupted "users.display_name" . mkDisplayName) mRawDisplayName
    pwHash       <- first (\ err -> CorruptedData ("Invalid users.password_hash in database: " <> toText err)) (parsePasswordHash rawPwHash)
    createdAt    <- parseSqliteUtcTime rawCreatedAt
    updatedAt    <- parseSqliteUtcTime rawUpdatedAt
    Right StoredUser
        { profile      = UserProfile
            { id          = uid
            , username    = uname
            , displayName = mDisplayName
            , createdAt   = createdAt
            }
        , passwordHash = pwHash
        , updatedAt    = updatedAt
        }

decodeSessionJoinRow :: SessionJoinRow -> Either StorageError AuthenticatedSession
decodeSessionJoinRow
    ( rawTokenHash
    , rawUserId
    , rawCsrfToken
    , rawSessionCreatedAt
    , rawSessionExpiresAt
    , rawLastAccessedAt
    , rawUsername
    , mRawDisplayName
    , rawUserCreatedAt
    ) = do
    tokenHashVal     <- first (\ err -> CorruptedData ("Invalid sessions.token_hash in database: " <> err)) (mkSessionTokenHash rawTokenHash)
    uid              <- mapValidationToCorrupted "sessions.user_id" (mkUserId rawUserId)
    csrf             <- mapValidationToCorrupted "sessions.csrf_token" (mkCsrfToken rawCsrfToken)
    sessionCreatedAt <- parseSqliteUtcTime rawSessionCreatedAt
    sessionExpiresAt <- parseSqliteUtcTime rawSessionExpiresAt
    lastAccessedAt   <- parseSqliteUtcTime rawLastAccessedAt
    uname            <- decodeCanonicalUsername rawUsername
    mDisplayName     <- traverse (mapValidationToCorrupted "users.display_name" . mkDisplayName) mRawDisplayName
    userCreatedAt    <- parseSqliteUtcTime rawUserCreatedAt
    Right AuthenticatedSession
        { session = StoredSession
            { tokenHash      = tokenHashVal
            , userId         = uid
            , csrfToken      = csrf
            , createdAt      = sessionCreatedAt
            , expiresAt      = sessionExpiresAt
            , lastAccessedAt = lastAccessedAt
            }
        , user    = UserProfile
            { id          = uid
            , username    = uname
            , displayName = mDisplayName
            , createdAt   = userCreatedAt
            }
        }

decodeCanonicalUsername :: Text -> Either StorageError Username
decodeCanonicalUsername rawUsername = do
    uname <- mapValidationToCorrupted "users.username" (mkUsername rawUsername)
    if toText uname == rawUsername
        then Right uname
        else Left (CorruptedData ("Non-canonical users.username in database: " <> rawUsername))

mapValidationToCorrupted :: Text -> Either ValidationError a -> Either StorageError a
mapValidationToCorrupted columnName =
    first (\ err -> CorruptedData ("Invalid " <> columnName <> " in database: " <> toText err))
