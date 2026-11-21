{-# LANGUAGE OverloadedStrings #-}

module TamaTsuzura.Common.Validation
    ( -- * ドメイン制約定数
      maxDocumentIdLength
    , maxDocumentTitleLength
    , maxDocumentBodyBytes
    , maxTagLength
    , maxTagsPerDocument
    , maxFolderPathLength
    , maxFolderDepth
    , maxSearchQueryLength
    , minListLimit
    , maxListLimit
    , defaultListLimit
    , maxUserIdLength
    , minUsernameLength
    , maxUsernameLength
    , maxDisplayNameLength
    , minPasswordLength
    , maxPasswordBytes
    , minCsrfTokenLength
    , maxCsrfTokenLength
    , maxRevisionIdLength
    , minRevisionNumber
    , contentHashHexLength
      -- * バリデーションエラー型
    , ValidationError(..)
    , validationErrorField
    ) where

import           Data.Text               (Text)
import qualified Data.Text               as Txt
import           TamaTsuzura.Common.Text (ToText(..))

-- | 文書 ID の最大文字数 (UUID 36 文字や将来的なプレフィックス付き ID を収容する上限)。
maxDocumentIdLength :: Int
maxDocumentIdLength = 64

-- | 文書タイトルの最大文字数。
maxDocumentTitleLength :: Int
maxDocumentTitleLength = 255

-- | 文書本文の最大バイト数 (UTF-8 換算で 5 MiB)。
-- 想定規模である「1 文書あたり最大数 MB 程度」を境界値として強制する。
maxDocumentBodyBytes :: Int
maxDocumentBodyBytes = 5 * 1024 * 1024

-- | 単一タグの最大文字数。
maxTagLength :: Int
maxTagLength = 64

-- | 1 文書に付与できるタグの最大数。
maxTagsPerDocument :: Int
maxTagsPerDocument = 50

-- | 正規化済みフォルダパスの最大文字数。
maxFolderPathLength :: Int
maxFolderPathLength = 255

-- | フォルダ階層の最大ネスト深さ (セグメント数)。
maxFolderDepth :: Int
maxFolderDepth = 10

-- | 文書検索クエリ (@q@) の最大文字数。
maxSearchQueryLength :: Int
maxSearchQueryLength = 200

-- | 一覧取得件数上限 (@limit@) の最小値。
minListLimit :: Int
minListLimit = 1

-- | 一覧取得件数上限 (@limit@) の最大値。
maxListLimit :: Int
maxListLimit = 200

-- | 一覧取得件数上限 (@limit@) の既定値。
defaultListLimit :: Int
defaultListLimit = 50

-- | ユーザー ID の最大文字数 (UUIDv4 36 文字や将来のプレフィックス付き ID を収容する上限)。
maxUserIdLength :: Int
maxUserIdLength = 64

-- | ユーザー名の最小文字数。
minUsernameLength :: Int
minUsernameLength = 3

-- | ユーザー名の最大文字数。
maxUsernameLength :: Int
maxUsernameLength = 32

-- | 任意の表示名の最大文字数。
maxDisplayNameLength :: Int
maxDisplayNameLength = 64

-- | 新規登録時に要求するパスワードの最小文字数。
minPasswordLength :: Int
minPasswordLength = 8

-- | パスワードの最大 UTF-8 バイト長 (Argon2id 計算による DoS を防ぐための上限)。
maxPasswordBytes :: Int
maxPasswordBytes = 128

-- | CSRF トークンの最小文字数 (256 bit エントロピーの Base64URL 等を保証)。
minCsrfTokenLength :: Int
minCsrfTokenLength = 32

-- | CSRF トークンの最大文字数。
maxCsrfTokenLength :: Int
maxCsrfTokenLength = 128

-- | リビジョン ID の最大文字数 (UUIDv4 36 文字や将来のプレフィックス付き ID を収容する上限)。
maxRevisionIdLength :: Int
maxRevisionIdLength = 64

-- | リビジョン番号の最小値 (1 始まりの単調増加連番)。
minRevisionNumber :: Int
minRevisionNumber = 1

-- | コンテンツハッシュ (SHA-256 小文字 16 進表現) の固定文字数。
contentHashHexLength :: Int
contentHashHexLength = 64

-- | 共通ドメイン型および API 入力パラメータの検証失敗理由。
-- 例外ではなく戻り値 ('Either') として扱うため、呼び出し側が機械的にエラー分類できる ADT として定義する。
data ValidationError
    = EmptyDocumentId
      -- ^ 文書 ID が空文字列である
    | DocumentIdTooLong !Int !Int
      -- ^ 文書 ID が上限文字数を超えている (実際値, 上限)
    | InvalidDocumentIdChar !Text
      -- ^ 文書 ID に URL セーフな ASCII 英数字・ハイフン・アンダースコア以外の文字が含まれている
    | TitleTooLong !Int !Int
      -- ^ タイトルが上限文字数を超えている (実際値, 上限)
    | TitleContainsControlChar
      -- ^ タイトルに改行・タブ・NUL などの制御文字が含まれている
    | BodyTooLarge !Int !Int
      -- ^ 本文の UTF-8 バイト長が上限を超えている (実際値, 上限)
    | InvalidDocumentFormat !Text
      -- ^ 未知の文書フォーマット識別子が指定された
    | EmptyTag
      -- ^ 前後空白除去後のタグ文字列が空である
    | TagTooLong !Int !Int
      -- ^ タグが上限文字数を超えている (実際値, 上限)
    | TagContainsControlChar !Text
      -- ^ タグに制御文字が含まれている
    | TooManyTags !Int !Int
      -- ^ 1 文書あたりのタグ数上限を超えている (実際値, 上限)
    | EmptyFolderPath
      -- ^ フォルダパスが空またはスラッシュのみである (ルートは 'Nothing' で表現する)
    | FolderPathTooLong !Int !Int
      -- ^ フォルダパスが上限文字数を超えている (実際値, 上限)
    | FolderPathTooDeep !Int !Int
      -- ^ フォルダ階層の深さが上限を超えている (実際値, 上限)
    | InvalidFolderPathSegment !Text
      -- ^ フォルダパスに禁止文字 (制御文字・バックスラッシュ) や不正セグメント (空・"."・".."・前後空白) が含まれている
    | InvalidDocumentStatusFilter !Text
      -- ^ 未知のステータスフィルタ識別子が指定された
    | SearchQueryTooLong !Int !Int
      -- ^ 検索クエリが上限文字数を超えている (実際値, 上限)
    | SearchQueryContainsControlChar
      -- ^ 検索クエリに制御文字が含まれている
    | ListLimitOutOfRange !Int !Int !Int
      -- ^ 一覧取得件数上限が許容範囲外である (実際値, 下限, 上限)
    | NegativeListOffset !Int
      -- ^ 一覧取得オフセットが負数である
    | EmptyUserId
      -- ^ ユーザー ID が空文字列である
    | UserIdTooLong !Int !Int
      -- ^ ユーザー ID が上限文字数を超えている (実際値, 上限)
    | InvalidUserIdChar !Text
      -- ^ ユーザー ID に URL セーフな ASCII 英数字・ハイフン・アンダースコア以外の文字が含まれている
    | UsernameTooShort !Int !Int
      -- ^ ユーザー名が下限文字数未満である (実際値, 下限)
    | UsernameTooLong !Int !Int
      -- ^ ユーザー名が上限文字数を超えている (実際値, 上限)
    | InvalidUsernameFormat !Text
      -- ^ ユーザー名が許可書式 (ASCII 英数字で始まり英数字・ハイフン・アンダースコアのみ) に違反している
    | EmptyDisplayName
      -- ^ 前後空白除去後の表示名が空である (未設定は 'Nothing' で表現する)
    | DisplayNameTooLong !Int !Int
      -- ^ 表示名が上限文字数を超えている (実際値, 上限)
    | DisplayNameContainsControlChar
      -- ^ 表示名に制御文字が含まれている
    | EmptyPassword
      -- ^ パスワードが空文字列である
    | PasswordTooShort !Int !Int
      -- ^ パスワードが下限文字数未満である (実際値, 下限)
    | PasswordTooLong !Int !Int
      -- ^ パスワードの UTF-8 バイト長が上限を超えている (実際値, 上限)
    | PasswordContainsNullChar
      -- ^ パスワードに NUL 文字が含まれている
    | InvalidCsrfToken
      -- ^ CSRF トークンの書式または長さが不正である
    | EmptyRevisionId
      -- ^ リビジョン ID が空文字列である
    | RevisionIdTooLong !Int !Int
      -- ^ リビジョン ID が上限文字数を超えている (実際値, 上限)
    | InvalidRevisionIdChar !Text
      -- ^ リビジョン ID に URL セーフな ASCII 英数字・ハイフン・アンダースコア以外の文字が含まれている
    | InvalidRevisionNumber !Int
      -- ^ リビジョン番号が 1 未満である (実際値)
    | InvalidContentHash !Text
      -- ^ コンテンツハッシュが 64 文字の小文字 16 進文字列 (SHA-256) ではない
    deriving (Eq)

instance ToText ValidationError where
    toText EmptyDocumentId =
        "Document ID must not be empty"
    toText (DocumentIdTooLong actual limit) =
        "Document ID length " <> showInt actual <> " exceeds maximum " <> showInt limit
    toText (InvalidDocumentIdChar raw) =
        "Document ID contains invalid characters: " <> raw
    toText (TitleTooLong actual limit) =
        "Document title length " <> showInt actual <> " exceeds maximum " <> showInt limit
    toText TitleContainsControlChar =
        "Document title must not contain control characters"
    toText (BodyTooLarge actual limit) =
        "Document body size " <> showInt actual <> " bytes exceeds maximum " <> showInt limit <> " bytes"
    toText (InvalidDocumentFormat raw) =
        "Invalid document format: " <> raw <> " (expected \"markdown\" or \"plain_text\")"
    toText EmptyTag =
        "Tag must not be empty"
    toText (TagTooLong actual limit) =
        "Tag length " <> showInt actual <> " exceeds maximum " <> showInt limit
    toText (TagContainsControlChar raw) =
        "Tag must not contain control characters: " <> raw
    toText (TooManyTags actual limit) =
        "Tag count " <> showInt actual <> " exceeds maximum " <> showInt limit
    toText EmptyFolderPath =
        "Folder path must not be empty (use null for root)"
    toText (FolderPathTooLong actual limit) =
        "Folder path length " <> showInt actual <> " exceeds maximum " <> showInt limit
    toText (FolderPathTooDeep actual limit) =
        "Folder depth " <> showInt actual <> " exceeds maximum " <> showInt limit
    toText (InvalidFolderPathSegment raw) =
        "Invalid folder path: " <> raw
    toText (InvalidDocumentStatusFilter raw) =
        "Invalid status filter: " <> raw <> " (expected \"active\", \"trashed\", or \"all\")"
    toText (SearchQueryTooLong actual limit) =
        "Search query length " <> showInt actual <> " exceeds maximum " <> showInt limit
    toText SearchQueryContainsControlChar =
        "Search query must not contain control characters"
    toText (ListLimitOutOfRange actual low high) =
        "List limit " <> showInt actual <> " is out of range (" <> showInt low <> ".." <> showInt high <> ")"
    toText (NegativeListOffset actual) =
        "List offset must be non-negative, got " <> showInt actual
    toText EmptyUserId =
        "User ID must not be empty"
    toText (UserIdTooLong actual limit) =
        "User ID length " <> showInt actual <> " exceeds maximum " <> showInt limit
    toText (InvalidUserIdChar raw) =
        "User ID contains invalid characters: " <> raw
    toText (UsernameTooShort actual minLen) =
        "Username length " <> showInt actual <> " is shorter than minimum " <> showInt minLen
    toText (UsernameTooLong actual maxLen) =
        "Username length " <> showInt actual <> " exceeds maximum " <> showInt maxLen
    toText (InvalidUsernameFormat raw) =
        "Invalid username format: " <> raw <> " (must start with ASCII letter/digit and use only ASCII letters, digits, '_', or '-')"
    toText EmptyDisplayName =
        "Display name must not be empty (use null when unset)"
    toText (DisplayNameTooLong actual limit) =
        "Display name length " <> showInt actual <> " exceeds maximum " <> showInt limit
    toText DisplayNameContainsControlChar =
        "Display name must not contain control characters"
    toText EmptyPassword =
        "Password must not be empty"
    toText (PasswordTooShort actual minLen) =
        "Password length " <> showInt actual <> " is shorter than minimum " <> showInt minLen
    toText (PasswordTooLong actual maxBytes) =
        "Password byte length " <> showInt actual <> " exceeds maximum " <> showInt maxBytes <> " bytes"
    toText PasswordContainsNullChar =
        "Password must not contain NUL characters"
    toText InvalidCsrfToken =
        "CSRF token is malformed"
    toText EmptyRevisionId =
        "Revision ID must not be empty"
    toText (RevisionIdTooLong actual limit) =
        "Revision ID length " <> showInt actual <> " exceeds maximum " <> showInt limit
    toText (InvalidRevisionIdChar raw) =
        "Revision ID contains invalid characters: " <> raw
    toText (InvalidRevisionNumber actual) =
        "Revision number must be at least " <> showInt minRevisionNumber <> ", got " <> showInt actual
    toText (InvalidContentHash raw) =
        "Content hash must be a " <> showInt contentHashHexLength <> "-character lowercase hexadecimal SHA-256 digest: " <> raw

instance Show ValidationError where
    show = Txt.unpack . toText

-- | バリデーションエラーに対応する API フィールド名・クエリパラメータ名を返す。
-- タグ系エラーはリクエストボディの複数形フィールド名 @"tags"@ を返すため、
-- 単数形クエリパラメータ (@GET /api/documents?tag=...@) の検証時に利用する場合は
-- 呼び出し側で @field@ を @"tag"@ へ上書きすること。
validationErrorField :: ValidationError -> Text
validationErrorField EmptyDocumentId                = "id"
validationErrorField DocumentIdTooLong{}            = "id"
validationErrorField InvalidDocumentIdChar{}        = "id"
validationErrorField TitleTooLong{}                 = "title"
validationErrorField TitleContainsControlChar       = "title"
validationErrorField BodyTooLarge{}                 = "body"
validationErrorField InvalidDocumentFormat{}        = "format"
validationErrorField EmptyTag                       = "tags"
validationErrorField TagTooLong{}                   = "tags"
validationErrorField TagContainsControlChar{}       = "tags"
validationErrorField TooManyTags{}                  = "tags"
validationErrorField EmptyFolderPath                = "folder"
validationErrorField FolderPathTooLong{}            = "folder"
validationErrorField FolderPathTooDeep{}            = "folder"
validationErrorField InvalidFolderPathSegment{}     = "folder"
validationErrorField InvalidDocumentStatusFilter{}  = "status"
validationErrorField SearchQueryTooLong{}           = "q"
validationErrorField SearchQueryContainsControlChar = "q"
validationErrorField ListLimitOutOfRange{}          = "limit"
validationErrorField NegativeListOffset{}           = "offset"
validationErrorField EmptyUserId                    = "user_id"
validationErrorField UserIdTooLong{}                = "user_id"
validationErrorField InvalidUserIdChar{}            = "user_id"
validationErrorField UsernameTooShort{}             = "username"
validationErrorField UsernameTooLong{}              = "username"
validationErrorField InvalidUsernameFormat{}        = "username"
validationErrorField EmptyDisplayName               = "display_name"
validationErrorField DisplayNameTooLong{}           = "display_name"
validationErrorField DisplayNameContainsControlChar = "display_name"
validationErrorField EmptyPassword                  = "password"
validationErrorField PasswordTooShort{}             = "password"
validationErrorField PasswordTooLong{}              = "password"
validationErrorField PasswordContainsNullChar       = "password"
validationErrorField InvalidCsrfToken               = "csrf_token"
validationErrorField EmptyRevisionId                = "revision_id"
validationErrorField RevisionIdTooLong{}            = "revision_id"
validationErrorField InvalidRevisionIdChar{}        = "revision_id"
validationErrorField InvalidRevisionNumber{}        = "revision"
validationErrorField InvalidContentHash{}           = "content_hash"

showInt :: Int -> Text
showInt = Txt.pack . show
