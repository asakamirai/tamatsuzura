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

showInt :: Int -> Text
showInt = Txt.pack . show
