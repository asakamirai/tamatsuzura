{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module TamaTsuzura.Common.Document
    ( -- * 文書識別子・基本値型
      DocumentId
    , mkDocumentId
    , DocumentTitle
    , mkDocumentTitle
    , emptyDocumentTitle
    , DocumentBody(..)
    , emptyDocumentBody
    , documentBodyByteLength
    , validateDocumentBody
    , mkDocumentBody
    , DocumentFormat(..)
    , defaultDocumentFormat
    , parseDocumentFormat
      -- * 整理メタデータ (タグ・フォルダ)
    , Tag
    , mkTag
    , TagSet
    , emptyTagSet
    , mkTagSet
    , mkTagSetFromTexts
    , tagSetToList
    , tagSetSize
    , memberTag
    , FolderPath
    , mkFolderPath
    , folderSegments
    , folderParent
    , isFolderPrefixOf
      -- * 文書エンティティ・サマリ
    , DocumentSummary(..)
    , Document(..)
    , toDocumentSummary
    , isTrashed
    , isSummaryTrashed
    ) where

import           Data.Aeson                    (FromJSON(..), ToJSON(..))
import qualified Data.Aeson                    as Aeson
import qualified Data.Char                     as Char
import           Data.List.NonEmpty            (NonEmpty(..))
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                    (isJust)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.String                   (IsString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Txt
import qualified Data.Text.Foreign             as TxtForeign
import           Data.Time.Clock               (UTCTime)
import           TamaTsuzura.Common.Text       (ToText(..))
import           TamaTsuzura.Common.Validation
    ( ValidationError(..)
    , maxDocumentBodyBytes
    , maxDocumentIdLength
    , maxDocumentTitleLength
    , maxFolderDepth
    , maxFolderPathLength
    , maxTagLength
    , maxTagsPerDocument
    )

-- | 文書を一意に識別する ID。
-- URL パスセグメントとして安全に扱える ASCII 英数字・ハイフン・アンダースコア (1〜64 文字) のみを許可し、
-- 不正な ID 文字列の混入を防ぐためコンストラクタを非公開とする。
newtype DocumentId = DocumentId Text
    deriving (Eq, Ord, ToText)

instance Show DocumentId where
    show = Txt.unpack . toText

instance ToJSON DocumentId where
    toJSON = toJSON . toText

instance FromJSON DocumentId where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkDocumentId raw)

-- | 文字列を検証して 'DocumentId' を構築するスマートコンストラクタ。
mkDocumentId :: Text -> Either ValidationError DocumentId
mkDocumentId raw
    | Txt.null raw                        = Left EmptyDocumentId
    | len > maxDocumentIdLength           = Left (DocumentIdTooLong len maxDocumentIdLength)
    | not (Txt.all isDocumentIdChar raw)  = Left (InvalidDocumentIdChar raw)
    | otherwise                           = Right (DocumentId raw)
  where
    len :: Int
    len = Txt.length raw

    isDocumentIdChar :: Char -> Bool
    isDocumentIdChar ch =
        (Char.isAscii ch && Char.isAlphaNum ch) || ch == '-' || ch == '_'

-- | 文書のタイトル。
-- モバイルでの即座のメモ作成を妨げないよう空文字を許容しつつ、
-- 制御文字を含まない単一行テキスト (最大 'maxDocumentTitleLength' 文字) であることを型で保証する。
newtype DocumentTitle = DocumentTitle Text
    deriving (Eq, Ord, ToText)

instance Show DocumentTitle where
    show = Txt.unpack . toText

instance ToJSON DocumentTitle where
    toJSON = toJSON . toText

instance FromJSON DocumentTitle where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkDocumentTitle raw)

-- | 未入力状態を表す空の文書タイトル。UI 側では「無題」等の代替ラベルとして表示する。
emptyDocumentTitle :: DocumentTitle
emptyDocumentTitle = DocumentTitle ""

-- | 制御文字を排除し前後空白を除去して 'DocumentTitle' を構築する。
-- 空文字列は有効なタイトルとして受け入れる。
mkDocumentTitle :: Text -> Either ValidationError DocumentTitle
mkDocumentTitle raw
    | Txt.any Char.isControl raw   = Left TitleContainsControlChar
    | len > maxDocumentTitleLength = Left (TitleTooLong len maxDocumentTitleLength)
    | otherwise                    = Right (DocumentTitle trimmed)
  where
    trimmed :: Text
    trimmed = Txt.strip raw

    len :: Int
    len = Txt.length trimmed

-- | 文書の本文テキスト (UTF-8)。
-- 拡張機能 API ('TamaTsuzura.ExtensionApi.Extension') が純粋な変換関数として本文を扱えるよう
-- コンストラクタを公開し、API 境界でのサイズ上限は 'validateDocumentBody' で明示的に検証する。
newtype DocumentBody = DocumentBody
    { unWrap :: Text
      -- ^ 生の本文テキスト (UTF-8)
    } deriving (Eq, Ord, IsString, Semigroup, Monoid, ToText)

instance Show DocumentBody where
    show = Txt.unpack . toText

instance ToJSON DocumentBody where
    toJSON = toJSON . toText

-- | JSON デコード段階ではサイズ上限を強制せず、API リクエスト検証層で 'validateDocumentBody' を適用する。
-- これによりサーバ側で 400 Bad Request ではなく 413 Payload Too Large を明確に返却できる。
instance FromJSON DocumentBody where
    parseJSON v = DocumentBody <$> parseJSON v

-- | 空の文書本文。
emptyDocumentBody :: DocumentBody
emptyDocumentBody = DocumentBody ""

-- | 文書本文の UTF-8 バイト長を O(1) で取得する。
documentBodyByteLength :: DocumentBody -> Int
documentBodyByteLength = TxtForeign.lengthWord8 . toText

-- | 文書本文の UTF-8 バイト長が 'maxDocumentBodyBytes' 以内であることを検証する。
validateDocumentBody :: DocumentBody -> Either ValidationError DocumentBody
validateDocumentBody body
    | byteLen > maxDocumentBodyBytes = Left (BodyTooLarge byteLen maxDocumentBodyBytes)
    | otherwise                      = Right body
  where
    byteLen :: Int
    byteLen = documentBodyByteLength body

-- | 生の 'Text' をサイズ検証付きで 'DocumentBody' へ変換する。
mkDocumentBody :: Text -> Either ValidationError DocumentBody
mkDocumentBody = validateDocumentBody . DocumentBody

-- | 文書の記法フォーマット種別。
-- クライアント側で Markdown プレビューやシンタックスハイライトをファイル拡張子に依存せず切替えるために保持する。
data DocumentFormat
    = FormatMarkdown
      -- ^ Markdown 文書 (既定値)
    | FormatPlainText
      -- ^ プレーンテキスト文書
    deriving (Eq, Ord, Enum, Bounded)

-- | 新規作成時の既定文書フォーマット ('FormatMarkdown')。
defaultDocumentFormat :: DocumentFormat
defaultDocumentFormat = FormatMarkdown

instance ToText DocumentFormat where
    toText FormatMarkdown  = "markdown"
    toText FormatPlainText = "plain_text"

instance Show DocumentFormat where
    show = Txt.unpack . toText

instance ToJSON DocumentFormat where
    toJSON = toJSON . toText

instance FromJSON DocumentFormat where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (parseDocumentFormat raw)

-- | API / DB の文字列表現から 'DocumentFormat' を復元する。
parseDocumentFormat :: Text -> Either ValidationError DocumentFormat
parseDocumentFormat "markdown"   = Right FormatMarkdown
parseDocumentFormat "plain_text" = Right FormatPlainText
parseDocumentFormat raw          = Left (InvalidDocumentFormat raw)

-- | 文書に付与する分類タグ。
-- 前後空白除去後に空でなく、制御文字を含まない 1〜'maxTagLength' 文字の文字列であることを保証する。
newtype Tag = Tag Text
    deriving (Eq, Ord, ToText)

instance Show Tag where
    show = Txt.unpack . toText

instance ToJSON Tag where
    toJSON = toJSON . toText

instance FromJSON Tag where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkTag raw)

-- | 文字列を検証・正規化して 'Tag' を構築するスマートコンストラクタ。
mkTag :: Text -> Either ValidationError Tag
mkTag raw
    | Txt.any Char.isControl raw = Left (TagContainsControlChar raw)
    | Txt.null trimmed           = Left EmptyTag
    | len > maxTagLength         = Left (TagTooLong len maxTagLength)
    | otherwise                  = Right (Tag trimmed)
  where
    trimmed :: Text
    trimmed = Txt.strip raw

    len :: Int
    len = Txt.length trimmed

-- | 文書に付与される重複なし・昇順正規化済みのタグ集合。
-- 重複の排除と最大数 ('maxTagsPerDocument') 制約を型で保証する。
newtype TagSet = TagSet
    { unWrap :: Set Tag
    } deriving (Eq, Ord)

instance ToText TagSet where
    toText tags = Txt.intercalate ", " (map toText (tagSetToList tags))

instance Show TagSet where
    show = Txt.unpack . toText

instance ToJSON TagSet where
    toJSON = toJSON . tagSetToList

instance FromJSON TagSet where
    parseJSON v = do
        rawTags <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkTagSet rawTags)

-- | 空のタグ集合。
emptyTagSet :: TagSet
emptyTagSet = TagSet Set.empty

-- | タグのリストに対して入力要素数の上限 ('maxTagsPerDocument') を検証した上で、
-- 重複を除去・昇順正規化して 'TagSet' を構築する。
-- OpenAPI の @maxItems: 50@ と一致させ、重複を含む過大な入力配列を早期に拒否する。
mkTagSet :: [Tag] -> Either ValidationError TagSet
mkTagSet rawTags
    | rawCount > maxTagsPerDocument = Left (TooManyTags rawCount maxTagsPerDocument)
    | otherwise                     = Right (TagSet (Set.fromList rawTags))
  where
    rawCount :: Int
    rawCount = length rawTags

-- | 生テキストのリストをすべて検証して 'TagSet' を構築する。
mkTagSetFromTexts :: [Text] -> Either ValidationError TagSet
mkTagSetFromTexts rawTexts = traverse mkTag rawTexts >>= mkTagSet

-- | 'TagSet' に含まれるタグを辞書順のリストとして取り出す。
tagSetToList :: TagSet -> [Tag]
tagSetToList tags = Set.toAscList tags.unWrap

-- | 'TagSet' に含まれるタグ数を返す。
tagSetSize :: TagSet -> Int
tagSetSize tags = Set.size tags.unWrap

-- | 指定したタグが 'TagSet' に含まれるか判定する。
memberTag :: Tag -> TagSet -> Bool
memberTag tag tags = Set.member tag tags.unWrap

-- | 文書を整理するための正規化済み論理フォルダパス (例: @"work/notes"@)。
-- ルート直下は 'Maybe FolderPath' の 'Nothing' で表現し、
-- 本型自体は常に先頭・末尾スラッシュなし、空セグメント・@.@・@..@・制御文字・バックスラッシュなしの
-- 正規化済み相対パスであることを保証する。
newtype FolderPath = FolderPath Text
    deriving (Eq, Ord, ToText)

instance Show FolderPath where
    show = Txt.unpack . toText

instance ToJSON FolderPath where
    toJSON = toJSON . toText

instance FromJSON FolderPath where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkFolderPath raw)

-- | 文字列を検証・正規化して 'FolderPath' を構築するスマートコンストラクタ。
-- 先頭および末尾の @/@ は正規化のため取り除かれるが、空結果や中間の空セグメント (@//@) は拒否する。
mkFolderPath :: Text -> Either ValidationError FolderPath
mkFolderPath raw
    | hasForbiddenChar                  = Left (InvalidFolderPathSegment raw)
    | Txt.null strippedSlashes          = Left EmptyFolderPath
    | not (all isValidSegment segments) = Left (InvalidFolderPathSegment raw)
    | depth > maxFolderDepth            = Left (FolderPathTooDeep depth maxFolderDepth)
    | len > maxFolderPathLength         = Left (FolderPathTooLong len maxFolderPathLength)
    | otherwise                         = Right (FolderPath normalized)
  where
    hasForbiddenChar :: Bool
    hasForbiddenChar = Txt.any Char.isControl raw || Txt.any (== '\\') raw

    strippedSlashes :: Text
    strippedSlashes = Txt.dropWhileEnd (== '/') (Txt.dropWhile (== '/') raw)

    segments :: [Text]
    segments = Txt.splitOn "/" strippedSlashes

    isValidSegment :: Text -> Bool
    isValidSegment seg =
        not (Txt.null seg)
            && seg /= "."
            && seg /= ".."
            && seg == Txt.strip seg

    depth :: Int
    depth = length segments

    normalized :: Text
    normalized = Txt.intercalate "/" segments

    len :: Int
    len = Txt.length normalized

-- | フォルダパスを階層ごとのセグメント列へ分解する。
folderSegments :: FolderPath -> NonEmpty Text
folderSegments fp = case Txt.splitOn "/" (toText fp) of
    seg : rest -> seg :| rest
    -- mkFolderPath が空文字列を拒否しているため空リストには到達しないが、全域性維持のため安全側を返す
    []         -> toText fp :| []

-- | 親フォルダパスを返す。最上位フォルダの場合は 'Nothing' (ルート) を返す。
folderParent :: FolderPath -> Maybe FolderPath
folderParent fp = case reverse (NE.toList (folderSegments fp)) of
    []       -> Nothing
    [_]      -> Nothing
    _ : rest -> Just (FolderPath (Txt.intercalate "/" (reverse rest)))

-- | 第 1 引数のフォルダが第 2 引数のフォルダと一致するか、その祖先フォルダであるかを判定する。
isFolderPrefixOf :: FolderPath -> FolderPath -> Bool
isFolderPrefixOf prefix target =
    prefix == target || Txt.isPrefixOf (toText prefix <> "/") (toText target)

-- | 文書一覧 API で返却する軽量な文書サマリ (本文を除くメタデータ)。
-- 1 文書あたり最大数 MB の本文を一覧取得時に転送しないことで、モバイル環境での帯域と描画負荷を抑える。
data DocumentSummary = DocumentSummary
    { id        :: !DocumentId
      -- ^ 文書の一意な識別子
    , title     :: !DocumentTitle
      -- ^ 文書タイトル (空文字可)
    , format    :: !DocumentFormat
      -- ^ 文書フォーマット種別
    , folder    :: !(Maybe FolderPath)
      -- ^ 所属フォルダパス (ルート直下は 'Nothing')
    , tags      :: !TagSet
      -- ^ 付与されたタグ集合
    , createdAt :: !UTCTime
      -- ^ 作成日時 (UTC)
    , updatedAt :: !UTCTime
      -- ^ 最終更新日時 (UTC)
    , deletedAt :: !(Maybe UTCTime)
      -- ^ 論理削除日時 (UTC)。稼働中文書は 'Nothing'、ゴミ箱内文書は 'Just'
    } deriving (Eq, Show)

instance ToText DocumentSummary where
    toText summary = toText summary.id <> ": " <> toText summary.title

instance ToJSON DocumentSummary where
    toJSON summary = Aeson.object
        [ "id"         Aeson..= summary.id
        , "title"      Aeson..= summary.title
        , "format"     Aeson..= summary.format
        , "folder"     Aeson..= summary.folder
        , "tags"       Aeson..= summary.tags
        , "created_at" Aeson..= summary.createdAt
        , "updated_at" Aeson..= summary.updatedAt
        , "deleted_at" Aeson..= summary.deletedAt
        ]

instance FromJSON DocumentSummary where
    parseJSON = Aeson.withObject "DocumentSummary" $ \ obj ->
        DocumentSummary
            <$> obj Aeson..: "id"
            <*> obj Aeson..: "title"
            <*> obj Aeson..: "format"
            <*> obj Aeson..: "folder"
            <*> obj Aeson..: "tags"
            <*> obj Aeson..: "created_at"
            <*> obj Aeson..: "updated_at"
            <*> obj Aeson..: "deleted_at"

-- | 本文を含む完全な文書エンティティ。
data Document = Document
    { id        :: !DocumentId
      -- ^ 文書の一意な識別子
    , title     :: !DocumentTitle
      -- ^ 文書タイトル (空文字可)
    , body      :: !DocumentBody
      -- ^ 文書本文 (UTF-8)
    , format    :: !DocumentFormat
      -- ^ 文書フォーマット種別
    , folder    :: !(Maybe FolderPath)
      -- ^ 所属フォルダパス (ルート直下は 'Nothing')
    , tags      :: !TagSet
      -- ^ 付与されたタグ集合
    , createdAt :: !UTCTime
      -- ^ 作成日時 (UTC)
    , updatedAt :: !UTCTime
      -- ^ 最終更新日時 (UTC)
    , deletedAt :: !(Maybe UTCTime)
      -- ^ 論理削除日時 (UTC)。稼働中文書は 'Nothing'、ゴミ箱内文書は 'Just'
    } deriving (Eq, Show)

instance ToText Document where
    toText doc = toText doc.id <> ": " <> toText doc.title

instance ToJSON Document where
    toJSON doc = Aeson.object
        [ "id"         Aeson..= doc.id
        , "title"      Aeson..= doc.title
        , "body"       Aeson..= doc.body
        , "format"     Aeson..= doc.format
        , "folder"     Aeson..= doc.folder
        , "tags"       Aeson..= doc.tags
        , "created_at" Aeson..= doc.createdAt
        , "updated_at" Aeson..= doc.updatedAt
        , "deleted_at" Aeson..= doc.deletedAt
        ]

instance FromJSON Document where
    parseJSON = Aeson.withObject "Document" $ \ obj ->
        Document
            <$> obj Aeson..: "id"
            <*> obj Aeson..: "title"
            <*> obj Aeson..: "body"
            <*> obj Aeson..: "format"
            <*> obj Aeson..: "folder"
            <*> obj Aeson..: "tags"
            <*> obj Aeson..: "created_at"
            <*> obj Aeson..: "updated_at"
            <*> obj Aeson..: "deleted_at"

-- | 完全な 'Document' から本文を除いた 'DocumentSummary' を生成する。
toDocumentSummary :: Document -> DocumentSummary
toDocumentSummary doc = DocumentSummary
    { id        = doc.id
    , title     = doc.title
    , format    = doc.format
    , folder    = doc.folder
    , tags      = doc.tags
    , createdAt = doc.createdAt
    , updatedAt = doc.updatedAt
    , deletedAt = doc.deletedAt
    }

-- | 文書が論理削除済み (ゴミ箱内) かどうかを返す。
isTrashed :: Document -> Bool
isTrashed doc = isJust doc.deletedAt

-- | 文書サマリが論理削除済み (ゴミ箱内) かどうかを返す。
isSummaryTrashed :: DocumentSummary -> Bool
isSummaryTrashed summary = isJust summary.deletedAt
