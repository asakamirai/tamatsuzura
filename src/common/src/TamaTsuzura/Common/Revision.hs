{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module TamaTsuzura.Common.Revision
    ( -- * リビジョン識別子・値型
      RevisionId
    , mkRevisionId
    , ContentHash
    , mkContentHash
    , revisionContentCanonicalText
      -- * リビジョンエンティティ・API DTO
    , RevisionSummary(..)
    , RevisionDetail(..)
    , toRevisionSummary
    , RevisionListResponse(..)
    , RevisionCompareResponse(..)
    , RestoreRevisionRequest(..)
    , defaultRestoreRevisionRequest
      -- re-export
    , RevisionNumber
    , mkRevisionNumber
    , initialRevisionNumber
    , nextRevisionNumber
    , revisionNumberValue
    ) where

import           Data.Aeson                    (FromJSON(..), ToJSON(..))
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types              (Pair)
import qualified Data.Char                     as Char
import           Data.Text                     (Text)
import qualified Data.Text                     as Txt
import           Data.Time.Clock               (UTCTime)
import           TamaTsuzura.Common.Diff       (UnifiedDiff)
import           TamaTsuzura.Common.Document
    ( DocumentBody
    , DocumentFormat
    , DocumentId
    , DocumentTitle
    , RevisionNumber
    , initialRevisionNumber
    , mkRevisionNumber
    , nextRevisionNumber
    , revisionNumberValue
    )
import           TamaTsuzura.Common.Text       (ToText(..))
import           TamaTsuzura.Common.Validation
    ( ValidationError(..)
    , contentHashHexLength
    , maxRevisionIdLength
    )

-- | リビジョンを一意に識別する ID。
-- URL パスや外部キーとして安全に扱える ASCII 英数字・ハイフン・アンダースコア (1〜64 文字) のみを許可し、
-- 不正な識別子文字列の混入を防ぐためコンストラクタを非公開とする。
newtype RevisionId = RevisionId Text
    deriving (Eq, Ord, ToText)

instance Show RevisionId where
    show = Txt.unpack . toText

instance ToJSON RevisionId where
    toJSON = toJSON . toText

instance FromJSON RevisionId where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkRevisionId raw)

-- | 文字列を検証して 'RevisionId' を構築するスマートコンストラクタ。
mkRevisionId :: Text -> Either ValidationError RevisionId
mkRevisionId raw
    | Txt.null raw                       = Left EmptyRevisionId
    | len > maxRevisionIdLength          = Left (RevisionIdTooLong len maxRevisionIdLength)
    | not (Txt.all isRevisionIdChar raw) = Left (InvalidRevisionIdChar raw)
    | otherwise                          = Right (RevisionId raw)
  where
    len :: Int
    len = Txt.length raw

    isRevisionIdChar :: Char -> Bool
    isRevisionIdChar ch =
        (Char.isAscii ch && Char.isAlphaNum ch) || ch == '-' || ch == '_'

-- | リビジョン内容 (タイトル・フォーマット・本文) の SHA-256 ダイジェスト (64 文字の小文字 16 進表現)。
-- 同一内容での不要なリビジョン重複生成の抑止および改ざん・再構成検証に用いる。
newtype ContentHash = ContentHash Text
    deriving (Eq, Ord, ToText)

instance Show ContentHash where
    show = Txt.unpack . toText

instance ToJSON ContentHash where
    toJSON = toJSON . toText

instance FromJSON ContentHash where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkContentHash raw)

-- | 64 文字の小文字 16 進文字列であることを検証して 'ContentHash' を構築する。
mkContentHash :: Text -> Either ValidationError ContentHash
mkContentHash raw
    | Txt.length raw == contentHashHexLength && Txt.all isLowerHex raw = Right (ContentHash raw)
    | otherwise                                                        = Left (InvalidContentHash raw)
  where
    isLowerHex :: Char -> Bool
    isLowerHex ch =
        Char.isDigit ch || (ch >= 'a' && ch <= 'f')

-- | リビジョン履歴の対象フィールド (タイトル・フォーマット・本文) から、
-- ハッシュ計算用の単射的な正規化文字列を生成する。
-- 各可変長フィールドに文字数プレフィックスを付与することで境界の曖昧さによるハッシュ入力衝突を排除する。
revisionContentCanonicalText :: DocumentTitle -> DocumentFormat -> DocumentBody -> Text
revisionContentCanonicalText title format body =
    let titleTxt :: Text
        titleTxt = toText title

        formatTxt :: Text
        formatTxt = toText format

        bodyTxt :: Text
        bodyTxt = toText body
    in  "v1:"
            <> Txt.pack (show (Txt.length titleTxt))
            <> ":"
            <> titleTxt
            <> "\n"
            <> formatTxt
            <> "\n"
            <> bodyTxt

-- | リビジョン履歴一覧 API (@GET /api/documents/{id}/revisions@) で返却する軽量サマリ。
-- 本文や差分テキストを含めないことで、モバイル環境でも履歴一覧を高速に表示できる。
data RevisionSummary = RevisionSummary
    { id               :: !RevisionId
      -- ^ リビジョンの一意な識別子
    , documentId       :: !DocumentId
      -- ^ 対象文書の ID
    , revision         :: !RevisionNumber
      -- ^ 文書内での 1 始まりリビジョン番号
    , parentRevisionId :: !(Maybe RevisionId)
      -- ^ 親リビジョンの ID (初期リビジョン 1 は 'Nothing')
    , title            :: !DocumentTitle
      -- ^ このリビジョン時点の文書タイトル
    , format           :: !DocumentFormat
      -- ^ このリビジョン時点の文書フォーマット
    , contentHash      :: !ContentHash
      -- ^ タイトル・フォーマット・本文の正規化 SHA-256 ダイジェスト
    , createdAt        :: !UTCTime
      -- ^ リビジョン確定日時 (UTC)
    } deriving (Eq, Show)

instance ToText RevisionSummary where
    toText summary =
        toText summary.documentId
            <> "@r"
            <> toText summary.revision
            <> " ("
            <> toText summary.id
            <> ")"

instance ToJSON RevisionSummary where
    toJSON summary = Aeson.object
        [ "id"                 Aeson..= summary.id
        , "document_id"        Aeson..= summary.documentId
        , "revision"           Aeson..= summary.revision
        , "parent_revision_id" Aeson..= summary.parentRevisionId
        , "title"              Aeson..= summary.title
        , "format"             Aeson..= summary.format
        , "content_hash"       Aeson..= summary.contentHash
        , "created_at"         Aeson..= summary.createdAt
        ]

instance FromJSON RevisionSummary where
    parseJSON = Aeson.withObject "RevisionSummary" $ \ obj ->
        RevisionSummary
            <$> obj Aeson..: "id"
            <*> obj Aeson..: "document_id"
            <*> obj Aeson..: "revision"
            <*> obj Aeson..: "parent_revision_id"
            <*> obj Aeson..: "title"
            <*> obj Aeson..: "format"
            <*> obj Aeson..: "content_hash"
            <*> obj Aeson..: "created_at"

-- | 特定リビジョン取得 API (@GET /api/documents/{id}/revisions/{revision}@) で返却する詳細エンティティ。
-- 再構成済みの本文スナップショットと親リビジョンからの unified diff の双方を含む。
data RevisionDetail = RevisionDetail
    { id               :: !RevisionId
      -- ^ リビジョンの一意な識別子
    , documentId       :: !DocumentId
      -- ^ 対象文書の ID
    , revision         :: !RevisionNumber
      -- ^ 文書内での 1 始まりリビジョン番号
    , parentRevisionId :: !(Maybe RevisionId)
      -- ^ 親リビジョンの ID (初期リビジョン 1 は 'Nothing')
    , title            :: !DocumentTitle
      -- ^ このリビジョン時点の文書タイトル
    , body             :: !DocumentBody
      -- ^ このリビジョン時点の再構成済み本文
    , format           :: !DocumentFormat
      -- ^ このリビジョン時点の文書フォーマット
    , diff             :: !UnifiedDiff
      -- ^ 親リビジョンからの本文 unified diff (初期リビジョンは空文書からの差分)
    , contentHash      :: !ContentHash
      -- ^ タイトル・フォーマット・本文の正規化 SHA-256 ダイジェスト
    , createdAt        :: !UTCTime
      -- ^ リビジョン確定日時 (UTC)
    } deriving (Eq, Show)

instance ToText RevisionDetail where
    toText detail = toText (toRevisionSummary detail)

instance ToJSON RevisionDetail where
    toJSON detail = Aeson.object
        [ "id"                 Aeson..= detail.id
        , "document_id"        Aeson..= detail.documentId
        , "revision"           Aeson..= detail.revision
        , "parent_revision_id" Aeson..= detail.parentRevisionId
        , "title"              Aeson..= detail.title
        , "body"               Aeson..= detail.body
        , "format"             Aeson..= detail.format
        , "diff"               Aeson..= detail.diff
        , "content_hash"       Aeson..= detail.contentHash
        , "created_at"         Aeson..= detail.createdAt
        ]

instance FromJSON RevisionDetail where
    parseJSON = Aeson.withObject "RevisionDetail" $ \ obj ->
        RevisionDetail
            <$> obj Aeson..: "id"
            <*> obj Aeson..: "document_id"
            <*> obj Aeson..: "revision"
            <*> obj Aeson..: "parent_revision_id"
            <*> obj Aeson..: "title"
            <*> obj Aeson..: "body"
            <*> obj Aeson..: "format"
            <*> obj Aeson..: "diff"
            <*> obj Aeson..: "content_hash"
            <*> obj Aeson..: "created_at"

-- | 'RevisionDetail' から本文・差分を除いた 'RevisionSummary' を生成する。
toRevisionSummary :: RevisionDetail -> RevisionSummary
toRevisionSummary detail = RevisionSummary
    { id               = detail.id
    , documentId       = detail.documentId
    , revision         = detail.revision
    , parentRevisionId = detail.parentRevisionId
    , title            = detail.title
    , format           = detail.format
    , contentHash      = detail.contentHash
    , createdAt        = detail.createdAt
    }

-- | リビジョン履歴一覧 API (@GET /api/documents/{id}/revisions@) のレスポンス DTO。
data RevisionListResponse = RevisionListResponse
    { revisions :: ![RevisionSummary]
      -- ^ リビジョン番号降順 (新しい順) のサマリ一覧
    , total     :: !Int
      -- ^ @limit@ / @offset@ 適用前の総リビジョン件数 (0 以上)
    } deriving (Eq, Show)

instance ToText RevisionListResponse where
    toText res =
        "RevisionListResponse(count="
            <> Txt.pack (show (length res.revisions))
            <> ", total="
            <> Txt.pack (show res.total)
            <> ")"

instance ToJSON RevisionListResponse where
    toJSON res = Aeson.object
        [ "revisions" Aeson..= res.revisions
        , "total"     Aeson..= res.total
        ]

instance FromJSON RevisionListResponse where
    parseJSON = Aeson.withObject "RevisionListResponse" $ \ obj -> do
        revs <- obj Aeson..: "revisions"
        tot  <- obj Aeson..: "total"
        if tot < 0
            then fail "total must be non-negative"
            else pure RevisionListResponse
                { revisions = revs
                , total     = tot
                }

-- | 任意 2 リビジョン比較 API (@GET /api/documents/{id}/revisions/compare?from={r1}&to={r2}@) のレスポンス DTO。
-- 比較元・比較先の各メタデータサマリと、比較元本文から比較先本文への unified diff を返す。
data RevisionCompareResponse = RevisionCompareResponse
    { documentId    :: !DocumentId
      -- ^ 対象文書 ID
    , fromRevision  :: !RevisionSummary
      -- ^ 比較元リビジョンのサマリ情報
    , toRevision    :: !RevisionSummary
      -- ^ 比較先リビジョンのサマリ情報
    , titleChanged  :: !Bool
      -- ^ 比較元と比較先でタイトルが異なるかどうか
    , formatChanged :: !Bool
      -- ^ 比較元と比較先でフォーマットが異なるかどうか
    , bodyDiff      :: !UnifiedDiff
      -- ^ 比較元本文から比較先本文への unified diff (変更なし時は空文字)
    } deriving (Eq, Show)

instance ToText RevisionCompareResponse where
    toText res =
        toText res.documentId
            <> "@r"
            <> toText res.fromRevision.revision
            <> "..r"
            <> toText res.toRevision.revision

instance ToJSON RevisionCompareResponse where
    toJSON res = Aeson.object
        [ "document_id"    Aeson..= res.documentId
        , "from_revision"  Aeson..= res.fromRevision
        , "to_revision"    Aeson..= res.toRevision
        , "title_changed"  Aeson..= res.titleChanged
        , "format_changed" Aeson..= res.formatChanged
        , "body_diff"      Aeson..= res.bodyDiff
        ]

instance FromJSON RevisionCompareResponse where
    parseJSON = Aeson.withObject "RevisionCompareResponse" $ \ obj ->
        RevisionCompareResponse
            <$> obj Aeson..: "document_id"
            <*> obj Aeson..: "from_revision"
            <*> obj Aeson..: "to_revision"
            <*> obj Aeson..: "title_changed"
            <*> obj Aeson..: "format_changed"
            <*> obj Aeson..: "body_diff"

-- | 過去リビジョン復元 API (@POST /api/documents/{id}/revisions/{revision}/restore@) のリクエスト DTO。
-- @baseRevision@ を指定した場合は現在の最新リビジョン番号と照合し、不一致時は 409 Conflict を返却する。
-- 空オブジェクト @{}@ でのリクエストも許容する。
newtype RestoreRevisionRequest = RestoreRevisionRequest
    { baseRevision :: Maybe RevisionNumber
      -- ^ 楽観的排他制御用の期待リビジョン番号 (任意)
    } deriving (Eq, Show)

-- | 楽観的排他指定なしの既定復元リクエスト。
defaultRestoreRevisionRequest :: RestoreRevisionRequest
defaultRestoreRevisionRequest = RestoreRevisionRequest
    { baseRevision = Nothing
    }

instance ToText RestoreRevisionRequest where
    toText req = case req.baseRevision of
        Nothing  -> "RestoreRevisionRequest(baseRevision=none)"
        Just rev -> "RestoreRevisionRequest(baseRevision=" <> toText rev <> ")"

instance ToJSON RestoreRevisionRequest where
    toJSON req = Aeson.object baseRevisionPair
      where
        baseRevisionPair :: [Pair]
        baseRevisionPair = case req.baseRevision of
            Nothing  -> []
            Just rev -> ["base_revision" Aeson..= rev]

instance FromJSON RestoreRevisionRequest where
    parseJSON = Aeson.withObject "RestoreRevisionRequest" $ \ obj ->
        RestoreRevisionRequest <$> obj Aeson..:? "base_revision"
