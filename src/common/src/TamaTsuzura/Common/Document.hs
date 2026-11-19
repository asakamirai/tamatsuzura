{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module TamaTsuzura.Common.Document
    ( DocumentId(..)
    , DocumentTitle(..)
    , DocumentBody(..)
    , HealthStatus(..)
    , healthOk
    ) where

import           Data.Aeson              (FromJSON(..), ToJSON(..))
import qualified Data.Aeson              as Aeson
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text               as Txt
import           TamaTsuzura.Common.Text (ToText(..))

-- | 文書を一意に識別する ID。生文字列との取り違えを防ぐため newtype で表現する。
newtype DocumentId = DocumentId
    { unWrap :: Text
    } deriving (Eq, Ord, IsString, ToText)

instance Show DocumentId where
    show = Txt.unpack . toText

instance ToJSON DocumentId where
    toJSON = toJSON . toText

instance FromJSON DocumentId where
    parseJSON v = DocumentId <$> parseJSON v

-- | 文書のタイトル。
newtype DocumentTitle = DocumentTitle
    { unWrap :: Text
    } deriving (Eq, Ord, IsString, ToText)

instance Show DocumentTitle where
    show = Txt.unpack . toText

instance ToJSON DocumentTitle where
    toJSON = toJSON . toText

instance FromJSON DocumentTitle where
    parseJSON v = DocumentTitle <$> parseJSON v

-- | 文書の本文テキスト (UTF-8)。
newtype DocumentBody = DocumentBody
    { unWrap :: Text
    } deriving (Eq, Ord, IsString, Semigroup, Monoid, ToText)

instance Show DocumentBody where
    show = Txt.unpack . toText

instance ToJSON DocumentBody where
    toJSON = toJSON . toText

instance FromJSON DocumentBody where
    parseJSON v = DocumentBody <$> parseJSON v

-- | ヘルスチェック API (@/api/health@) のレスポンス表現。
-- OpenAPI 仕様 (@docs/api/openapi.yaml@) と一致する JSON 形式を保証する。
newtype HealthStatus = HealthStatus
    { status :: Text
      -- ^ サーバ稼働状態を示すステータス文字列 (正常時は @"ok"@)
    } deriving (Eq)

instance ToText HealthStatus where
    toText health = health.status

instance Show HealthStatus where
    show = Txt.unpack . toText

instance ToJSON HealthStatus where
    toJSON health = Aeson.object
        [ "status" Aeson..= health.status
        ]

instance FromJSON HealthStatus where
    parseJSON = Aeson.withObject "HealthStatus" $ \ obj ->
        HealthStatus <$> obj Aeson..: "status"

-- | 正常稼働時の標準ヘルスチェック値。
healthOk :: HealthStatus
healthOk = HealthStatus
    { status = "ok"
    }
