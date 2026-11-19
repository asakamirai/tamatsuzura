{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module TamaTsuzura.ExtensionApi
    ( ExtensionId(..)
    , ExtensionName(..)
    , ExtensionMetadata(..)
    , Extension(..)
    ) where

import           Data.String                 (IsString)
import           Data.Text                   (Text)
import qualified Data.Text                   as Txt
import           TamaTsuzura.Common.Document (DocumentBody)
import           TamaTsuzura.Common.Text     (ToText(..))

-- | 拡張機能を一意に識別する ID。
newtype ExtensionId = ExtensionId
    { unWrap :: Text
    } deriving (Eq, Ord, IsString, ToText)

instance Show ExtensionId where
    show = Txt.unpack . toText

-- | UI 上に表示する拡張機能の名称。
newtype ExtensionName = ExtensionName
    { unWrap :: Text
    } deriving (Eq, Ord, IsString, ToText)

instance Show ExtensionName where
    show = Txt.unpack . toText

-- | 拡張機能の基本メタデータ。
data ExtensionMetadata = ExtensionMetadata
    { extensionId :: !ExtensionId
      -- ^ 拡張機能の一意な識別子
    , name        :: !ExtensionName
      -- ^ ユーザー向け表示名
    , description :: !Text
      -- ^ 拡張機能の概要説明
    } deriving (Eq)

instance ToText ExtensionMetadata where
    toText meta = toText meta.extensionId <> ": " <> toText meta.name

instance Show ExtensionMetadata where
    show = Txt.unpack . toText

-- | 拡張機能の基本契約。
-- M5 の同梱拡張および将来の外部 Wasm 拡張の双方で共有できるよう、
-- 純粋なバッファ変換関数としてインターフェースを抽象化する。
data Extension = Extension
    { metadata        :: !ExtensionMetadata
      -- ^ 拡張機能の識別情報と説明
    , transformBuffer :: !(DocumentBody -> DocumentBody)
      -- ^ エディタ本文に対する純粋な変換処理
    }
