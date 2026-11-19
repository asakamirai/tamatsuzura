{-# LANGUAGE DefaultSignatures #-}

module TamaTsuzura.Common.Text
    ( ToText(..)
    ) where

import           Data.Coerce (Coercible, coerce)
import           Data.Text   (Text)

-- | 値を表示・シリアライズ用の 'Text' へ変換するための型クラス。
-- newtype に対する変換定義の重複を避けるため、'Coercible' による既定実装を提供する。
class ToText a where
    -- | 値を 'Text' 表現へ変換する。
    toText :: a -> Text
    default toText :: Coercible a Text => a -> Text
    toText = coerce

instance ToText Text where
    toText = id
