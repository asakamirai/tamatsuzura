{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module TamaTsuzura.Client.App
    ( ClientState(..)
    , initialClientState
    , applyExtensionById
    ) where

import           Data.List                   (find)
import           TamaTsuzura.Common.Document (DocumentBody(..), DocumentTitle, emptyDocumentBody, emptyDocumentTitle)
import           TamaTsuzura.ExtensionApi    (Extension(..), ExtensionId, ExtensionMetadata(..))

-- | クライアントコアの状態モデル (M0 最小骨格)。
-- 本モジュールは 'tamatsuzura-common' と 'tamatsuzura-extension-api' のみに依存し、
-- 同梱拡張の具体実装には依存しない。
data ClientState = ClientState
    { title      :: !DocumentTitle
      -- ^ 現在編集中の文書タイトル
    , body       :: !DocumentBody
      -- ^ 現在編集中の文書本文
    , extensions :: ![Extension]
      -- ^ 結線層から注入された利用可能な拡張一覧
    }

-- | 拡張一覧を注入して初期クライアント状態を構築する。
initialClientState :: [Extension] -> ClientState
initialClientState exts = ClientState
    { title      = emptyDocumentTitle
    , body       = emptyDocumentBody
    , extensions = exts
    }

-- | 指定した ID の拡張を現在の本文へ適用する。該当拡張が存在しない場合は状態を変更しない。
applyExtensionById :: ExtensionId -> ClientState -> ClientState
applyExtensionById targetId state = case find hasTargetId state.extensions of
    Nothing  -> state
    Just ext -> state { body = ext.transformBuffer state.body }
  where
    hasTargetId :: Extension -> Bool
    hasTargetId ext = ext.metadata.extensionId == targetId
