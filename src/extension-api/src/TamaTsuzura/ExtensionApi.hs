{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | TamaTsuzura の拡張機能向け公開インターフェース。
-- M5 の同梱拡張および将来の外部 Wasm / Web Worker 分離拡張の双方で後方互換性を保つため、
-- すべての入出力を純粋な値型 ('ExtensionContext' / 'ExtensionResult' / 'ExtensionError') で定義する。
module TamaTsuzura.ExtensionApi
    ( -- * 識別子・バージョン・カテゴリ型
      ExtensionId(..)
    , ExtensionName(..)
    , ExtensionVersion(..)
    , ExtensionCategory(..)
    , CommandId(..)
    , CommandName(..)
      -- * コマンド実行入出力・エラー型
    , ExtensionContext(..)
    , ExtensionResult(..)
    , ExtensionError(..)
      -- * 拡張メタデータ・コマンド・本体定義
    , ExtensionMetadata(..)
    , ExtensionCommandInfo(..)
    , toExtensionCommandInfo
    , ExtensionCommand(..)
    , Extension(..)
      -- * ヘルパ・レジストリ検証
    , defaultExtensionVersion
    , mkSimpleExtension
    , findExtensionCommand
    , runExtensionCommand
    , ExtensionRegistryError(..)
    , validateExtensionRegistry
    ) where

import           Data.List                   (find)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.String                 (IsString)
import           Data.Text                   (Text)
import qualified Data.Text                   as Txt
import           TamaTsuzura.Common.Document (DocumentBody, DocumentFormat(..))
import           TamaTsuzura.Common.Text     (ToText(..))

-- | 拡張機能を一意に識別する ID。
newtype ExtensionId = ExtensionId
    { unWrap :: Text
      -- ^ 拡張 ID 文字列 (例: @"trim-trailing-spaces"@)
    } deriving (Eq, Ord, IsString, ToText)

instance Show ExtensionId where
    show = Txt.unpack . toText

-- | UI 上に表示する拡張機能の名称。
newtype ExtensionName = ExtensionName
    { unWrap :: Text
      -- ^ 拡張表示名
    } deriving (Eq, Ord, IsString, ToText)

instance Show ExtensionName where
    show = Txt.unpack . toText

-- | 拡張機能のセマンティックバージョン表現 (例: @"0.1.0"@)。
newtype ExtensionVersion = ExtensionVersion
    { unWrap :: Text
      -- ^ バージョン文字列
    } deriving (Eq, Ord, IsString, ToText)

instance Show ExtensionVersion where
    show = Txt.unpack . toText

-- | 同梱拡張の既定バージョン (@"0.1.0"@)。
defaultExtensionVersion :: ExtensionVersion
defaultExtensionVersion = ExtensionVersion "0.1.0"

-- | 拡張機能の分類カテゴリ。UI でのグルーピングやフィルタ表示に用いる。
data ExtensionCategory
    = CategoryFormatting
      -- ^ 空白・改行・インデント等の書式整形
    | CategoryMarkdown
      -- ^ Markdown 構造 (見出し・箇条書き・目次等) の整形・補助
    | CategoryTextTransform
      -- ^ 行ソート・重複除去・文字種変換等の汎用テキスト変換
    deriving (Eq, Ord, Enum, Bounded)

instance ToText ExtensionCategory where
    toText CategoryFormatting    = "formatting"
    toText CategoryMarkdown      = "markdown"
    toText CategoryTextTransform = "text_transform"

instance Show ExtensionCategory where
    show = Txt.unpack . toText

-- | 単一拡張内でコマンドを一意に識別する ID。
newtype CommandId = CommandId
    { unWrap :: Text
      -- ^ コマンド ID 文字列 (例: @"format-markdown"@)
    } deriving (Eq, Ord, IsString, ToText)

instance Show CommandId where
    show = Txt.unpack . toText

-- | UI 上に表示する拡張コマンドのラベル。
newtype CommandName = CommandName
    { unWrap :: Text
      -- ^ コマンド表示名
    } deriving (Eq, Ord, IsString, ToText)

instance Show CommandName where
    show = Txt.unpack . toText

-- | 拡張コマンド実行時にエディタから渡される純粋なコンテキスト。
-- 将来の外部 Wasm / Web Worker 境界でのシリアライズを想定し、IO 参照を含めない。
data ExtensionContext = ExtensionContext
    { body   :: !DocumentBody
      -- ^ 現在のエディタ本文
    , format :: !DocumentFormat
      -- ^ 現在の文書フォーマット (Markdown / プレーンテキスト)
    } deriving (Eq, Show)

-- | 拡張コマンドの正常実行結果。
data ExtensionResult = ExtensionResult
    { nextBody :: !DocumentBody
      -- ^ 変換後のエディタ本文
    , summary  :: !(Maybe Text)
      -- ^ ユーザー向け通知に表示する任意の実行サマリ (例: @"3 行を整形しました"@)
    } deriving (Eq, Show)

-- | 拡張コマンド実行時の構造化エラー。
data ExtensionError
    = ExtensionCommandNotFound !ExtensionId !CommandId
      -- ^ 指定されたコマンド ID が対象拡張に存在しない
    | ExtensionUnsupportedFormat !DocumentFormat !DocumentFormat
      -- ^ 対象フォーマット不一致 (期待フォーマット, 実際のフォーマット)
    | ExtensionCommandFailed !Text
      -- ^ 拡張固有の変換失敗理由
    deriving (Eq)

instance ToText ExtensionError where
    toText (ExtensionCommandNotFound extId cmdId) =
        "Command not found: " <> toText extId <> "/" <> toText cmdId
    toText (ExtensionUnsupportedFormat expected actual) =
        "Unsupported document format: expected " <> toText expected <> ", got " <> toText actual
    toText (ExtensionCommandFailed detail) =
        detail

instance Show ExtensionError where
    show = Txt.unpack . toText

-- | 拡張機能の基本メタデータ。
data ExtensionMetadata = ExtensionMetadata
    { extensionId    :: !ExtensionId
      -- ^ 拡張機能の一意な識別子
    , name           :: !ExtensionName
      -- ^ ユーザー向け表示名
    , description    :: !Text
      -- ^ 拡張機能の概要説明
    , version        :: !ExtensionVersion
      -- ^ 拡張機能のバージョン
    , category       :: !ExtensionCategory
      -- ^ UI 表示用カテゴリ
    , defaultEnabled :: !Bool
      -- ^ 初回起動時に既定で有効化するか ('False' の場合は設定 UI からオプトイン)
    } deriving (Eq)

instance ToText ExtensionMetadata where
    toText meta = toText meta.extensionId <> ": " <> toText meta.name

instance Show ExtensionMetadata where
    show = Txt.unpack . toText

-- | UI 表示や 'Eq' / 'Show' を必要とするモデル層向けのコマンド純粋メタデータ。
-- 実行関数 ('runCommand') を含めないことで、Miso Model ('ClientState') で安全に保持・比較できる。
data ExtensionCommandInfo = ExtensionCommandInfo
    { commandId    :: !CommandId
      -- ^ 拡張内で一意なコマンド識別子
    , label        :: !CommandName
      -- ^ UI ボタン・メニューに表示するコマンド名
    , description  :: !Text
      -- ^ コマンドの挙動説明
    , markdownOnly :: !Bool
      -- ^ Markdown 文書でのみ実行を許可する場合は 'True'
    } deriving (Eq, Show)

-- | 'ExtensionCommand' から実行関数を除いた 'ExtensionCommandInfo' を抽出する。
toExtensionCommandInfo :: ExtensionCommand -> ExtensionCommandInfo
toExtensionCommandInfo cmd = ExtensionCommandInfo
    { commandId    = cmd.commandId
    , label        = cmd.label
    , description  = cmd.description
    , markdownOnly = cmd.markdownOnly
    }

-- | 拡張機能が提供する単一の実行可能コマンド。
data ExtensionCommand = ExtensionCommand
    { commandId    :: !CommandId
      -- ^ 拡張内で一意なコマンド識別子
    , label        :: !CommandName
      -- ^ UI ボタン・メニューに表示するコマンド名
    , description  :: !Text
      -- ^ コマンドの挙動説明
    , markdownOnly :: !Bool
      -- ^ Markdown 文書でのみ実行を許可する場合は 'True'
    , runCommand   :: !(ExtensionContext -> Either ExtensionError ExtensionResult)
      -- ^ 純粋な本文変換処理本体
    }

-- | 拡張機能の基本契約。
-- M5 の同梱拡張および将来の外部 Wasm 拡張の双方で共有できるよう、
-- 純粋なコマンド群 ('commands') と互換用プライマリ変換 ('transformBuffer') を提供する。
data Extension = Extension
    { metadata        :: !ExtensionMetadata
      -- ^ 拡張機能の識別情報と説明
    , commands        :: ![ExtensionCommand]
      -- ^ この拡張が提供するコマンド一覧 (1 件以上)
    , transformBuffer :: !(DocumentBody -> DocumentBody)
      -- ^ 互換用プライマリ変換関数 (単一コマンド拡張や直接適用テスト向け)
    }

-- | 単一の純粋本文変換関数から 'Extension' を簡潔に構築するヘルパ。
mkSimpleExtension ::
       ExtensionMetadata
    -> CommandId
    -> Bool
    -> (DocumentBody -> DocumentBody)
    -> Extension
mkSimpleExtension meta cmdId isMarkdownOnly fn = Extension
    { metadata        = meta
    , commands        =
        [ ExtensionCommand
            { commandId    = cmdId
            , label        = CommandName (toText meta.name)
            , description  = meta.description
            , markdownOnly = isMarkdownOnly
            , runCommand   = \ ctx ->
                Right ExtensionResult
                    { nextBody = fn ctx.body
                    , summary  = Nothing
                    }
            }
        ]
    , transformBuffer = fn
    }

-- | 拡張内から指定 'CommandId' のコマンドを検索する。
findExtensionCommand :: Extension -> CommandId -> Maybe ExtensionCommand
findExtensionCommand ext targetCmdId =
    find (\ cmd -> cmd.commandId == targetCmdId) ext.commands

-- | 拡張の指定コマンドをフォーマット制約チェック付きで実行する。
-- @markdownOnly == True@ のコマンドをプレーンテキスト文書へ適用しようとした場合は
-- コマンド本体を呼ばずに 'ExtensionUnsupportedFormat' を返す。
runExtensionCommand ::
       Extension
    -> CommandId
    -> ExtensionContext
    -> Either ExtensionError ExtensionResult
runExtensionCommand ext cmdId ctx = case findExtensionCommand ext cmdId of
    Nothing ->
        Left (ExtensionCommandNotFound ext.metadata.extensionId cmdId)
    Just cmd
        | cmd.markdownOnly && ctx.format /= FormatMarkdown ->
            Left (ExtensionUnsupportedFormat FormatMarkdown ctx.format)
        | otherwise ->
            cmd.runCommand ctx

-- | 拡張レジストリ整合性検証エラー。
data ExtensionRegistryError
    = DuplicateExtensionId !ExtensionId
      -- ^ 同一の 'ExtensionId' を持つ拡張が重複登録されている
    | EmptyExtensionCommands !ExtensionId
      -- ^ コマンドを 1 つも持たない拡張が登録されている
    | DuplicateCommandId !ExtensionId !CommandId
      -- ^ 同一拡張内で 'CommandId' が重複している
    deriving (Eq)

instance ToText ExtensionRegistryError where
    toText (DuplicateExtensionId extId) =
        "Duplicate extension ID: " <> toText extId
    toText (EmptyExtensionCommands extId) =
        "Extension has no commands: " <> toText extId
    toText (DuplicateCommandId extId cmdId) =
        "Duplicate command ID in extension " <> toText extId <> ": " <> toText cmdId

instance Show ExtensionRegistryError where
    show = Txt.unpack . toText

-- | 拡張レジストリ全体の不変条件 (拡張 ID 重複なし・各拡張 1 コマンド以上・拡張内コマンド ID 重複なし) を検証する。
validateExtensionRegistry :: [Extension] -> Either ExtensionRegistryError [Extension]
validateExtensionRegistry exts = go Set.empty exts >> Right exts
  where
    go :: Set ExtensionId -> [Extension] -> Either ExtensionRegistryError ()
    go _ [] = Right ()
    go seenExtIds (ext : rest) =
        let extId :: ExtensionId
            extId = ext.metadata.extensionId
        in  if Set.member extId seenExtIds
                then Left (DuplicateExtensionId extId)
                else if null ext.commands
                    then Left (EmptyExtensionCommands extId)
                    else do
                        checkCommands extId Set.empty ext.commands
                        go (Set.insert extId seenExtIds) rest

    checkCommands ::
           ExtensionId
        -> Set CommandId
        -> [ExtensionCommand]
        -> Either ExtensionRegistryError ()
    checkCommands _     _          []           = Right ()
    checkCommands extId seenCmdIds (cmd : rest)
        | Set.member cmd.commandId seenCmdIds =
            Left (DuplicateCommandId extId cmd.commandId)
        | otherwise =
            checkCommands extId (Set.insert cmd.commandId seenCmdIds) rest
