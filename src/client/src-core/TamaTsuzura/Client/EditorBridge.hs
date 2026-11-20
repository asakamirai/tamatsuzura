{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.EditorBridge
    ( -- * エディタ統計・検索状態・コマンド/イベント型
      EditorStats(..)
    , emptyEditorStats
    , computeTextStats
    , EditorSyncPayload(..)
    , EditorSearchParams(..)
    , EditorSearchStatus(..)
    , emptyEditorSearchStatus
    , EditorCommand(..)
    , EditorEvent(..)
      -- * 純粋な JSON シリアライズ / デシリアライズ
    , encodeEditorCommandJson
    , decodeEditorEventJson
      -- * 薄い JSFFI エンドポイント・Miso Subscription
    , dispatchEditorCommand
    , editorEventsSub
    ) where

import           Data.Aeson                  (FromJSON(..), ToJSON(..))
import qualified Data.Aeson                  as Aeson
import           Data.Aeson.Types            (Parser)
import qualified Data.ByteString.Lazy        as LazyBS
import           Data.Text                   (Text)
import qualified Data.Text                   as Txt
import qualified Data.Text.Encoding          as TxtEnc
import           Miso.Effect                 (Sub)
import           TamaTsuzura.Common.Document (DocumentFormat)
import           TamaTsuzura.Common.Text     (ToText(..))
#ifdef WASM
import           GHC.Wasm.Prim               (JSVal)
import           Miso.DSL                    (asyncCallback1, fromJSVal, toJSVal)
#endif

-- | CodeMirror 6 エディタの文書統計およびカーソル位置。
data EditorStats = EditorStats
    { lineCount  :: !Int
      -- ^ 総行数 (1 以上)
    , charCount  :: !Int
      -- ^ 総文字数 (0 以上)
    , cursorLine :: !Int
      -- ^ 現在のカーソル行 (1 始まり)
    , cursorCol  :: !Int
      -- ^ 現在のカーソル列 (1 始まり)
    } deriving (Eq, Show)

instance FromJSON EditorStats where
    parseJSON = Aeson.withObject "EditorStats" $ \ obj ->
        EditorStats
            <$> obj Aeson..: "lineCount"
            <*> obj Aeson..: "charCount"
            <*> obj Aeson..: "cursorLine"
            <*> obj Aeson..: "cursorCol"

-- | 空文書時の初期エディタ統計。
emptyEditorStats :: EditorStats
emptyEditorStats = EditorStats
    { lineCount  = 1
    , charCount  = 0
    , cursorLine = 1
    , cursorCol  = 1
    }

-- | 生テキストから行数・文字数を純粋に算出する (エディタ未マウント時の初期表示・テスト用)。
-- CodeMirror 6 の @doc.length@ と一致するよう改行コードを LF へ正規化した文字数を返す。
computeTextStats :: Text -> EditorStats
computeTextStats body =
    let normalized :: Text
        normalized = Txt.replace "\r" "\n" (Txt.replace "\r\n" "\n" body)

        linesCnt :: Int
        linesCnt = 1 + Txt.count "\n" normalized
    in  EditorStats
            { lineCount  = linesCnt
            , charCount  = Txt.length normalized
            , cursorLine = 1
            , cursorCol  = 1
            }

-- | CodeMirror 6 へ文書状態を一括同期するためのペイロード。
data EditorSyncPayload = EditorSyncPayload
    { docId        :: !Text
      -- ^ 開いている文書 ID (切替時に CodeMirror の Undo 履歴をリセットするためのキー)
    , version      :: !Int
      -- ^ 外部からの本文置換 (拡張適用・再読込等) を識別するための単調増加バージョン番号
    , body         :: !Text
      -- ^ 同期する文書本文
    , format       :: !DocumentFormat
      -- ^ シンタックスハイライト切替用のフォーマット種別
    , readOnly     :: !Bool
      -- ^ 読み取り専用フラグ (ゴミ箱内文書閲覧時は 'True')
    , placeholder  :: !Text
      -- ^ 本文が空のときに表示するプレースホルダー文言
    , resetHistory :: !Bool
      -- ^ 同一文書であっても EditorState ごと再構築して Undo/Redo 履歴を破棄するか
    } deriving (Eq, Show)

-- | CodeMirror 6 検索・置換パネルへ渡す検索条件。
data EditorSearchParams = EditorSearchParams
    { query         :: !Text
      -- ^ 検索文字列または正規表現パターン
    , replacement   :: !Text
      -- ^ 置換後文字列
    , caseSensitive :: !Bool
      -- ^ 大文字・小文字を区別するか
    , useRegex      :: !Bool
      -- ^ 正規表現モードを有効にするか
    } deriving (Eq, Show)

-- | CodeMirror 6 から通知される検索一致状況。
data EditorSearchStatus = EditorSearchStatus
    { matchCount  :: !Int
      -- ^ 文書全体での一致件数
    , activeMatch :: !Int
      -- ^ 現在選択中の一致番号 (1 始まり、未選択時は 0)
    , regexError  :: !(Maybe Text)
      -- ^ 正規表現が不正な場合のエラー詳細
    } deriving (Eq, Show)

-- | 検索未実行時の初期一致状況。
emptyEditorSearchStatus :: EditorSearchStatus
emptyEditorSearchStatus = EditorSearchStatus
    { matchCount  = 0
    , activeMatch = 0
    , regexError  = Nothing
    }

-- | Haskell (Miso) から CodeMirror 6 ブリッジへ送信するコマンド。
data EditorCommand
    = CmdSyncDocument !EditorSyncPayload
      -- ^ 文書内容・フォーマット・読み取り専用設定を同期する
    | CmdResetEditor
      -- ^ 文書選択解除時にエディタ内部の保留状態・検索・Undo 履歴を初期化する
    | CmdApplySearch !EditorSearchParams
      -- ^ 検索条件を適用し一致ハイライトを更新する
    | CmdClearSearch
      -- ^ 検索ハイライトと検索クエリを解除する
    | CmdFindNext
      -- ^ 次の一致箇所へ移動する
    | CmdFindPrevious
      -- ^ 前の一致箇所へ移動する
    | CmdReplaceNext
      -- ^ 現在の一致箇所を置換する
    | CmdReplaceAll
      -- ^ 文書内の一致箇所をすべて置換する
    | CmdUndo
      -- ^ 編集操作を 1 ステップ元に戻す
    | CmdRedo
      -- ^ 元に戻した編集操作をやり直す
    | CmdFocus
      -- ^ エディタへ入力フォーカスを移す
    deriving (Eq, Show)

instance ToJSON EditorCommand where
    toJSON (CmdSyncDocument payload) = Aeson.object
        [ "type"         Aeson..= ("syncDocument" :: Text)
        , "docId"        Aeson..= payload.docId
        , "version"      Aeson..= payload.version
        , "body"         Aeson..= payload.body
        , "format"       Aeson..= toText payload.format
        , "readOnly"     Aeson..= payload.readOnly
        , "placeholder"  Aeson..= payload.placeholder
        , "resetHistory" Aeson..= payload.resetHistory
        ]
    toJSON CmdResetEditor = Aeson.object
        [ "type" Aeson..= ("resetEditor" :: Text)
        ]
    toJSON (CmdApplySearch params) = Aeson.object
        [ "type"          Aeson..= ("applySearch" :: Text)
        , "query"         Aeson..= params.query
        , "replacement"   Aeson..= params.replacement
        , "caseSensitive" Aeson..= params.caseSensitive
        , "useRegex"      Aeson..= params.useRegex
        ]
    toJSON CmdClearSearch = Aeson.object
        [ "type" Aeson..= ("clearSearch" :: Text)
        ]
    toJSON CmdFindNext = Aeson.object
        [ "type" Aeson..= ("findNext" :: Text)
        ]
    toJSON CmdFindPrevious = Aeson.object
        [ "type" Aeson..= ("findPrevious" :: Text)
        ]
    toJSON CmdReplaceNext = Aeson.object
        [ "type" Aeson..= ("replaceNext" :: Text)
        ]
    toJSON CmdReplaceAll = Aeson.object
        [ "type" Aeson..= ("replaceAll" :: Text)
        ]
    toJSON CmdUndo = Aeson.object
        [ "type" Aeson..= ("undo" :: Text)
        ]
    toJSON CmdRedo = Aeson.object
        [ "type" Aeson..= ("redo" :: Text)
        ]
    toJSON CmdFocus = Aeson.object
        [ "type" Aeson..= ("focus" :: Text)
        ]

-- | CodeMirror 6 ブリッジから Haskell (Miso) へ通知されるイベント。
data EditorEvent
    = EvtBodyChanged !Text !EditorStats
      -- ^ エディタ本文が変更された (更新後本文, 最新統計)
    | EvtCursorMoved !EditorStats
      -- ^ カーソル・選択範囲が移動した
    | EvtSaveShortcut
      -- ^ 明示的保存ショートカット (@Ctrl+S@ / @Cmd+S@) が押された
    | EvtOpenSearch !Bool
      -- ^ 検索 (@Ctrl+F@) または置換 (@Ctrl+H@) ショートカットが押された (置換欄展開フラグ)
    | EvtSearchStatus !EditorSearchStatus
      -- ^ 検索一致件数や正規表現エラー状態が更新された
    | EvtHistoryState !Bool !Bool
      -- ^ Undo / Redo 可否が更新された (canUndo, canRedo)
    deriving (Eq, Show)

instance FromJSON EditorEvent where
    parseJSON = Aeson.withObject "EditorEvent" $ \ obj -> do
        evtType <- obj Aeson..: "type" :: Parser Text
        case evtType of
            "bodyChanged" ->
                EvtBodyChanged
                    <$> obj Aeson..: "body"
                    <*> obj Aeson..: "stats"
            "cursorMoved" ->
                EvtCursorMoved
                    <$> obj Aeson..: "stats"
            "saveShortcut" ->
                pure EvtSaveShortcut
            "openSearch" ->
                EvtOpenSearch
                    <$> obj Aeson..:? "showReplace" Aeson..!= False
            "searchStatus" -> do
                cnt <- obj Aeson..:  "matchCount"
                act <- obj Aeson..:  "activeMatch"
                err <- obj Aeson..:? "regexError"
                pure $ EvtSearchStatus EditorSearchStatus
                    { matchCount  = cnt
                    , activeMatch = act
                    , regexError  = err
                    }
            "historyState" ->
                EvtHistoryState
                    <$> obj Aeson..: "canUndo"
                    <*> obj Aeson..: "canRedo"
            other ->
                fail ("Unknown editor event type: " <> Txt.unpack other)

-- | 'EditorCommand' を JSFFI 送信用の UTF-8 JSON テキストへ純粋にエンコードする。
encodeEditorCommandJson :: EditorCommand -> Text
encodeEditorCommandJson =
    TxtEnc.decodeUtf8Lenient . LazyBS.toStrict . Aeson.encode

-- | JSFFI 経由で受信した JSON テキストを 'EditorEvent' へ純粋にデコードする。
decodeEditorEventJson :: Text -> Either Text EditorEvent
decodeEditorEventJson rawJson =
    case Aeson.eitherDecodeStrict' (TxtEnc.encodeUtf8 rawJson) of
        Left errMsg -> Left (Txt.pack errMsg)
        Right evt   -> Right evt

#ifdef WASM
foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraEditor && typeof globalThis.__tamatsuzuraEditor.dispatchCommand === 'function') { globalThis.__tamatsuzuraEditor.dispatchCommand($1); }"
    jsDispatchEditorCommand :: JSVal -> IO ()

foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraEditor && typeof globalThis.__tamatsuzuraEditor.subscribe === 'function') { globalThis.__tamatsuzuraEditor.subscribe($1); }"
    jsSubscribeEditor :: JSVal -> IO ()
#endif

-- | CodeMirror 6 ブリッジへコマンドを送信する。
-- ネイティブ (非 Wasm) 環境ではテスト安全のため no-op とする。
dispatchEditorCommand :: EditorCommand -> IO ()
#ifdef WASM
dispatchEditorCommand cmd = do
    cmdVal <- toJSVal (encodeEditorCommandJson cmd)
    jsDispatchEditorCommand cmdVal
#else
dispatchEditorCommand _cmd = pure ()
#endif

-- | CodeMirror 6 ブリッジからのイベントを受信して Miso アクションへ変換する Subscription。
editorEventsSub :: (EditorEvent -> action) -> Sub model action
#ifdef WASM
editorEventsSub toAction sink _getModel = do
    cb <- asyncCallback1 $ \ rawVal -> do
        mTxt <- fromJSVal rawVal :: IO (Maybe Text)
        case mTxt of
            Nothing  -> pure ()
            Just txt -> case decodeEditorEventJson txt of
                Left _    -> pure ()
                Right evt -> sink (toAction evt)
    jsSubscribeEditor cb
#else
editorEventsSub _toAction _sink _getModel = pure ()
#endif
