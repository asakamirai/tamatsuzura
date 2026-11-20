{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.DraftStorage
    ( -- * ドラフト永続化モデル・同期状態・イベント型
      StoredDraft(..)
    , DraftSyncState(..)
    , DraftStorageEvent(..)
    , DraftStorage(..)
    , defaultDraftDebounceMs
      -- * 純粋な JSON シリアライズ / デシリアライズ・補助関数
    , encodeStoredDraftJson
    , decodeStoredDraftListJson
    , decodeDraftStorageEventJson
    , latestStoredDraft
      -- * ブラウザ IndexedDB 実装・Miso Subscription
    , browserDraftStorage
    , draftStorageEventsSub
    ) where

import           Data.Aeson                  (FromJSON(..), ToJSON(..))
import qualified Data.Aeson                  as Aeson
import           Data.Aeson.Types            (Parser)
import qualified Data.ByteString.Lazy        as LazyBS
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Txt
import qualified Data.Text.Encoding          as TxtEnc
import           Data.Time.Clock             (UTCTime)
import           Miso.Effect                 (Sub)
import           TamaTsuzura.Common.Document
    ( DocumentBody
    , DocumentFormat
    , DocumentId
    , mkDocumentId
    )
#ifdef WASM
import           GHC.Wasm.Prim               (JSVal)
import           Miso.DSL                    (asyncCallback1, fromJSVal, toJSVal)
import           TamaTsuzura.Common.Text     (ToText(..))
#endif

-- | IndexedDB に自動退避される未保存ドラフト 1 件分のレコード。
-- サーバ側の更新日時 ('baseUpdatedAt') を併せて記録することで、
-- 再読み込み時にサーバ側で別更新が行われていた場合の上書き衝突を安全に検知する。
data StoredDraft = StoredDraft
    { docId         :: !DocumentId
      -- ^ 対象文書の ID (IndexedDB の主キー)
    , titleInput    :: !Text
      -- ^ 未保存のタイトル入力文字列
    , body          :: !DocumentBody
      -- ^ 未保存の本文テキスト
    , format        :: !DocumentFormat
      -- ^ 未保存のフォーマット種別
    , folderInput   :: !Text
      -- ^ 未保存のフォルダ入力文字列
    , tagsInput     :: !Text
      -- ^ 未保存のタグ入力文字列
    , baseUpdatedAt :: !UTCTime
      -- ^ ドラフト作成元となったサーバ側文書の最終更新日時
    , savedAt       :: !UTCTime
      -- ^ ブラウザのローカルストレージへ退避した日時 (UTC)
    } deriving (Eq, Show)

instance ToJSON StoredDraft where
    toJSON sd = Aeson.object
        [ "docId"         Aeson..= sd.docId
        , "titleInput"    Aeson..= sd.titleInput
        , "body"          Aeson..= sd.body
        , "format"        Aeson..= sd.format
        , "folderInput"   Aeson..= sd.folderInput
        , "tagsInput"     Aeson..= sd.tagsInput
        , "baseUpdatedAt" Aeson..= sd.baseUpdatedAt
        , "savedAt"       Aeson..= sd.savedAt
        ]

instance FromJSON StoredDraft where
    parseJSON = Aeson.withObject "StoredDraft" $ \ obj ->
        StoredDraft
            <$> obj Aeson..: "docId"
            <*> obj Aeson..: "titleInput"
            <*> obj Aeson..: "body"
            <*> obj Aeson..: "format"
            <*> obj Aeson..: "folderInput"
            <*> obj Aeson..: "tagsInput"
            <*> obj Aeson..: "baseUpdatedAt"
            <*> obj Aeson..: "savedAt"

-- | 現在編集中の文書に関するローカルドラフト自動退避ステータス。
data DraftSyncState
    = DraftSyncIdle
      -- ^ 未保存変更なし、または退避不要
    | DraftSyncPending
      -- ^ デバウンス待機中または IndexedDB への書き込み中
    | DraftSyncSaved !UTCTime
      -- ^ IndexedDB への退避完了 (退避日時)
    | DraftSyncFailed !Text
      -- ^ IndexedDB 書き込み失敗 (エラー詳細)
    deriving (Eq, Show)

-- | ブラウザの IndexedDB ブリッジから非同期に通知されるイベント。
data DraftStorageEvent
    = DraftEvtSaved !DocumentId !UTCTime
      -- ^ 指定文書のドラフトが IndexedDB へ正常に書き込まれた
    | DraftEvtSaveFailed !(Maybe DocumentId) !Text
      -- ^ ドラフトの書き込みに失敗した (文書 ID が特定できない場合は 'Nothing')
    deriving (Eq, Show)

instance FromJSON DraftStorageEvent where
    parseJSON = Aeson.withObject "DraftStorageEvent" $ \ obj -> do
        evtType <- obj Aeson..: "type" :: Parser Text
        case evtType of
            "draftSaved" ->
                DraftEvtSaved
                    <$> obj Aeson..: "docId"
                    <*> obj Aeson..: "savedAt"
            "draftSaveFailed" -> do
                mRawDocId <- obj Aeson..:? "docId" :: Parser (Maybe Text)
                errMsg    <- obj Aeson..:  "error"
                let mDocId :: Maybe DocumentId
                    mDocId = mRawDocId >>= either (const Nothing) Just . mkDocumentId
                pure (DraftEvtSaveFailed mDocId errMsg)
            other ->
                fail ("Unknown draft storage event type: " <> Txt.unpack other)

-- | 差し替え可能なローカルドラフト永続化インターフェース。
-- 本番 (Wasm) では IndexedDB ブリッジ ('browserDraftStorage') を使用し、
-- 自動テストではインメモリモックを注入して全状態遷移を決定的に検証する。
data DraftStorage = DraftStorage
    { scheduleSaveDraft :: !(StoredDraft -> IO ())
      -- ^ 未保存ドラフトの自動退避を予約する (ブラウザ側でデバウンス＋離脱時即時フラッシュ)
    , deleteStoredDraft :: !(DocumentId -> IO ())
      -- ^ 指定文書の未保存ドラフトを保留タイマーごと破棄・削除する
    , listStoredDrafts  :: !(IO (Either Text [StoredDraft]))
      -- ^ 保存済みの全未保存ドラフトを読み込む
    }

-- | 既定のドラフト自動退避デバウンス間隔 (ミリ秒)。
-- 連続タイピング中の IndexedDB トランザクション頻度を抑制しつつ、
-- 離脱イベント (@pagehide@ / @visibilitychange@) で未フラッシュ分を即時書き込む前提で 400ms とする。
defaultDraftDebounceMs :: Int
defaultDraftDebounceMs = 400

-- | 'StoredDraft' を JSFFI 送信用の UTF-8 JSON テキストへ純粋にエンコードする。
encodeStoredDraftJson :: StoredDraft -> Text
encodeStoredDraftJson =
    TxtEnc.decodeUtf8Lenient . LazyBS.toStrict . Aeson.encode

-- | JSFFI から返却されたドラフト一覧 JSON ラッパー。
data DraftListJsPayload = DraftListJsPayload
    { ok     :: !Bool
    , drafts :: !(Maybe [StoredDraft])
    , errMsg :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON DraftListJsPayload where
    parseJSON = Aeson.withObject "DraftListJsPayload" $ \ obj ->
        DraftListJsPayload
            <$> obj Aeson..:  "ok"
            <*> obj Aeson..:? "drafts"
            <*> obj Aeson..:? "error"

-- | JSFFI の @listDrafts()@ 応答 JSON をデコードする。
decodeStoredDraftListJson :: Text -> Either Text [StoredDraft]
decodeStoredDraftListJson rawJson =
    case ePayload of
        Left parseErr ->
            Left ("Malformed IndexedDB draft list payload: " <> Txt.pack parseErr)
        Right payload
            | payload.ok ->
                Right (fromMaybe [] payload.drafts)
            | otherwise ->
                Left (fromMaybe "Failed to read local drafts" payload.errMsg)
  where
    ePayload :: Either String DraftListJsPayload
    ePayload = Aeson.eitherDecodeStrict' (TxtEnc.encodeUtf8 rawJson)

-- | JSFFI のストレージイベント JSON を 'DraftStorageEvent' へ純粋にデコードする。
decodeDraftStorageEventJson :: Text -> Either Text DraftStorageEvent
decodeDraftStorageEventJson rawJson =
    case Aeson.eitherDecodeStrict' (TxtEnc.encodeUtf8 rawJson) of
        Left parseErr -> Left (Txt.pack parseErr)
        Right evt     -> Right evt

-- | 複数の未保存ドラフトから最も新しく退避された 1 件 ('savedAt' 最大) を返す。
-- 同日時の場合は 'docId' の順序で決定的に選択する。
latestStoredDraft :: [StoredDraft] -> Maybe StoredDraft
latestStoredDraft []       = Nothing
latestStoredDraft (x : xs) = Just (foldl' pickNewer x xs)
  where
    pickNewer :: StoredDraft -> StoredDraft -> StoredDraft
    pickNewer best candidate
        | candidate.savedAt > best.savedAt = candidate
        | candidate.savedAt == best.savedAt && candidate.docId > best.docId = candidate
        | otherwise = best

#ifdef WASM
foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraStorage && typeof globalThis.__tamatsuzuraStorage.scheduleSave === 'function') { globalThis.__tamatsuzuraStorage.scheduleSave($1, $2); }"
    jsScheduleSaveDraft :: JSVal -> Int -> IO ()

foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraStorage && typeof globalThis.__tamatsuzuraStorage.deleteDraft === 'function') { globalThis.__tamatsuzuraStorage.deleteDraft($1); }"
    jsDeleteDraft :: JSVal -> IO ()

foreign import javascript safe
    "(async () => { if (globalThis.__tamatsuzuraStorage && typeof globalThis.__tamatsuzuraStorage.listDrafts === 'function') { return await globalThis.__tamatsuzuraStorage.listDrafts(); } return JSON.stringify({ ok: false, error: 'IndexedDB storage bridge is unavailable' }); })()"
    jsListDrafts :: IO JSVal

foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraStorage && typeof globalThis.__tamatsuzuraStorage.subscribe === 'function') { globalThis.__tamatsuzuraStorage.subscribe($1); }"
    jsSubscribeDraftStorage :: JSVal -> IO ()
#endif

-- | ブラウザの IndexedDB ブリッジを用いる標準 'DraftStorage'。
-- ネイティブ (非 Wasm) ビルド時は安全なフォールバック実装を返す。
browserDraftStorage :: DraftStorage
browserDraftStorage = DraftStorage
    { scheduleSaveDraft = scheduleSaveBrowser
    , deleteStoredDraft = deleteDraftBrowser
    , listStoredDrafts  = listDraftsBrowser
    }

scheduleSaveBrowser :: StoredDraft -> IO ()
#ifdef WASM
scheduleSaveBrowser sd = do
    payloadVal <- toJSVal (encodeStoredDraftJson sd)
    jsScheduleSaveDraft payloadVal defaultDraftDebounceMs
#else
scheduleSaveBrowser _sd = pure ()
#endif

deleteDraftBrowser :: DocumentId -> IO ()
#ifdef WASM
deleteDraftBrowser targetId = do
    idVal <- toJSVal (toText targetId)
    jsDeleteDraft idVal
#else
deleteDraftBrowser _targetId = pure ()
#endif

listDraftsBrowser :: IO (Either Text [StoredDraft])
#ifdef WASM
listDraftsBrowser = do
    rawVal  <- jsListDrafts
    mRawTxt <- fromJSVal rawVal :: IO (Maybe Text)
    pure $ case mRawTxt of
        Nothing     -> Left "JSFFI listDrafts returned a non-string value"
        Just rawTxt -> decodeStoredDraftListJson rawTxt
#else
listDraftsBrowser = pure (Right [])
#endif

-- | IndexedDB ブリッジからの退避完了・失敗イベントを購読する Miso Subscription。
draftStorageEventsSub :: (DraftStorageEvent -> action) -> Sub model action
#ifdef WASM
draftStorageEventsSub toAction sink _getModel = do
    cb <- asyncCallback1 $ \ rawVal -> do
        mTxt <- fromJSVal rawVal :: IO (Maybe Text)
        case mTxt of
            Nothing  -> pure ()
            Just txt -> case decodeDraftStorageEventJson txt of
                Left _    -> pure ()
                Right evt -> sink (toAction evt)
    jsSubscribeDraftStorage cb
#else
draftStorageEventsSub _toAction _sink _getModel = pure ()
#endif
