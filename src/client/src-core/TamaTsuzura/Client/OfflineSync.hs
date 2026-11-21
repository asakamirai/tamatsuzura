{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module TamaTsuzura.Client.OfflineSync
    ( -- * ネットワーク・オフラインキャッシュ・未同期保存キュー・競合退避型
      NetworkStatus(..)
    , CachedDocument(..)
    , PendingSyncSave(..)
    , SyncConflict(..)
    , SyncStatus(..)
    , OfflineStore(..)
      -- * 純粋な JSON シリアライズ / デシリアライズ
    , encodeCachedDocumentJson
    , decodeCachedDocumentListJson
    , encodePendingSyncSaveJson
    , decodePendingSyncSaveListJson
    , encodeSyncConflictJson
    , decodeSyncConflictListJson
      -- * 純粋な同期キュー・オフライン検索・競合判定ロジック
    , enqueuePendingSave
    , nextPendingSyncSave
    , buildSyncUpdateRequest
    , buildConflictRecoveryRequest
    , computeSyncStatus
    , filterCachedDocuments
    , isConflictError
    , isNetworkError
    , isNotFoundError
      -- * ブラウザ IndexedDB / ネットワーク状態実装・Miso Subscription
    , browserOfflineStore
    , noopOfflineStore
    , networkStatusSub
    ) where

import           Data.Aeson                    (FromJSON(..), ToJSON(..))
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as LazyBS
import           Data.Either                   (fromRight)
import           Data.List                     (sortBy)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Ord                      (Down(..), comparing)
import           Data.Text                     (Text)
import qualified Data.Text                     as Txt
import qualified Data.Text.Encoding            as TxtEnc
import           Data.Time.Clock               (UTCTime)
import           Miso.Effect                   (Sub)
import           TamaTsuzura.Client.Api        (ApiClientError(..))
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , CreateDocumentRequest(..)
    , DocumentStatusFilter
    , UpdateDocumentRequest(..)
    , matchesDocumentStatus
    )
import           TamaTsuzura.Common.Auth       (UserId)
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody
    , DocumentFormat
    , DocumentId
    , DocumentSummary(..)
    , DocumentTitle
    , FolderPath
    , RevisionNumber
    , Tag
    , TagSet
    , emptyDocumentTitle
    , isFolderPrefixOf
    , memberTag
    , mkDocumentTitle
    , toDocumentSummary
    , validateDocumentBody
    )
import           TamaTsuzura.Common.Text       (ToText(..))
import           TamaTsuzura.Common.Validation (ValidationError, maxDocumentTitleLength)
#ifdef WASM
import           GHC.Wasm.Prim                 (JSVal)
import           Miso.DSL                      (asyncCallback1, fromJSVal, toJSVal)
#endif

-- | ブラウザのネットワークオンライン / オフライン状態。
data NetworkStatus
    = NetworkOnline
      -- ^ ネットワーク接続あり (ブラウザ @navigator.onLine == true@)
    | NetworkOffline
      -- ^ ネットワーク切断状態 (ブラウザ @navigator.onLine == false@)
    deriving (Eq, Show)

-- | IndexedDB にキャッシュされる閲覧・オフライン編集用の文書レコード。
-- 同一ブラウザ上でのアカウント切替による情報漏洩を防ぐため、所有者 ID ('ownerId') を必須保持する。
data CachedDocument = CachedDocument
    { ownerId  :: !UserId
      -- ^ キャッシュ所有者のユーザー ID
    , document :: !Document
      -- ^ キャッシュされた文書スナップショット (本文・メタデータ・リビジョン番号を含む)
    , cachedAt :: !UTCTime
      -- ^ キャッシュ格納日時 (UTC)
    } deriving (Eq, Show)

instance ToJSON CachedDocument where
    toJSON cd = Aeson.object
        [ "docId"    Aeson..= cd.document.id
        , "ownerId"  Aeson..= cd.ownerId
        , "document" Aeson..= cd.document
        , "cachedAt" Aeson..= cd.cachedAt
        ]

instance FromJSON CachedDocument where
    parseJSON = Aeson.withObject "CachedDocument" $ \ obj ->
        CachedDocument
            <$> obj Aeson..: "ownerId"
            <*> obj Aeson..: "document"
            <*> obj Aeson..: "cachedAt"

-- | オフライン中またはネットワーク断時に明示的保存された未同期の変更レコード。
-- 同一文書へ複数回保存された場合は最新内容へ集約しつつ、最初に編集を開始した
-- サーバ確定リビジョン番号 ('baseRevision') を保持することで、
-- オンライン復帰時の 409 Conflict 検出 (楽観的排他) を確実に機能させる。
data PendingSyncSave = PendingSyncSave
    { docId        :: !DocumentId
      -- ^ 対象文書 ID (IndexedDB 主キー)
    , ownerId      :: !UserId
      -- ^ 所有者ユーザー ID (マルチユーザー分離用)
    , title        :: !DocumentTitle
      -- ^ 保存予定の検証済みタイトル
    , body         :: !DocumentBody
      -- ^ 保存予定の検証済み本文
    , format       :: !DocumentFormat
      -- ^ 保存予定のフォーマット種別
    , folder       :: !(Maybe FolderPath)
      -- ^ 保存予定のフォルダパス
    , tags         :: !TagSet
      -- ^ 保存予定のタグ集合
    , baseRevision :: !RevisionNumber
      -- ^ 同期時の楽観的排他検証に用いる期待リビジョン番号 (@base_revision@)
    , queuedAt     :: !UTCTime
      -- ^ キューへ積まれた日時 (UTC)
    } deriving (Eq, Show)

instance ToJSON PendingSyncSave where
    toJSON ps = Aeson.object
        [ "docId"        Aeson..= ps.docId
        , "ownerId"      Aeson..= ps.ownerId
        , "title"        Aeson..= ps.title
        , "body"         Aeson..= ps.body
        , "format"       Aeson..= ps.format
        , "folder"       Aeson..= ps.folder
        , "tags"         Aeson..= ps.tags
        , "baseRevision" Aeson..= ps.baseRevision
        , "queuedAt"     Aeson..= ps.queuedAt
        ]

instance FromJSON PendingSyncSave where
    parseJSON = Aeson.withObject "PendingSyncSave" $ \ obj ->
        PendingSyncSave
            <$> obj Aeson..:  "docId"
            <*> obj Aeson..:  "ownerId"
            <*> obj Aeson..:  "title"
            <*> obj Aeson..:  "body"
            <*> obj Aeson..:  "format"
            <*> obj Aeson..:? "folder"
            <*> obj Aeson..:  "tags"
            <*> obj Aeson..:  "baseRevision"
            <*> obj Aeson..:  "queuedAt"

-- | 同期時または保存時に @base_revision@ 不一致 (409 Conflict) 等が検出された際、
-- サーバ側を上書きせずに安全に退避されたローカル変更レコード。
-- M6 では退避通知・別文書としての救出・破棄を提供し、M7 で 3-way マージ解決 UI へ接続する。
data SyncConflict = SyncConflict
    { docId      :: !DocumentId
      -- ^ 競合が発生した文書 ID (IndexedDB 主キー)
    , ownerId    :: !UserId
      -- ^ 所有者ユーザー ID (マルチユーザー分離用)
    , localSave  :: !PendingSyncSave
      -- ^ サーバ側を上書きせず退避されたローカル変更内容
    , detectedAt :: !UTCTime
      -- ^ 競合を検出した日時 (UTC)
    , reason     :: !Text
      -- ^ ユーザー向け競合理由メッセージ
    } deriving (Eq, Show)

instance ToJSON SyncConflict where
    toJSON sc = Aeson.object
        [ "docId"      Aeson..= sc.docId
        , "ownerId"    Aeson..= sc.ownerId
        , "localSave"  Aeson..= sc.localSave
        , "detectedAt" Aeson..= sc.detectedAt
        , "reason"     Aeson..= sc.reason
        ]

instance FromJSON SyncConflict where
    parseJSON = Aeson.withObject "SyncConflict" $ \ obj ->
        SyncConflict
            <$> obj Aeson..: "docId"
            <*> obj Aeson..: "ownerId"
            <*> obj Aeson..: "localSave"
            <*> obj Aeson..: "detectedAt"
            <*> obj Aeson..: "reason"

-- | UI インジケータ向けに集約された現在の同期ステータス。
data SyncStatus
    = SyncIdle
      -- ^ 未同期キュー・競合ともにゼロ
    | SyncSyncing !Int
      -- ^ オンライン復帰後の自動・手動同期を実行中 (残り件数)
    | SyncPending !Int
      -- ^ 未同期の保存キューが待機中 (件数)
    | SyncConflictDetected !Int
      -- ^ 競合退避された変更が存在する (件数)
    deriving (Eq, Show)

-- | 差し替え可能なオフラインキャッシュ・未同期保存キュー・競合退避ストア抽象。
-- 本番 (Wasm) では IndexedDB ブリッジ ('browserOfflineStore') を使用し、
-- 自動テストではインメモリモックを注入してオフライン編集・復帰自動同期・競合退避を検証する。
data OfflineStore = OfflineStore
    { putCachedDocument    :: !(CachedDocument -> IO ())
      -- ^ 取得・保存済み文書をローカルキャッシュへ格納する
    , deleteCachedDocument :: !(DocumentId -> IO ())
      -- ^ 指定文書をローカルキャッシュから削除する
    , listCachedDocuments  :: !(IO (Either Text [CachedDocument]))
      -- ^ キャッシュ済み文書一覧を読み込む
    , putPendingSave       :: !(PendingSyncSave -> IO ())
      -- ^ 未同期保存キュー項目を格納・更新する
    , deletePendingSave    :: !(DocumentId -> IO ())
      -- ^ 指定文書の未同期保存キュー項目を削除する
    , listPendingSaves     :: !(IO (Either Text [PendingSyncSave]))
      -- ^ 未同期保存キュー一覧を読み込む
    , putSyncConflict      :: !(SyncConflict -> IO ())
      -- ^ 競合退避レコードを格納する
    , deleteSyncConflict   :: !(DocumentId -> IO ())
      -- ^ 指定文書の競合退避レコードを削除する
    , listSyncConflicts    :: !(IO (Either Text [SyncConflict]))
      -- ^ 競合退避レコード一覧を読み込む
    , isBrowserOnline      :: !(IO Bool)
      -- ^ 現在のブラウザオンライン状態 (@navigator.onLine@) を取得する
    }

-- | 'CachedDocument' を JSFFI 送信用の UTF-8 JSON テキストへエンコードする。
encodeCachedDocumentJson :: CachedDocument -> Text
encodeCachedDocumentJson =
    TxtEnc.decodeUtf8Lenient . LazyBS.toStrict . Aeson.encode

-- | 'PendingSyncSave' を JSFFI 送信用の UTF-8 JSON テキストへエンコードする。
encodePendingSyncSaveJson :: PendingSyncSave -> Text
encodePendingSyncSaveJson =
    TxtEnc.decodeUtf8Lenient . LazyBS.toStrict . Aeson.encode

-- | 'SyncConflict' を JSFFI 送信用の UTF-8 JSON テキストへエンコードする。
encodeSyncConflictJson :: SyncConflict -> Text
encodeSyncConflictJson =
    TxtEnc.decodeUtf8Lenient . LazyBS.toStrict . Aeson.encode

data StoreListPayload a = StoreListPayload
    { ok     :: !Bool
    , items  :: !(Maybe [a])
    , errMsg :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON a => FromJSON (StoreListPayload a) where
    parseJSON = Aeson.withObject "StoreListPayload" $ \ obj ->
        StoreListPayload
            <$> obj Aeson..:  "ok"
            <*> obj Aeson..:? "items"
            <*> obj Aeson..:? "error"

decodeStoreListJson :: forall a. FromJSON a => Text -> Text -> Either Text [a]
decodeStoreListJson storeLabel rawJson =
    case ePayload of
        Left parseErr ->
            Left ("Malformed IndexedDB " <> storeLabel <> " payload: " <> Txt.pack parseErr)
        Right payload
            | payload.ok ->
                Right (fromMaybe [] payload.items)
            | otherwise ->
                Left (fromMaybe ("Failed to read " <> storeLabel) payload.errMsg)
  where
    ePayload :: Either String (StoreListPayload a)
    ePayload = Aeson.eitherDecodeStrict' (TxtEnc.encodeUtf8 rawJson)

-- | JSFFI の @listCachedDocuments()@ 応答 JSON をデコードする。
decodeCachedDocumentListJson :: Text -> Either Text [CachedDocument]
decodeCachedDocumentListJson = decodeStoreListJson "cached_docs"

-- | JSFFI の @listPendingSaves()@ 応答 JSON をデコードする。
decodePendingSyncSaveListJson :: Text -> Either Text [PendingSyncSave]
decodePendingSyncSaveListJson = decodeStoreListJson "pending_saves"

-- | JSFFI の @listSyncConflicts()@ 応答 JSON をデコードする。
decodeSyncConflictListJson :: Text -> Either Text [SyncConflict]
decodeSyncConflictListJson = decodeStoreListJson "sync_conflicts"

-- | 未同期保存キューへ新しい保存項目を追加・集約する。
-- 同一文書 ID の未同期項目がすでに存在する場合、最初に記録された 'baseRevision' を維持して
-- 最新のタイトル・本文・メタデータへ更新する（オフライン中の連続保存でも楽観的排他基準を保ち続けるため）。
enqueuePendingSave ::
       PendingSyncSave
    -> Map DocumentId PendingSyncSave
    -> Map DocumentId PendingSyncSave
enqueuePendingSave incoming queue =
    let merged :: PendingSyncSave
        merged = case Map.lookup incoming.docId queue of
            Nothing   -> incoming
            Just prev -> PendingSyncSave
                { docId        = incoming.docId
                , ownerId      = incoming.ownerId
                , title        = incoming.title
                , body         = incoming.body
                , format       = incoming.format
                , folder       = incoming.folder
                , tags         = incoming.tags
                , baseRevision = prev.baseRevision
                , queuedAt     = incoming.queuedAt
                }
    in  Map.insert incoming.docId merged queue

-- | 未同期保存キューから次に同期すべき 1 件を 'queuedAt' 昇順（同日時は 'docId' 昇順）で決定的に選ぶ。
nextPendingSyncSave :: Map DocumentId PendingSyncSave -> Maybe PendingSyncSave
nextPendingSyncSave queue = case Map.elems queue of
    []     -> Nothing
    x : xs -> Just (foldl' pickEarlier x xs)
  where
    pickEarlier :: PendingSyncSave -> PendingSyncSave -> PendingSyncSave
    pickEarlier best candidate
        | candidate.queuedAt < best.queuedAt                                    = candidate
        | candidate.queuedAt == best.queuedAt && candidate.docId < best.docId   = candidate
        | otherwise                                                             = best

-- | 'PendingSyncSave' から楽観的排他チェック (@base_revision@) 付きの 'UpdateDocumentRequest' を構築する。
buildSyncUpdateRequest :: PendingSyncSave -> UpdateDocumentRequest
buildSyncUpdateRequest ps = UpdateDocumentRequest
    { title        = ps.title
    , body         = ps.body
    , format       = ps.format
    , folder       = ps.folder
    , tags         = ps.tags
    , baseRevision = Just ps.baseRevision
    }

-- | 競合退避された 'SyncConflict' を別名の新規文書として安全に救出するための 'CreateDocumentRequest' を構築する。
-- タイトル上限 ('maxDocumentTitleLength') を超えないよう安全に切り詰めた上で接尾辞を付与する。
buildConflictRecoveryRequest :: Text -> SyncConflict -> Either ValidationError CreateDocumentRequest
buildConflictRecoveryRequest titleSuffix conflict = do
    validBody <- validateDocumentBody conflict.localSave.body
    let baseTitle :: Text
        baseTitle = toText conflict.localSave.title

        maxBaseLen :: Int
        maxBaseLen = max 0 (maxDocumentTitleLength - Txt.length titleSuffix)

        candidateTitle :: Text
        candidateTitle =
            if Txt.null baseTitle
                then Txt.strip titleSuffix
                else Txt.take maxBaseLen baseTitle <> titleSuffix

        validTitle :: DocumentTitle
        validTitle = fromRight emptyDocumentTitle (mkDocumentTitle candidateTitle)
    Right CreateDocumentRequest
        { title  = validTitle
        , body   = validBody
        , format = conflict.localSave.format
        , folder = conflict.localSave.folder
        , tags   = conflict.localSave.tags
        }

-- | 現在の同期中文書・未同期キュー・競合マップから UI インジケータ用の 'SyncStatus' を算出する。
computeSyncStatus ::
       Maybe DocumentId
    -> Map DocumentId PendingSyncSave
    -> Map DocumentId SyncConflict
    -> SyncStatus
computeSyncStatus mSyncing queue conflicts
    | not (Map.null conflicts) = SyncConflictDetected (Map.size conflicts)
    | Just _ <- mSyncing       = SyncSyncing (max 1 (Map.size queue))
    | not (Map.null queue)     = SyncPending (Map.size queue)
    | otherwise                = SyncIdle

-- | オフライン時にローカルキャッシュ済み文書から指定フィルタ条件に一致するサマリ一覧を構築する。
-- 現在ユーザー所有の文書のみを対象とし、更新日時降順（同日時は文書 ID 昇順）で返す。
filterCachedDocuments ::
       UserId
    -> DocumentStatusFilter
    -> Text
    -> Maybe Tag
    -> Maybe FolderPath
    -> Bool
    -> [CachedDocument]
    -> [DocumentSummary]
filterCachedDocuments uid statusFilter rawSearch mTag mFolder includeSubfolders cachedDocs =
    let matched :: [DocumentSummary]
        matched =
            [ toDocumentSummary cd.document
            | cd <- cachedDocs
            , cd.ownerId == uid
            , matchesStatus cd.document
            , matchesTag cd.document
            , matchesFolder cd.document
            , matchesSearch cd.document
            ]
    in  sortBy (comparing (\ s -> (Down s.updatedAt, s.id))) matched
  where
    normalizedSearch :: Text
    normalizedSearch = Txt.toCaseFold (Txt.strip rawSearch)

    matchesStatus :: Document -> Bool
    matchesStatus doc = matchesDocumentStatus statusFilter doc.deletedAt

    matchesTag :: Document -> Bool
    matchesTag doc = case mTag of
        Nothing -> True
        Just t  -> memberTag t doc.tags

    matchesFolder :: Document -> Bool
    matchesFolder doc = case mFolder of
        Nothing -> True
        Just targetFolder -> case doc.folder of
            Nothing        -> False
            Just docFolder ->
                if includeSubfolders
                    then isFolderPrefixOf targetFolder docFolder
                    else docFolder == targetFolder

    matchesSearch :: Document -> Bool
    matchesSearch doc
        | Txt.null normalizedSearch = True
        | otherwise =
            Txt.isInfixOf normalizedSearch (Txt.toCaseFold (toText doc.title))
                || Txt.isInfixOf normalizedSearch (Txt.toCaseFold (toText doc.body))

-- | エラーが HTTP 409 Conflict (リビジョン競合等) に該当するか判定する。
isConflictError :: ApiClientError -> Bool
isConflictError (ClientApiError status apiErr) =
    status == 409 || apiErr.code == ErrConflict
isConflictError (ClientHttpError status _) =
    status == 409
isConflictError _ = False

-- | エラーがネットワーク切断・到達不能のトランスポートエラーに該当するか判定する。
isNetworkError :: ApiClientError -> Bool
isNetworkError ClientNetworkError{} = True
isNetworkError _                    = False

-- | エラーが HTTP 404 Not Found に該当するか判定する。
isNotFoundError :: ApiClientError -> Bool
isNotFoundError (ClientApiError status apiErr) =
    status == 404 || apiErr.code == ErrNotFound
isNotFoundError (ClientHttpError status _) =
    status == 404
isNotFoundError _ = False

-- | 何もしない既定の 'OfflineStore' (テストや非 Wasm ビルド用)。
noopOfflineStore :: OfflineStore
noopOfflineStore = OfflineStore
    { putCachedDocument    = \ _ -> pure ()
    , deleteCachedDocument = \ _ -> pure ()
    , listCachedDocuments  = pure (Right [])
    , putPendingSave       = \ _ -> pure ()
    , deletePendingSave    = \ _ -> pure ()
    , listPendingSaves     = pure (Right [])
    , putSyncConflict      = \ _ -> pure ()
    , deleteSyncConflict   = \ _ -> pure ()
    , listSyncConflicts    = pure (Right [])
    , isBrowserOnline      = pure True
    }

#ifdef WASM
foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.putCachedDocument === 'function') { globalThis.__tamatsuzuraOffline.putCachedDocument($1); }"
    jsPutCachedDocument :: JSVal -> IO ()

foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.deleteCachedDocument === 'function') { globalThis.__tamatsuzuraOffline.deleteCachedDocument($1); }"
    jsDeleteCachedDocument :: JSVal -> IO ()

foreign import javascript safe
    "(async () => { if (globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.listCachedDocuments === 'function') { return await globalThis.__tamatsuzuraOffline.listCachedDocuments(); } return JSON.stringify({ ok: false, error: 'Offline store bridge is unavailable' }); })()"
    jsListCachedDocuments :: IO JSVal

foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.putPendingSave === 'function') { globalThis.__tamatsuzuraOffline.putPendingSave($1); }"
    jsPutPendingSave :: JSVal -> IO ()

foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.deletePendingSave === 'function') { globalThis.__tamatsuzuraOffline.deletePendingSave($1); }"
    jsDeletePendingSave :: JSVal -> IO ()

foreign import javascript safe
    "(async () => { if (globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.listPendingSaves === 'function') { return await globalThis.__tamatsuzuraOffline.listPendingSaves(); } return JSON.stringify({ ok: false, error: 'Offline store bridge is unavailable' }); })()"
    jsListPendingSaves :: IO JSVal

foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.putSyncConflict === 'function') { globalThis.__tamatsuzuraOffline.putSyncConflict($1); }"
    jsPutSyncConflict :: JSVal -> IO ()

foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.deleteSyncConflict === 'function') { globalThis.__tamatsuzuraOffline.deleteSyncConflict($1); }"
    jsDeleteSyncConflict :: JSVal -> IO ()

foreign import javascript safe
    "(async () => { if (globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.listSyncConflicts === 'function') { return await globalThis.__tamatsuzuraOffline.listSyncConflicts(); } return JSON.stringify({ ok: false, error: 'Offline store bridge is unavailable' }); })()"
    jsListSyncConflicts :: IO JSVal

foreign import javascript unsafe
    "(globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.isOnline === 'function') ? globalThis.__tamatsuzuraOffline.isOnline() : true"
    jsIsBrowserOnline :: IO Bool

foreign import javascript unsafe
    "if (globalThis.__tamatsuzuraOffline && typeof globalThis.__tamatsuzuraOffline.subscribeNetwork === 'function') { globalThis.__tamatsuzuraOffline.subscribeNetwork($1); }"
    jsSubscribeNetwork :: JSVal -> IO ()
#endif

-- | ブラウザの IndexedDB / ネットワーク状態ブリッジを用いる標準 'OfflineStore'。
-- ネイティブ (非 Wasm) ビルド時は安全なフォールバック ('noopOfflineStore') を返す。
browserOfflineStore :: OfflineStore
#ifdef WASM
browserOfflineStore = OfflineStore
    { putCachedDocument    = putCachedBrowser
    , deleteCachedDocument = deleteCachedBrowser
    , listCachedDocuments  = listCachedBrowser
    , putPendingSave       = putPendingBrowser
    , deletePendingSave    = deletePendingBrowser
    , listPendingSaves     = listPendingBrowser
    , putSyncConflict      = putConflictBrowser
    , deleteSyncConflict   = deleteConflictBrowser
    , listSyncConflicts    = listConflictBrowser
    , isBrowserOnline      = jsIsBrowserOnline
    }
  where
    putCachedBrowser :: CachedDocument -> IO ()
    putCachedBrowser cd = do
        val <- toJSVal (encodeCachedDocumentJson cd)
        jsPutCachedDocument val

    deleteCachedBrowser :: DocumentId -> IO ()
    deleteCachedBrowser docId = do
        val <- toJSVal (toText docId)
        jsDeleteCachedDocument val

    listCachedBrowser :: IO (Either Text [CachedDocument])
    listCachedBrowser = do
        rawVal  <- jsListCachedDocuments
        mRawTxt <- fromJSVal rawVal :: IO (Maybe Text)
        pure $ case mRawTxt of
            Nothing     -> Left "JSFFI listCachedDocuments returned a non-string value"
            Just rawTxt -> decodeCachedDocumentListJson rawTxt

    putPendingBrowser :: PendingSyncSave -> IO ()
    putPendingBrowser ps = do
        val <- toJSVal (encodePendingSyncSaveJson ps)
        jsPutPendingSave val

    deletePendingBrowser :: DocumentId -> IO ()
    deletePendingBrowser docId = do
        val <- toJSVal (toText docId)
        jsDeletePendingSave val

    listPendingBrowser :: IO (Either Text [PendingSyncSave])
    listPendingBrowser = do
        rawVal  <- jsListPendingSaves
        mRawTxt <- fromJSVal rawVal :: IO (Maybe Text)
        pure $ case mRawTxt of
            Nothing     -> Left "JSFFI listPendingSaves returned a non-string value"
            Just rawTxt -> decodePendingSyncSaveListJson rawTxt

    putConflictBrowser :: SyncConflict -> IO ()
    putConflictBrowser sc = do
        val <- toJSVal (encodeSyncConflictJson sc)
        jsPutSyncConflict val

    deleteConflictBrowser :: DocumentId -> IO ()
    deleteConflictBrowser docId = do
        val <- toJSVal (toText docId)
        jsDeleteSyncConflict val

    listConflictBrowser :: IO (Either Text [SyncConflict])
    listConflictBrowser = do
        rawVal  <- jsListSyncConflicts
        mRawTxt <- fromJSVal rawVal :: IO (Maybe Text)
        pure $ case mRawTxt of
            Nothing     -> Left "JSFFI listSyncConflicts returned a non-string value"
            Just rawTxt -> decodeSyncConflictListJson rawTxt
#else
browserOfflineStore = noopOfflineStore
#endif

-- | ブラウザの @online@ / @offline@ イベントを購読する Miso Subscription。
networkStatusSub :: (NetworkStatus -> action) -> Sub model action
#ifdef WASM
networkStatusSub toAction sink _getModel = do
    cb <- asyncCallback1 $ \ rawVal -> do
        mTxt <- fromJSVal rawVal :: IO (Maybe Text)
        case mTxt of
            Just "online"  -> sink (toAction NetworkOnline)
            Just "offline" -> sink (toAction NetworkOffline)
            _              -> pure ()
    jsSubscribeNetwork cb
#else
networkStatusSub _toAction _sink _getModel = pure ()
#endif
