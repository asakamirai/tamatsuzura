{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module TamaTsuzura.Client.App
    ( -- * ランタイム環境・状態・アクション型
      ClientEnv(..)
    , ConnectionStatus(..)
    , ClientState(..)
    , ClientAction(..)
      -- * 純粋な状態初期化・遷移関数
    , initialClientState
    , applyExtensionById
    , applyHealthResult
    , applyDocumentsResult
    , applyDocumentCreated
    , dismissError
      -- * Miso MVU 更新ロジック
    , updateClient
    , mkClientComponent
    ) where

import           Data.List                   (find)
import           Data.Text                   (Text)
import           Miso                        (App, Component(..), Effect, View, component, io, modify)
import           TamaTsuzura.Client.Api
    ( ApiClient
    , ApiClientError
    , createDocument
    , fetchHealth
    , listDocuments
    )
import           TamaTsuzura.Client.I18n     (UiMessages, formatApiClientError)
import           TamaTsuzura.Common.Api
    ( DocumentListResponse(..)
    , HealthStatus
    , defaultCreateDocumentRequest
    , defaultDocumentListQuery
    )
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody
    , DocumentSummary(..)
    , DocumentTitle
    , emptyDocumentBody
    , emptyDocumentTitle
    , toDocumentSummary
    )
import           TamaTsuzura.ExtensionApi    (Extension(..), ExtensionId, ExtensionMetadata(..))

-- | Miso コンポーネント実行時に外部から注入される不変の依存環境。
-- 関数を含む 'Extension' や 'HttpTransport' を 'ClientState' (Model) から切り離すことで、
-- Model の純粋な同値比較 ('Eq') と単体テスト容易性を両立する。
data ClientEnv = ClientEnv
    { apiClient  :: !ApiClient
      -- ^ M1 REST API 通信クライアント
    , extensions :: ![Extension]
      -- ^ 結線層から注入された同梱拡張一覧
    , messages   :: !UiMessages
      -- ^ UI 表示文言カタログ
    }

-- | サーバとの接続状態。
data ConnectionStatus
    = ConnChecking
      -- ^ 初回または再確認のヘルスチェック実行中
    | ConnOnline
      -- ^ サーバ疎通確認済み
    | ConnError !Text
      -- ^ 接続失敗 (ユーザー向け説明メッセージ)
    deriving (Eq, Show)

-- | クライアントコアの状態モデル (Miso Model)。
-- 本モジュールは 'tamatsuzura-common' と 'tamatsuzura-extension-api' のみに依存し、
-- 同梱拡張の具体実装には依存しない。
data ClientState = ClientState
    { connectionStatus   :: !ConnectionStatus
      -- ^ API サーバの疎通状態
    , documentSummaries  :: ![DocumentSummary]
      -- ^ 取得済みの文書サマリ一覧
    , totalDocuments     :: !Int
      -- ^ サーバ側の総一致文書件数
    , isLoadingDocuments :: !Bool
      -- ^ 文書一覧読み込み中フラグ
    , isCreatingDocument :: !Bool
      -- ^ 新規文書作成中フラグ
    , lastError          :: !(Maybe Text)
      -- ^ 直近発生したユーザー向けエラーメッセージ
    , selectedDocument   :: !(Maybe Document)
      -- ^ 直近選択・作成された文書エンティティ
    , draftTitle         :: !DocumentTitle
      -- ^ 現在編集中の文書タイトル (Phase 2 エディタ統合の基盤)
    , draftBody          :: !DocumentBody
      -- ^ 現在編集中の文書本文 (Phase 2 エディタ統合の基盤)
    , extensionMetadatas :: ![ExtensionMetadata]
      -- ^ 結線層から登録された拡張機能の表示用メタデータ一覧
    } deriving (Eq, Show)

-- | クライアント MVU ループで処理するアクション。
data ClientAction
    = Initialize
      -- ^ マウント時の初回疎通・一覧取得トリガ
    | RefreshAll
      -- ^ ヘルスチェックおよび文書一覧の手動再取得
    | HealthChecked !(Either ApiClientError HealthStatus)
      -- ^ @GET /api/health@ の結果受信
    | DocumentsLoaded !(Either ApiClientError DocumentListResponse)
      -- ^ @GET /api/documents@ の結果受信
    | CreateBlankDocument
      -- ^ 空の Markdown 文書を新規作成する
    | DocumentCreated !(Either ApiClientError Document)
      -- ^ @POST /api/documents@ の結果受信
    | ApplyExtension !ExtensionId
      -- ^ 指定した拡張機能を現在の編集本文へ適用する
    | DismissError
      -- ^ 表示中のエラーバナーを閉じる
    deriving (Eq, Show)

-- | 拡張一覧のメタデータを取り込んで初期クライアント状態を構築する。
initialClientState :: [Extension] -> ClientState
initialClientState exts = ClientState
    { connectionStatus   = ConnChecking
    , documentSummaries  = []
    , totalDocuments     = 0
    , isLoadingDocuments = False
    , isCreatingDocument = False
    , lastError          = Nothing
    , selectedDocument   = Nothing
    , draftTitle         = emptyDocumentTitle
    , draftBody          = emptyDocumentBody
    , extensionMetadatas = map (\ ext -> ext.metadata) exts
    }

-- | 指定した ID の拡張を現在の本文へ適用する。該当拡張が存在しない場合は状態を変更しない。
applyExtensionById :: [Extension] -> ExtensionId -> ClientState -> ClientState
applyExtensionById exts targetId state = case find hasTargetId exts of
    Nothing  -> state
    Just ext -> state { draftBody = ext.transformBuffer state.draftBody }
  where
    hasTargetId :: Extension -> Bool
    hasTargetId ext = ext.metadata.extensionId == targetId

-- | ヘルスチェック応答をクライアント状態へ純粋に反映する。
applyHealthResult ::
       UiMessages
    -> Either ApiClientError HealthStatus
    -> ClientState
    -> ClientState
applyHealthResult _ (Right _health) state =
    state { connectionStatus = ConnOnline }
applyHealthResult msgs (Left err) state =
    let formatted :: Text
        formatted = formatApiClientError msgs err
    in  state
            { connectionStatus = ConnError formatted
            , lastError        = Just formatted
            }

-- | 文書一覧応答をクライアント状態へ純粋に反映する。
-- 並行実行されたヘルスチェックが既に接続エラーを設定している場合は、そのエラー表示を消去しない。
applyDocumentsResult ::
       UiMessages
    -> Either ApiClientError DocumentListResponse
    -> ClientState
    -> ClientState
applyDocumentsResult _ (Right res) state =
    let nextError :: Maybe Text
        nextError = case state.connectionStatus of
            ConnError{} -> state.lastError
            _           -> Nothing
    in  state
            { documentSummaries  = res.documents
            , totalDocuments     = res.total
            , isLoadingDocuments = False
            , lastError          = nextError
            }
applyDocumentsResult msgs (Left err) state =
    state
        { isLoadingDocuments = False
        , lastError          = Just (formatApiClientError msgs err)
        }

-- | 文書新規作成応答をクライアント状態へ純粋に反映する。
-- 新規作成直後の文書を先頭へ即時反映し、画面の応答性を高める。
applyDocumentCreated ::
       UiMessages
    -> Either ApiClientError Document
    -> ClientState
    -> ClientState
applyDocumentCreated _ (Right doc) state =
    let summary :: DocumentSummary
        summary = toDocumentSummary doc

        remaining :: [DocumentSummary]
        remaining = filter (\ item -> item.id /= doc.id) state.documentSummaries

        nextTotal :: Int
        nextTotal =
            if length remaining == length state.documentSummaries
                then state.totalDocuments + 1
                else state.totalDocuments
    in  state
            { documentSummaries  = summary : remaining
            , totalDocuments     = nextTotal
            , isCreatingDocument = False
            , lastError          = Nothing
            , selectedDocument   = Just doc
            , draftTitle         = doc.title
            , draftBody          = doc.body
            }
applyDocumentCreated msgs (Left err) state =
    state
        { isCreatingDocument = False
        , lastError          = Just (formatApiClientError msgs err)
        }

-- | 表示中のエラーメッセージをクリアする。
dismissError :: ClientState -> ClientState
dismissError state = state { lastError = Nothing }

-- | Miso の MVU 更新関数。純粋な状態更新と非同期 API 呼び出しのスケジュールを担う。
updateClient :: ClientEnv -> ClientAction -> Effect () () ClientState ClientAction
updateClient env action = case action of
    Initialize -> triggerRefresh
    RefreshAll -> triggerRefresh
    HealthChecked eHealth ->
        modify (applyHealthResult env.messages eHealth)
    DocumentsLoaded eDocs ->
        modify (applyDocumentsResult env.messages eDocs)
    CreateBlankDocument -> do
        modify $ \ st -> st
            { isCreatingDocument = True
            , lastError          = Nothing
            }
        io (DocumentCreated <$> createDocument env.apiClient defaultCreateDocumentRequest)
    DocumentCreated eDoc ->
        modify (applyDocumentCreated env.messages eDoc)
    ApplyExtension extId ->
        modify (applyExtensionById env.extensions extId)
    DismissError ->
        modify dismissError
  where
    triggerRefresh :: Effect () () ClientState ClientAction
    triggerRefresh = do
        modify $ \ st -> st
            { connectionStatus   = ConnChecking
            , isLoadingDocuments = True
            , lastError          = Nothing
            }
        io (HealthChecked <$> fetchHealth env.apiClient)
        io (DocumentsLoaded <$> listDocuments env.apiClient defaultDocumentListQuery)

-- | ビュー関数と依存環境を束ねてトップレベル Miso 'App' コンポーネントを構築する。
-- マウント直後に 'Initialize' を発火し、ヘルスチェックと文書一覧取得を自動実行する。
mkClientComponent ::
       ClientEnv
    -> (UiMessages -> ClientState -> View () () ClientState ClientAction)
    -> App ClientState ClientAction
mkClientComponent env renderView =
    (component (initialClientState env.extensions) (updateClient env) (renderView env.messages))
        { mount = Just Initialize
        }
