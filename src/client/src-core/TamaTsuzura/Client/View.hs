{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.View
    ( viewClient
    , displayTitle
    , formatUtcTimestamp
    ) where

import           Data.Text                   (Text)
import qualified Data.Text                   as Txt
import           Data.Time.Clock             (UTCTime)
import           Data.Time.Format            (defaultTimeLocale, formatTime)
import           Miso                        (Attribute, View, text)
import qualified Miso.Html.Element           as H
import qualified Miso.Html.Event             as HE
import qualified Miso.Html.Property          as HP
import           Miso.String                 (ms)
import           TamaTsuzura.Client.App
    ( ClientAction(..)
    , ClientState(..)
    , ConnectionStatus(..)
    )
import           TamaTsuzura.Client.I18n     (UiMessages(..), formatDocumentFormat)
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentSummary(..)
    , DocumentTitle
    )
import           TamaTsuzura.Common.Text     (ToText(..))
import           TamaTsuzura.ExtensionApi    (ExtensionMetadata(..))

-- | M2 Phase 1 のモバイルファースト骨格 UI をレンダリングする純粋ビュー関数。
-- Phase 2 以降で本シェルの上に CodeMirror 6 エディタ・プレビュー・検索/置換・整理 UI を統合する。
viewClient :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewClient msgs state =
    H.div_ [ HP.class_ "tt-app" ]
        [ viewHeader msgs state
        , H.main_ [ HP.class_ "tt-main" ]
            [ viewErrorBanner msgs state.lastError
            , viewActionBar msgs state
            , viewSelectedNotice msgs state.selectedDocument
            , viewDocumentsSection msgs state
            , viewExtensionsSection msgs state.extensionMetadatas
            ]
        ]

viewHeader :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewHeader msgs state =
    H.header_ [ HP.class_ "tt-header" ]
        [ H.div_ [ HP.class_ "tt-brand" ]
            [ H.h1_ [ HP.class_ "tt-title" ] [ text (ms msgs.appTitle) ]
            , H.p_ [ HP.class_ "tt-subtitle" ] [ text (ms msgs.appSubtitle) ]
            ]
        , viewConnectionBadge msgs state.connectionStatus
        ]

viewConnectionBadge :: UiMessages -> ConnectionStatus -> View () () ClientState ClientAction
viewConnectionBadge msgs status =
    let (badgeClass, labelText) = case status of
            ConnChecking -> ("tt-badge tt-badge-checking", msgs.statusChecking)
            ConnOnline   -> ("tt-badge tt-badge-online",   msgs.statusOnline)
            ConnError _  -> ("tt-badge tt-badge-error",    msgs.statusError)
    in  H.span_ [ HP.class_ badgeClass, HP.role_ "status" ]
            [ text (ms labelText) ]

viewErrorBanner :: UiMessages -> Maybe Text -> View () () ClientState ClientAction
viewErrorBanner _ Nothing = H.div_ [ HP.class_ "tt-error-slot" ] []
viewErrorBanner msgs (Just errText) =
    H.div_ [ HP.class_ "tt-error-banner", HP.role_ "alert" ]
        [ H.span_ [ HP.class_ "tt-error-text" ] [ text (ms errText) ]
        , H.button_
            [ HP.type_ "button"
            , HP.class_ "tt-btn tt-btn-ghost"
            , HE.onClick DismissError
            ]
            [ text (ms msgs.actionDismissError) ]
        ]

viewActionBar :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewActionBar msgs state =
    H.div_ [ HP.class_ "tt-toolbar" ]
        [ H.button_
            ( [ HP.type_ "button"
              , HP.class_ "tt-btn tt-btn-primary"
              , HE.onClick CreateBlankDocument
              ] <> disabledAttr state.isCreatingDocument
            )
            [ text (ms createLabel) ]
        , H.button_
            ( [ HP.type_ "button"
              , HP.class_ "tt-btn tt-btn-secondary"
              , HE.onClick RefreshAll
              ] <> disabledAttr isRefreshing
            )
            [ text (ms msgs.actionReload) ]
        ]
  where
    createLabel :: Text
    createLabel =
        if state.isCreatingDocument
            then msgs.actionCreating
            else msgs.actionNewDocument

    isRefreshing :: Bool
    isRefreshing = state.isLoadingDocuments || state.connectionStatus == ConnChecking

    disabledAttr :: Bool -> [Attribute ClientState ClientAction]
    disabledAttr True  = [ HP.disabled_ ]
    disabledAttr False = []

viewSelectedNotice :: UiMessages -> Maybe Document -> View () () ClientState ClientAction
viewSelectedNotice _ Nothing = H.div_ [ HP.class_ "tt-selected-slot" ] []
viewSelectedNotice msgs (Just doc) =
    H.section_ [ HP.class_ "tt-card tt-selected-card" ]
        [ H.h2_ [ HP.class_ "tt-section-heading" ] [ text (ms msgs.lastCreatedHeading) ]
        , H.p_ [ HP.class_ "tt-selected-meta" ]
            [ text (ms (displayTitle msgs doc.title <> " (" <> toText doc.id <> ")")) ]
        ]

viewDocumentsSection :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewDocumentsSection msgs state =
    H.section_ [ HP.class_ "tt-card" ]
        [ H.div_ [ HP.class_ "tt-section-header" ]
            [ H.h2_ [ HP.class_ "tt-section-heading" ] [ text (ms msgs.documentsHeading) ]
            , H.span_ [ HP.class_ "tt-count" ]
                [ text (ms (Txt.pack (show state.totalDocuments) <> msgs.documentsCountUnit)) ]
            ]
        , contentView
        ]
  where
    contentView :: View () () ClientState ClientAction
    contentView
        | state.isLoadingDocuments && null state.documentSummaries =
            H.p_ [ HP.class_ "tt-empty" ] [ text (ms msgs.loadingDocuments) ]
        | null state.documentSummaries =
            H.p_ [ HP.class_ "tt-empty" ] [ text (ms msgs.emptyDocuments) ]
        | otherwise =
            H.ul_ [ HP.class_ "tt-doc-list" ]
                (map (viewDocumentRow msgs) state.documentSummaries)

viewDocumentRow :: UiMessages -> DocumentSummary -> View () () ClientState ClientAction
viewDocumentRow msgs summary =
    H.li_ [ HP.class_ "tt-doc-item" ]
        [ H.div_ [ HP.class_ "tt-doc-title" ]
            [ text (ms (displayTitle msgs summary.title)) ]
        , H.div_ [ HP.class_ "tt-doc-meta" ]
            [ H.span_ [ HP.class_ "tt-doc-format" ]
                [ text (ms (formatDocumentFormat msgs summary.format)) ]
            , H.span_ [ HP.class_ "tt-doc-updated" ]
                [ text (ms (formatUtcTimestamp summary.updatedAt)) ]
            ]
        ]

viewExtensionsSection :: UiMessages -> [ExtensionMetadata] -> View () () ClientState ClientAction
viewExtensionsSection msgs metas =
    H.section_ [ HP.class_ "tt-card tt-ext-card" ]
        [ H.h2_ [ HP.class_ "tt-section-heading" ] [ text (ms msgs.extensionsHeading) ]
        , H.ul_ [ HP.class_ "tt-ext-list" ]
            (map viewExtItem metas)
        ]
  where
    viewExtItem :: ExtensionMetadata -> View () () ClientState ClientAction
    viewExtItem meta =
        H.li_ [ HP.class_ "tt-ext-item" ]
            [ H.strong_ [] [ text (ms (toText meta.name)) ]
            , H.span_ [ HP.class_ "tt-ext-desc" ] [ text (ms (" — " <> meta.description)) ]
            ]

-- | 空タイトルを UI カタログの「無題」ラベルへ補完して表示文字列を返す。
displayTitle :: UiMessages -> DocumentTitle -> Text
displayTitle msgs docTitle =
    let raw :: Text
        raw = toText docTitle
    in  if Txt.null raw then msgs.untitledDocument else raw

-- | UTC タイムスタンプを一覧表示用の簡潔な日時文字列 (@YYYY-MM-DD HH:MM UTC@) へ整形する。
formatUtcTimestamp :: UTCTime -> Text
formatUtcTimestamp = Txt.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC"
