{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.View
    ( viewClient
    , displayTitle
    , formatUtcTimestamp
    , formatSearchMatchCounter
    ) where

import           Data.Either                     (fromRight)
import           Data.Maybe                      (isJust, isNothing)
import           Data.Text                       (Text)
import qualified Data.Text                       as Txt
import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Format                (defaultTimeLocale, formatTime)
import           Miso                            (Attribute, Checked(..), View, text)
import qualified Miso.Html.Element               as H
import qualified Miso.Html.Event                 as HE
import qualified Miso.Html.Property              as HP
import           Miso.String                     (MisoString, fromMisoString, ms)
import           TamaTsuzura.Client.App
    ( ClientAction(..)
    , ClientState(..)
    , ConnectionStatus(..)
    , EditorDraft(..)
    , EditorViewMode(..)
    , FilterState(..)
    , MobilePane(..)
    , SearchPanelState(..)
    , hasActiveFilters
    , isCurrentDocumentDirty
    )
import           TamaTsuzura.Client.EditorBridge (EditorStats(..))
import           TamaTsuzura.Client.I18n         (UiMessages(..), formatDocumentFormat)
import           TamaTsuzura.Client.Markdown     (renderMarkdownView)
import           TamaTsuzura.Common.Api          (DocumentStatusFilter(..))
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentFormat(..)
    , DocumentId
    , DocumentSummary(..)
    , DocumentTitle
    , FolderPath
    , Tag
    , isSummaryTrashed
    , isTrashed
    , mkFolderPath
    , mkTag
    , parseDocumentFormat
    , tagSetToList
    )
import           TamaTsuzura.Common.Text         (ToText(..))
import           TamaTsuzura.ExtensionApi        (ExtensionMetadata(..))

-- | M2 Phase 2 のモバイルファースト日本語 UI をレンダリングする純粋ビュー関数。
-- スマートフォン幅では一覧ペインとエディタペインを切り替え、
-- PC / タブレット広幅では左サイドバー (一覧・整理) + 右ワークスペース (CodeMirror 6 / プレビュー) の
-- 2 ペイン構成で表示する。
viewClient :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewClient msgs state =
    H.div_ [ HP.class_ (ms appClass) ]
        [ viewHeader msgs state
        , viewNotifications msgs state
        , H.div_ [ HP.class_ "tt-shell" ]
            [ viewListPane msgs state
            , viewEditorPane msgs state
            ]
        , viewModals msgs state
        ]
  where
    appClass :: Text
    appClass = case state.mobilePane of
        PaneList   -> "tt-app tt-pane-list"
        PaneEditor -> "tt-app tt-pane-editor"

viewHeader :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewHeader msgs state =
    H.header_ [ HP.class_ "tt-header" ]
        [ H.div_ [ HP.class_ "tt-brand" ]
            [ H.h1_ [ HP.class_ "tt-title" ] [ text (ms msgs.appTitle) ]
            , H.p_ [ HP.class_ "tt-subtitle" ] [ text (ms msgs.appSubtitle) ]
            ]
        , H.div_ [ HP.class_ "tt-header-actions" ]
            [ viewConnectionBadge msgs state.connectionStatus
            , H.button_
                ( [ HP.type_ "button"
                  , HP.class_ "tt-btn tt-btn-primary tt-btn-compact"
                  , HE.onClick RequestCreateDocument
                  ] <> disabledAttr state.isCreatingDocument
                )
                [ text (ms createLabel) ]
            , H.button_
                ( [ HP.type_ "button"
                  , HP.class_ "tt-btn tt-btn-secondary tt-btn-compact"
                  , HE.onClick RefreshAll
                  ] <> disabledAttr isRefreshing
                )
                [ text (ms msgs.actionReload) ]
            ]
        ]
  where
    createLabel :: Text
    createLabel =
        if state.isCreatingDocument
            then msgs.actionCreating
            else msgs.actionNewDocument

    isRefreshing :: Bool
    isRefreshing = state.isLoadingDocuments || state.connectionStatus == ConnChecking

viewConnectionBadge :: UiMessages -> ConnectionStatus -> View () () ClientState ClientAction
viewConnectionBadge msgs status =
    let (badgeClass, labelText) = case status of
            ConnChecking -> ("tt-badge tt-badge-checking", msgs.statusChecking)
            ConnOnline   -> ("tt-badge tt-badge-online",   msgs.statusOnline)
            ConnError{}  -> ("tt-badge tt-badge-error",    msgs.statusError)
    in  H.span_ [ HP.class_ badgeClass, HP.role_ "status" ]
            [ text (ms labelText) ]

viewNotifications :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewNotifications msgs state =
    H.div_ [ HP.class_ "tt-notifications" ]
        [ viewErrorBanner msgs state.lastError
        , viewNoticeBanner msgs state.statusNotice
        ]

viewErrorBanner :: UiMessages -> Maybe Text -> View () () ClientState ClientAction
viewErrorBanner _ Nothing = H.div_ [ HP.class_ "tt-slot-empty" ] []
viewErrorBanner msgs (Just errText) =
    H.div_ [ HP.class_ "tt-banner tt-banner-error", HP.role_ "alert" ]
        [ H.span_ [ HP.class_ "tt-banner-text" ] [ text (ms errText) ]
        , H.button_
            [ HP.type_ "button"
            , HP.class_ "tt-btn tt-btn-ghost"
            , HE.onClick DismissError
            ]
            [ text (ms msgs.actionDismissError) ]
        ]

viewNoticeBanner :: UiMessages -> Maybe Text -> View () () ClientState ClientAction
viewNoticeBanner _ Nothing = H.div_ [ HP.class_ "tt-slot-empty" ] []
viewNoticeBanner msgs (Just noticeText) =
    H.div_ [ HP.class_ "tt-banner tt-banner-notice", HP.role_ "status" ]
        [ H.span_ [ HP.class_ "tt-banner-text" ] [ text (ms noticeText) ]
        , H.button_
            [ HP.type_ "button"
            , HP.class_ "tt-btn tt-btn-ghost"
            , HE.onClick DismissNotice
            ]
            [ text (ms msgs.actionDismissError) ]
        ]

viewListPane :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewListPane msgs state =
    H.aside_ [ HP.class_ "tt-sidebar" ]
        [ viewStatusTabs msgs state.filterState.status
        , viewListFilters msgs state
        , H.div_ [ HP.class_ "tt-section-header tt-list-header" ]
            [ H.h2_ [ HP.class_ "tt-section-heading" ] [ text (ms headingText) ]
            , H.span_ [ HP.class_ "tt-count" ]
                [ text (ms (Txt.pack (show state.totalDocuments) <> msgs.documentsCountUnit)) ]
            ]
        , viewDocumentListBody msgs state
        ]
  where
    headingText :: Text
    headingText = case state.filterState.status of
        StatusTrashed -> msgs.tabTrashDocuments
        _             -> msgs.documentsHeading

viewStatusTabs :: UiMessages -> DocumentStatusFilter -> View () () ClientState ClientAction
viewStatusTabs msgs currentStatus =
    H.div_ [ HP.class_ "tt-segmented", HP.role_ "tablist" ]
        [ tabButton StatusActive  msgs.tabActiveDocuments
        , tabButton StatusTrashed msgs.tabTrashDocuments
        ]
  where
    tabButton :: DocumentStatusFilter -> Text -> View () () ClientState ClientAction
    tabButton target labelText =
        let isActive :: Bool
            isActive = currentStatus == target

            btnClass :: Text
            btnClass = if isActive then "tt-seg-btn tt-seg-btn-active" else "tt-seg-btn"
        in  H.button_
                [ HP.type_ "button"
                , HP.class_ (ms btnClass)
                , HP.role_ "tab"
                , HP.aria_ "selected" (if isActive then "true" else "false")
                , HE.onClick (RequestStatusFilter target)
                ]
                [ text (ms labelText) ]

viewListFilters :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewListFilters msgs state =
    H.div_ [ HP.class_ "tt-filters" ]
        [ H.form_ [ HP.class_ "tt-search-form", HE.onSubmit SubmitListSearch ]
            [ H.input_
                [ HP.type_ "search"
                , HP.class_ "tt-input tt-search-input"
                , HP.value_ (ms flt.searchInput)
                , HP.placeholder_ (ms msgs.listSearchPlaceholder)
                , HP.aria_ "label" (ms msgs.listSearchPlaceholder)
                , HE.onInput (UpdateListSearchInput . toTextFromMiso)
                ]
            , H.button_
                [ HP.type_ "submit"
                , HP.class_ "tt-btn tt-btn-secondary tt-btn-compact"
                ]
                [ text (ms msgs.actionSearchRun) ]
            ]
        , H.div_ [ HP.class_ "tt-filter-selects" ]
            [ H.select_
                [ HP.class_ "tt-select"
                , HP.value_ (ms (maybe "" toText flt.selectedFolder))
                , HP.aria_ "label" (ms msgs.filterFolderLabel)
                , HE.onChange (onFolderFilterChange . toTextFromMiso)
                ]
                ( H.option_
                    [ HP.value_ ""
                    , HP.selected_ (isNothing flt.selectedFolder)
                    ]
                    [ text (ms msgs.filterAllFolders) ]
                : map folderOption folderChoices
                )
            , H.select_
                [ HP.class_ "tt-select"
                , HP.value_ (ms (maybe "" toText flt.selectedTag))
                , HP.aria_ "label" (ms msgs.filterTagLabel)
                , HE.onChange (onTagFilterChange . toTextFromMiso)
                ]
                ( H.option_
                    [ HP.value_ ""
                    , HP.selected_ (isNothing flt.selectedTag)
                    ]
                    [ text (ms msgs.filterAllTags) ]
                : map tagOption tagChoices
                )
            ]
        , viewFilterSubRow
        ]
  where
    flt :: FilterState
    flt = state.filterState

    -- 絞り込み中のフォルダ・タグが候補一覧から消えた場合も現在の選択値を失わないよう選択肢へ含める
    folderChoices :: [FolderPath]
    folderChoices = case flt.selectedFolder of
        Just fp | fp `notElem` state.availableFolders -> fp : state.availableFolders
        _                                             -> state.availableFolders

    tagChoices :: [Tag]
    tagChoices = case flt.selectedTag of
        Just t | t `notElem` state.availableTags -> t : state.availableTags
        _                                        -> state.availableTags

    folderOption :: FolderPath -> View () () ClientState ClientAction
    folderOption fp =
        let txt :: Text
            txt = toText fp
        in  H.option_
                [ HP.value_ (ms txt)
                , HP.selected_ (flt.selectedFolder == Just fp)
                ]
                [ text (ms txt) ]

    tagOption :: Tag -> View () () ClientState ClientAction
    tagOption tag =
        let txt :: Text
            txt = toText tag
        in  H.option_
                [ HP.value_ (ms txt)
                , HP.selected_ (flt.selectedTag == Just tag)
                ]
                [ text (ms ("#" <> txt)) ]

    onFolderFilterChange :: Text -> ClientAction
    onFolderFilterChange raw =
        SelectFilterFolder (either (const Nothing) Just (mkFolderPath raw))

    onTagFilterChange :: Text -> ClientAction
    onTagFilterChange raw =
        SelectFilterTag (either (const Nothing) Just (mkTag raw))

    viewFilterSubRow :: View () () ClientState ClientAction
    viewFilterSubRow
        | isJust flt.selectedFolder || hasActiveFilters flt =
            H.div_ [ HP.class_ "tt-filter-subrow" ]
                [ if isJust flt.selectedFolder
                    then H.label_ [ HP.class_ "tt-checkbox-label" ]
                        [ H.input_
                            [ HP.type_ "checkbox"
                            , HP.checked_ flt.includeSubfolders
                            , HE.onChecked (\ (Checked b) -> ToggleIncludeSubfolders b)
                            ]
                        , text (ms msgs.filterIncludeSubfolders)
                        ]
                    else H.span_ [] []
                , if hasActiveFilters flt
                    then H.button_
                        [ HP.type_ "button"
                        , HP.class_ "tt-btn tt-btn-ghost tt-btn-xs"
                        , HE.onClick ClearFilters
                        ]
                        [ text (ms msgs.actionClearFilters) ]
                    else H.span_ [] []
                ]
        | otherwise = H.div_ [ HP.class_ "tt-slot-empty" ] []

viewDocumentListBody :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewDocumentListBody msgs state
    | state.isLoadingDocuments && null state.documentSummaries =
        H.p_ [ HP.class_ "tt-empty" ] [ text (ms msgs.loadingDocuments) ]
    | null state.documentSummaries =
        H.p_ [ HP.class_ "tt-empty" ] [ text (ms emptyMessage) ]
    | otherwise =
        H.ul_ [ HP.class_ "tt-doc-list" ]
            (map (viewDocumentRow msgs state) state.documentSummaries)
  where
    emptyMessage :: Text
    emptyMessage
        | hasActiveFilters state.filterState         = msgs.emptyFilteredDocuments
        | state.filterState.status == StatusTrashed  = msgs.emptyTrashDocuments
        | otherwise                                  = msgs.emptyDocuments

viewDocumentRow :: UiMessages -> ClientState -> DocumentSummary -> View () () ClientState ClientAction
viewDocumentRow msgs state summary =
    H.li_ [ HP.class_ (ms rowClass) ]
        [ H.button_
            [ HP.type_ "button"
            , HP.class_ "tt-doc-select-btn"
            , HE.onClick (RequestOpenDocument summary.id)
            ]
            [ H.div_ [ HP.class_ "tt-doc-title" ]
                [ text (ms (displayTitle msgs summary.title)) ]
            , H.div_ [ HP.class_ "tt-doc-meta" ]
                ( [ H.span_ [ HP.class_ "tt-doc-format" ]
                        [ text (ms (formatDocumentFormat msgs summary.format)) ]
                  ]
                    <> folderBadge summary.folder
                    <> [ H.span_ [ HP.class_ "tt-doc-updated" ]
                            [ text (ms (formatUtcTimestamp summary.updatedAt)) ]
                       ]
                )
            , viewRowTags summary
            ]
        , if isSummaryTrashed summary
            then viewTrashQuickActions msgs state summary.id
            else H.div_ [ HP.class_ "tt-slot-empty" ] []
        ]
  where
    isSelected :: Bool
    isSelected = fmap (.id) state.selectedDocument == Just summary.id

    rowClass :: Text
    rowClass = if isSelected then "tt-doc-item tt-doc-item-active" else "tt-doc-item"

    folderBadge :: Maybe FolderPath -> [View () () ClientState ClientAction]
    folderBadge Nothing   = []
    folderBadge (Just fp) =
        [ H.span_ [ HP.class_ "tt-doc-folder" ] [ text (ms (toText fp)) ] ]

    viewRowTags :: DocumentSummary -> View () () ClientState ClientAction
    viewRowTags item = case tagSetToList item.tags of
        []   -> H.div_ [ HP.class_ "tt-slot-empty" ] []
        tags -> H.div_ [ HP.class_ "tt-tag-chips" ]
            (map (\ t -> H.span_ [ HP.class_ "tt-tag-chip" ] [ text (ms ("#" <> toText t)) ]) tags)

viewTrashQuickActions :: UiMessages -> ClientState -> DocumentId -> View () () ClientState ClientAction
viewTrashQuickActions msgs state docId =
    H.div_ [ HP.class_ "tt-doc-row-actions" ]
        [ H.button_
            ( [ HP.type_ "button"
              , HP.class_ "tt-btn tt-btn-secondary tt-btn-xs"
              , HE.onClick (RestoreDocument docId)
              ] <> disabledAttr isBusy
            )
            [ text (ms msgs.actionRestore) ]
        , H.button_
            ( [ HP.type_ "button"
              , HP.class_ "tt-btn tt-btn-danger tt-btn-xs"
              , HE.onClick (RequestPermanentDelete docId)
              ] <> disabledAttr isBusy
            )
            [ text (ms msgs.actionPermanentDelete) ]
        ]
  where
    isBusy :: Bool
    isBusy = isJust state.mutatingDocumentId

viewEditorPane :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewEditorPane msgs state =
    H.main_ [ HP.class_ "tt-editor-pane" ]
        [ case state.selectedDocument of
            Nothing
                | isJust state.openingDocumentId ->
                    H.div_ [ HP.class_ "tt-editor-empty" ]
                        [ H.p_ [] [ text (ms msgs.loadingDocumentBody) ] ]
                | otherwise ->
                    H.div_ [ HP.class_ "tt-editor-empty" ]
                        [ H.p_ [] [ text (ms msgs.emptyEditorPrompt) ]
                        , H.button_
                            ( [ HP.type_ "button"
                              , HP.class_ "tt-btn tt-btn-primary"
                              , HE.onClick RequestCreateDocument
                              ] <> disabledAttr state.isCreatingDocument
                            )
                            [ text (ms msgs.actionNewDocument) ]
                        ]
            Just doc ->
                viewActiveEditor msgs state doc
        ]

viewActiveEditor :: UiMessages -> ClientState -> Document -> View () () ClientState ClientAction
viewActiveEditor msgs state doc =
    H.div_ [ HP.class_ "tt-editor-shell" ]
        [ viewEditorToolbar msgs state doc
        , if readOnly
            then H.div_ [ HP.class_ "tt-banner tt-banner-warn" ]
                [ H.span_ [] [ text (ms msgs.trashReadOnlyBanner) ] ]
            else H.div_ [ HP.class_ "tt-slot-empty" ] []
        , viewTitleAndMetadata msgs state readOnly
        , if state.searchPanel.isOpen
            then viewSearchPanel msgs state.searchPanel readOnly
            else H.div_ [ HP.class_ "tt-slot-empty" ] []
        , viewEditorWorkspace msgs state
        , viewEditorStatusBar msgs state readOnly
        ]
  where
    readOnly :: Bool
    readOnly = isTrashed doc

viewEditorToolbar :: UiMessages -> ClientState -> Document -> View () () ClientState ClientAction
viewEditorToolbar msgs state doc =
    H.div_ [ HP.class_ "tt-editor-toolbar" ]
        [ H.div_ [ HP.class_ "tt-toolbar-group" ]
            [ H.button_
                [ HP.type_ "button"
                , HP.class_ "tt-btn tt-btn-secondary tt-btn-compact tt-mobile-only"
                , HE.onClick ReturnToList
                ]
                [ text (ms msgs.actionBackToList) ]
            , viewSaveOrTrashPrimaryActions
            ]
        , H.div_ [ HP.class_ "tt-toolbar-group" ]
            ( viewModeButtons
                <> [ H.button_
                        ( [ HP.type_ "button"
                          , HP.class_ "tt-btn tt-btn-secondary tt-btn-compact"
                          , HE.onClick EditorUndoClicked
                          ] <> disabledAttr (readOnly || not state.canUndo)
                        )
                        [ text (ms msgs.actionUndo) ]
                   , H.button_
                        ( [ HP.type_ "button"
                          , HP.class_ "tt-btn tt-btn-secondary tt-btn-compact"
                          , HE.onClick EditorRedoClicked
                          ] <> disabledAttr (readOnly || not state.canRedo)
                        )
                        [ text (ms msgs.actionRedo) ]
                   , H.button_
                        [ HP.type_ "button"
                        , HP.class_ (ms searchBtnClass)
                        , HE.onClick (ToggleSearchPanel False)
                        ]
                        [ text (ms msgs.actionSearchToggle) ]
                   , H.button_
                        [ HP.type_ "button"
                        , HP.class_ (ms metaBtnClass)
                        , HE.onClick ToggleMetadataPanel
                        ]
                        [ text (ms msgs.actionMetadataToggle) ]
                   ]
                <> [ H.button_
                        ( [ HP.type_ "button"
                          , HP.class_ "tt-btn tt-btn-danger-ghost tt-btn-compact"
                          , HE.onClick (SoftDeleteDocument doc.id)
                          ] <> disabledAttr isBusy
                        )
                        [ text (ms msgs.actionMoveToTrash) ]
                   | not readOnly
                   ]
            )
        ]
  where
    readOnly :: Bool
    readOnly = isTrashed doc

    isDirty :: Bool
    isDirty = isCurrentDocumentDirty state

    isSaving :: Bool
    isSaving = isJust state.savingDocument

    isBusy :: Bool
    isBusy = isJust state.mutatingDocumentId || isSaving

    searchBtnClass :: Text
    searchBtnClass =
        if state.searchPanel.isOpen
            then "tt-btn tt-btn-secondary tt-btn-compact tt-btn-active"
            else "tt-btn tt-btn-secondary tt-btn-compact"

    metaBtnClass :: Text
    metaBtnClass =
        if state.metadataPanelOpen
            then "tt-btn tt-btn-secondary tt-btn-compact tt-btn-active"
            else "tt-btn tt-btn-secondary tt-btn-compact"

    viewSaveOrTrashPrimaryActions :: View () () ClientState ClientAction
    viewSaveOrTrashPrimaryActions
        | readOnly =
            H.div_ [ HP.class_ "tt-toolbar-inline" ]
                [ H.span_ [ HP.class_ "tt-state-pill tt-state-readonly" ]
                    [ text (ms msgs.stateReadOnlyLabel) ]
                , H.button_
                    ( [ HP.type_ "button"
                      , HP.class_ "tt-btn tt-btn-primary tt-btn-compact"
                      , HE.onClick (RestoreDocument doc.id)
                      ] <> disabledAttr isBusy
                    )
                    [ text (ms msgs.actionRestore) ]
                , H.button_
                    ( [ HP.type_ "button"
                      , HP.class_ "tt-btn tt-btn-danger tt-btn-compact"
                      , HE.onClick (RequestPermanentDelete doc.id)
                      ] <> disabledAttr isBusy
                    )
                    [ text (ms msgs.actionPermanentDelete) ]
                ]
        | otherwise =
            let saveLabel :: Text
                saveLabel = if isSaving then msgs.actionSaving else msgs.actionSave

                stateClass :: Text
                stateClass =
                    if isDirty
                        then "tt-state-pill tt-state-unsaved"
                        else "tt-state-pill tt-state-saved"

                stateLabel :: Text
                stateLabel =
                    if isDirty
                        then msgs.stateUnsavedLabel
                        else msgs.stateSavedLabel
            in  H.div_ [ HP.class_ "tt-toolbar-inline" ]
                    [ H.button_
                        ( [ HP.type_ "button"
                          , HP.class_ "tt-btn tt-btn-primary tt-btn-compact"
                          , HE.onClick SaveCurrentDocument
                          ] <> disabledAttr (isSaving || not isDirty)
                        )
                        [ text (ms saveLabel) ]
                    , H.span_ [ HP.class_ (ms stateClass) ] [ text (ms stateLabel) ]
                    ]

    viewModeButtons :: [View () () ClientState ClientAction]
    viewModeButtons = case state.draft.format of
        FormatPlainText -> []
        FormatMarkdown  ->
            [ H.div_ [ HP.class_ "tt-mode-switch", HP.role_ "tablist" ]
                [ modeBtn ModeEdit    msgs.modeEditLabel    ""
                , modeBtn ModeSplit   msgs.modeSplitLabel   " tt-desktop-only"
                , modeBtn ModePreview msgs.modePreviewLabel ""
                ]
            ]

    modeBtn :: EditorViewMode -> Text -> Text -> View () () ClientState ClientAction
    modeBtn mode labelText extraClass =
        let activeClass :: Text
            activeClass = if state.editorViewMode == mode then " tt-mode-btn-active" else ""
        in  H.button_
                [ HP.type_ "button"
                , HP.class_ (ms ("tt-mode-btn" <> activeClass <> extraClass))
                , HP.role_ "tab"
                , HP.aria_ "selected" (if state.editorViewMode == mode then "true" else "false")
                , HE.onClick (SetEditorViewMode mode)
                ]
                [ text (ms labelText) ]

viewTitleAndMetadata :: UiMessages -> ClientState -> Bool -> View () () ClientState ClientAction
viewTitleAndMetadata msgs state readOnly =
    H.div_ [ HP.class_ "tt-doc-header-fields" ]
        [ H.input_
            [ HP.type_ "text"
            , HP.class_ "tt-title-input"
            , HP.value_ (ms state.draft.titleInput)
            , HP.placeholder_ (ms msgs.fieldTitlePlaceholder)
            , HP.aria_ "label" (ms msgs.fieldTitleLabel)
            , HP.readonly_ readOnly
            , HE.onInput (UpdateDraftTitle . toTextFromMiso)
            ]
        , if state.metadataPanelOpen
            then viewMetadataPanel msgs state.draft readOnly
            else H.div_ [ HP.class_ "tt-slot-empty" ] []
        ]

viewMetadataPanel :: UiMessages -> EditorDraft -> Bool -> View () () ClientState ClientAction
viewMetadataPanel msgs currentDraft readOnly =
    H.div_ [ HP.class_ "tt-metadata-panel" ]
        [ H.label_ [ HP.class_ "tt-field" ]
            [ H.span_ [ HP.class_ "tt-field-label" ] [ text (ms msgs.fieldFormatLabel) ]
            , H.select_
                ( [ HP.class_ "tt-select"
                  , HP.value_ (ms (toText currentDraft.format))
                  , HE.onChange (onFormatChange . toTextFromMiso)
                  ] <> disabledAttr readOnly
                )
                [ H.option_
                    [ HP.value_ "markdown"
                    , HP.selected_ (currentDraft.format == FormatMarkdown)
                    ]
                    [ text (ms msgs.formatMarkdownLabel) ]
                , H.option_
                    [ HP.value_ "plain_text"
                    , HP.selected_ (currentDraft.format == FormatPlainText)
                    ]
                    [ text (ms msgs.formatPlainTextLabel) ]
                ]
            ]
        , H.label_ [ HP.class_ "tt-field" ]
            [ H.span_ [ HP.class_ "tt-field-label" ] [ text (ms msgs.fieldFolderLabel) ]
            , H.input_
                [ HP.type_ "text"
                , HP.class_ "tt-input"
                , HP.value_ (ms currentDraft.folderInput)
                , HP.placeholder_ (ms msgs.fieldFolderPlaceholder)
                , HP.readonly_ readOnly
                , HE.onInput (UpdateDraftFolder . toTextFromMiso)
                ]
            ]
        , H.label_ [ HP.class_ "tt-field tt-field-wide" ]
            [ H.span_ [ HP.class_ "tt-field-label" ] [ text (ms msgs.fieldTagsLabel) ]
            , H.input_
                [ HP.type_ "text"
                , HP.class_ "tt-input"
                , HP.value_ (ms currentDraft.tagsInput)
                , HP.placeholder_ (ms msgs.fieldTagsPlaceholder)
                , HP.readonly_ readOnly
                , HE.onInput (UpdateDraftTags . toTextFromMiso)
                ]
            ]
        ]
  where
    onFormatChange :: Text -> ClientAction
    onFormatChange raw =
        UpdateDraftFormat (fromRight FormatMarkdown (parseDocumentFormat raw))

viewSearchPanel :: UiMessages -> SearchPanelState -> Bool -> View () () ClientState ClientAction
viewSearchPanel msgs sp readOnly =
    H.div_ [ HP.class_ "tt-search-panel", HP.role_ "search" ]
        [ H.div_ [ HP.class_ "tt-search-row" ]
            [ H.input_
                [ HP.type_ "text"
                , HP.class_ "tt-input tt-search-field"
                , HP.value_ (ms sp.query)
                , HP.placeholder_ (ms msgs.searchInputPlaceholder)
                , HP.aria_ "label" (ms msgs.searchInputPlaceholder)
                , HE.onInput (UpdateSearchQuery . toTextFromMiso)
                ]
            , H.button_
                [ HP.type_ "button"
                , HP.class_ (ms (toggleBtnClass sp.caseSensitive))
                , HP.aria_ "pressed" (if sp.caseSensitive then "true" else "false")
                , HE.onClick ToggleSearchCaseSensitive
                ]
                [ text (ms msgs.searchCaseSensitiveLabel) ]
            , H.button_
                [ HP.type_ "button"
                , HP.class_ (ms (toggleBtnClass sp.useRegex))
                , HP.aria_ "pressed" (if sp.useRegex then "true" else "false")
                , HE.onClick ToggleSearchRegex
                ]
                [ text (ms msgs.searchRegexLabel) ]
            , H.span_ [ HP.class_ "tt-search-counter" ]
                [ text (ms (formatSearchMatchCounter msgs sp)) ]
            , H.button_
                [ HP.type_ "button"
                , HP.class_ "tt-btn tt-btn-secondary tt-btn-xs"
                , HE.onClick FindPrevMatch
                ]
                [ text (ms msgs.searchPrevLabel) ]
            , H.button_
                [ HP.type_ "button"
                , HP.class_ "tt-btn tt-btn-secondary tt-btn-xs"
                , HE.onClick FindNextMatch
                ]
                [ text (ms msgs.searchNextLabel) ]
            , if not readOnly
                then H.button_
                    [ HP.type_ "button"
                    , HP.class_ (ms (toggleBtnClass sp.showReplace))
                    , HE.onClick ToggleReplaceRow
                    ]
                    [ text (ms msgs.searchToggleReplaceLabel) ]
                else H.span_ [] []
            , H.button_
                [ HP.type_ "button"
                , HP.class_ "tt-btn tt-btn-ghost tt-btn-xs"
                , HE.onClick CloseSearchPanel
                ]
                [ text (ms msgs.actionDismissError) ]
            ]
        , if sp.showReplace && not readOnly
            then H.div_ [ HP.class_ "tt-search-row" ]
                [ H.input_
                    [ HP.type_ "text"
                    , HP.class_ "tt-input tt-search-field"
                    , HP.value_ (ms sp.replacement)
                    , HP.placeholder_ (ms msgs.replaceInputPlaceholder)
                    , HP.aria_ "label" (ms msgs.replaceInputPlaceholder)
                    , HE.onInput (UpdateReplaceText . toTextFromMiso)
                    ]
                , H.button_
                    [ HP.type_ "button"
                    , HP.class_ "tt-btn tt-btn-secondary tt-btn-xs"
                    , HE.onClick ReplaceCurrentMatch
                    ]
                    [ text (ms msgs.searchReplaceLabel) ]
                , H.button_
                    [ HP.type_ "button"
                    , HP.class_ "tt-btn tt-btn-secondary tt-btn-xs"
                    , HE.onClick ReplaceAllMatches
                    ]
                    [ text (ms msgs.searchReplaceAllLabel) ]
                ]
            else H.div_ [ HP.class_ "tt-slot-empty" ] []
        ]
  where
    toggleBtnClass :: Bool -> Text
    toggleBtnClass True  = "tt-btn tt-btn-secondary tt-btn-xs tt-btn-active"
    toggleBtnClass False = "tt-btn tt-btn-secondary tt-btn-xs"

-- | CodeMirror 6 ホストと Markdown プレビューのワークスペース。
-- 編集/プレビュー切替時も Custom Element @\<tt-codemirror-editor\>@ を DOM から破棄せず
-- CSS クラスで可視性のみ切り替えることで、CodeMirror 6 の Undo 履歴とカーソル位置を保持する。
viewEditorWorkspace :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewEditorWorkspace msgs state =
    H.div_ [ HP.class_ (ms workspaceClass) ]
        [ H.div_ [ HP.class_ "tt-cm-pane" ]
            [ H.nodeHtml "tt-codemirror-editor"
                [ HP.id_ "tt-cm-editor"
                , HP.class_ "tt-cm-host"
                ]
                []
            ]
        , H.div_ [ HP.class_ "tt-preview-pane" ]
            [ if shouldRenderPreview
                then viewMarkdownContent
                else H.div_ [ HP.class_ "tt-slot-empty" ] []
            ]
        ]
  where
    workspaceClass :: Text
    workspaceClass = case state.editorViewMode of
        ModeEdit    -> "tt-workspace tt-workspace-edit"
        ModeSplit   -> "tt-workspace tt-workspace-split"
        ModePreview -> "tt-workspace tt-workspace-preview"

    shouldRenderPreview :: Bool
    shouldRenderPreview =
        state.draft.format == FormatMarkdown && state.editorViewMode /= ModeEdit

    viewMarkdownContent :: View () () ClientState ClientAction
    viewMarkdownContent =
        let rawBody :: Text
            rawBody = toText state.draft.body
        in  if Txt.null (Txt.strip rawBody)
                then H.p_ [ HP.class_ "tt-empty" ] [ text (ms msgs.previewEmptyHint) ]
                else renderMarkdownView rawBody

viewEditorStatusBar :: UiMessages -> ClientState -> Bool -> View () () ClientState ClientAction
viewEditorStatusBar msgs state readOnly =
    H.footer_ [ HP.class_ "tt-statusbar" ]
        [ H.div_ [ HP.class_ "tt-status-metrics" ]
            [ H.span_ []
                [ text (ms (showInt stats.lineCount <> msgs.statsLinesUnit)) ]
            , H.span_ []
                [ text (ms (showInt stats.charCount <> msgs.statsCharsUnit)) ]
            , H.span_ []
                [ text (ms (msgs.statsCursorPrefix <> " " <> showInt stats.cursorLine <> ":" <> showInt stats.cursorCol)) ]
            ]
        , viewExtensionQuickActions msgs state.extensionMetadatas readOnly
        ]
  where
    stats :: EditorStats
    stats = state.editorStats

    showInt :: Int -> Text
    showInt = Txt.pack . show

viewExtensionQuickActions ::
       UiMessages
    -> [ExtensionMetadata]
    -> Bool
    -> View () () ClientState ClientAction
viewExtensionQuickActions _ [] _ = H.div_ [ HP.class_ "tt-slot-empty" ] []
viewExtensionQuickActions msgs metas readOnly =
    H.div_ [ HP.class_ "tt-ext-bar" ]
        ( H.span_ [ HP.class_ "tt-ext-label" ] [ text (ms (msgs.extensionsHeading <> ":")) ]
        : map extButton metas
        )
  where
    extButton :: ExtensionMetadata -> View () () ClientState ClientAction
    extButton meta =
        H.button_
            ( [ HP.type_ "button"
              , HP.class_ "tt-btn tt-btn-ghost tt-btn-xs"
              , HP.title_ (ms meta.description)
              , HE.onClick (ApplyExtension meta.extensionId)
              ] <> disabledAttr readOnly
            )
            [ text (ms (toText meta.name)) ]

viewModals :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewModals msgs state =
    H.div_ [ HP.class_ "tt-modals" ]
        [ case state.pendingNavigation of
            Nothing -> H.div_ [ HP.class_ "tt-slot-empty" ] []
            Just _  ->
                viewConfirmModal
                    msgs.confirmUnsavedTitle
                    msgs.confirmUnsavedBody
                    msgs.actionDiscardAndContinue
                    msgs.actionCancel
                    ConfirmDiscardNavigation
                    CancelPendingNavigation
                    "tt-btn tt-btn-danger"
        , case state.confirmPermanentDelete of
            Nothing    -> H.div_ [ HP.class_ "tt-slot-empty" ] []
            Just docId ->
                viewConfirmModal
                    msgs.confirmPermanentDelTitle
                    msgs.confirmPermanentDelBody
                    msgs.actionConfirmPermanentDel
                    msgs.actionCancel
                    (ConfirmPermanentDelete docId)
                    CancelPermanentDelete
                    "tt-btn tt-btn-danger"
        ]

viewConfirmModal ::
       Text
    -> Text
    -> Text
    -> Text
    -> ClientAction
    -> ClientAction
    -> Text
    -> View () () ClientState ClientAction
viewConfirmModal titleText bodyText confirmLabel cancelLabel onConfirm onCancel confirmClass =
    H.div_ [ HP.class_ "tt-modal-backdrop", HP.role_ "dialog", HP.aria_ "modal" "true" ]
        [ H.div_ [ HP.class_ "tt-modal-card" ]
            [ H.h2_ [ HP.class_ "tt-modal-title" ] [ text (ms titleText) ]
            , H.p_ [ HP.class_ "tt-modal-body" ] [ text (ms bodyText) ]
            , H.div_ [ HP.class_ "tt-modal-actions" ]
                [ H.button_
                    [ HP.type_ "button"
                    , HP.class_ "tt-btn tt-btn-secondary"
                    , HE.onClick onCancel
                    ]
                    [ text (ms cancelLabel) ]
                , H.button_
                    [ HP.type_ "button"
                    , HP.class_ (ms confirmClass)
                    , HE.onClick onConfirm
                    ]
                    [ text (ms confirmLabel) ]
                ]
            ]
        ]

disabledAttr :: Bool -> [Attribute ClientState ClientAction]
disabledAttr True  = [ HP.disabled_ ]
disabledAttr False = []

toTextFromMiso :: MisoString -> Text
toTextFromMiso = fromMisoString

-- | 空タイトルを UI カタログの「無題」ラベルへ補完して表示文字列を返す。
displayTitle :: UiMessages -> DocumentTitle -> Text
displayTitle msgs docTitle =
    let raw :: Text
        raw = toText docTitle
    in  if Txt.null raw then msgs.untitledDocument else raw

-- | UTC タイムスタンプを一覧表示用の簡潔な日時文字列 (@YYYY-MM-DD HH:MM UTC@) へ整形する。
formatUtcTimestamp :: UTCTime -> Text
formatUtcTimestamp = Txt.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC"

-- | エディタ内検索の一致件数・エラー表示文字列を整形する。
formatSearchMatchCounter :: UiMessages -> SearchPanelState -> Text
formatSearchMatchCounter msgs sp
    | Txt.null sp.query       = ""
    | isJust sp.regexError    = msgs.searchInvalidRegex
    | sp.matchCount == 0      = msgs.searchNoMatches
    | sp.activeMatch > 0      =
        Txt.pack (show sp.activeMatch) <> " / " <> Txt.pack (show sp.matchCount) <> msgs.documentsCountUnit
    | otherwise               =
        Txt.pack (show sp.matchCount) <> msgs.documentsCountUnit
