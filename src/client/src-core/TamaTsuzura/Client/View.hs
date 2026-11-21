{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.View
    ( viewClient
    , displayTitle
    , displayUserLabel
    , formatUtcTimestamp
    , formatSearchMatchCounter
    ) where

import           Data.Either                     (fromRight)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (isJust, isNothing)
import           Data.Text                       (Text)
import qualified Data.Text                       as Txt
import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Format                (defaultTimeLocale, formatTime)
import           Miso                            (Attribute, Checked(..), View, boolProp, text, textProp)
import qualified Miso.Html.Element               as H
import qualified Miso.Html.Event                 as HE
import qualified Miso.Html.Property              as HP
import           Miso.String                     (MisoString, fromMisoString, ms)
import           TamaTsuzura.Client.App
    ( AuthFormMode(..)
    , AuthFormState(..)
    , AuthState(..)
    , ClientAction(..)
    , ClientState(..)
    , ConnectionStatus(..)
    , EditorDraft(..)
    , EditorViewMode(..)
    , FilterState(..)
    , MobilePane(..)
    , PendingNavigation(..)
    , RevisionDetailTab(..)
    , RevisionHistoryState(..)
    , SearchPanelState(..)
    , hasActiveFilters
    , isCurrentDocumentDirty
    )
import           TamaTsuzura.Client.DraftStorage (DraftSyncState(..), StoredDraft(..))
import           TamaTsuzura.Client.EditorBridge (EditorStats(..))
import           TamaTsuzura.Client.I18n         (UiMessages(..), formatDocumentFormat)
import           TamaTsuzura.Client.Markdown     (renderMarkdownView)
import           TamaTsuzura.Common.Api          (DocumentStatusFilter(..))
import           TamaTsuzura.Common.Auth         (AuthSessionResponse(..), UserProfile(..))
import           TamaTsuzura.Common.Diff
    ( DiffHunk(..)
    , DiffLine(..)
    , DiffLineKind(..)
    , ParsedDiff(..)
    , UnifiedDiff
    , parseUnifiedDiff
    )
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
import           TamaTsuzura.Common.Revision     (RevisionDetail(..), RevisionSummary(..))
import           TamaTsuzura.Common.Text         (ToText(..))
import           TamaTsuzura.ExtensionApi        (ExtensionMetadata(..))

-- | モバイルファースト日本語 UI をレンダリングする純粋ビュー関数。
-- 未認証時は中央配置のログイン・新規登録画面を表示し、認証済み時は
-- スマートフォン幅で一覧/エディタ切替、PC 広幅で 2 ペインワークスペースを表示する。
viewClient :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewClient msgs state =
    H.div_ [ HP.class_ (ms appClass) ]
        [ viewHeader msgs state
        , viewNotifications msgs state
        , viewMainBody msgs state
        , viewModals msgs state
        ]
  where
    appClass :: Text
    appClass = case state.authState of
        AuthAuthenticated{} -> case state.mobilePane of
            PaneList   -> "tt-app tt-pane-list"
            PaneEditor -> "tt-app tt-pane-editor"
        _ -> "tt-app tt-app-auth"

viewMainBody :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewMainBody msgs state = case state.authState of
    AuthChecking ->
        H.main_ [ HP.class_ "tt-auth-shell" ]
            [ H.div_ [ HP.class_ "tt-auth-card tt-auth-checking", HP.role_ "status" ]
                [ H.p_ [] [ text (ms msgs.authCheckingMessage) ] ]
            ]
    AuthUnauthenticated form ->
        viewAuthScreen msgs form
    AuthAuthenticated{} ->
        H.div_ [ HP.class_ "tt-shell" ]
            [ viewListPane msgs state
            , viewEditorPane msgs state
            ]

viewAuthScreen :: UiMessages -> AuthFormState -> View () () ClientState ClientAction
viewAuthScreen msgs form =
    H.main_ [ HP.class_ "tt-auth-shell" ]
        [ H.section_ [ HP.class_ "tt-auth-card", HP.aria_ "label" (ms headingText) ]
            [ H.div_ [ HP.class_ "tt-segmented", HP.role_ "tablist" ]
                [ modeTab AuthModeLogin    msgs.authTabLogin
                , modeTab AuthModeRegister msgs.authTabRegister
                ]
            , H.h2_ [ HP.class_ "tt-auth-heading" ] [ text (ms headingText) ]
            , H.p_ [ HP.class_ "tt-auth-lead" ] [ text (ms leadText) ]
            , viewFormError form.formError
            , H.form_
                [ HP.class_ "tt-auth-form"
                , boolProp "noValidate" True
                , HE.onSubmit SubmitAuthForm
                ]
                ( [ H.label_ [ HP.class_ "tt-field" ]
                        [ H.span_ [ HP.class_ "tt-field-label" ] [ text (ms msgs.authUsernameLabel) ]
                        , H.input_
                            ( [ HP.type_ "text"
                              , HP.name_ "username"
                              , HP.class_ "tt-input"
                              , HP.value_ (ms form.usernameInput)
                              , HP.placeholder_ (ms msgs.authUsernamePlaceholder)
                              , textProp "autocomplete" "username"
                              , textProp "autocapitalize" "none"
                              , boolProp "spellcheck" False
                              , HE.onInput (UpdateAuthUsername . toTextFromMiso)
                              ] <> disabledAttr form.isSubmitting
                            )
                        , H.span_ [ HP.class_ "tt-auth-hint" ] [ text (ms msgs.authUsernameHint) ]
                        ]
                  ]
                    <> displayNameField
                    <> [ H.label_ [ HP.class_ "tt-field" ]
                            [ H.span_ [ HP.class_ "tt-field-label" ] [ text (ms msgs.authPasswordLabel) ]
                            , H.input_
                                ( [ HP.type_ "password"
                                  , HP.name_ "password"
                                  , HP.class_ "tt-input"
                                  , HP.value_ (ms form.passwordInput)
                                  , HP.placeholder_ (ms msgs.authPasswordPlaceholder)
                                  , textProp "autocomplete" passwordAutoComplete
                                  , HE.onInput (UpdateAuthPassword . toTextFromMiso)
                                  ] <> disabledAttr form.isSubmitting
                                )
                            ]
                       ]
                    <> passwordRegisterFields
                    <> [ H.button_
                            ( [ HP.type_ "submit"
                              , HP.class_ "tt-btn tt-btn-primary tt-auth-submit"
                              ] <> disabledAttr form.isSubmitting
                            )
                            [ text (ms submitLabel) ]
                       ]
                )
            ]
        ]
  where
    isRegister :: Bool
    isRegister = form.mode == AuthModeRegister

    headingText :: Text
    headingText =
        if isRegister then msgs.authRegisterHeading else msgs.authLoginHeading

    leadText :: Text
    leadText =
        if isRegister then msgs.authRegisterLead else msgs.authLoginLead

    passwordAutoComplete :: MisoString
    passwordAutoComplete =
        if isRegister then "new-password" else "current-password"

    submitLabel :: Text
    submitLabel = case (form.mode, form.isSubmitting) of
        (AuthModeLogin,    False) -> msgs.actionLogin
        (AuthModeLogin,    True)  -> msgs.actionLoggingIn
        (AuthModeRegister, False) -> msgs.actionRegister
        (AuthModeRegister, True)  -> msgs.actionRegistering

    modeTab :: AuthFormMode -> Text -> View () () ClientState ClientAction
    modeTab target labelText =
        let isActive :: Bool
            isActive = form.mode == target

            btnClass :: Text
            btnClass = if isActive then "tt-seg-btn tt-seg-btn-active" else "tt-seg-btn"
        in  H.button_
                ( [ HP.type_ "button"
                  , HP.class_ (ms btnClass)
                  , HP.role_ "tab"
                  , HP.aria_ "selected" (if isActive then "true" else "false")
                  , HE.onClick (SwitchAuthMode target)
                  ] <> disabledAttr form.isSubmitting
                )
                [ text (ms labelText) ]

    viewFormError :: Maybe Text -> View () () ClientState ClientAction
    viewFormError Nothing = H.div_ [ HP.class_ "tt-slot-empty" ] []
    viewFormError (Just errMsg) =
        H.div_ [ HP.class_ "tt-banner tt-banner-error tt-auth-error", HP.role_ "alert" ]
            [ H.span_ [ HP.class_ "tt-banner-text" ] [ text (ms errMsg) ] ]

    displayNameField :: [View () () ClientState ClientAction]
    displayNameField
        | not isRegister = []
        | otherwise =
            [ H.label_ [ HP.class_ "tt-field" ]
                [ H.span_ [ HP.class_ "tt-field-label" ] [ text (ms msgs.authDisplayNameLabel) ]
                , H.input_
                    ( [ HP.type_ "text"
                      , HP.name_ "display_name"
                      , HP.class_ "tt-input"
                      , HP.value_ (ms form.displayNameInput)
                      , HP.placeholder_ (ms msgs.authDisplayNamePlaceholder)
                      , textProp "autocomplete" "nickname"
                      , HE.onInput (UpdateAuthDisplayName . toTextFromMiso)
                      ] <> disabledAttr form.isSubmitting
                    )
                ]
            ]

    passwordRegisterFields :: [View () () ClientState ClientAction]
    passwordRegisterFields
        | not isRegister = []
        | otherwise =
            [ H.span_ [ HP.class_ "tt-auth-hint" ] [ text (ms msgs.authPasswordHint) ]
            , H.label_ [ HP.class_ "tt-field" ]
                [ H.span_ [ HP.class_ "tt-field-label" ] [ text (ms msgs.authPasswordConfirmLabel) ]
                , H.input_
                    ( [ HP.type_ "password"
                      , HP.name_ "password_confirm"
                      , HP.class_ "tt-input"
                      , HP.value_ (ms form.passwordConfirmInput)
                      , HP.placeholder_ (ms msgs.authPasswordConfirmPh)
                      , textProp "autocomplete" "new-password"
                      , HE.onInput (UpdateAuthPasswordConfirm . toTextFromMiso)
                      ] <> disabledAttr form.isSubmitting
                    )
                ]
            ]

viewHeader :: UiMessages -> ClientState -> View () () ClientState ClientAction
viewHeader msgs state =
    H.header_ [ HP.class_ "tt-header" ]
        [ H.div_ [ HP.class_ "tt-brand" ]
            [ H.h1_ [ HP.class_ "tt-title" ] [ text (ms msgs.appTitle) ]
            , H.p_ [ HP.class_ "tt-subtitle" ] [ text (ms msgs.appSubtitle) ]
            ]
        , H.div_ [ HP.class_ "tt-header-actions" ]
            ( viewConnectionBadge msgs state.connectionStatus
            : headerAuthActions
            )
        ]
  where
    headerAuthActions :: [View () () ClientState ClientAction]
    headerAuthActions = case state.authState of
        AuthAuthenticated session ->
            [ H.span_
                [ HP.class_ "tt-user-pill"
                , HP.title_ (ms (toText session.user))
                ]
                [ text (ms (displayUserLabel session.user)) ]
            , H.button_
                ( [ HP.type_ "button"
                  , HP.class_ "tt-btn tt-btn-primary tt-btn-compact"
                  , HE.onClick RequestCreateDocument
                  ] <> disabledAttr isCreatingOrImporting
                )
                [ text (ms createLabel) ]
            , H.button_
                ( [ HP.type_ "button"
                  , HP.class_ "tt-btn tt-btn-secondary tt-btn-compact"
                  , HE.onClick RequestImportDocument
                  ] <> disabledAttr isCreatingOrImporting
                )
                [ text (ms importLabel) ]
            , H.button_
                ( [ HP.type_ "button"
                  , HP.class_ "tt-btn tt-btn-secondary tt-btn-compact"
                  , HE.onClick RefreshAll
                  ] <> disabledAttr isRefreshing
                )
                [ text (ms msgs.actionReload) ]
            , H.button_
                ( [ HP.type_ "button"
                  , HP.class_ "tt-btn tt-btn-secondary tt-btn-compact"
                  , HE.onClick RequestLogout
                  ] <> disabledAttr state.isLoggingOut
                )
                [ text (ms logoutLabel) ]
            ]
        AuthChecking ->
            [ reloadOnlyButton True ]
        AuthUnauthenticated form ->
            [ reloadOnlyButton form.isSubmitting ]

    reloadOnlyButton :: Bool -> View () () ClientState ClientAction
    reloadOnlyButton isDisabled =
        H.button_
            ( [ HP.type_ "button"
              , HP.class_ "tt-btn tt-btn-secondary tt-btn-compact"
              , HE.onClick RefreshAll
              ] <> disabledAttr isDisabled
            )
            [ text (ms msgs.actionReload) ]

    createLabel :: Text
    createLabel =
        if state.isCreatingDocument
            then msgs.actionCreating
            else msgs.actionNewDocument

    importLabel :: Text
    importLabel =
        if state.isImportingDocument
            then msgs.actionImporting
            else msgs.actionImport

    logoutLabel :: Text
    logoutLabel =
        if state.isLoggingOut
            then msgs.actionLoggingOut
            else msgs.actionLogout

    isCreatingOrImporting :: Bool
    isCreatingOrImporting = state.isCreatingDocument || state.isImportingDocument || state.isLoggingOut

    isRefreshing :: Bool
    isRefreshing = state.isLoadingDocuments || state.connectionStatus == ConnChecking || state.isLoggingOut

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
        , case state.authState of
            AuthAuthenticated{} -> viewOrphanedDraftBanner msgs state state.orphanedDraft
            _                   -> H.div_ [ HP.class_ "tt-slot-empty" ] []
        ]

viewOrphanedDraftBanner ::
       UiMessages
    -> ClientState
    -> Maybe StoredDraft
    -> View () () ClientState ClientAction
viewOrphanedDraftBanner _ _ Nothing = H.div_ [ HP.class_ "tt-slot-empty" ] []
viewOrphanedDraftBanner msgs state (Just _) =
    H.div_ [ HP.class_ "tt-banner tt-banner-warn", HP.role_ "alert" ]
        [ H.span_ [ HP.class_ "tt-banner-text" ] [ text (ms msgs.draftOrphanedBanner) ]
        , H.div_ [ HP.class_ "tt-banner-actions" ]
            [ H.button_
                ( [ HP.type_ "button"
                  , HP.class_ "tt-btn tt-btn-primary tt-btn-xs"
                  , HE.onClick RecoverOrphanedDraft
                  ] <> disabledAttr (state.isCreatingDocument || state.isImportingDocument)
                )
                [ text (ms msgs.actionRecoverOrphanDraft) ]
            , H.button_
                [ HP.type_ "button"
                , HP.class_ "tt-btn tt-btn-ghost tt-btn-xs"
                , HE.onClick DiscardOrphanedDraft
                ]
                [ text (ms msgs.actionDiscardDraft) ]
            ]
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
                    <> draftIndicator
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

    hasStoredDraft :: Bool
    hasStoredDraft = Map.member summary.id state.storedDrafts

    draftIndicator :: [View () () ClientState ClientAction]
    draftIndicator
        | hasStoredDraft =
            [ H.span_ [ HP.class_ "tt-doc-draft-badge" ] [ text (ms msgs.badgeHasDraft) ] ]
        | otherwise = []

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
                              ] <> disabledAttr (state.isCreatingDocument || state.isImportingDocument)
                            )
                            [ text (ms msgs.actionNewDocument) ]
                        ]
            Just doc ->
                viewActiveEditor msgs state doc
        ]

viewActiveEditor :: UiMessages -> ClientState -> Document -> View () () ClientState ClientAction
viewActiveEditor msgs state doc =
    H.div_ [ HP.class_ (ms shellClass) ]
        [ viewEditorToolbar msgs state doc
        , if readOnly
            then H.div_ [ HP.class_ "tt-banner tt-banner-warn", HP.role_ "status" ]
                [ H.span_ [] [ text (ms msgs.trashReadOnlyBanner) ] ]
            else H.div_ [ HP.class_ "tt-slot-empty" ] []
        , viewDraftRecoveryBanner msgs state doc readOnly
        , viewTitleAndMetadata msgs state readOnly
        , if state.searchPanel.isOpen
            then viewSearchPanel msgs state.searchPanel readOnly
            else H.div_ [ HP.class_ "tt-slot-empty" ] []
        , if state.revisionHistory.isHistoryOpen
            then viewRevisionHistoryPanel msgs state doc readOnly
            else H.div_ [ HP.class_ "tt-slot-empty" ] []
        , viewEditorWorkspace msgs state
        , viewEditorStatusBar msgs state readOnly
        ]
  where
    readOnly :: Bool
    readOnly = isTrashed doc

    shellClass :: Text
    shellClass =
        if state.revisionHistory.isHistoryOpen
            then "tt-editor-shell tt-history-open"
            else "tt-editor-shell"

viewDraftRecoveryBanner ::
       UiMessages
    -> ClientState
    -> Document
    -> Bool
    -> View () () ClientState ClientAction
viewDraftRecoveryBanner msgs state doc readOnly =
    let isSaving :: Bool
        isSaving = isJust state.savingDocument
    in  case state.staleDraftPrompt of
            Just sd | sd.docId == doc.id ->
                H.div_ [ HP.class_ "tt-banner tt-banner-warn", HP.role_ "status" ]
                    [ H.span_ [ HP.class_ "tt-banner-text" ]
                        [ text (ms (if readOnly then msgs.draftTrashedBanner else msgs.draftStaleBanner)) ]
                    , H.div_ [ HP.class_ "tt-banner-actions" ]
                        ( [ H.button_
                                ( [ HP.type_ "button"
                                  , HP.class_ "tt-btn tt-btn-primary tt-btn-xs"
                                  , HE.onClick ApplyStaleDraft
                                  ] <> disabledAttr isSaving
                                )
                                [ text (ms msgs.actionApplyDraft) ]
                          | not readOnly
                          ]
                            <> [ H.button_
                                    ( [ HP.type_ "button"
                                      , HP.class_ "tt-btn tt-btn-ghost tt-btn-xs"
                                      , HE.onClick DiscardCurrentDraft
                                      ] <> disabledAttr isSaving
                                    )
                                    [ text (ms msgs.actionDiscardDraft) ]
                               ]
                        )
                    ]
            _
                | not readOnly && state.restoredDraftDocId == Just doc.id ->
                    H.div_ [ HP.class_ "tt-banner tt-banner-info", HP.role_ "status" ]
                        [ H.span_ [ HP.class_ "tt-banner-text" ] [ text (ms msgs.draftRestoredBanner) ]
                        , H.div_ [ HP.class_ "tt-banner-actions" ]
                            [ H.button_
                                ( [ HP.type_ "button"
                                  , HP.class_ "tt-btn tt-btn-ghost tt-btn-xs"
                                  , HE.onClick DiscardCurrentDraft
                                  ] <> disabledAttr isSaving
                                )
                                [ text (ms msgs.actionDiscardDraft) ]
                            ]
                        ]
                | otherwise ->
                    H.div_ [ HP.class_ "tt-slot-empty" ] []

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
                   , H.button_
                        [ HP.type_ "button"
                        , HP.class_ (ms historyBtnClass)
                        , HE.onClick ToggleRevisionHistory
                        ]
                        [ text (ms msgs.actionHistoryToggle) ]
                   , H.button_
                        [ HP.type_ "button"
                        , HP.class_ "tt-btn tt-btn-secondary tt-btn-compact"
                        , HE.onClick ExportCurrentDocument
                        ]
                        [ text (ms msgs.actionExport) ]
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

    historyBtnClass :: Text
    historyBtnClass =
        if state.revisionHistory.isHistoryOpen
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

viewRevisionHistoryPanel ::
       UiMessages
    -> ClientState
    -> Document
    -> Bool
    -> View () () ClientState ClientAction
viewRevisionHistoryPanel msgs state doc readOnly =
    H.section_ [ HP.class_ "tt-history-panel", HP.aria_ "label" (ms msgs.historyHeading) ]
        [ H.div_ [ HP.class_ "tt-history-header" ]
            [ H.div_ [ HP.class_ "tt-section-header" ]
                [ H.h2_ [ HP.class_ "tt-section-heading" ] [ text (ms msgs.historyHeading) ]
                , H.span_ [ HP.class_ "tt-count" ]
                    [ text (ms countLabel) ]
                ]
            , H.div_ [ HP.class_ "tt-history-header-actions" ]
                [ H.button_
                    ( [ HP.type_ "button"
                      , HP.class_ "tt-btn tt-btn-secondary tt-btn-xs"
                      , HE.onClick RefreshRevisionHistory
                      ] <> disabledAttr (rh.isLoadingList || rh.isLoadingMore || isRestoring)
                    )
                    [ text (ms msgs.actionRefreshHistory) ]
                , H.button_
                    [ HP.type_ "button"
                    , HP.class_ "tt-btn tt-btn-ghost tt-btn-xs"
                    , HE.onClick ToggleRevisionHistory
                    ]
                    [ text (ms msgs.actionCloseHistory) ]
                ]
            ]
        , H.div_ [ HP.class_ "tt-history-body" ]
            [ H.div_ [ HP.class_ "tt-history-list-col" ]
                [ viewRevisionList
                ]
            , H.div_ [ HP.class_ "tt-history-detail-col" ]
                [ viewRevisionDetailCol
                ]
            ]
        ]
  where
    rh :: RevisionHistoryState
    rh = state.revisionHistory

    isSaving :: Bool
    isSaving = isJust state.savingDocument

    isRestoring :: Bool
    isRestoring = isJust state.restoringRevisionDoc

    isMutating :: Bool
    isMutating = isJust state.mutatingDocumentId

    loadedCount :: Int
    loadedCount = length rh.revisions

    hasMoreRevisions :: Bool
    hasMoreRevisions = loadedCount < rh.totalRevisions

    countLabel :: Text
    countLabel
        | hasMoreRevisions && loadedCount > 0 =
            Txt.pack (show loadedCount) <> " / " <> Txt.pack (show rh.totalRevisions) <> msgs.documentsCountUnit
        | otherwise =
            Txt.pack (show rh.totalRevisions) <> msgs.documentsCountUnit

    viewRevisionList :: View () () ClientState ClientAction
    viewRevisionList
        | rh.isLoadingList && null rh.revisions =
            H.p_ [ HP.class_ "tt-empty" ] [ text (ms msgs.historyLoadingList) ]
        | null rh.revisions =
            H.p_ [ HP.class_ "tt-empty" ] [ text (ms msgs.historyEmpty) ]
        | otherwise =
            H.div_ []
                ( H.ul_ [ HP.class_ "tt-rev-list" ]
                    (map viewRevisionSummaryRow rh.revisions)
                : [ H.button_
                        ( [ HP.type_ "button"
                          , HP.class_ "tt-btn tt-btn-secondary tt-btn-xs tt-auth-submit"
                          , HE.onClick LoadMoreRevisions
                          ] <> disabledAttr (rh.isLoadingList || rh.isLoadingMore || isRestoring)
                        )
                        [ text (ms (if rh.isLoadingMore then msgs.historyLoadingList else msgs.actionLoadMoreHistory)) ]
                  | hasMoreRevisions
                  ]
                )

    viewRevisionSummaryRow :: RevisionSummary -> View () () ClientState ClientAction
    viewRevisionSummaryRow summary =
        let isSelected :: Bool
            isSelected = rh.selectedRevision == Just summary.revision

            isCurrent :: Bool
            isCurrent = summary.revision == doc.revision

            itemClass :: Text
            itemClass =
                if isSelected
                    then "tt-rev-item tt-rev-item-active"
                    else "tt-rev-item"
        in  H.li_ [ HP.class_ (ms itemClass) ]
                [ H.button_
                    [ HP.type_ "button"
                    , HP.class_ "tt-rev-select-btn"
                    , HE.onClick (SelectRevision summary.revision)
                    ]
                    [ H.div_ [ HP.class_ "tt-rev-row-top" ]
                        ( [ H.span_ [ HP.class_ "tt-rev-num" ]
                                [ text (ms ("r" <> toText summary.revision)) ]
                          ]
                            <> [ H.span_ [ HP.class_ "tt-rev-current-badge" ]
                                    [ text (ms msgs.historyCurrentBadge) ]
                               | isCurrent
                               ]
                            <> [ H.span_ [ HP.class_ "tt-rev-time" ]
                                    [ text (ms (formatUtcTimestamp summary.createdAt)) ]
                               ]
                        )
                    , H.div_ [ HP.class_ "tt-rev-title" ]
                        [ text (ms (displayTitle msgs summary.title)) ]
                    ]
                ]

    viewRevisionDetailCol :: View () () ClientState ClientAction
    viewRevisionDetailCol
        | isRestoring || rh.isLoadingDetail || (rh.isLoadingList && isNothing rh.revisionDetail) =
            H.p_ [ HP.class_ "tt-empty" ] [ text (ms msgs.historyLoadingDetail) ]
        | otherwise = case rh.revisionDetail of
            Nothing ->
                H.p_ [ HP.class_ "tt-empty" ] [ text (ms msgs.historySelectPrompt) ]
            Just detail ->
                let isCurrentRev :: Bool
                    isCurrentRev = detail.revision == doc.revision

                    restoreLabel :: Text
                    restoreLabel =
                        if isRestoring
                            then msgs.actionRestoringRevision
                            else msgs.actionRestoreRevision
                in  H.div_ [ HP.class_ "tt-rev-detail" ]
                        [ H.div_ [ HP.class_ "tt-rev-detail-head" ]
                            [ H.div_ [ HP.class_ "tt-rev-detail-meta" ]
                                [ H.span_ [ HP.class_ "tt-rev-num" ]
                                    [ text (ms ("r" <> toText detail.revision)) ]
                                , H.span_ [ HP.class_ "tt-doc-format" ]
                                    [ text (ms (formatDocumentFormat msgs detail.format)) ]
                                , H.span_ [ HP.class_ "tt-rev-time" ]
                                    [ text (ms (formatUtcTimestamp detail.createdAt)) ]
                                ]
                            , H.div_ [ HP.class_ "tt-rev-detail-title" ]
                                [ text (ms (displayTitle msgs detail.title)) ]
                            , viewRestoreActionBar detail isCurrentRev restoreLabel
                            ]
                        , H.div_ [ HP.class_ "tt-segmented tt-rev-tabs", HP.role_ "tablist" ]
                            [ detailTabButton RevTabBody msgs.historyTabBody
                            , detailTabButton RevTabDiff msgs.historyTabDiff
                            ]
                        , H.div_ [ HP.class_ "tt-rev-content" ]
                            [ case rh.activeTab of
                                RevTabBody ->
                                    H.pre_ [ HP.class_ "tt-rev-body-pre" ]
                                        [ text (ms (toText detail.body)) ]
                                RevTabDiff ->
                                    viewUnifiedDiff msgs detail.diff
                            ]
                        ]

    viewRestoreActionBar ::
           RevisionDetail
        -> Bool
        -> Text
        -> View () () ClientState ClientAction
    viewRestoreActionBar detail isCurrentRev restoreLabel
        | readOnly =
            H.p_ [ HP.class_ "tt-rev-hint" ] [ text (ms msgs.historyTrashedRestoreHint) ]
        | isCurrentRev =
            H.p_ [ HP.class_ "tt-rev-hint" ] [ text (ms msgs.historyCurrentVersionHint) ]
        | otherwise =
            H.div_ [ HP.class_ "tt-rev-restore-bar" ]
                [ H.button_
                    ( [ HP.type_ "button"
                      , HP.class_ "tt-btn tt-btn-primary tt-btn-compact"
                      , HE.onClick (RequestRestoreRevision detail.revision)
                      ] <> disabledAttr (isRestoring || isSaving || isMutating)
                    )
                    [ text (ms restoreLabel) ]
                ]

    detailTabButton :: RevisionDetailTab -> Text -> View () () ClientState ClientAction
    detailTabButton tab labelText =
        let isActive :: Bool
            isActive = rh.activeTab == tab

            btnClass :: Text
            btnClass = if isActive then "tt-seg-btn tt-seg-btn-active" else "tt-seg-btn"
        in  H.button_
                [ HP.type_ "button"
                , HP.class_ (ms btnClass)
                , HP.role_ "tab"
                , HP.aria_ "selected" (if isActive then "true" else "false")
                , HE.onClick (SetRevisionDetailTab tab)
                ]
                [ text (ms labelText) ]

viewUnifiedDiff :: UiMessages -> UnifiedDiff -> View () () ClientState ClientAction
viewUnifiedDiff msgs diff
    | Txt.null (toText diff) =
        H.p_ [ HP.class_ "tt-empty" ] [ text (ms msgs.historyDiffEmpty) ]
    | otherwise = case parseUnifiedDiff diff of
        Left _ ->
            H.pre_ [ HP.class_ "tt-diff-raw" ] [ text (ms (toText diff)) ]
        Right parsed
            | null parsed.hunks ->
                H.p_ [ HP.class_ "tt-empty" ] [ text (ms msgs.historyDiffEmpty) ]
            | otherwise ->
                H.div_ [ HP.class_ "tt-diff-view" ]
                    (map viewDiffHunk parsed.hunks)
  where
    viewDiffHunk :: DiffHunk -> View () () ClientState ClientAction
    viewDiffHunk hunk =
        let headerText :: Text
            headerText =
                "@@ -"
                    <> formatRange hunk.oldStart hunk.oldCount
                    <> " +"
                    <> formatRange hunk.newStart hunk.newCount
                    <> " @@"
        in  H.div_ [ HP.class_ "tt-diff-hunk" ]
                ( H.div_ [ HP.class_ "tt-diff-hunk-header" ] [ text (ms headerText) ]
                : concatMap viewDiffLine hunk.lines
                )

    formatRange :: Int -> Int -> Text
    formatRange start count
        | count == 1 = Txt.pack (show start)
        | otherwise  = Txt.pack (show start) <> "," <> Txt.pack (show count)

    viewDiffLine :: DiffLine -> [View () () ClientState ClientAction]
    viewDiffLine dl =
        let (lineClass, prefix) = case dl.kind of
                ContextLine -> ("tt-diff-line tt-diff-ctx", " ")
                RemovedLine -> ("tt-diff-line tt-diff-del", "-")
                AddedLine   -> ("tt-diff-line tt-diff-add", "+")

            baseRow :: View () () ClientState ClientAction
            baseRow =
                H.div_ [ HP.class_ lineClass ]
                    [ H.span_ [ HP.class_ "tt-diff-prefix" ] [ text prefix ]
                    , H.span_ [ HP.class_ "tt-diff-text" ] [ text (ms dl.content) ]
                    ]
        in  if dl.hasNewline
                then [baseRow]
                else
                    [ baseRow
                    , H.div_ [ HP.class_ "tt-diff-line tt-diff-meta" ]
                        [ text "\\ No newline at end of file" ]
                    ]

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
            ( revisionBadge
                <> [ H.span_ []
                        [ text (ms (showInt stats.lineCount <> msgs.statsLinesUnit)) ]
                   , H.span_ []
                        [ text (ms (showInt stats.charCount <> msgs.statsCharsUnit)) ]
                   , H.span_ []
                        [ text (ms (msgs.statsCursorPrefix <> " " <> showInt stats.cursorLine <> ":" <> showInt stats.cursorCol)) ]
                   ]
                <> draftSyncIndicator
            )
        , viewExtensionQuickActions msgs state.extensionMetadatas readOnly
        ]
  where
    stats :: EditorStats
    stats = state.editorStats

    showInt :: Int -> Text
    showInt = Txt.pack . show

    revisionBadge :: [View () () ClientState ClientAction]
    revisionBadge = case state.selectedDocument of
        Nothing  -> []
        Just doc ->
            [ H.span_ [ HP.class_ "tt-rev-status-badge" ]
                [ text (ms ("r" <> toText doc.revision)) ]
            ]

    draftSyncIndicator :: [View () () ClientState ClientAction]
    draftSyncIndicator
        | readOnly = []
        | otherwise = case state.draftSyncState of
            DraftSyncIdle -> []
            DraftSyncPending ->
                [ H.span_ [ HP.class_ "tt-draft-status tt-draft-status-pending", HP.role_ "status" ]
                    [ text (ms msgs.draftStatePending) ]
                ]
            DraftSyncSaved{} ->
                [ H.span_ [ HP.class_ "tt-draft-status tt-draft-status-saved", HP.role_ "status" ]
                    [ text (ms msgs.draftStateSaved) ]
                ]
            DraftSyncFailed detail ->
                [ H.span_
                    [ HP.class_ "tt-draft-status tt-draft-status-failed"
                    , HP.role_ "alert"
                    , HP.title_ (ms (msgs.errDraftStoragePrefix <> ": " <> detail))
                    ]
                    [ text (ms msgs.draftStateFailed) ]
                ]

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
viewModals msgs state = case state.authState of
    AuthAuthenticated{} ->
        H.div_ [ HP.class_ "tt-modals" ]
            [ case state.pendingNavigation of
                Nothing -> H.div_ [ HP.class_ "tt-slot-empty" ] []
                Just NavLogout ->
                    viewConfirmModal
                        msgs.confirmUnsavedTitle
                        msgs.confirmUnsavedLogoutBody
                        msgs.actionDiscardAndLogout
                        msgs.actionCancel
                        ConfirmDiscardNavigation
                        CancelPendingNavigation
                        "tt-btn tt-btn-danger"
                Just _ ->
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
            , case state.revisionHistory.confirmRestoreRev of
                Nothing    -> H.div_ [ HP.class_ "tt-slot-empty" ] []
                Just revNo ->
                    viewConfirmModal
                        msgs.confirmRestoreRevTitle
                        msgs.confirmRestoreRevBody
                        msgs.actionConfirmRestoreRev
                        msgs.actionCancel
                        (ConfirmRestoreRevision revNo)
                        CancelRestoreRevision
                        "tt-btn tt-btn-primary"
            ]
    _ -> H.div_ [ HP.class_ "tt-slot-empty" ] []

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

-- | ヘッダ等へ表示する簡潔なユーザーラベルを返す。
-- 表示名が設定されていれば表示名、未設定であれば @\"@username\"@ を返す。
displayUserLabel :: UserProfile -> Text
displayUserLabel profile = case profile.displayName of
    Just name -> toText name
    Nothing   -> "@" <> toText profile.username

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
