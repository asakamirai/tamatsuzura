{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main
    ( main
    ) where

import qualified Data.Aeson                           as Aeson
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy                 as LazyBS
import           Data.IORef                           (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (isNothing)
import           Data.Text                            (Text)
import qualified Data.Text                            as Txt
import qualified Data.Text.Encoding                   as TxtEnc
import           Data.Time.Calendar                   (fromGregorian)
import           Data.Time.Clock                      (UTCTime(..), secondsToDiffTime)
import           Miso.DSL                             (jsNull)
import           Miso.Effect                          (Schedule(..), mkComponentInfo, runEffect)
import           Miso.Html.Render                     (toHtmlWith)
import           TamaTsuzura.Client.Api
    ( ApiBaseUrl
    , ApiClient(..)
    , ApiClientError(..)
    , HttpMethod(..)
    , HttpRequest(..)
    , HttpResponse(..)
    , HttpTransport(..)
    , buildCreateDocumentRequest
    , buildDocumentListQueryString
    , buildGetCurrentUserRequest
    , buildGetDocumentRequest
    , buildGetRevisionRequest
    , buildHealthRequest
    , buildListDocumentsRequest
    , buildListFoldersRequest
    , buildListRevisionsRequest
    , buildListTagsRequest
    , buildLoginRequest
    , buildLogoutRequest
    , buildPermanentDeleteDocumentRequest
    , buildRegisterRequest
    , buildRestoreDocumentRequest
    , buildRestoreRevisionRequest
    , buildRevisionListQueryString
    , buildSoftDeleteDocumentRequest
    , buildUpdateDocumentRequest
    , createDocument
    , csrfHeaderName
    , defaultApiBaseUrl
    , fetchHealth
    , getCurrentUser
    , getDocument
    , getDocumentRevision
    , isUnauthorizedError
    , listDocumentRevisions
    , listDocuments
    , listFolders
    , listTags
    , loginUser
    , logoutUser
    , mkApiBaseUrl
    , parseJsonResponse
    , parseNoContentResponse
    , percentEncodeQueryComponent
    , permanentDeleteDocument
    , registerUser
    , restoreDocument
    , restoreDocumentRevision
    , softDeleteDocument
    , updateDocument
    )
import           TamaTsuzura.Client.App
    ( AuthFormMode(..)
    , AuthFormState(..)
    , AuthState(..)
    , ClientAction(..)
    , ClientEnv(..)
    , ClientState(..)
    , ConnectionStatus(..)
    , DraftReconcileResult(..)
    , EditorDraft(..)
    , EditorViewMode(..)
    , FilterState(..)
    , MobilePane(..)
    , PendingNavigation(..)
    , RevisionDetailTab(..)
    , RevisionHistoryState(..)
    , SearchPanelState(..)
    , applyAuthSessionChecked
    , applyAuthSubmitted
    , applyDocumentCreated
    , applyDocumentLoaded
    , applyDocumentPermanentlyDeleted
    , applyDocumentRestored
    , applyDocumentSaved
    , applyDocumentSoftDeleted
    , applyDocumentsResult
    , applyDraftStorageEvent
    , applyDraftsLoaded
    , applyEditorEvent
    , applyExtensionById
    , applyFoldersResult
    , applyHealthResult
    , applyRevisionDetailLoaded
    , applyRevisionListLoaded
    , applyRevisionRestored
    , applySessionExpired
    , applyTagsResult
    , buildEditorSyncPayload
    , buildListQueryFromFilter
    , buildRecoveryRequestFromStoredDraft
    , currentAuthSession
    , currentUserId
    , defaultFilterState
    , dismissError
    , dismissNotice
    , draftFromDocument
    , editorDraftFromStored
    , emptyAuthFormState
    , emptyEditorDraft
    , emptyRevisionHistoryState
    , emptySearchPanelState
    , formatTagsInput
    , hasActiveFilters
    , initialClientState
    , isCurrentDocumentDirty
    , isDraftDirty
    , mergeDraftAfterSave
    , parseDisplayNameInput
    , parseFolderInput
    , parseTagsInput
    , reconcileDocumentWithDraft
    , resetRevisionHistory
    , resetWorkspaceForUnauthenticated
    , toStoredDraft
    , updateClient
    , validateEditorDraft
    , validateLoginForm
    , validateRegisterForm
    )
import           TamaTsuzura.Client.DraftStorage
    ( DraftStorage(..)
    , DraftStorageEvent(..)
    , DraftSyncState(..)
    , StoredDraft(..)
    , decodeDraftStorageEventJson
    , decodeStoredDraftListJson
    , encodeStoredDraftJson
    , latestStoredDraft
    )
import           TamaTsuzura.Client.EditorBridge
    ( EditorCommand(..)
    , EditorEvent(..)
    , EditorSearchParams(..)
    , EditorSearchStatus(..)
    , EditorStats(..)
    , EditorSyncPayload(..)
    , computeTextStats
    , decodeEditorEventJson
    , encodeEditorCommandJson
    )
import           TamaTsuzura.Client.ExtensionRegistry (bundledExtensions)
import           TamaTsuzura.Client.Ffi
    ( RawFetchResult(..)
    , decodeFetchResultJson
    , encodeFetchOptionsJson
    )
import           TamaTsuzura.Client.FileTransfer
    ( ExportSpec(..)
    , FileImportError(..)
    , FilePickResult(..)
    , FileTransferClient(..)
    , ImportedFile(..)
    , buildExportFileName
    , buildExportSpec
    , decodeExportResultJson
    , decodeFilePickResultJson
    , deriveTitleFromFileName
    , exportMimeType
    , inferDocumentFormatFromFileName
    , prepareImportedDocumentRequest
    , sanitizeExportBaseName
    , stripUtf8Bom
    )
import           TamaTsuzura.Client.I18n
    ( UiMessages(..)
    , formatApiClientError
    , formatDocumentFormat
    , formatFileImportError
    , formatLoginApiError
    , formatRegisterApiError
    , formatValidationError
    , jaMessages
    )
import           TamaTsuzura.Client.Markdown
    ( ListItem(..)
    , MarkdownBlock(..)
    , MarkdownInline(..)
    , TableAlign(..)
    , mkSafeUrl
    , parseInlines
    , parseMarkdown
    , renderMarkdownView
    )
import           TamaTsuzura.Client.View
    ( displayTitle
    , displayUserLabel
    , formatSearchMatchCounter
    , formatUtcTimestamp
    , viewClient
    )
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , CreateDocumentRequest(..)
    , DocumentListQuery(..)
    , DocumentListResponse(..)
    , DocumentStatusFilter(..)
    , FolderListResponse(..)
    , TagListResponse(..)
    , UpdateDocumentRequest(..)
    , defaultCreateDocumentRequest
    , defaultDocumentListQuery
    , healthOk
    , mkListLimit
    , mkListOffset
    , mkSearchQuery
    )
import           TamaTsuzura.Common.Auth
    ( AuthSessionResponse(..)
    , CsrfToken
    , LoginRequest(..)
    , RegisterRequest(..)
    , UserId
    , UserProfile(..)
    , mkCsrfToken
    , mkDisplayName
    , mkPassword
    , mkUserId
    , mkUsername
    )
import           TamaTsuzura.Common.Diff              (UnifiedDiff(..), emptyUnifiedDiff, generateUnifiedDiff)
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody(..)
    , DocumentFormat(..)
    , DocumentId
    , DocumentSummary(..)
    , RevisionNumber
    , Tag
    , TagSet
    , emptyDocumentTitle
    , emptyTagSet
    , initialRevisionNumber
    , mkDocumentId
    , mkDocumentTitle
    , mkFolderPath
    , mkRevisionNumber
    , mkTag
    , mkTagSet
    , toDocumentSummary
    )
import           TamaTsuzura.Common.Revision
    ( RestoreRevisionRequest(..)
    , RevisionDetail(..)
    , RevisionListResponse(..)
    , RevisionSummary(..)
    , mkContentHash
    , mkRevisionId
    , toRevisionSummary
    )
import           TamaTsuzura.Common.Text              (ToText(..))
import           TamaTsuzura.Common.Validation        (ValidationError(..), maxDocumentBodyBytes)
import           TamaTsuzura.ExtensionApi             (ExtensionId(..))
import           Test.Hspec                           (Spec, describe, expectationFailure, hspec, it, shouldBe, shouldSatisfy)
import           Test.QuickCheck                      (property)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "TamaTsuzura.Client.Ffi pure JSON bridge" $ do
        it "encodes request options distinguishing Nothing vs empty body" $ do
            let noBodyJson :: Text
                noBodyJson = encodeFetchOptionsJson "GET" [("Accept", "application/json")] Nothing

                emptyBodyJson :: Text
                emptyBodyJson = encodeFetchOptionsJson "POST" [] (Just "")
            noBodyJson `shouldSatisfy` Txt.isInfixOf "\"body\":null"
            emptyBodyJson `shouldSatisfy` Txt.isInfixOf "\"body\":\"\""

        it "decodes ok and error JSFFI fetch payloads" $ do
            decodeFetchResultJson "{\"ok\":true,\"status\":200,\"body\":\"{\\\"status\\\":\\\"ok\\\"}\"}"
                `shouldBe` RawFetchOk 200 "{\"status\":\"ok\"}"
            decodeFetchResultJson "{\"ok\":true,\"status\":204,\"body\":\"\"}"
                `shouldBe` RawFetchOk 204 ""
            decodeFetchResultJson "{\"ok\":false,\"error\":\"Failed to fetch\"}"
                `shouldBe` RawFetchFailed "Failed to fetch"

        it "rejects malformed JSFFI payloads safely" $ do
            case decodeFetchResultJson "not-json" of
                RawFetchFailed msg -> msg `shouldSatisfy` Txt.isInfixOf "Malformed"
                RawFetchOk{}       -> expectationFailure "Expected RawFetchFailed"
            case decodeFetchResultJson "{\"ok\":true}" of
                RawFetchFailed msg -> msg `shouldSatisfy` Txt.isInfixOf "Missing HTTP status"
                RawFetchOk{}       -> expectationFailure "Expected RawFetchFailed"

    describe "TamaTsuzura.Client.Api URL encoding and request builders" $ do
        it "normalizes trailing slashes in ApiBaseUrl" $ do
            toText defaultApiBaseUrl `shouldBe` ""
            toText (mkApiBaseUrl "  /notes/// ") `shouldBe` "/notes"
            toText (mkApiBaseUrl "http://127.0.0.1:8080/") `shouldBe` "http://127.0.0.1:8080"

        it "percent-encodes UTF-8 Japanese and reserved query characters" $ do
            percentEncodeQueryComponent "abc-123_.~" `shouldBe` "abc-123_.~"
            percentEncodeQueryComponent "a&b=c d+e%f" `shouldBe` "a%26b%3Dc%20d%2Be%25f"
            percentEncodeQueryComponent "仕事/メモ" `shouldBe` "%E4%BB%95%E4%BA%8B%2F%E3%83%A1%E3%83%A2"

        it "never emits unencoded query delimiter characters (QuickCheck)" $
            property $ \ (rawStr :: String) ->
                let encoded :: Text
                    encoded = percentEncodeQueryComponent (Txt.pack rawStr)
                in  not (Txt.any (`elem` ['&', '=', '?', '#', ' ', '+']) encoded)

        it "omits default parameters in DocumentListQuery and encodes non-default ones" $ do
            buildDocumentListQueryString defaultDocumentListQuery `shouldBe` ""
            tagVal    <- expectRight (mkTag "仕事")
            folderVal <- expectRight (mkFolderPath "work/notes")
            searchVal <- expectRight (mkSearchQuery "検索 & テスト")
            limitVal  <- expectRight (mkListLimit 20)
            offsetVal <- expectRight (mkListOffset 40)

            let query :: DocumentListQuery
                query = DocumentListQuery
                    { status            = StatusTrashed
                    , tag               = Just tagVal
                    , folder            = Just folderVal
                    , includeSubfolders = True
                    , search            = searchVal
                    , limit             = limitVal
                    , offset            = offsetVal
                    }
                qs :: Text
                qs = buildDocumentListQueryString query
            qs `shouldBe`
                "?status=trashed&tag=%E4%BB%95%E4%BA%8B&folder=work%2Fnotes&include_subfolders=true&q=%E6%A4%9C%E7%B4%A2%20%26%20%E3%83%86%E3%82%B9%E3%83%88&limit=20&offset=40"

        it "does not emit include_subfolders when folder is Nothing" $ do
            let query :: DocumentListQuery
                query = DocumentListQuery
                    { status            = defaultDocumentListQuery.status
                    , tag               = defaultDocumentListQuery.tag
                    , folder            = Nothing
                    , includeSubfolders = True
                    , search            = defaultDocumentListQuery.search
                    , limit             = defaultDocumentListQuery.limit
                    , offset            = defaultDocumentListQuery.offset
                    }
            buildDocumentListQueryString query `shouldBe` ""

        it "builds accurate HTTP requests for all M1/M3 endpoints and attaches X-CSRF-Token on state changes" $ do
            docId <- expectRight (mkDocumentId "doc-123")
            csrf  <- mkSampleCsrfToken
            uname <- expectRight (mkUsername "alice_01")
            pass  <- expectRight (mkPassword "secret-pass-123")
            let base :: ApiBaseUrl
                base = mkApiBaseUrl "/app/"

                healthReq :: HttpRequest
                healthReq = buildHealthRequest base

                meReq :: HttpRequest
                meReq = buildGetCurrentUserRequest base

                loginReq :: HttpRequest
                loginReq = buildLoginRequest base (LoginRequest uname pass)

                logoutReq :: HttpRequest
                logoutReq = buildLogoutRequest base csrf

                listReq :: HttpRequest
                listReq = buildListDocumentsRequest base defaultDocumentListQuery

                getReq :: HttpRequest
                getReq = buildGetDocumentRequest base docId

                softDelReq :: HttpRequest
                softDelReq = buildSoftDeleteDocumentRequest base csrf docId

                restoreReq :: HttpRequest
                restoreReq = buildRestoreDocumentRequest base csrf docId

                permDelReq :: HttpRequest
                permDelReq = buildPermanentDeleteDocumentRequest base csrf docId

                tagsReq :: HttpRequest
                tagsReq = buildListTagsRequest base

                foldersReq :: HttpRequest
                foldersReq = buildListFoldersRequest base

            (healthReq.method, healthReq.url) `shouldBe` (MethodGet, "/app/api/health")
            (meReq.method, meReq.url) `shouldBe` (MethodGet, "/app/api/auth/me")
            (loginReq.method, loginReq.url) `shouldBe` (MethodPost, "/app/api/auth/login")
            (logoutReq.method, logoutReq.url) `shouldBe` (MethodPost, "/app/api/auth/logout")
            lookup csrfHeaderName logoutReq.headers `shouldBe` Just (toText csrf)
            (listReq.method, listReq.url) `shouldBe` (MethodGet, "/app/api/documents")
            (getReq.method, getReq.url) `shouldBe` (MethodGet, "/app/api/documents/doc-123")
            (softDelReq.method, softDelReq.url) `shouldBe` (MethodDelete, "/app/api/documents/doc-123")
            lookup csrfHeaderName softDelReq.headers `shouldBe` Just (toText csrf)
            (restoreReq.method, restoreReq.url) `shouldBe` (MethodPost, "/app/api/documents/doc-123/restore")
            lookup csrfHeaderName restoreReq.headers `shouldBe` Just (toText csrf)
            (permDelReq.method, permDelReq.url) `shouldBe` (MethodDelete, "/app/api/documents/doc-123/permanent")
            lookup csrfHeaderName permDelReq.headers `shouldBe` Just (toText csrf)
            (tagsReq.method, tagsReq.url) `shouldBe` (MethodGet, "/app/api/tags")
            (foldersReq.method, foldersReq.url) `shouldBe` (MethodGet, "/app/api/folders")

            regReq <- expectRight (buildRegisterRequest base (RegisterRequest uname Nothing pass))
            regReq.method `shouldBe` MethodPost
            regReq.url `shouldBe` "/app/api/auth/register"
            regReq.body `shouldSatisfy` (/= Nothing)

            shortPass <- expectRight (mkPassword "short")
            buildRegisterRequest base (RegisterRequest uname Nothing shortPass)
                `shouldBe` Left (PasswordTooShort 5 8)

            createReq <- expectRight (buildCreateDocumentRequest base csrf defaultCreateDocumentRequest)
            createReq.method `shouldBe` MethodPost
            createReq.url `shouldBe` "/app/api/documents"
            createReq.body `shouldSatisfy` (/= Nothing)
            lookup csrfHeaderName createReq.headers `shouldBe` Just (toText csrf)

            updateReq <- expectRight $ buildUpdateDocumentRequest base csrf docId UpdateDocumentRequest
                { title        = emptyDocumentTitle
                , body         = DocumentBody "updated"
                , format       = FormatPlainText
                , folder       = Nothing
                , tags         = emptyTagSet
                , baseRevision = Nothing
                }
            updateReq.method `shouldBe` MethodPut
            updateReq.url `shouldBe` "/app/api/documents/doc-123"
            lookup csrfHeaderName updateReq.headers `shouldBe` Just (toText csrf)

        it "rejects oversized document body and short register password before hitting transport" $ do
            csrf <- mkSampleCsrfToken
            uname <- expectRight (mkUsername "alice_01")
            shortPass <- expectRight (mkPassword "short")
            let hugeBody :: DocumentBody
                hugeBody = DocumentBody (Txt.replicate (maxDocumentBodyBytes + 1) "a")

                req :: CreateDocumentRequest
                req = CreateDocumentRequest
                    { title  = emptyDocumentTitle
                    , body   = hugeBody
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = emptyTagSet
                    }

                client :: ApiClient
                client = ApiClient
                    { baseUrl   = defaultApiBaseUrl
                    , transport = HttpTransport (\ _ -> pure (Left "must not be called"))
                    }
            regRes <- registerUser client (RegisterRequest uname Nothing shortPass)
            case regRes of
                Left (ClientValidationError PasswordTooShort{}) -> pure ()
                other -> expectationFailure ("Expected PasswordTooShort, got: " <> show other)

            res <- createDocument client csrf req
            case res of
                Left (ClientValidationError BodyTooLarge{}) -> pure ()
                other -> expectationFailure ("Expected ClientValidationError BodyTooLarge, got: " <> show other)

            docId <- expectRight (mkDocumentId "doc-huge")
            let updReq :: UpdateDocumentRequest
                updReq = UpdateDocumentRequest
                    { title        = emptyDocumentTitle
                    , body         = hugeBody
                    , format       = FormatMarkdown
                    , folder       = Nothing
                    , tags         = emptyTagSet
                    , baseRevision = Nothing
                    }
            updRes <- updateDocument client csrf docId updReq
            case updRes of
                Left (ClientValidationError BodyTooLarge{}) -> pure ()
                other -> expectationFailure ("Expected ClientValidationError BodyTooLarge on update, got: " <> show other)

    describe "TamaTsuzura.Client.Api response parsing and transport execution" $ do
        it "parses structured ApiError JSON on 4xx/5xx responses" $ do
            let apiErr :: ApiError
                apiErr = ApiError
                    { code    = ErrConflict
                    , message = "Cannot update trashed document"
                    , field   = Nothing
                    }
                res :: HttpResponse
                res = HttpResponse
                    { statusCode = 409
                    , body       = encodeStrict apiErr
                    }
            (parseJsonResponse 200 res :: Either ApiClientError Document)
                `shouldBe` Left (ClientApiError 409 apiErr)

        it "falls back to ClientHttpError on non-JSON HTTP errors and handles 204 No Content" $ do
            let htmlErr :: HttpResponse
                htmlErr = HttpResponse
                    { statusCode = 502
                    , body       = "Bad Gateway"
                    }

                redirectRes :: HttpResponse
                redirectRes = HttpResponse
                    { statusCode = 302
                    , body       = "Found"
                    }

                malformedOk :: HttpResponse
                malformedOk = HttpResponse
                    { statusCode = 200
                    , body       = "{invalid-json"
                    }
            (parseJsonResponse 200 htmlErr :: Either ApiClientError Document)
                `shouldBe` Left (ClientHttpError 502 "Bad Gateway")
            (parseJsonResponse 200 redirectRes :: Either ApiClientError Document)
                `shouldBe` Left (ClientHttpError 302 "Found")
            case (parseJsonResponse 200 malformedOk :: Either ApiClientError Document) of
                Left ClientDecodeError{} -> pure ()
                other -> expectationFailure ("Expected ClientDecodeError, got: " <> show other)
            parseNoContentResponse (HttpResponse 204 "") `shouldBe` Right ()
            parseNoContentResponse htmlErr `shouldBe` Left (ClientHttpError 502 "Bad Gateway")

        it "executes all high-level auth and document API operations against injected HttpTransport" $ do
            sampleDoc <- mkSampleDocument "doc-1" "執筆メモ"
            sampleTag <- expectRight (mkTag "メモ")
            sampleFolder <- expectRight (mkFolderPath "diary/2026")
            session <- mkSampleAuthSession "user-alice" "alice" (Just "アリス")
            pass <- expectRight (mkPassword "secret-password-123")
            let csrf :: CsrfToken
                csrf = session.csrfToken

                sampleSummary :: DocumentSummary
                sampleSummary = toDocumentSummary sampleDoc

                sampleList :: DocumentListResponse
                sampleList = DocumentListResponse
                    { documents = [sampleSummary]
                    , total     = 1
                    }

                sampleTags :: TagListResponse
                sampleTags = TagListResponse { tags = [sampleTag] }

                sampleFolders :: FolderListResponse
                sampleFolders = FolderListResponse { folders = [sampleFolder] }

            lastReqRef <- newIORef (Nothing :: Maybe HttpRequest)
            let mkClient :: HttpResponse -> ApiClient
                mkClient fixedRes = ApiClient
                    { baseUrl   = defaultApiBaseUrl
                    , transport = recordingTransport lastReqRef (Right fixedRes)
                    }

            eHealth <- fetchHealth (mkClient (HttpResponse 200 (encodeStrict healthOk)))
            eHealth `shouldBe` Right healthOk

            eReg <- registerUser (mkClient (HttpResponse 201 (encodeStrict session)))
                (RegisterRequest session.user.username session.user.displayName pass)
            eReg `shouldBe` Right session

            eLogin <- loginUser (mkClient (HttpResponse 200 (encodeStrict session)))
                (LoginRequest session.user.username pass)
            eLogin `shouldBe` Right session

            eMe <- getCurrentUser (mkClient (HttpResponse 200 (encodeStrict session)))
            eMe `shouldBe` Right session

            eLogout <- logoutUser (mkClient (HttpResponse 204 "")) csrf
            eLogout `shouldBe` Right ()

            eList <- listDocuments (mkClient (HttpResponse 200 (encodeStrict sampleList))) defaultDocumentListQuery
            eList `shouldBe` Right sampleList

            eCreated <- createDocument (mkClient (HttpResponse 201 (encodeStrict sampleDoc))) csrf defaultCreateDocumentRequest
            eCreated `shouldBe` Right sampleDoc

            eGot <- getDocument (mkClient (HttpResponse 200 (encodeStrict sampleDoc))) sampleDoc.id
            eGot `shouldBe` Right sampleDoc

            let updReq :: UpdateDocumentRequest
                updReq = UpdateDocumentRequest
                    { title        = sampleDoc.title
                    , body         = DocumentBody "更新本文"
                    , format       = FormatMarkdown
                    , folder       = Nothing
                    , tags         = emptyTagSet
                    , baseRevision = Nothing
                    }
            eUpdated <- updateDocument (mkClient (HttpResponse 200 (encodeStrict sampleDoc))) csrf sampleDoc.id updReq
            eUpdated `shouldBe` Right sampleDoc

            eSoftDel <- softDeleteDocument (mkClient (HttpResponse 200 (encodeStrict sampleDoc))) csrf sampleDoc.id
            eSoftDel `shouldBe` Right sampleDoc

            eRestored <- restoreDocument (mkClient (HttpResponse 200 (encodeStrict sampleDoc))) csrf sampleDoc.id
            eRestored `shouldBe` Right sampleDoc

            ePermDel <- permanentDeleteDocument (mkClient (HttpResponse 204 "")) csrf sampleDoc.id
            ePermDel `shouldBe` Right ()

            eTags <- listTags (mkClient (HttpResponse 200 (encodeStrict sampleTags)))
            eTags `shouldBe` Right sampleTags

            eFolders <- listFolders (mkClient (HttpResponse 200 (encodeStrict sampleFolders)))
            eFolders `shouldBe` Right sampleFolders

            mLastReq <- readIORef lastReqRef
            fmap (\ r -> r.url) mLastReq `shouldBe` Just "/api/folders"

            isUnauthorizedError (ClientApiError 401 (ApiError ErrUnauthorized "unauth" Nothing)) `shouldBe` True
            isUnauthorizedError (ClientHttpError 401 "Unauthorized") `shouldBe` True
            isUnauthorizedError (ClientApiError 403 (ApiError ErrForbidden "csrf" Nothing)) `shouldBe` False
            isUnauthorizedError (ClientNetworkError "offline") `shouldBe` False

        it "propagates network transport failures as ClientNetworkError" $ do
            let failClient :: ApiClient
                failClient = ApiClient
                    { baseUrl   = defaultApiBaseUrl
                    , transport = HttpTransport (\ _ -> pure (Left "offline"))
                    }
            res <- fetchHealth failClient
            res `shouldBe` Left (ClientNetworkError "offline")

    describe "TamaTsuzura.Client.App state transitions and bundled extensions" $ do
        it "initializes empty document state with injected bundled extension metadata" $ do
            let state :: ClientState
                state = initialClientState bundledExtensions
            state.connectionStatus `shouldBe` ConnChecking
            state.mobilePane `shouldBe` PaneList
            state.editorViewMode `shouldBe` ModeEdit
            state.draft `shouldBe` emptyEditorDraft
            length state.extensionMetadatas `shouldBe` 1

        it "applies a bundled extension by id, bumps sync version, and ignores trashed or unknown ids" $ do
            doc <- mkSampleDocument "doc-ext" "拡張テスト"
            let base :: ClientState
                base = (initialClientState bundledExtensions)
                    { selectedDocument = Just doc
                    , draft            = withDraftBody (DocumentBody "line1   \nline2\t\n") emptyEditorDraft
                    }
                updated :: ClientState
                updated = applyExtensionById bundledExtensions (ExtensionId "trim-trailing-spaces") base

                unchanged :: ClientState
                unchanged = applyExtensionById bundledExtensions (ExtensionId "non-existent") base

                trashedBase :: ClientState
                trashedBase = base { selectedDocument = Just (withDocDeletedAt (Just sampleUtcTime) doc) }

                trashedAfter :: ClientState
                trashedAfter = applyExtensionById bundledExtensions (ExtensionId "trim-trailing-spaces") trashedBase
            updated.draft.body `shouldBe` DocumentBody "line1\nline2\n"
            updated.editorSyncVersion `shouldBe` base.editorSyncVersion + 1
            unchanged.draft.body `shouldBe` base.draft.body
            trashedAfter.draft.body `shouldBe` trashedBase.draft.body

        it "parses folder/tags inputs and validates EditorDraft into UpdateDocumentRequest" $ do
            parseFolderInput "   " `shouldBe` Right Nothing
            expectedFolder <- expectRight (mkFolderPath "work/notes")
            parseFolderInput " /work/notes/ " `shouldBe` Right (Just expectedFolder)
            parseFolderInput "a/../b" `shouldSatisfy` isLeftEither

            parsedTags <- expectRight (parseTagsInput "メモ, アイデア、作業，メモ ")
            formatTagsInput parsedTags `shouldBe` "アイデア, メモ, 作業"

            let validDraft :: EditorDraft
                validDraft = EditorDraft
                    { titleInput  = "  設計書  "
                    , body        = DocumentBody "# タイトル\n本文"
                    , format      = FormatMarkdown
                    , folderInput = "work/notes"
                    , tagsInput   = "設計, メモ"
                    }
            req <- expectRight (validateEditorDraft validDraft)
            toText req.title `shouldBe` "設計書"
            req.folder `shouldBe` Just expectedFolder
            formatTagsInput req.tags `shouldBe` "メモ, 設計"

            let invalidTitleDraft :: EditorDraft
                invalidTitleDraft = withDraftTitle "bad\ntitle" validDraft
            validateEditorDraft invalidTitleDraft `shouldBe` Left TitleContainsControlChar

            let oversizedDraft :: EditorDraft
                oversizedDraft = withDraftBody (DocumentBody (Txt.replicate (maxDocumentBodyBytes + 1) "a")) validDraft
            validateEditorDraft oversizedDraft
                `shouldBe` Left (BodyTooLarge (maxDocumentBodyBytes + 1) maxDocumentBodyBytes)

        it "detects unsaved dirty state accurately for active and trashed documents" $ do
            doc <- mkSampleDocument "doc-dirty" "元のタイトル"
            let cleanDraft :: EditorDraft
                cleanDraft = draftFromDocument doc
            isDraftDirty doc cleanDraft `shouldBe` False
            isDraftDirty doc (withDraftTitle "  元のタイトル  " cleanDraft) `shouldBe` False
            isDraftDirty doc (withDraftBody (DocumentBody "変更後") cleanDraft) `shouldBe` True
            isDraftDirty doc (withDraftFormat FormatPlainText cleanDraft) `shouldBe` True
            isDraftDirty doc (withDraftFolder "work/sub" cleanDraft) `shouldBe` True
            isDraftDirty doc (withDraftFolder "../invalid" cleanDraft) `shouldBe` True
            isDraftDirty doc (withDraftTags "新規タグ" cleanDraft) `shouldBe` True
            let trashedDoc :: Document
                trashedDoc = withDocDeletedAt (Just sampleUtcTime) doc
            isDraftDirty trashedDoc (withDraftBody (DocumentBody "ignored") cleanDraft) `shouldBe` False

        it "builds DocumentListQuery from FilterState and checks active filter presence" $ do
            hasActiveFilters defaultFilterState `shouldBe` False
            tagA <- expectRight (mkTag "メモ")
            folderA <- expectRight (mkFolderPath "work/docs")
            let flt :: FilterState
                flt = FilterState
                    { status            = StatusTrashed
                    , searchInput       = "  検索語  "
                    , selectedTag       = Just tagA
                    , selectedFolder    = Just folderA
                    , includeSubfolders = True
                    }
            hasActiveFilters flt `shouldBe` True
            q <- expectRight (buildListQueryFromFilter flt)
            q.status `shouldBe` StatusTrashed
            q.tag `shouldBe` Just tagA
            q.folder `shouldBe` Just folderA
            q.includeSubfolders `shouldBe` True
            fmap toText q.search `shouldBe` Just "検索語"
            buildListQueryFromFilter (flt { searchInput = "bad\nquery" })
                `shouldBe` Left SearchQueryContainsControlChar

        it "transitions document loading with stale-response guard, save, soft-delete, restore, and permanent delete" $ do
            docA <- mkSampleDocument "doc-a" "文書A"
            docB <- mkSampleDocument "doc-b" "文書B"
            let s0 :: ClientState
                s0 = (initialClientState bundledExtensions)
                    { openingDocumentId = Just docB.id
                    }
                -- Stale response for docA must be ignored while waiting for docB
                afterStale :: ClientState
                afterStale = applyDocumentLoaded jaMessages docA.id (Right docA) s0

                afterLoadB :: ClientState
                afterLoadB = applyDocumentLoaded jaMessages docB.id (Right docB) afterStale

                edited :: ClientState
                edited = afterLoadB
                    { draft          = withDraftBody (DocumentBody "更新本文") afterLoadB.draft
                    , savingDocument = Just (docB.id, withDraftBody (DocumentBody "更新本文") afterLoadB.draft)
                    }

                savedDocB :: Document
                savedDocB = withDocBody (DocumentBody "更新本文") docB

                afterSave :: ClientState
                afterSave = applyDocumentSaved jaMessages docB.id (Right savedDocB) edited

                trashedDocB :: Document
                trashedDocB = withDocDeletedAt (Just sampleUtcTime) savedDocB

                afterTrash :: ClientState
                afterTrash = applyDocumentSoftDeleted jaMessages docB.id (Right trashedDocB) afterSave

                inTrashView :: ClientState
                inTrashView = afterTrash
                    { filterState       = withFilterStatus StatusTrashed defaultFilterState
                    , documentSummaries = [toDocumentSummary trashedDocB]
                    , totalDocuments    = 1
                    , selectedDocument  = Just trashedDocB
                    , draft             = draftFromDocument trashedDocB
                    }

                afterRestore :: ClientState
                afterRestore = applyDocumentRestored jaMessages docB.id (Right savedDocB) inTrashView

                afterPermDel :: ClientState
                afterPermDel = applyDocumentPermanentlyDeleted jaMessages docB.id (Right ()) inTrashView
            afterStale.selectedDocument `shouldBe` Nothing
            afterLoadB.selectedDocument `shouldBe` Just docB
            afterLoadB.mobilePane `shouldBe` PaneEditor
            isCurrentDocumentDirty edited `shouldBe` True
            isCurrentDocumentDirty afterSave `shouldBe` False
            afterSave.statusNotice `shouldBe` Just jaMessages.noticeSaved
            dismissNotice afterSave `shouldSatisfy` (isNothing . (.statusNotice))
            afterTrash.selectedDocument `shouldBe` Nothing
            afterTrash.mobilePane `shouldBe` PaneList
            afterTrash.statusNotice `shouldBe` Just jaMessages.noticeMovedToTrash
            afterRestore.documentSummaries `shouldBe` []
            afterRestore.selectedDocument `shouldBe` Just savedDocB
            afterRestore.statusNotice `shouldBe` Just jaMessages.noticeRestored
            afterPermDel.documentSummaries `shouldBe` []
            afterPermDel.selectedDocument `shouldBe` Nothing
            afterPermDel.statusNotice `shouldBe` Just jaMessages.noticePermanentlyDeleted

        it "preserves concurrent edits typed during in-flight save and prevents stale/trash list corruption" $ do
            doc <- mkSampleDocument "doc-race" "初期"
            let sentDraft :: EditorDraft
                sentDraft = withDraftTitle "  送信タイトル  " (draftFromDocument doc)

                typedDuringSave :: EditorDraft
                typedDuringSave = withDraftBody (DocumentBody "保存中に追記した本文") sentDraft

                inFlightState :: ClientState
                inFlightState = (initialClientState bundledExtensions)
                    { selectedDocument  = Just doc
                    , draft             = typedDuringSave
                    , savingDocument    = Just (doc.id, sentDraft)
                    , documentSummaries = [toDocumentSummary doc]
                    , totalDocuments    = 1
                    }

            savedTitle <- expectRight (mkDocumentTitle "送信タイトル")
            let serverDoc :: Document
                serverDoc = Document
                    { id        = doc.id
                    , title     = savedTitle
                    , body      = sentDraft.body
                    , format    = doc.format
                    , folder    = doc.folder
                    , tags      = doc.tags
                    , revision  = doc.revision
                    , createdAt = doc.createdAt
                    , updatedAt = doc.updatedAt
                    , deletedAt = Nothing
                    }

                afterSave :: ClientState
                afterSave = applyDocumentSaved jaMessages doc.id (Right serverDoc) inFlightState

                merged :: EditorDraft
                merged = mergeDraftAfterSave sentDraft (draftFromDocument serverDoc) typedDuringSave

                inTrashTab :: ClientState
                inTrashTab = inFlightState
                    { filterState       = withFilterStatus StatusTrashed defaultFilterState
                    , documentSummaries = []
                    }

                afterSaveInTrash :: ClientState
                afterSaveInTrash = applyDocumentSaved jaMessages doc.id (Right serverDoc) inTrashTab
            merged.titleInput `shouldBe` "送信タイトル"
            merged.body `shouldBe` DocumentBody "保存中に追記した本文"
            afterSave.draft `shouldBe` merged
            -- Concurrent body edit keeps dirty flag True so user can save again without data loss
            isCurrentDocumentDirty afterSave `shouldBe` True
            -- Saving an active doc while viewing Trash must not inject it into the Trash list
            afterSaveInTrash.documentSummaries `shouldBe` []

        it "ignores stale DocumentsLoaded responses with outdated request sequence numbers" $ do
            docNew <- mkSampleDocument "doc-latest" "最新一覧"
            docOld <- mkSampleDocument "doc-stale" "古い一覧"
            let s0 :: ClientState
                s0 = (initialClientState bundledExtensions)
                    { listRequestSeq     = 3
                    , isLoadingDocuments = True
                    }

                afterStale :: ClientState
                afterStale = applyDocumentsResult jaMessages 2 (Right (DocumentListResponse [toDocumentSummary docOld] 1)) s0

                afterFresh :: ClientState
                afterFresh = applyDocumentsResult jaMessages 3 (Right (DocumentListResponse [toDocumentSummary docNew] 1)) afterStale
            afterStale.documentSummaries `shouldBe` []
            afterStale.isLoadingDocuments `shouldBe` True
            afterFresh.documentSummaries `shouldBe` [toDocumentSummary docNew]
            afterFresh.isLoadingDocuments `shouldBe` False

        it "applies CodeMirror editor events and ignores body changes on read-only trashed documents" $ do
            doc <- mkSampleDocument "doc-evt" "編集対象"
            let base :: ClientState
                base = (initialClientState bundledExtensions)
                    { selectedDocument = Just doc
                    , draft            = draftFromDocument doc
                    }
                stats :: EditorStats
                stats = EditorStats 3 12 2 5

                afterBody :: ClientState
                afterBody = applyEditorEvent (EvtBodyChanged "新しい本文" stats) base

                afterSearchOpen :: ClientState
                afterSearchOpen = applyEditorEvent (EvtOpenSearch True) afterBody

                afterSearchStat :: ClientState
                afterSearchStat = applyEditorEvent (EvtSearchStatus (EditorSearchStatus 4 2 Nothing)) afterSearchOpen

                afterHist :: ClientState
                afterHist = applyEditorEvent (EvtHistoryState True False) afterSearchStat

                trashed :: ClientState
                trashed = base { selectedDocument = Just (withDocDeletedAt (Just sampleUtcTime) doc) }

                trashedAfterBody :: ClientState
                trashedAfterBody = applyEditorEvent (EvtBodyChanged "改ざん" stats) trashed
            afterBody.draft.body `shouldBe` DocumentBody "新しい本文"
            afterBody.editorStats `shouldBe` stats
            afterSearchOpen.searchPanel.isOpen `shouldBe` True
            afterSearchOpen.searchPanel.showReplace `shouldBe` True
            afterSearchStat.searchPanel.matchCount `shouldBe` 4
            afterSearchStat.searchPanel.activeMatch `shouldBe` 2
            afterHist.canUndo `shouldBe` True
            afterHist.canRedo `shouldBe` False
            trashedAfterBody.draft.body `shouldBe` doc.body
            fmap (.readOnly) (buildEditorSyncPayload jaMessages False trashed) `shouldBe` Just True

        it "updates available tags and folders while keeping existing values on error" $ do
            tagA <- expectRight (mkTag "メモ")
            folderA <- expectRight (mkFolderPath "work")
            let s0 :: ClientState
                s0 = initialClientState bundledExtensions

                s1 :: ClientState
                s1 = applyFoldersResult (Right (FolderListResponse [folderA]))
                    (applyTagsResult (Right (TagListResponse [tagA])) s0)

                s2 :: ClientState
                s2 = applyFoldersResult (Left (ClientNetworkError "offline"))
                    (applyTagsResult (Left (ClientNetworkError "offline")) s1)
            s1.availableTags `shouldBe` [tagA]
            s1.availableFolders `shouldBe` [folderA]
            s2.availableTags `shouldBe` [tagA]
            s2.availableFolders `shouldBe` [folderA]

        it "transitions connection status and document list across API results" $ do
            doc <- mkSampleDocument "doc-new" "新しいメモ"
            let s0 :: ClientState
                s0 = initialClientState bundledExtensions

                s1 :: ClientState
                s1 = applyHealthResult jaMessages (Right healthOk) s0

                listRes :: DocumentListResponse
                listRes = DocumentListResponse
                    { documents = []
                    , total     = 0
                    }

                s2 :: ClientState
                s2 = applyDocumentsResult jaMessages 0 (Right listRes) s1

                s3 :: ClientState
                s3 = applyDocumentCreated jaMessages (Right doc) s2

                sErr :: ClientState
                sErr = applyHealthResult jaMessages (Left (ClientNetworkError "timeout")) s3
            s1.connectionStatus `shouldBe` ConnOnline
            s2.totalDocuments `shouldBe` 0
            s3.totalDocuments `shouldBe` 1
            length s3.documentSummaries `shouldBe` 1
            s3.selectedDocument `shouldBe` Just doc
            case sErr.connectionStatus of
                ConnError msg -> msg `shouldSatisfy` Txt.isInfixOf jaMessages.errNetworkPrefix
                other         -> expectationFailure ("Expected ConnError, got: " <> show other)

        it "preserves health error banner across concurrent list completion and handles failure/dismiss transitions" $ do
            let base :: ClientState
                base = (initialClientState bundledExtensions)
                    { isLoadingDocuments = True
                    , isCreatingDocument = True
                    }

                afterHealthErr :: ClientState
                afterHealthErr = applyHealthResult jaMessages (Left (ClientNetworkError "down")) base

                -- Concurrent DocumentsLoaded (Right) arriving after HealthChecked (Left) must not clear lastError
                afterListOk :: ClientState
                afterListOk = applyDocumentsResult jaMessages 0 (Right (DocumentListResponse [] 0)) afterHealthErr

                afterDismiss :: ClientState
                afterDismiss = dismissError afterListOk

                afterListErr :: ClientState
                afterListErr = applyDocumentsResult jaMessages 0 (Left (ClientHttpError 503 "Unavailable")) afterDismiss

                afterCreateErr :: ClientState
                afterCreateErr = applyDocumentCreated jaMessages (Left (ClientApiError 409 (ApiError ErrConflict "conflict" Nothing))) afterListErr

            afterListOk.isLoadingDocuments `shouldBe` False
            afterListOk.lastError `shouldBe` afterHealthErr.lastError
            afterDismiss.lastError `shouldBe` Nothing
            afterListErr.isLoadingDocuments `shouldBe` False
            afterListErr.lastError `shouldSatisfy` (/= Nothing)
            afterCreateErr.isCreatingDocument `shouldBe` False
            afterCreateErr.lastError `shouldBe` Just jaMessages.errConflictMessage

    describe "TamaTsuzura.Client.App MVU updateClient orchestration" $ do
        it "guards unsaved changes, resets history on discard, and resets editor on tab switch" $ do
            docA <- mkSampleDocument "doc-mvu-a" "文書A"
            docB <- mkSampleDocument "doc-mvu-b" "文書B"
            authBase <- mkAuthenticatedState
            (env, reqRef, cmdRef) <- mkMockClientEnv (\ _ -> pure (Right (HttpResponse 200 (encodeStrict (DocumentListResponse [] 0)))))
            let dirtyState :: ClientState
                dirtyState = authBase
                    { selectedDocument = Just docA
                    , draft            = withDraftBody (DocumentBody "未保存編集") (draftFromDocument docA)
                    , mobilePane       = PaneEditor
                    }
            -- Requesting another doc while dirty prompts confirmation without firing HTTP requests
            (sGuard, acts1) <- runUpdateStep env (RequestOpenDocument docB.id) dirtyState
            sGuard.pendingNavigation `shouldBe` Just (NavOpenDocument docB.id)
            acts1 `shouldBe` []
            readIORef reqRef >>= (`shouldBe` [])

            -- Canceling clears pending navigation and preserves draft
            (sCancel, _) <- runUpdateStep env CancelPendingNavigation sGuard
            sCancel.pendingNavigation `shouldBe` Nothing
            sCancel.draft.body `shouldBe` DocumentBody "未保存編集"

            -- Discarding on ReturnToList reverts draft and sends CmdSyncDocument with resetHistory=True
            (sRetGuard, _) <- runUpdateStep env ReturnToList sCancel
            sRetGuard.pendingNavigation `shouldBe` Just NavReturnToList
            (sDiscarded, _) <- runUpdateStep env ConfirmDiscardNavigation sRetGuard
            sDiscarded.mobilePane `shouldBe` PaneList
            sDiscarded.draft `shouldBe` draftFromDocument docA
            cmdsAfterDiscard <- readIORef cmdRef
            case reverse cmdsAfterDiscard of
                (CmdSyncDocument payload : _) -> payload.resetHistory `shouldBe` True
                other -> expectationFailure ("Expected CmdSyncDocument with resetHistory=True, got: " <> show other)

            -- Switching to Trash tab clears selectedDocument and emits CmdResetEditor
            writeIORef cmdRef []
            (sTrashTab, _) <- runUpdateStep env (RequestStatusFilter StatusTrashed) sDiscarded
            sTrashTab.selectedDocument `shouldBe` Nothing
            sTrashTab.filterState.status `shouldBe` StatusTrashed
            cmdsTrash <- readIORef cmdRef
            cmdsTrash `shouldSatisfy` elem CmdResetEditor

        it "validates before saving, blocks trash during in-flight save, and defers list reload on create from Trash" $ do
            doc <- mkSampleDocument "doc-mvu-save" "保存対象"
            authBase <- mkAuthenticatedState
            (env, reqRef, cmdRef) <- mkMockClientEnv $ \ req ->
                if req.method == MethodPost && req.url == "/api/documents"
                    then pure (Right (HttpResponse 201 (encodeStrict doc)))
                    else if req.method == MethodGet && Txt.isPrefixOf "/api/documents" req.url
                        then pure (Right (HttpResponse 200 (encodeStrict (DocumentListResponse [toDocumentSummary doc] 1))))
                        else if req.url == "/api/tags"
                            then pure (Right (HttpResponse 200 (encodeStrict (TagListResponse []))))
                            else if req.url == "/api/folders"
                                then pure (Right (HttpResponse 200 (encodeStrict (FolderListResponse []))))
                                else pure (Right (HttpResponse 200 (encodeStrict doc)))

            -- Invalid title blocks PUT and records localized validation error
            let badDraftState :: ClientState
                badDraftState = authBase
                    { selectedDocument = Just doc
                    , draft            = withDraftTitle "bad\ntitle" (draftFromDocument doc)
                    }
            (sValErr, actsErr) <- runUpdateStep env SaveCurrentDocument badDraftState
            actsErr `shouldBe` []
            sValErr.savingDocument `shouldBe` Nothing
            sValErr.lastError `shouldSatisfy` maybe False (Txt.isInfixOf jaMessages.valTitleControlChar)
            readIORef reqRef >>= (`shouldBe` [])

            -- Valid save records savingDocument and blocks SoftDeleteDocument while in flight
            let goodDirty :: ClientState
                goodDirty = badDraftState { draft = withDraftBody (DocumentBody "更新内容") (draftFromDocument doc) }
            (sSaving, _) <- runUpdateStep env SaveCurrentDocument goodDirty
            sSaving.savingDocument `shouldSatisfy` (/= Nothing)
            writeIORef reqRef []
            (sBlockedTrash, _) <- runUpdateStep env (SoftDeleteDocument doc.id) sSaving
            sBlockedTrash.mutatingDocumentId `shouldBe` Nothing
            readIORef reqRef >>= (`shouldBe` [])

            -- Creating a document from Trash tab issues POST first and only fetches list after DocumentCreated
            let trashState :: ClientState
                trashState = authBase
                    { filterState = withFilterStatus StatusTrashed defaultFilterState
                    }
            writeIORef reqRef []
            (sCreating, createActs) <- runUpdateStep env RequestCreateDocument trashState
            sCreating.pendingListReloadAfterCreate `shouldBe` True
            -- Filter stays on StatusTrashed until POST succeeds so failure does not desync tab and list
            sCreating.filterState.status `shouldBe` StatusTrashed
            reqsBeforeCreated <- readIORef reqRef
            map (.method) reqsBeforeCreated `shouldBe` [MethodPost]

            writeIORef reqRef []
            writeIORef cmdRef []
            (sCreated, _) <- case createActs of
                (act : _) -> runUpdateStep env act sCreating
                []        -> expectationFailure "Expected DocumentCreated action" >> pure (sCreating, [])
            sCreated.pendingListReloadAfterCreate `shouldBe` False
            sCreated.filterState.status `shouldBe` StatusActive
            reqsAfterCreated <- readIORef reqRef
            any (\ r -> r.method == MethodGet && r.url == "/api/documents") reqsAfterCreated `shouldBe` True
            cmdsAfterCreated <- readIORef cmdRef
            cmdsAfterCreated `shouldSatisfy` elem CmdFocus

        it "dispatches search, format normalization, extension sync, and clears openingDocumentId on same-doc select" $ do
            doc <- mkSampleDocument "doc-mvu-ops" "操作テスト"
            otherId <- expectRight (mkDocumentId "doc-other")
            authBase <- mkAuthenticatedState
            (env, _reqRef, cmdRef) <- mkMockClientEnv (\ _ -> pure (Right (HttpResponse 200 "{}")))
            let base :: ClientState
                base = authBase
                    { selectedDocument  = Just doc
                    , openingDocumentId = Just otherId
                    , draft             = withDraftBody (DocumentBody "abc   \n") (draftFromDocument doc)
                    , editorViewMode    = ModePreview
                    }
            -- Selecting already-selected doc cancels stale openingDocumentId
            (sSame, _) <- runUpdateStep env (RequestOpenDocument doc.id) base
            sSame.openingDocumentId `shouldBe` Nothing
            sSame.mobilePane `shouldBe` PaneEditor

            -- Switching format to PlainText normalizes view mode back to ModeEdit and syncs editor
            writeIORef cmdRef []
            (sPlain, _) <- runUpdateStep env (UpdateDraftFormat FormatPlainText) sSame
            sPlain.editorViewMode `shouldBe` ModeEdit
            cmdsFmt <- readIORef cmdRef
            length cmdsFmt `shouldBe` 1

            -- Search panel open, query update, next match, close
            writeIORef cmdRef []
            (sSearch1, _) <- runUpdateStep env (ToggleSearchPanel False) sPlain
            (sSearch2, _) <- runUpdateStep env (UpdateSearchQuery "abc") sSearch1
            (sSearch3, _) <- runUpdateStep env FindNextMatch sSearch2
            (sSearch4, _) <- runUpdateStep env CloseSearchPanel sSearch3
            sSearch4.searchPanel.isOpen `shouldBe` False
            cmdsSearch <- readIORef cmdRef
            cmdsSearch `shouldSatisfy` elem CmdFindNext
            cmdsSearch `shouldSatisfy` elem CmdClearSearch

            -- Unknown extension emits no notice or command; known extension transforms and syncs
            writeIORef cmdRef []
            (sExtUnknown, _) <- runUpdateStep env (ApplyExtension (ExtensionId "does-not-exist")) sSearch4
            sExtUnknown.statusNotice `shouldBe` Nothing
            readIORef cmdRef >>= (`shouldBe` [])

            (sExtKnown, _) <- runUpdateStep env (ApplyExtension (ExtensionId "trim-trailing-spaces")) sExtUnknown
            sExtKnown.draft.body `shouldBe` DocumentBody "abc\n"
            sExtKnown.statusNotice `shouldBe` Just jaMessages.noticeExtensionApplied
            cmdsExt <- readIORef cmdRef
            length cmdsExt `shouldBe` 1

    describe "TamaTsuzura.Client.EditorBridge pure JSON protocol" $ do
        it "encodes sync, reset, search, and editing commands to valid JSON" $ do
            let syncCmd :: EditorCommand
                syncCmd = CmdSyncDocument EditorSyncPayload
                    { docId        = "doc-1"
                    , version      = 3
                    , body         = "こんにちは\n世界"
                    , format       = FormatMarkdown
                    , readOnly     = False
                    , placeholder  = "入力"
                    , resetHistory = True
                    }

                searchCmd :: EditorCommand
                searchCmd = CmdApplySearch EditorSearchParams
                    { query         = "世界"
                    , replacement   = "TamaTsuzura"
                    , caseSensitive = True
                    , useRegex      = False
                    }
            encodeEditorCommandJson syncCmd `shouldSatisfy` Txt.isInfixOf "\"syncDocument\""
            encodeEditorCommandJson syncCmd `shouldSatisfy` Txt.isInfixOf "\"resetHistory\":true"
            encodeEditorCommandJson syncCmd `shouldSatisfy` Txt.isInfixOf "\"markdown\""
            encodeEditorCommandJson CmdResetEditor `shouldBe` "{\"type\":\"resetEditor\"}"
            encodeEditorCommandJson searchCmd `shouldSatisfy` Txt.isInfixOf "\"applySearch\""
            encodeEditorCommandJson CmdFindNext `shouldBe` "{\"type\":\"findNext\"}"
            encodeEditorCommandJson CmdReplaceAll `shouldBe` "{\"type\":\"replaceAll\"}"
            encodeEditorCommandJson CmdUndo `shouldBe` "{\"type\":\"undo\"}"
            encodeEditorCommandJson CmdRedo `shouldBe` "{\"type\":\"redo\"}"

        it "decodes all CodeMirror editor events and rejects malformed JSON" $ do
            decodeEditorEventJson "{\"type\":\"saveShortcut\"}" `shouldBe` Right EvtSaveShortcut
            decodeEditorEventJson "{\"type\":\"openSearch\",\"showReplace\":true}" `shouldBe` Right (EvtOpenSearch True)
            decodeEditorEventJson "{\"type\":\"historyState\",\"canUndo\":true,\"canRedo\":false}"
                `shouldBe` Right (EvtHistoryState True False)
            decodeEditorEventJson "{\"type\":\"searchStatus\",\"matchCount\":5,\"activeMatch\":2,\"regexError\":null}"
                `shouldBe` Right (EvtSearchStatus (EditorSearchStatus 5 2 Nothing))
            decodeEditorEventJson
                "{\"type\":\"bodyChanged\",\"body\":\"abc\",\"stats\":{\"lineCount\":1,\"charCount\":3,\"cursorLine\":1,\"cursorCol\":4}}"
                `shouldBe` Right (EvtBodyChanged "abc" (EditorStats 1 3 1 4))
            decodeEditorEventJson "{\"type\":\"unknownEvent\"}" `shouldSatisfy` isLeftEither
            decodeEditorEventJson "{broken" `shouldSatisfy` isLeftEither

        it "computes line and LF-normalized character statistics purely" $ do
            computeTextStats "" `shouldBe` EditorStats 1 0 1 1
            computeTextStats "一行目\r\n二行目\n三行目" `shouldBe` EditorStats 3 11 1 1

    describe "TamaTsuzura.Client.Markdown pure parser, sanitizer, and Miso renderer" $ do
        it "sanitizes safe URLs and rejects dangerous schemes, backslash bypasses, and control chars" $ do
            fmap toText (mkSafeUrl "  https://example.com/docs?a=1  ") `shouldBe` Just "https://example.com/docs?a=1"
            fmap toText (mkSafeUrl "mailto:user@example.com") `shouldBe` Just "mailto:user@example.com"
            fmap toText (mkSafeUrl "./notes/intro.md") `shouldBe` Just "./notes/intro.md"
            fmap toText (mkSafeUrl "#section-1") `shouldBe` Just "#section-1"
            mkSafeUrl "javascript:alert(1)" `shouldBe` Nothing
            mkSafeUrl "JAVASCRIPT:alert(1)" `shouldBe` Nothing
            mkSafeUrl "java\nscript:alert(1)" `shouldBe` Nothing
            mkSafeUrl "data:text/html,<script>alert(1)</script>" `shouldBe` Nothing
            mkSafeUrl "vbscript:msgbox(1)" `shouldBe` Nothing
            mkSafeUrl "//evil.example/payload" `shouldBe` Nothing
            mkSafeUrl "/\\evil.example/payload" `shouldBe` Nothing
            mkSafeUrl "\\\\evil.example\\payload" `shouldBe` Nothing
            mkSafeUrl "\\/evil.example/payload" `shouldBe` Nothing
            mkSafeUrl "   " `shouldBe` Nothing

        it "parses headings, fenced code, nested quotes, ordered start, task lists, escaped-pipe tables, and inline markup" $ do
            let sampleMd :: Text
                sampleMd = Txt.unlines
                    [ "# 見出し1"
                    , ""
                    , "通常文と **太字** と *斜体* と ~~取消~~ と `code` と snake_case_name と See [note] and [リンク](https://example.com)。"
                    , ""
                    , "- [x] 完了タスク"
                    , "- [ ] 未完了タスク"
                    , ""
                    , "3. 三番目"
                    , "4. 四番目"
                    , ""
                    , "> 外側引用"
                    , "> > 内側引用"
                    , ""
                    , "| 項目 | 値 |"
                    , "| :--- | ---: |"
                    , "| A\\|B | 100 |"
                    , ""
                    , "```haskell"
                    , "main = putStrLn \"hi\""
                    , "```"
                    , ""
                    , "---"
                    ]
                blocks :: [MarkdownBlock]
                blocks = parseMarkdown sampleMd

                renderedHtml :: Text
                renderedHtml = TxtEnc.decodeUtf8Lenient
                    (LazyBS.toStrict (toHtmlWith () () () (renderMarkdownView sampleMd)))
            blocks `shouldSatisfy` (\ bs -> length bs == 8)
            case blocks of
                (MdHeading 1 [InlineText "見出し1"] : _) -> pure ()
                other -> expectationFailure ("Expected H1 first, got: " <> show other)
            case parseInlines "**太字** and `x`" of
                [InlineStrong [InlineText "太字"], InlineText " and ", InlineCode "x"] -> pure ()
                other -> expectationFailure ("Unexpected inline AST: " <> show other)
            -- Bracket before link must stay literal text, and snake_case_name must not become <em>
            case parseInlines "See [note] and [link](https://example.com) with snake_case_name" of
                [ InlineText "See [note] and "
                  , InlineLink _ [InlineText "link"]
                  , InlineText " with snake_case_name"
                  ] -> pure ()
                other -> expectationFailure ("Unexpected bracket/underscore AST: " <> show other)
            case parseMarkdown "- [x] done\n- [ ] todo" of
                [MdUnorderedList [ListItem (Just True) _, ListItem (Just False) _]] -> pure ()
                other -> expectationFailure ("Unexpected task list AST: " <> show other)
            case parseMarkdown "| L | C | R |\n| :--- | :---: | ---: |\n| 1 | 2 | 3 |" of
                [MdTable [AlignLeft, AlignCenter, AlignRight] _ _] -> pure ()
                other -> expectationFailure ("Unexpected table AST: " <> show other)
            renderedHtml `shouldSatisfy` Txt.isInfixOf "<h1>見出し1</h1>"
            renderedHtml `shouldSatisfy` Txt.isInfixOf "<strong>太字</strong>"
            renderedHtml `shouldSatisfy` Txt.isInfixOf "<em>斜体</em>"
            renderedHtml `shouldSatisfy` Txt.isInfixOf "<del>取消</del>"
            renderedHtml `shouldSatisfy` Txt.isInfixOf "snake_case_name"
            renderedHtml `shouldSatisfy` Txt.isInfixOf "See [note] and "
            renderedHtml `shouldSatisfy` Txt.isInfixOf "start=\"3\""
            renderedHtml `shouldSatisfy` Txt.isInfixOf "A|B"
            renderedHtml `shouldSatisfy` Txt.isInfixOf "rel=\"noopener noreferrer\""
            renderedHtml `shouldSatisfy` Txt.isInfixOf "language-haskell"
            renderedHtml `shouldSatisfy` Txt.isInfixOf "<table"

        it "neutralizes raw HTML tags and javascript: links in AST and rendered preview output" $ do
            let hostile :: Text
                hostile = "<script>alert(1)</script> [click](javascript:alert(1)) ![img](data:text/html,evil)"

                parsed :: [MarkdownBlock]
                parsed = parseMarkdown hostile

                renderedHtml :: Text
                renderedHtml = TxtEnc.decodeUtf8Lenient
                    (LazyBS.toStrict (toHtmlWith () () () (renderMarkdownView hostile)))

                wikiMd :: Text
                wikiMd = "[Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language))\r\nsecond"
            -- Raw HTML stays strictly inside InlineText (rendered via createTextNode in DOM), never as an element,
            -- and unsafe schemes never produce InlineLink / InlineImage nodes.
            parsed `shouldBe` [MdParagraph [InlineText "<script>alert(1)</script> click img"]]
            renderedHtml `shouldSatisfy` (not . Txt.isInfixOf "<a")
            renderedHtml `shouldSatisfy` (not . Txt.isInfixOf "<img")
            renderedHtml `shouldSatisfy` (not . Txt.isInfixOf "javascript:")
            renderedHtml `shouldSatisfy` (not . Txt.isInfixOf "data:text/html")
            case parseMarkdown wikiMd of
                [MdParagraph [InlineLink safeUrl [InlineText "Haskell"], InlineSoftBreak, InlineText "second"]] ->
                    toText safeUrl `shouldBe` "https://en.wikipedia.org/wiki/Haskell_(programming_language)"
                other ->
                    expectationFailure ("Expected balanced-paren link and CRLF soft break, got: " <> show other)

        it "handles arbitrary text safely in parseMarkdown and renderMarkdownView (QuickCheck)" $
            property $ \ (rawStr :: String) ->
                let md :: Text
                    md = Txt.pack rawStr

                    rendered :: Text
                    rendered = TxtEnc.decodeUtf8Lenient
                        (LazyBS.toStrict (toHtmlWith () () () (renderMarkdownView md)))
                in  not (Txt.isInfixOf "href=\"javascript:" (Txt.toLower rendered))
                        && not (Txt.isInfixOf "src=\"javascript:" (Txt.toLower rendered))

    describe "TamaTsuzura.Client.I18n and View helpers" $ do
        it "formats titles, formats, timestamps, search counters, and Japanese error messages" $ do
            displayTitle jaMessages emptyDocumentTitle `shouldBe` jaMessages.untitledDocument
            titled <- expectRight (mkDocumentTitle "議事録")
            displayTitle jaMessages titled `shouldBe` "議事録"
            formatDocumentFormat jaMessages FormatMarkdown `shouldBe` "Markdown"
            formatDocumentFormat jaMessages FormatPlainText `shouldBe` "テキスト"
            formatUtcTimestamp sampleUtcTime `shouldBe` "2026-11-20 09:30 UTC"
            formatApiClientError jaMessages (ClientApiError 404 (ApiError ErrNotFound "missing" Nothing))
                `shouldBe` jaMessages.errNotFoundMessage
            formatValidationError jaMessages TitleContainsControlChar
                `shouldSatisfy` Txt.isInfixOf jaMessages.errValidationPrefix
            formatSearchMatchCounter jaMessages emptySearchPanelState `shouldBe` ""
            formatSearchMatchCounter jaMessages (mkTestSearchPanel "abc" 0 0 Nothing)
                `shouldBe` jaMessages.searchNoMatches
            formatSearchMatchCounter jaMessages (mkTestSearchPanel "abc" 5 2 Nothing)
                `shouldBe` "2 / 5件"
            formatSearchMatchCounter jaMessages (mkTestSearchPanel "[" 0 0 (Just "bad"))
                `shouldBe` jaMessages.searchInvalidRegex

        it "renders mobile-first Japanese Miso view HTML for editor, preview, search panel, and modals" $ do
            doc <- mkSampleDocument "doc-view-1" "設計メモ"
            authBase <- mkAuthenticatedState
            let onlineState :: ClientState
                onlineState =
                    (applyDocumentCreated jaMessages (Right doc)
                        (applyHealthResult jaMessages (Right healthOk) authBase))
                        { editorViewMode         = ModeSplit
                        , metadataPanelOpen      = True
                        , searchPanel            = SearchPanelState
                            { isOpen        = True
                            , showReplace   = True
                            , query         = "見出し"
                            , replacement   = ""
                            , caseSensitive = False
                            , useRegex      = False
                            , matchCount    = 1
                            , activeMatch   = 1
                            , regexError    = Nothing
                            }
                        , pendingNavigation      = Just NavReturnToList
                        , confirmPermanentDelete = Just doc.id
                        }

                onlineHtml :: Text
                onlineHtml = TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (toHtmlWith () () onlineState (viewClient jaMessages onlineState)))

                errState :: ClientState
                errState = applyHealthResult jaMessages (Left (ClientNetworkError "offline")) onlineState

                errHtml :: Text
                errHtml = TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (toHtmlWith () () errState (viewClient jaMessages errState)))

            onlineHtml `shouldSatisfy` Txt.isInfixOf jaMessages.appTitle
            onlineHtml `shouldSatisfy` Txt.isInfixOf jaMessages.statusOnline
            onlineHtml `shouldSatisfy` Txt.isInfixOf "設計メモ"
            onlineHtml `shouldSatisfy` Txt.isInfixOf "<tt-codemirror-editor"
            onlineHtml `shouldSatisfy` Txt.isInfixOf "<h1>見出し</h1>"
            onlineHtml `shouldSatisfy` Txt.isInfixOf jaMessages.searchReplaceAllLabel
            onlineHtml `shouldSatisfy` Txt.isInfixOf jaMessages.confirmUnsavedTitle
            onlineHtml `shouldSatisfy` Txt.isInfixOf jaMessages.confirmPermanentDelTitle
            errHtml `shouldSatisfy` Txt.isInfixOf jaMessages.statusError
            errHtml `shouldSatisfy` Txt.isInfixOf jaMessages.errNetworkPrefix

            -- Ensure selected <option> stays rendered even when selected folder/tag is absent from available lists
            missingFolder <- expectRight (mkFolderPath "archived/dir")
            missingTag <- expectRight (mkTag "保留")
            let plainFilteredState :: ClientState
                plainFilteredState = onlineState
                    { filterState      = defaultFilterState
                        { selectedFolder = Just missingFolder
                        , selectedTag    = Just missingTag
                        }
                    , availableFolders = []
                    , availableTags    = []
                    , draft            = withDraftFormat FormatPlainText onlineState.draft
                    }
                plainHtml :: Text
                plainHtml = TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (toHtmlWith () () plainFilteredState (viewClient jaMessages plainFilteredState)))
            plainHtml `shouldSatisfy` Txt.isInfixOf "archived/dir"
            plainHtml `shouldSatisfy` Txt.isInfixOf "#保留"
            plainHtml `shouldSatisfy` Txt.isInfixOf "value=\"plain_text\" selected"

        it "renders draft badges, recovery/stale/orphaned banners, sync status, and export/import buttons" $ do
            doc <- mkSampleDocument "doc-view-draft" "ドラフト表示テスト"
            otherId <- expectRight (mkDocumentId "doc-orphan")
            authBase <- mkAuthenticatedState
            uid <- mkSampleUserId "user-alice"
            let sd :: StoredDraft
                sd = toStoredDraft uid doc (withDraftBody (DocumentBody "ローカル変更") (draftFromDocument doc)) sampleLaterUtcTime

                orphanSd :: StoredDraft
                orphanSd = StoredDraft
                    { docId         = otherId
                    , ownerId       = Just uid
                    , titleInput    = "消えた文書"
                    , body          = DocumentBody "救出本文"
                    , format        = FormatMarkdown
                    , folderInput   = ""
                    , tagsInput     = ""
                    , baseUpdatedAt = sampleUtcTime
                    , savedAt       = sampleLaterUtcTime
                    }

                restoredState :: ClientState
                restoredState = authBase
                    { documentSummaries  = [toDocumentSummary doc]
                    , totalDocuments     = 1
                    , selectedDocument   = Just doc
                    , draft              = editorDraftFromStored sd
                    , storedDrafts       = Map.singleton doc.id sd
                    , draftSyncState     = DraftSyncSaved sampleLaterUtcTime
                    , restoredDraftDocId = Just doc.id
                    , orphanedDraft      = Just orphanSd
                    }

                restoredHtml :: Text
                restoredHtml = TxtEnc.decodeUtf8Lenient
                    (LazyBS.toStrict (toHtmlWith () () restoredState (viewClient jaMessages restoredState)))

                staleState :: ClientState
                staleState = restoredState
                    { restoredDraftDocId = Nothing
                    , staleDraftPrompt   = Just sd
                    }

                staleHtml :: Text
                staleHtml = TxtEnc.decodeUtf8Lenient
                    (LazyBS.toStrict (toHtmlWith () () staleState (viewClient jaMessages staleState)))
            restoredHtml `shouldSatisfy` Txt.isInfixOf jaMessages.actionImport
            restoredHtml `shouldSatisfy` Txt.isInfixOf jaMessages.actionExport
            restoredHtml `shouldSatisfy` Txt.isInfixOf jaMessages.badgeHasDraft
            restoredHtml `shouldSatisfy` Txt.isInfixOf jaMessages.draftStateSaved
            restoredHtml `shouldSatisfy` Txt.isInfixOf jaMessages.draftRestoredBanner
            restoredHtml `shouldSatisfy` Txt.isInfixOf jaMessages.draftOrphanedBanner
            restoredHtml `shouldSatisfy` Txt.isInfixOf jaMessages.actionRecoverOrphanDraft
            staleHtml `shouldSatisfy` Txt.isInfixOf jaMessages.draftStaleBanner
            staleHtml `shouldSatisfy` Txt.isInfixOf jaMessages.actionApplyDraft

    describe "TamaTsuzura.Client.DraftStorage pure serialization and selection" $ do
        it "round-trips StoredDraft through JSON list payloads and selects latest draft deterministically" $ do
            doc1 <- mkSampleDocument "doc-d1" "一番目"
            doc2 <- mkSampleDocument "doc-d2" "二番目"
            uid  <- mkSampleUserId "user-alice"
            let sd1 :: StoredDraft
                sd1 = toStoredDraft uid doc1 (withDraftBody (DocumentBody "draft1") (draftFromDocument doc1)) sampleUtcTime

                sd2 :: StoredDraft
                sd2 = toStoredDraft uid doc2 (withDraftBody (DocumentBody "draft2") (draftFromDocument doc2)) sampleLaterUtcTime

                listJson :: Text
                listJson = "{\"ok\":true,\"drafts\":[" <> encodeStoredDraftJson sd1 <> "," <> encodeStoredDraftJson sd2 <> "]}"

                legacyJson :: Text
                legacyJson = "{\"ok\":true,\"drafts\":[{\"docId\":\"doc-d1\",\"titleInput\":\"一番目\",\"body\":\"old\",\"format\":\"markdown\",\"folderInput\":\"\",\"tagsInput\":\"\",\"baseUpdatedAt\":\"2026-11-20T09:30:00Z\",\"savedAt\":\"2026-11-20T09:30:00Z\"}]}"
            decodeStoredDraftListJson listJson `shouldBe` Right [sd1, sd2]
            case decodeStoredDraftListJson legacyJson of
                Right [legacySd] -> legacySd.ownerId `shouldBe` Nothing
                other            -> expectationFailure ("Expected legacy draft with Nothing ownerId, got: " <> show other)
            decodeStoredDraftListJson "{\"ok\":false,\"error\":\"QuotaExceededError\"}"
                `shouldBe` Left "QuotaExceededError"
            decodeStoredDraftListJson "{broken" `shouldSatisfy` isLeftEither
            latestStoredDraft [] `shouldBe` Nothing
            latestStoredDraft [sd1, sd2] `shouldBe` Just sd2
            let sd1SameTime :: StoredDraft
                sd1SameTime = sd1 { savedAt = sampleLaterUtcTime }
            latestStoredDraft [sd1SameTime, sd2] `shouldBe` Just sd2
            latestStoredDraft [sd2, sd1SameTime] `shouldBe` Just sd2

        it "decodes draft storage events including empty or missing docId on failure" $ do
            docId <- expectRight (mkDocumentId "doc-evt-1")
            decodeDraftStorageEventJson
                "{\"type\":\"draftSaved\",\"docId\":\"doc-evt-1\",\"savedAt\":\"2026-11-20T09:30:00Z\"}"
                `shouldBe` Right (DraftEvtSaved docId sampleUtcTime)
            decodeDraftStorageEventJson
                "{\"type\":\"draftSaveFailed\",\"docId\":\"doc-evt-1\",\"error\":\"quota\"}"
                `shouldBe` Right (DraftEvtSaveFailed (Just docId) "quota")
            decodeDraftStorageEventJson
                "{\"type\":\"draftSaveFailed\",\"docId\":\"\",\"error\":\"parse\"}"
                `shouldBe` Right (DraftEvtSaveFailed Nothing "parse")
            decodeDraftStorageEventJson "{\"type\":\"unknown\"}" `shouldSatisfy` isLeftEither

    describe "TamaTsuzura.Client.FileTransfer pure export/import logic" $ do
        it "sanitizes export file names, avoids duplicate extensions, and sets UTF-8 MIME types" $ do
            sanitizeExportBaseName "  議事録 2026/11:20?  " `shouldBe` "議事録 2026_11_20"
            sanitizeExportBaseName "   ...   " `shouldBe` "untitled"
            sanitizeExportBaseName "a\nb\tc" `shouldBe` "a_b_c"
            buildExportFileName "議事録" FormatMarkdown `shouldBe` "議事録.md"
            buildExportFileName "README.md" FormatMarkdown `shouldBe` "README.md"
            buildExportFileName "notes.MARKDOWN" FormatMarkdown `shouldBe` "notes.MARKDOWN"
            buildExportFileName "メモ" FormatPlainText `shouldBe` "メモ.txt"
            buildExportFileName "memo.txt" FormatPlainText `shouldBe` "memo.txt"
            exportMimeType FormatMarkdown `shouldBe` "text/markdown;charset=utf-8"
            exportMimeType FormatPlainText `shouldBe` "text/plain;charset=utf-8"
            let exportSpec :: ExportSpec
                exportSpec = buildExportSpec "" (DocumentBody "hello") FormatMarkdown
            exportSpec `shouldBe` ExportSpec "untitled.md" "text/markdown;charset=utf-8" "hello"

        it "never emits forbidden OS filename characters or control chars from sanitizeExportBaseName (QuickCheck)" $
            property $ \ (rawStr :: String) ->
                let sanitized :: Text
                    sanitized = sanitizeExportBaseName (Txt.pack rawStr)
                in  not (Txt.null sanitized)
                        && not (Txt.any (`elem` ['/', '\\', ':', '*', '?', '"', '<', '>', '|']) sanitized)
                        && not (Txt.any (\ ch -> ch < ' ' || ch == '\DEL') sanitized)

        it "infers format, derives title, strips UTF-8 BOM, and validates imported documents" $ do
            inferDocumentFormatFromFileName "README.md" `shouldBe` FormatMarkdown
            inferDocumentFormatFromFileName "notes.MARKDOWN" `shouldBe` FormatMarkdown
            inferDocumentFormatFromFileName "memo.txt" `shouldBe` FormatPlainText
            inferDocumentFormatFromFileName "plain-no-ext" `shouldBe` FormatPlainText

            deriveTitleFromFileName "dir/sub/設計メモ.md" `shouldBe` "設計メモ"
            deriveTitleFromFileName "C:\\notes\\todo.TXT" `shouldBe` "todo"
            deriveTitleFromFileName "  trailing.md  " `shouldBe` "trailing"
            deriveTitleFromFileName "bad\ntitle.md" `shouldBe` "bad title"
            Txt.length (deriveTitleFromFileName (Txt.replicate 300 "あ" <> ".md")) `shouldBe` 255
            stripUtf8Bom "\xFEFF# 見出し" `shouldBe` "# 見出し"
            stripUtf8Bom "# 見出し" `shouldBe` "# 見出し"

            folderVal <- expectRight (mkFolderPath "work/imports")
            tagVal <- expectRight (mkTag "取込")
            let imported :: ImportedFile
                imported = ImportedFile
                    { fileName = "週次メモ.md"
                    , content  = "\xFEFF# 週次\n本文"
                    }
            req <- expectRight (prepareImportedDocumentRequest imported (Just folderVal) (expectPureTagSet tagVal))
            toText req.title `shouldBe` "週次メモ"
            req.format `shouldBe` FormatMarkdown
            req.body `shouldBe` DocumentBody "# 週次\n本文"
            req.folder `shouldBe` Just folderVal

            let hugeImported :: ImportedFile
                hugeImported = ImportedFile
                    { fileName = "huge.txt"
                    , content  = Txt.replicate (maxDocumentBodyBytes + 1) "x"
                    }
            case prepareImportedDocumentRequest hugeImported Nothing emptyTagSet of
                Left (ImportValidationFailed BodyTooLarge{}) -> pure ()
                other -> expectationFailure ("Expected BodyTooLarge, got: " <> show other)

        it "decodes export and file-pick JSFFI JSON results and formats Japanese import errors" $ do
            decodeExportResultJson "{\"ok\":true}" `shouldBe` Right ()
            decodeExportResultJson "{\"ok\":false,\"error\":\"blocked\"}" `shouldBe` Left "blocked"
            decodeFilePickResultJson "{\"status\":\"cancelled\"}" `shouldBe` FilePickCancelled
            decodeFilePickResultJson "{\"status\":\"selected\",\"fileName\":\"a.md\",\"content\":\"hi\"}"
                `shouldBe` FilePickSelected (ImportedFile "a.md" "hi")
            decodeFilePickResultJson "{\"status\":\"error\",\"code\":\"too_large\",\"actualBytes\":6000000,\"maxBytes\":5242880}"
                `shouldBe` FilePickError (ImportFileTooLarge 6000000 5242880)
            decodeFilePickResultJson "{\"status\":\"error\",\"code\":\"invalid_utf8\",\"message\":\"bad\"}"
                `shouldBe` FilePickError (ImportInvalidUtf8 "bad")
            formatFileImportError jaMessages (ImportInvalidUtf8 "bad") `shouldBe` jaMessages.errImportInvalidUtf8
            formatFileImportError jaMessages (ImportFileTooLarge 10 5) `shouldBe` jaMessages.errPayloadTooLarge

    describe "TamaTsuzura.Client.App Phase 3 draft autosave, recovery, and file transfer MVU" $ do
        it "reconciles server documents against local drafts and builds safe recovery requests" $ do
            doc <- mkSampleDocument "doc-rec" "照合対象"
            uid <- mkSampleUserId "user-alice"
            let cleanSd :: StoredDraft
                cleanSd = toStoredDraft uid doc (draftFromDocument doc) sampleLaterUtcTime

                dirtySd :: StoredDraft
                dirtySd = toStoredDraft uid doc (withDraftBody (DocumentBody "未保存追記") (draftFromDocument doc)) sampleLaterUtcTime

                staleDoc :: Document
                staleDoc = withDocUpdatedAt sampleLaterUtcTime doc

                staleSd :: StoredDraft
                staleSd = dirtySd { baseUpdatedAt = sampleUtcTime }

                trashedDoc :: Document
                trashedDoc = withDocDeletedAt (Just sampleLaterUtcTime) doc
            reconcileDocumentWithDraft doc Nothing `shouldBe` NoLocalDraft
            reconcileDocumentWithDraft doc (Just cleanSd) `shouldBe` RedundantLocalDraft
            reconcileDocumentWithDraft doc (Just dirtySd) `shouldBe` RestoreLocalDraft dirtySd
            reconcileDocumentWithDraft staleDoc (Just staleSd) `shouldBe` PromptStaleLocalDraft staleSd
            reconcileDocumentWithDraft trashedDoc (Just dirtySd) `shouldBe` PromptStaleLocalDraft dirtySd

            let brokenMetaSd :: StoredDraft
                brokenMetaSd = StoredDraft
                    { docId         = dirtySd.docId
                    , ownerId       = Just uid
                    , titleInput    = "bad\ntitle"
                    , body          = dirtySd.body
                    , format        = dirtySd.format
                    , folderInput   = "../bad"
                    , tagsInput     = "bad\ntag"
                    , baseUpdatedAt = dirtySd.baseUpdatedAt
                    , savedAt       = dirtySd.savedAt
                    }
            recReq <- expectRight (buildRecoveryRequestFromStoredDraft brokenMetaSd)
            recReq.body `shouldBe` DocumentBody "未保存追記"
            recReq.title `shouldBe` emptyDocumentTitle
            recReq.folder `shouldBe` Nothing
            recReq.tags `shouldBe` emptyTagSet

        it "autosaves dirty edits to IndexedDB, clears on clean revert/save/discard/trash, and resaves concurrent edits" $ do
            doc <- mkSampleDocument "doc-auto" "自動退避"
            authBase <- mkAuthenticatedState
            uid <- mkSampleUserId "user-alice"
            let savedDoc :: Document
                savedDoc = withDocUpdatedAt sampleLaterUtcTime (withDocBody (DocumentBody "編集中1") doc)
            (env, _reqRef, _cmdRef, savedDraftsRef, deletedIdsRef, _exportsRef) <-
                mkFullMockClientEnv
                    (\ _ -> pure (Right (HttpResponse 200 (encodeStrict savedDoc))))
                    (pure (Right []))
                    (pure FilePickCancelled)

            let base :: ClientState
                base = authBase
                    { selectedDocument = Just doc
                    , draft            = draftFromDocument doc
                    }

            -- Editing body queues local draft and marks pending
            (sEdited, _) <- runUpdateStep env (EditorEventReceived (EvtBodyChanged "編集中1" (EditorStats 1 4 1 5))) base
            sEdited.draftSyncState `shouldBe` DraftSyncPending
            Map.member doc.id sEdited.storedDrafts `shouldBe` True
            saved1 <- readIORef savedDraftsRef
            length saved1 `shouldBe` 1

            -- Receiving storage confirmation transitions to DraftSyncSaved
            let sConfirmed :: ClientState
                sConfirmed = applyDraftStorageEvent (DraftEvtSaved doc.id sampleLaterUtcTime) sEdited
            sConfirmed.draftSyncState `shouldBe` DraftSyncSaved sampleLaterUtcTime

            -- Reverting body back to server version deletes local draft
            (sReverted, _) <- runUpdateStep env (EditorEventReceived (EvtBodyChanged (toText doc.body) (EditorStats 2 8 1 1))) sConfirmed
            sReverted.draftSyncState `shouldBe` DraftSyncIdle
            Map.member doc.id sReverted.storedDrafts `shouldBe` False
            readIORef deletedIdsRef >>= (`shouldBe` [doc.id])

            -- Explicit save clears draft when clean; concurrent edit during save re-schedules with new baseUpdatedAt
            writeIORef savedDraftsRef []
            writeIORef deletedIdsRef []
            let sentDraft :: EditorDraft
                sentDraft = withDraftBody (DocumentBody "編集中1") (draftFromDocument doc)

                cleanInFlight :: ClientState
                cleanInFlight = base
                    { draft          = sentDraft
                    , storedDrafts   = Map.singleton doc.id (toStoredDraft uid doc sentDraft sampleUtcTime)
                    , savingDocument = Just (doc.id, sentDraft)
                    }
            (sSavedClean, _) <- runUpdateStep env (DocumentSaved authBase.authSessionSeq doc.id (Right savedDoc)) cleanInFlight
            sSavedClean.draftSyncState `shouldBe` DraftSyncIdle
            Map.member doc.id sSavedClean.storedDrafts `shouldBe` False
            readIORef deletedIdsRef >>= (`shouldBe` [doc.id])

            writeIORef savedDraftsRef []
            writeIORef deletedIdsRef []
            let concurrentInFlight :: ClientState
                concurrentInFlight = cleanInFlight
                    { draft = withDraftBody (DocumentBody "保存中にさらに追記") sentDraft
                    }
            (sSavedConcurrent, _) <- runUpdateStep env (DocumentSaved authBase.authSessionSeq doc.id (Right savedDoc)) concurrentInFlight
            sSavedConcurrent.draftSyncState `shouldBe` DraftSyncPending
            resaved <- readIORef savedDraftsRef
            case resaved of
                [sd] -> do
                    sd.ownerId `shouldBe` Just uid
                    sd.baseUpdatedAt `shouldBe` savedDoc.updatedAt
                    sd.body `shouldBe` DocumentBody "保存中にさらに追記"
                other -> expectationFailure ("Expected 1 re-saved draft, got: " <> show other)

            -- ConfirmDiscardNavigation, SoftDelete, and PermanentDelete all purge local draft
            writeIORef deletedIdsRef []
            let pendingDiscard :: ClientState
                pendingDiscard = sEdited { pendingNavigation = Just NavReturnToList }
            (sAfterDiscard, _) <- runUpdateStep env ConfirmDiscardNavigation pendingDiscard
            Map.member doc.id sAfterDiscard.storedDrafts `shouldBe` False
            readIORef deletedIdsRef >>= (`shouldBe` [doc.id])

            writeIORef deletedIdsRef []
            let trashedDoc :: Document
                trashedDoc = withDocDeletedAt (Just sampleLaterUtcTime) doc
            (sAfterSoftDel, _) <- runUpdateStep env (DocumentSoftDeleted authBase.authSessionSeq doc.id (Right trashedDoc)) sEdited
            Map.member doc.id sAfterSoftDel.storedDrafts `shouldBe` False
            readIORef deletedIdsRef >>= (`shouldBe` [doc.id])

            writeIORef deletedIdsRef []
            (sAfterPermDel, _) <- runUpdateStep env (DocumentPermanentlyDeleted authBase.authSessionSeq doc.id (Right ())) sEdited
            Map.member doc.id sAfterPermDel.storedDrafts `shouldBe` False
            readIORef deletedIdsRef >>= (`shouldBe` [doc.id])

            -- Saving docA after switching to docB preserves docB's draft status and recovery banners
            docB <- mkSampleDocument "doc-other-open" "別文書"
            let sdB :: StoredDraft
                sdB = toStoredDraft uid docB (withDraftBody (DocumentBody "docB未保存") (draftFromDocument docB)) sampleLaterUtcTime

                switchedDuringSave :: ClientState
                switchedDuringSave = authBase
                    { selectedDocument   = Just docB
                    , draft              = editorDraftFromStored sdB
                    , storedDrafts       = Map.fromList [(doc.id, toStoredDraft uid doc sentDraft sampleUtcTime), (docB.id, sdB)]
                    , draftSyncState     = DraftSyncSaved sampleLaterUtcTime
                    , restoredDraftDocId = Just docB.id
                    , staleDraftPrompt   = Just sdB
                    , savingDocument     = Just (doc.id, sentDraft)
                    }
                afterBgSave :: ClientState
                afterBgSave = applyDocumentSaved jaMessages doc.id (Right savedDoc) switchedDuringSave
            afterBgSave.draftSyncState `shouldBe` DraftSyncSaved sampleLaterUtcTime
            afterBgSave.restoredDraftDocId `shouldBe` Just docB.id
            afterBgSave.staleDraftPrompt `shouldBe` Just sdB
            Map.member doc.id afterBgSave.storedDrafts `shouldBe` False
            Map.member docB.id afterBgSave.storedDrafts `shouldBe` True

            -- DraftsLoaded failure and DraftEvtSaveFailed (matching / non-matching / Nothing)
            let sLoadFail :: ClientState
                sLoadFail = applyDraftsLoaded jaMessages (Left "idb-blocked") sEdited

                sOtherFail :: ClientState
                sOtherFail = applyDraftStorageEvent (DraftEvtSaveFailed (Just docB.id) "other-err") sConfirmed

                sMatchFail :: ClientState
                sMatchFail = applyDraftStorageEvent (DraftEvtSaveFailed (Just doc.id) "quota") sConfirmed

                sGlobalFail :: ClientState
                sGlobalFail = applyDraftStorageEvent (DraftEvtSaveFailed Nothing "parse") sConfirmed
            sLoadFail.draftSyncState `shouldBe` DraftSyncFailed "idb-blocked"
            sOtherFail.draftSyncState `shouldBe` DraftSyncSaved sampleLaterUtcTime
            sMatchFail.draftSyncState `shouldBe` DraftSyncFailed "quota"
            sGlobalFail.draftSyncState `shouldBe` DraftSyncFailed "parse"

        it "auto-opens and restores latest draft on reload, handles late DraftsLoaded, stale conflicts, and 404 orphan recovery" $ do
            doc <- mkSampleDocument "doc-recover" "復元対象"
            orphanId <- expectRight (mkDocumentId "doc-deleted-remote")
            recoveredDoc <- mkSampleDocument "doc-new-recovered" "孤立復元"
            authBase <- mkAuthenticatedState
            uid <- mkSampleUserId "user-alice"
            let storedForDoc :: StoredDraft
                storedForDoc = toStoredDraft uid doc (withDraftBody (DocumentBody "リロード前ドラフト") (draftFromDocument doc)) sampleLaterUtcTime

                orphanSd :: StoredDraft
                orphanSd = StoredDraft
                    { docId         = orphanId
                    , ownerId       = Just uid
                    , titleInput    = "孤立復元"
                    , body          = storedForDoc.body
                    , format        = storedForDoc.format
                    , folderInput   = storedForDoc.folderInput
                    , tagsInput     = storedForDoc.tagsInput
                    , baseUpdatedAt = storedForDoc.baseUpdatedAt
                    , savedAt       = storedForDoc.savedAt
                    }

            (env, _reqRef, cmdRef, _savedRef, deletedIdsRef, _exportsRef) <-
                mkFullMockClientEnv
                    (\ req ->
                        if req.method == MethodGet && req.url == "/api/documents/doc-recover"
                            then pure (Right (HttpResponse 200 (encodeStrict doc)))
                            else if req.method == MethodGet && req.url == "/api/documents/doc-deleted-remote"
                                then pure (Right (HttpResponse 404 (encodeStrict (ApiError ErrNotFound "not found" Nothing))))
                                else if req.method == MethodPost && req.url == "/api/documents"
                                    then pure (Right (HttpResponse 201 (encodeStrict recoveredDoc)))
                                    else pure (Right (HttpResponse 200 (encodeStrict (DocumentListResponse [] 0))))
                    )
                    (pure (Right [storedForDoc]))
                    (pure FilePickCancelled)

            -- DraftsLoaded triggers auto-open GET and DocumentLoaded restores draft automatically
            (sAutoOpen, autoActs) <- runUpdateStep env (DraftsLoaded authBase.authSessionSeq (Right [storedForDoc])) authBase
            sAutoOpen.autoOpeningDraftId `shouldBe` Just doc.id
            (sRestored, _) <- case autoActs of
                (act : _) -> runUpdateStep env act sAutoOpen
                []        -> expectationFailure "Expected DocumentLoaded action" >> pure (sAutoOpen, [])
            sRestored.draft.body `shouldBe` DocumentBody "リロード前ドラフト"
            sRestored.restoredDraftDocId `shouldBe` Just doc.id
            sRestored.statusNotice `shouldBe` Just jaMessages.noticeDraftRestored

            -- DiscardCurrentDraft is blocked while save is in flight to prevent stale merge resurrection
            writeIORef deletedIdsRef []
            let savingRestored :: ClientState
                savingRestored = sRestored { savingDocument = Just (doc.id, sRestored.draft) }
            (sBlockedDiscard, _) <- runUpdateStep env DiscardCurrentDraft savingRestored
            sBlockedDiscard.draft `shouldBe` sRestored.draft
            readIORef deletedIdsRef >>= (`shouldBe` [])

            -- DiscardCurrentDraft reverts to server version, resets history, and deletes from IndexedDB
            writeIORef cmdRef []
            writeIORef deletedIdsRef []
            (sDiscarded, _) <- runUpdateStep env DiscardCurrentDraft sRestored
            sDiscarded.draft `shouldBe` draftFromDocument doc
            sDiscarded.restoredDraftDocId `shouldBe` Nothing
            readIORef deletedIdsRef >>= (`shouldBe` [doc.id])
            cmdsDiscard <- readIORef cmdRef
            case reverse cmdsDiscard of
                (CmdSyncDocument payload : _) -> payload.resetHistory `shouldBe` True
                other -> expectationFailure ("Expected resetHistory=True sync, got: " <> show other)

            -- DiscardCurrentDraft on staleDraftPrompt (e.g. trashed read-only doc) clears prompt without overwriting editor
            writeIORef deletedIdsRef []
            let trashedDoc :: Document
                trashedDoc = withDocDeletedAt (Just sampleLaterUtcTime) doc

                trashedWithStale :: ClientState
                trashedWithStale = authBase
                    { selectedDocument = Just trashedDoc
                    , draft            = draftFromDocument trashedDoc
                    , storedDrafts     = Map.singleton doc.id storedForDoc
                    , staleDraftPrompt = Just storedForDoc
                    }
            (sTrashedCleared, _) <- runUpdateStep env DiscardCurrentDraft trashedWithStale
            sTrashedCleared.staleDraftPrompt `shouldBe` Nothing
            Map.member doc.id sTrashedCleared.storedDrafts `shouldBe` False
            readIORef deletedIdsRef >>= (`shouldBe` [doc.id])

            -- Late DraftsLoaded arriving after user already opened clean doc still restores
            let alreadyOpenClean :: ClientState
                alreadyOpenClean = authBase
                    { selectedDocument = Just doc
                    , draft            = draftFromDocument doc
                    }
                sLateRestored :: ClientState
                sLateRestored = applyDraftsLoaded jaMessages (Right [storedForDoc]) alreadyOpenClean
            sLateRestored.draft.body `shouldBe` DocumentBody "リロード前ドラフト"
            sLateRestored.restoredDraftDocId `shouldBe` Just doc.id

            -- Stale conflict (server updated newer than draft baseUpdatedAt) prompts without overwriting, then ApplyStaleDraft applies
            let newerServerDoc :: Document
                newerServerDoc = withDocUpdatedAt sampleLaterUtcTime doc

                staleStored :: StoredDraft
                staleStored = storedForDoc { baseUpdatedAt = sampleUtcTime }

                waitingLoad :: ClientState
                waitingLoad = authBase
                    { openingDocumentId = Just doc.id
                    , storedDrafts      = Map.singleton doc.id staleStored
                    }
                sStale :: ClientState
                sStale = applyDocumentLoaded jaMessages doc.id (Right newerServerDoc) waitingLoad
            sStale.draft.body `shouldBe` newerServerDoc.body
            sStale.staleDraftPrompt `shouldBe` Just staleStored
            (sAppliedStale, _) <- runUpdateStep env ApplyStaleDraft sStale
            sAppliedStale.draft.body `shouldBe` DocumentBody "リロード前ドラフト"
            sAppliedStale.staleDraftPrompt `shouldBe` Nothing

            -- Auto-open 404 surfaces orphanedDraft; RecoverOrphanedDraft guards dirty doc, recovers, and DiscardOrphanedDraft purges
            writeIORef deletedIdsRef []
            (sOrphanOpen, orphanActs) <- runUpdateStep env (DraftsLoaded authBase.authSessionSeq (Right [orphanSd])) authBase
            (sOrphaned, _) <- case orphanActs of
                (act : _) -> runUpdateStep env act sOrphanOpen
                []        -> expectationFailure "Expected DocumentLoaded 404 action" >> pure (sOrphanOpen, [])
            sOrphaned.orphanedDraft `shouldBe` Just orphanSd
            sOrphaned.mobilePane `shouldBe` PaneList

            -- Dirty current doc triggers NavRecoverOrphanedDraft guard first
            let dirtyWithOrphan :: ClientState
                dirtyWithOrphan = sOrphaned
                    { selectedDocument = Just doc
                    , draft            = withDraftBody (DocumentBody "他文書編集") (draftFromDocument doc)
                    }
            (sOrphanGuard, _) <- runUpdateStep env RecoverOrphanedDraft dirtyWithOrphan
            sOrphanGuard.pendingNavigation `shouldBe` Just NavRecoverOrphanedDraft

            -- DiscardOrphanedDraft removes orphan banner and IndexedDB key
            writeIORef deletedIdsRef []
            (sOrphanDiscarded, _) <- runUpdateStep env DiscardOrphanedDraft sOrphaned
            sOrphanDiscarded.orphanedDraft `shouldBe` Nothing
            sOrphanDiscarded.statusNotice `shouldBe` Just jaMessages.noticeDraftDiscarded
            readIORef deletedIdsRef >>= (`shouldBe` [orphanId])

            writeIORef deletedIdsRef []
            (sRecovering, recActs) <- runUpdateStep env RecoverOrphanedDraft sOrphaned
            (sRecovered, _) <- case recActs of
                (act : _) -> runUpdateStep env act sRecovering
                []        -> expectationFailure "Expected OrphanDraftRecovered action" >> pure (sRecovering, [])
            sRecovered.orphanedDraft `shouldBe` Nothing
            sRecovered.selectedDocument `shouldBe` Just recoveredDoc
            readIORef deletedIdsRef >>= (`shouldBe` [orphanId])

        it "exports active drafts and imports Markdown/text files with unsaved guard and filter inheritance" $ do
            doc <- mkSampleDocument "doc-io" "入出力テスト"
            importedDoc <- mkSampleDocument "doc-imported" "取り込みメモ"
            authBase <- mkAuthenticatedState
            folderVal <- expectRight (mkFolderPath "work/import")
            tagVal <- expectRight (mkTag "取込")
            let picked :: FilePickResult
                picked = FilePickSelected ImportedFile
                    { fileName = "取り込みメモ.md"
                    , content  = "\xFEFF# 取り込み\n本文"
                    }
            (env, reqRef, cmdRef, _savedRef, _delRef, exportsRef) <-
                mkFullMockClientEnv
                    (\ req ->
                        if req.method == MethodPost && req.url == "/api/documents"
                            then pure (Right (HttpResponse 201 (encodeStrict importedDoc)))
                            else if req.url == "/api/tags"
                                then pure (Right (HttpResponse 200 (encodeStrict (TagListResponse []))))
                                else if req.url == "/api/folders"
                                    then pure (Right (HttpResponse 200 (encodeStrict (FolderListResponse []))))
                                    else pure (Right (HttpResponse 200 (encodeStrict (DocumentListResponse [] 0))))
                    )
                    (pure (Right []))
                    (pure picked)

            let activeState :: ClientState
                activeState = authBase
                    { selectedDocument = Just doc
                    , draft            = withDraftBody (DocumentBody "エクスポート本文") (draftFromDocument doc)
                    , filterState      = defaultFilterState
                        { selectedFolder = Just folderVal
                        , selectedTag    = Just tagVal
                        }
                    }

            -- Export dispatches ExportSpec with current draft content and shows notice
            (sExp1, expActs) <- runUpdateStep env ExportCurrentDocument activeState
            exports <- readIORef exportsRef
            exports `shouldBe` [ExportSpec "入出力テスト.md" "text/markdown;charset=utf-8" "エクスポート本文"]
            (sExp2, _) <- case expActs of
                (act : _) -> runUpdateStep env act sExp1
                []        -> expectationFailure "Expected ExportFinished action" >> pure (sExp1, [])
            sExp2.statusNotice `shouldBe` Just (jaMessages.noticeExportedPrefix <> "入出力テスト.md")

            -- RequestImportDocument guards dirty state before launching file picker
            (sImportGuard, _) <- runUpdateStep env RequestImportDocument activeState
            sImportGuard.pendingNavigation `shouldBe` Just NavImportDocument

            -- ConfirmDiscardNavigation reverts draft before launching file picker so cancelling never leaves unsaved orphan edits
            writeIORef reqRef []
            writeIORef cmdRef []
            (sAfterConfirm, pickActs) <- runUpdateStep env ConfirmDiscardNavigation sImportGuard
            isCurrentDocumentDirty sAfterConfirm `shouldBe` False
            sAfterConfirm.draft `shouldBe` draftFromDocument doc
            (sImporting, createActs) <- case pickActs of
                (act : _) -> runUpdateStep env act sAfterConfirm
                []        -> expectationFailure "Expected ImportFilePicked action" >> pure (sAfterConfirm, [])
            sImporting.isImportingDocument `shouldBe` True
            reqs <- readIORef reqRef
            map (.method) reqs `shouldBe` [MethodPost]
            (sImported, _) <- case createActs of
                (act : _) -> runUpdateStep env act sImporting
                []        -> expectationFailure "Expected DocumentImported action" >> pure (sImporting, [])
            sImported.isImportingDocument `shouldBe` False
            sImported.selectedDocument `shouldBe` Just importedDoc
            sImported.statusNotice `shouldBe` Just (jaMessages.noticeImportedPrefix <> "取り込みメモ.md")
            cmdsAfterImport <- readIORef cmdRef
            cmdsAfterImport `shouldSatisfy` elem CmdFocus

            -- Cancelled picker produces no error; export/import failures set localized errors
            (sCancelPick, _) <- runUpdateStep env (ImportFilePicked FilePickCancelled) sImported
            sCancelPick.lastError `shouldBe` Nothing
            (sErrPick, _) <- runUpdateStep env (ImportFilePicked (FilePickError (ImportInvalidUtf8 "bad"))) sImported
            sErrPick.lastError `shouldBe` Just jaMessages.errImportInvalidUtf8
            (sExpFail, _) <- runUpdateStep env (ExportFinished "a.md" (Left "blocked")) sImported
            sExpFail.lastError `shouldSatisfy` maybe False (Txt.isInfixOf jaMessages.errExportFailedPrefix)
            (sImpFail, _) <- runUpdateStep env (DocumentImported authBase.authSessionSeq "a.md" (Left (ClientNetworkError "offline"))) sImporting
            sImpFail.isImportingDocument `shouldBe` False
            sImpFail.lastError `shouldSatisfy` maybe False (Txt.isInfixOf jaMessages.errNetworkPrefix)

            -- Importing from Trash tab switches back to StatusActive and reloads list only on success
            writeIORef reqRef []
            let trashImportState :: ClientState
                trashImportState = authBase
                    { filterState = withFilterStatus StatusTrashed defaultFilterState
                    }
            (sTrashImp1, trashImpActs) <- runUpdateStep env (ImportFilePicked picked) trashImportState
            sTrashImp1.pendingListReloadAfterImport `shouldBe` True
            sTrashImp1.filterState.status `shouldBe` StatusTrashed
            writeIORef reqRef []
            (sTrashImp2, _) <- case trashImpActs of
                (act : _) -> runUpdateStep env act sTrashImp1
                []        -> expectationFailure "Expected DocumentImported from trash" >> pure (sTrashImp1, [])
            sTrashImp2.filterState.status `shouldBe` StatusActive
            trashReqs <- readIORef reqRef
            any (\ r -> r.method == MethodGet && r.url == "/api/documents") trashReqs `shouldBe` True

    describe "TamaTsuzura.Client M3 authentication UI, CSRF, 401 handling, and multi-user isolation" $ do
        it "validates login/register forms, formats auth errors, and builds user display labels" $ do
            parseDisplayNameInput "   " `shouldBe` Right Nothing
            dn <- expectRight (mkDisplayName "アリス")
            parseDisplayNameInput " アリス " `shouldBe` Right (Just dn)
            parseDisplayNameInput "bad\nname" `shouldBe` Left DisplayNameContainsControlChar

            let validLogin :: AuthFormState
                validLogin = emptyAuthFormState
                    { usernameInput = "  Alice_01 "
                    , passwordInput = "password123"
                    }
            loginReq <- expectRight (validateLoginForm jaMessages validLogin)
            toText loginReq.username `shouldBe` "alice_01"

            validateLoginForm jaMessages emptyAuthFormState `shouldSatisfy` isLeftEither
            validateLoginForm jaMessages (validLogin { passwordInput = "" })
                `shouldSatisfy` either (Txt.isInfixOf jaMessages.valEmptyPassword) (const False)

            let baseRegister :: AuthFormState
                baseRegister = emptyAuthFormState
                    { mode                 = AuthModeRegister
                    , usernameInput        = "bob-99"
                    , displayNameInput     = "ボブ"
                    , passwordInput        = "strong-pass-123"
                    , passwordConfirmInput = "strong-pass-123"
                    }
            regReq <- expectRight (validateRegisterForm jaMessages baseRegister)
            toText regReq.username `shouldBe` "bob-99"
            fmap toText regReq.displayName `shouldBe` Just "ボブ"

            validateRegisterForm jaMessages (baseRegister { passwordInput = "short", passwordConfirmInput = "short" })
                `shouldSatisfy` either (Txt.isInfixOf jaMessages.valPasswordTooShort) (const False)
            validateRegisterForm jaMessages (baseRegister { passwordConfirmInput = "different-pass" })
                `shouldSatisfy` either (Txt.isInfixOf jaMessages.valPasswordMismatch) (const False)

            formatLoginApiError jaMessages (ClientApiError 401 (ApiError ErrUnauthorized "invalid" Nothing))
                `shouldBe` jaMessages.errInvalidCredentials
            formatRegisterApiError jaMessages (ClientApiError 409 (ApiError ErrConflict "dup" Nothing))
                `shouldBe` jaMessages.errUsernameAlreadyExists
            formatRegisterApiError jaMessages (ClientApiError 403 (ApiError ErrForbidden "closed" Nothing))
                `shouldBe` jaMessages.errRegistrationDisabled

            aliceSession <- mkSampleAuthSession "user-alice" "alice" (Just "アリス")
            bobSession   <- mkSampleAuthSession "user-bob" "bob" Nothing
            displayUserLabel aliceSession.user `shouldBe` "アリス"
            displayUserLabel bobSession.user `shouldBe` "@bob"

        it "bootstraps with /api/health and /api/auth/me, handles 401 login gate, register, and CSRF-authenticated workspace" $ do
            aliceSession <- mkSampleAuthSession "user-alice" "alice" (Just "アリス")
            aliceDoc <- mkSampleDocument "doc-alice-1" "アリスのメモ"
            (env, reqRef, _cmdRef, _savedRef, _delRef, _exportsRef) <-
                mkFullMockClientEnv
                    (\ req -> case (req.method, req.url) of
                        (MethodGet, "/api/health") ->
                            pure (Right (HttpResponse 200 (encodeStrict healthOk)))
                        (MethodGet, "/api/auth/me") ->
                            pure (Right (HttpResponse 401 (encodeStrict (ApiError ErrUnauthorized "Authentication required" Nothing))))
                        (MethodPost, "/api/auth/register") ->
                            pure (Right (HttpResponse 201 (encodeStrict aliceSession)))
                        (MethodGet, "/api/documents") ->
                            pure (Right (HttpResponse 200 (encodeStrict (DocumentListResponse [toDocumentSummary aliceDoc] 1))))
                        (MethodGet, "/api/tags") ->
                            pure (Right (HttpResponse 200 (encodeStrict (TagListResponse []))))
                        (MethodGet, "/api/folders") ->
                            pure (Right (HttpResponse 200 (encodeStrict (FolderListResponse []))))
                        (MethodPost, "/api/documents") ->
                            pure (Right (HttpResponse 201 (encodeStrict aliceDoc)))
                        _ ->
                            pure (Right (HttpResponse 200 (encodeStrict aliceDoc)))
                    )
                    (pure (Right []))
                    (pure FilePickCancelled)

            -- Initialize only calls /api/health and /api/auth/me (no document/draft fetches before auth)
            (sBoot, bootActs) <- runUpdateStep env Initialize (initialClientState bundledExtensions)
            sBoot.authState `shouldBe` AuthChecking
            bootReqs <- readIORef reqRef
            map (\ r -> (r.method, r.url)) bootReqs
                `shouldBe` [(MethodGet, "/api/health"), (MethodGet, "/api/auth/me")]

            -- Receiving 401 from /api/auth/me transitions cleanly to login screen without error banner
            (sAfterBoot, _) <- runActionQueue env bootActs sBoot
            case sAfterBoot.authState of
                AuthUnauthenticated form -> do
                    form.mode `shouldBe` AuthModeLogin
                    form.formError `shouldBe` Nothing
                other -> expectationFailure ("Expected AuthUnauthenticated, got: " <> show other)
            sAfterBoot.lastError `shouldBe` Nothing
            sAfterBoot.connectionStatus `shouldBe` ConnOnline

            -- Switch to register tab, fill valid fields, submit -> authenticated & workspace loaded
            writeIORef reqRef []
            (sReg1, _) <- runUpdateStep env (SwitchAuthMode AuthModeRegister) sAfterBoot
            (sReg2, _) <- runUpdateStep env (UpdateAuthUsername "Alice") sReg1
            (sReg3, _) <- runUpdateStep env (UpdateAuthDisplayName "アリス") sReg2
            (sReg4, _) <- runUpdateStep env (UpdateAuthPassword "alice-secret-123") sReg3
            (sReg5, _) <- runUpdateStep env (UpdateAuthPasswordConfirm "alice-secret-123") sReg4
            (sSubmitting, submitActs) <- runUpdateStep env SubmitAuthForm sReg5
            (sAuthed, _) <- runActionQueue env submitActs sSubmitting
            currentAuthSession sAuthed `shouldBe` Just aliceSession
            sAuthed.statusNotice `shouldBe` Just jaMessages.noticeRegistered
            sAuthed.totalDocuments `shouldBe` 1

            -- Creating a document attaches session's X-CSRF-Token header
            writeIORef reqRef []
            (sCreating, createActs) <- runUpdateStep env RequestCreateDocument sAuthed
            createReqs <- readIORef reqRef
            case createReqs of
                [postReq] -> do
                    postReq.method `shouldBe` MethodPost
                    postReq.url `shouldBe` "/api/documents"
                    lookup csrfHeaderName postReq.headers `shouldBe` Just (toText aliceSession.csrfToken)
                other -> expectationFailure ("Expected single POST /api/documents, got: " <> show other)
            (sCreated, _) <- runActionQueue env createActs sCreating
            sCreated.selectedDocument `shouldBe` Just aliceDoc

        it "expires session to login screen on 401 while preserving IndexedDB drafts and protecting active login input" $ do
            doc <- mkSampleDocument "doc-401" "作業中メモ"
            authBase <- mkAuthenticatedState
            uid <- mkSampleUserId "user-alice"
            let localSd :: StoredDraft
                localSd = toStoredDraft uid doc (withDraftBody (DocumentBody "未保存の重要本文") (draftFromDocument doc)) sampleLaterUtcTime

                dirtyState :: ClientState
                dirtyState = authBase
                    { documentSummaries = [toDocumentSummary doc]
                    , totalDocuments    = 1
                    , selectedDocument  = Just doc
                    , draft             = editorDraftFromStored localSd
                    , storedDrafts      = Map.singleton doc.id localSd
                    , mobilePane        = PaneEditor
                    }
            (env, _reqRef, cmdRef, _savedRef, deletedIdsRef, _exportsRef) <-
                mkFullMockClientEnv
                    (\ _ -> pure (Right (HttpResponse 401 (encodeStrict (ApiError ErrUnauthorized "Session expired" Nothing)))))
                    (pure (Right [localSd]))
                    (pure FilePickCancelled)

            -- Saving triggers 401 -> clears workspace, resets CodeMirror, shows session expired error, keeps IndexedDB draft
            (sSaving, saveActs) <- runUpdateStep env SaveCurrentDocument dirtyState
            (sExpired, _) <- runActionQueue env saveActs sSaving
            case sExpired.authState of
                AuthUnauthenticated{} -> pure ()
                other -> expectationFailure ("Expected AuthUnauthenticated after 401, got: " <> show other)
            sExpired.lastError `shouldBe` Just jaMessages.errSessionExpired
            sExpired.documentSummaries `shouldBe` []
            sExpired.selectedDocument `shouldBe` Nothing
            sExpired.storedDrafts `shouldBe` Map.empty
            sExpired.mobilePane `shouldBe` PaneList
            readIORef cmdRef >>= (`shouldSatisfy` elem CmdResetEditor)
            readIORef deletedIdsRef >>= (`shouldBe` [])

            -- Late 401 response from previous session must not wipe user's active login form input
            (sTyping, _) <- runUpdateStep env (UpdateAuthUsername "alice") sExpired
            (sAfterLate401, _) <- runUpdateStep env
                (TagsLoaded authBase.authSessionSeq (Left (ClientApiError 401 (ApiError ErrUnauthorized "expired" Nothing))))
                sTyping
            case sAfterLate401.authState of
                AuthUnauthenticated form -> form.usernameInput `shouldBe` "alice"
                other -> expectationFailure ("Expected login form preserved, got: " <> show other)

        it "guards dirty document on logout, sends CSRF token to /api/auth/logout, and isolates IndexedDB drafts per user" $ do
            docAlice <- mkSampleDocument "doc-alice-iso" "アリス文書"
            docBob   <- mkSampleDocument "doc-bob-iso" "ボブ文書"
            docLeg   <- mkSampleDocument "doc-legacy-iso" "旧形式文書"
            aliceSession <- mkSampleAuthSession "user-alice" "alice" (Just "アリス")
            bobSession   <- mkSampleAuthSession "user-bob" "bob" (Just "ボブ")

            let sdAlice :: StoredDraft
                sdAlice = toStoredDraft aliceSession.user.id docAlice
                    (withDraftBody (DocumentBody "アリス未保存") (draftFromDocument docAlice))
                    sampleLaterUtcTime

                sdBob :: StoredDraft
                sdBob = toStoredDraft bobSession.user.id docBob
                    (withDraftBody (DocumentBody "ボブ未保存") (draftFromDocument docBob))
                    sampleUtcTime

                sdLegacy :: StoredDraft
                sdLegacy = (toStoredDraft aliceSession.user.id docLeg
                    (withDraftBody (DocumentBody "旧未保存") (draftFromDocument docLeg))
                    sampleLaterUtcTime)
                    { ownerId = Nothing }

            (env, reqRef, cmdRef, _savedRef, deletedIdsRef, _exportsRef) <-
                mkFullMockClientEnv
                    (\ req -> case (req.method, req.url) of
                        (MethodPost, "/api/auth/logout") ->
                            pure (Right (HttpResponse 204 ""))
                        (MethodGet, "/api/documents/doc-bob-iso") ->
                            pure (Right (HttpResponse 200 (encodeStrict docBob)))
                        _ ->
                            pure (Right (HttpResponse 200 (encodeStrict (DocumentListResponse [] 0))))
                    )
                    (pure (Right [sdAlice, sdBob, sdLegacy]))
                    (pure FilePickCancelled)

            -- Dirty doc triggers NavLogout guard before sending POST /api/auth/logout
            let aliceDirty :: ClientState
                aliceDirty = (mkAuthenticatedStateFor aliceSession)
                    { selectedDocument = Just docAlice
                    , draft            = editorDraftFromStored sdAlice
                    , storedDrafts     = Map.singleton docAlice.id sdAlice
                    }
            (sLogoutGuard, actsGuard) <- runUpdateStep env RequestLogout aliceDirty
            sLogoutGuard.pendingNavigation `shouldBe` Just NavLogout
            actsGuard `shouldBe` []
            readIORef reqRef >>= (`shouldBe` [])

            -- Confirming discard deletes Alice's discarded draft, calls POST /api/auth/logout with CSRF, and resets editor
            (sLoggingOut, logoutActs) <- runUpdateStep env ConfirmDiscardNavigation sLogoutGuard
            readIORef deletedIdsRef >>= (`shouldBe` [docAlice.id])
            logoutReqs <- readIORef reqRef
            case logoutReqs of
                [logoutReq] -> do
                    logoutReq.method `shouldBe` MethodPost
                    logoutReq.url `shouldBe` "/api/auth/logout"
                    lookup csrfHeaderName logoutReq.headers `shouldBe` Just (toText aliceSession.csrfToken)
                other -> expectationFailure ("Expected POST /api/auth/logout, got: " <> show other)
            (sLoggedOut, _) <- runActionQueue env logoutActs sLoggingOut
            case sLoggedOut.authState of
                AuthUnauthenticated{} -> pure ()
                other -> expectationFailure ("Expected AuthUnauthenticated after logout, got: " <> show other)
            sLoggedOut.statusNotice `shouldBe` Just jaMessages.noticeLoggedOut
            readIORef cmdRef >>= (`shouldSatisfy` elem CmdResetEditor)

            -- When Bob logs in, DraftsLoaded filters strictly by Bob's userId (Alice's newer draft and legacy Nothing ownerId are hidden)
            let bobState :: ClientState
                bobState = mkAuthenticatedStateFor bobSession
            (sBobDrafts, bobActs) <- runUpdateStep env
                (DraftsLoaded bobState.authSessionSeq (Right [sdAlice, sdBob, sdLegacy]))
                bobState
            Map.keys sBobDrafts.storedDrafts `shouldBe` [docBob.id]
            sBobDrafts.autoOpeningDraftId `shouldBe` Just docBob.id
            (sBobRestored, _) <- runActionQueue env bobActs sBobDrafts
            sBobRestored.selectedDocument `shouldBe` Just docBob
            sBobRestored.draft.body `shouldBe` DocumentBody "ボブ未保存"

        it "handles login 401 failure, ignores RefreshAll/duplicate submit while submitting, and handles logout 401/network errors" $ do
            (env, reqRef, cmdRef) <- mkMockClientEnv $ \ req -> case (req.method, req.url) of
                (MethodPost, "/api/auth/login") ->
                    pure (Right (HttpResponse 401 (encodeStrict (ApiError ErrUnauthorized "Invalid credentials" Nothing))))
                (MethodPost, "/api/auth/logout") ->
                    pure (Left "network-offline")
                _ ->
                    pure (Right (HttpResponse 200 (encodeStrict healthOk)))

            let loginFormState :: ClientState
                loginFormState = resetWorkspaceForUnauthenticated
                    (emptyAuthFormState { usernameInput = "alice", passwordInput = "wrong-pass" })
                    Nothing
                    Nothing
                    (initialClientState bundledExtensions)

            -- Submit enters isSubmitting=True; duplicate submit and RefreshAll are ignored while in flight
            (sSubmitting, submitActs) <- runUpdateStep env SubmitAuthForm loginFormState
            writeIORef reqRef []
            (sDupSubmit, dupActs) <- runUpdateStep env SubmitAuthForm sSubmitting
            dupActs `shouldBe` []
            sDupSubmit `shouldBe` sSubmitting
            (sDuringRefresh, refActs) <- runUpdateStep env RefreshAll sSubmitting
            refActs `shouldBe` []
            sDuringRefresh `shouldBe` sSubmitting
            readIORef reqRef >>= (`shouldBe` [])

            -- Receiving 401 login failure clears isSubmitting and shows invalid credentials error
            (sLoginFailed, _) <- runActionQueue env submitActs sSubmitting
            case sLoginFailed.authState of
                AuthUnauthenticated form -> do
                    form.isSubmitting `shouldBe` False
                    form.formError `shouldBe` Just jaMessages.errInvalidCredentials
                other -> expectationFailure ("Expected AuthUnauthenticated with error, got: " <> show other)

            -- Logout network failure keeps user authenticated and resets isLoggingOut
            authBase <- mkAuthenticatedState
            (sLogoutStart, logoutNetActs) <- runUpdateStep env RequestLogout authBase
            sLogoutStart.isLoggingOut `shouldBe` True
            -- RefreshAll while logging out is ignored
            writeIORef reqRef []
            (sRefreshWhileLogout, _) <- runUpdateStep env RefreshAll sLogoutStart
            sRefreshWhileLogout `shouldBe` sLogoutStart
            readIORef reqRef >>= (`shouldBe` [])
            (sLogoutNetErr, _) <- runActionQueue env logoutNetActs sLogoutStart
            sLogoutNetErr.isLoggingOut `shouldBe` False
            currentAuthSession sLogoutNetErr `shouldSatisfy` (/= Nothing)
            sLogoutNetErr.lastError `shouldSatisfy` maybe False (Txt.isInfixOf jaMessages.errNetworkPrefix)

            -- Logout returning 401 (already expired server-side) still transitions cleanly to unauthenticated
            writeIORef cmdRef []
            (sLogout401, _) <- runUpdateStep env
                (LoggedOut authBase.authSessionSeq (Left (ClientApiError 401 (ApiError ErrUnauthorized "expired" Nothing))))
                sLogoutStart
            case sLogout401.authState of
                AuthUnauthenticated{} -> pure ()
                other -> expectationFailure ("Expected AuthUnauthenticated on logout 401, got: " <> show other)
            sLogout401.statusNotice `shouldBe` Just jaMessages.noticeLoggedOut
            readIORef cmdRef >>= (`shouldSatisfy` elem CmdResetEditor)

        it "renders auth checking, login/register screens, authenticated user badge, logout button, and logout confirm modal" $ do
            let checkingState :: ClientState
                checkingState = initialClientState bundledExtensions

                checkingHtml :: Text
                checkingHtml = TxtEnc.decodeUtf8Lenient
                    (LazyBS.toStrict (toHtmlWith () () checkingState (viewClient jaMessages checkingState)))

                loginState :: ClientState
                loginState = resetWorkspaceForUnauthenticated
                    (emptyAuthFormState { formError = Just jaMessages.errInvalidCredentials })
                    (Just jaMessages.errSessionExpired)
                    Nothing
                    checkingState

                loginHtml :: Text
                loginHtml = TxtEnc.decodeUtf8Lenient
                    (LazyBS.toStrict (toHtmlWith () () loginState (viewClient jaMessages loginState)))

                registerState :: ClientState
                registerState = resetWorkspaceForUnauthenticated
                    (emptyAuthFormState { mode = AuthModeRegister })
                    Nothing
                    Nothing
                    checkingState

                registerHtml :: Text
                registerHtml = TxtEnc.decodeUtf8Lenient
                    (LazyBS.toStrict (toHtmlWith () () registerState (viewClient jaMessages registerState)))

            authStateSample <- mkAuthenticatedState
            let logoutModalState :: ClientState
                logoutModalState = authStateSample { pendingNavigation = Just NavLogout }

                authedHtml :: Text
                authedHtml = TxtEnc.decodeUtf8Lenient
                    (LazyBS.toStrict (toHtmlWith () () logoutModalState (viewClient jaMessages logoutModalState)))

            checkingHtml `shouldSatisfy` Txt.isInfixOf jaMessages.authCheckingMessage
            checkingHtml `shouldSatisfy` (not . Txt.isInfixOf jaMessages.actionLogout)

            loginHtml `shouldSatisfy` Txt.isInfixOf jaMessages.authLoginHeading
            loginHtml `shouldSatisfy` Txt.isInfixOf jaMessages.errInvalidCredentials
            loginHtml `shouldSatisfy` Txt.isInfixOf jaMessages.errSessionExpired
            loginHtml `shouldSatisfy` (not . Txt.isInfixOf jaMessages.authPasswordConfirmLabel)

            registerHtml `shouldSatisfy` Txt.isInfixOf jaMessages.authRegisterHeading
            registerHtml `shouldSatisfy` Txt.isInfixOf jaMessages.authDisplayNameLabel
            registerHtml `shouldSatisfy` Txt.isInfixOf jaMessages.authPasswordConfirmLabel
            registerHtml `shouldSatisfy` Txt.isInfixOf jaMessages.actionRegister

            authedHtml `shouldSatisfy` Txt.isInfixOf "アリス"
            authedHtml `shouldSatisfy` Txt.isInfixOf jaMessages.actionLogout
            authedHtml `shouldSatisfy` Txt.isInfixOf jaMessages.confirmUnsavedLogoutBody
            authedHtml `shouldSatisfy` Txt.isInfixOf jaMessages.actionDiscardAndLogout

        it "handles pure auth state transitions (applyAuthSessionChecked, applyAuthSubmitted, applySessionExpired, currentUserId)" $ do
            session <- mkSampleAuthSession "user-pure" "pureuser" Nothing
            let s0 :: ClientState
                s0 = initialClientState bundledExtensions

                sAuthed :: ClientState
                sAuthed = applyAuthSessionChecked jaMessages 0 (Right session) s0

                sExpired :: ClientState
                sExpired = applySessionExpired jaMessages sAuthed

                sSubmitted :: ClientState
                sSubmitted = applyAuthSubmitted jaMessages sExpired.authSessionSeq AuthModeLogin (Right session) sExpired
            currentUserId s0 `shouldBe` Nothing
            currentUserId sAuthed `shouldBe` Just session.user.id
            currentUserId sExpired `shouldBe` Nothing
            sExpired.lastError `shouldBe` Just jaMessages.errSessionExpired
            currentUserId sSubmitted `shouldBe` Just session.user.id
            sSubmitted.statusNotice `shouldBe` Just jaMessages.noticeLoggedIn

    describe "M4 Phase 3: Revision history API, MVU flow, and View" $ do
        it "builds revision list/detail/restore requests with query encoding and CSRF header" $ do
            docId <- expectRight (mkDocumentId "doc-rev")
            rev2  <- expectRight (mkRevisionNumber 2)
            rev3  <- expectRight (mkRevisionNumber 3)
            lim   <- expectRight (mkListLimit 10)
            off   <- expectRight (mkListOffset 5)
            csrf  <- mkSampleCsrfToken
            let base :: ApiBaseUrl
                base = mkApiBaseUrl "/app/"

                listDefaultReq :: HttpRequest
                listDefaultReq = buildListRevisionsRequest base docId Nothing Nothing

                listPagedReq :: HttpRequest
                listPagedReq = buildListRevisionsRequest base docId (Just lim) (Just off)

                detailReq :: HttpRequest
                detailReq = buildGetRevisionRequest base docId rev2

                restoreReq :: HttpRequest
                restoreReq = buildRestoreRevisionRequest base csrf docId rev2 (RestoreRevisionRequest (Just rev3))

            buildRevisionListQueryString Nothing Nothing `shouldBe` ""
            buildRevisionListQueryString (Just lim) (Just off) `shouldBe` "?limit=10&offset=5"
            (listDefaultReq.method, listDefaultReq.url) `shouldBe` (MethodGet, "/app/api/documents/doc-rev/revisions")
            (listPagedReq.method, listPagedReq.url) `shouldBe` (MethodGet, "/app/api/documents/doc-rev/revisions?limit=10&offset=5")
            (detailReq.method, detailReq.url) `shouldBe` (MethodGet, "/app/api/documents/doc-rev/revisions/2")
            (restoreReq.method, restoreReq.url) `shouldBe` (MethodPost, "/app/api/documents/doc-rev/revisions/2/restore")
            lookup csrfHeaderName restoreReq.headers `shouldBe` Just (toText csrf)
            restoreReq.body `shouldSatisfy` maybe False (Txt.isInfixOf "\"base_revision\":3" . TxtEnc.decodeUtf8Lenient)

        it "executes listDocumentRevisions, getDocumentRevision, and restoreDocumentRevision against HttpTransport" $ do
            doc <- mkSampleDocument "doc-rev-io" "最新タイトル"
            revId1 <- expectRight (mkRevisionId "rev-1")
            hash1  <- expectRight (mkContentHash (Txt.replicate 64 "a"))
            csrf   <- mkSampleCsrfToken
            let detail1 :: RevisionDetail
                detail1 = RevisionDetail
                    { id               = revId1
                    , documentId       = doc.id
                    , revision         = initialRevisionNumber
                    , parentRevisionId = Nothing
                    , title            = doc.title
                    , body             = doc.body
                    , format           = doc.format
                    , diff             = generateUnifiedDiff "" (toText doc.body)
                    , contentHash      = hash1
                    , createdAt        = sampleUtcTime
                    }
                listRes :: RevisionListResponse
                listRes = RevisionListResponse
                    { revisions = [toRevisionSummary detail1]
                    , total     = 1
                    }
                mkClient :: HttpResponse -> ApiClient
                mkClient fixed = ApiClient
                    { baseUrl   = defaultApiBaseUrl
                    , transport = HttpTransport (\ _ -> pure (Right fixed))
                    }
            eList <- listDocumentRevisions (mkClient (HttpResponse 200 (encodeStrict listRes))) doc.id Nothing Nothing
            eList `shouldBe` Right listRes
            eDetail <- getDocumentRevision (mkClient (HttpResponse 200 (encodeStrict detail1))) doc.id initialRevisionNumber
            eDetail `shouldBe` Right detail1
            eRestored <- restoreDocumentRevision
                (mkClient (HttpResponse 200 (encodeStrict doc)))
                csrf
                doc.id
                initialRevisionNumber
                (RestoreRevisionRequest (Just doc.revision))
            eRestored `shouldBe` Right doc

        it "handles pure revision state transitions with stale-sequence guards and resetRevisionHistory" $ do
            doc1 <- mkSampleDocument "doc-pure-rev" "版2タイトル"
            rev2 <- expectRight (mkRevisionNumber 2)
            rev3 <- expectRight (mkRevisionNumber 3)
            revId1 <- expectRight (mkRevisionId "rev-p1")
            revId2 <- expectRight (mkRevisionId "rev-p2")
            hash1  <- expectRight (mkContentHash (Txt.replicate 64 "1"))
            hash2  <- expectRight (mkContentHash (Txt.replicate 64 "2"))
            let docAtRev2 :: Document
                docAtRev2 = withDocRevision rev2 doc1

                detail1 :: RevisionDetail
                detail1 = RevisionDetail
                    { id               = revId1
                    , documentId       = doc1.id
                    , revision         = initialRevisionNumber
                    , parentRevisionId = Nothing
                    , title            = doc1.title
                    , body             = DocumentBody "初版本文"
                    , format           = FormatMarkdown
                    , diff             = generateUnifiedDiff "" "初版本文"
                    , contentHash      = hash1
                    , createdAt        = sampleUtcTime
                    }

                detail2 :: RevisionDetail
                detail2 = RevisionDetail
                    { id               = revId2
                    , documentId       = doc1.id
                    , revision         = rev2
                    , parentRevisionId = Just revId1
                    , title            = doc1.title
                    , body             = DocumentBody "第2版本文"
                    , format           = FormatMarkdown
                    , diff             = generateUnifiedDiff "初版本文" "第2版本文"
                    , contentHash      = hash2
                    , createdAt        = sampleLaterUtcTime
                    }

                listRes :: RevisionListResponse
                listRes = RevisionListResponse
                    { revisions = [toRevisionSummary detail2, toRevisionSummary detail1]
                    , total     = 2
                    }

                sOpen :: ClientState
                sOpen = (initialClientState bundledExtensions)
                    { selectedDocument = Just docAtRev2
                    , draft            = draftFromDocument docAtRev2
                    , revisionHistory  = emptyRevisionHistoryState
                        { isHistoryOpen  = True
                        , documentId     = Just doc1.id
                        , listRequestSeq = 1
                        , isLoadingList  = True
                        }
                    }

                -- Stale list sequence must be ignored
                sStaleList :: ClientState
                sStaleList = applyRevisionListLoaded jaMessages 999 doc1.id False (Right listRes) sOpen

                -- Closed panel response must be ignored without touching lastError
                sClosedPanel :: ClientState
                sClosedPanel = applyRevisionListLoaded
                    jaMessages
                    1
                    doc1.id
                    False
                    (Left (ClientNetworkError "closed"))
                    (sOpen { revisionHistory = sOpen.revisionHistory { isHistoryOpen = False }, lastError = Just "keep-me" })

                sListLoaded :: ClientState
                sListLoaded = applyRevisionListLoaded jaMessages 1 doc1.id False (Right listRes) sOpen

                sDetailWait :: ClientState
                sDetailWait = sListLoaded
                    { revisionHistory = sListLoaded.revisionHistory
                        { detailRequestSeq = 2
                        , isLoadingDetail  = True
                        }
                    }

                -- Stale detail sequence must be ignored
                sStaleDetail :: ClientState
                sStaleDetail = applyRevisionDetailLoaded jaMessages 1 doc1.id rev2 (Right detail2) sDetailWait

                sDetailLoaded :: ClientState
                sDetailLoaded = applyRevisionDetailLoaded jaMessages 2 doc1.id rev2 (Right detail2) sDetailWait

                restoredDoc :: Document
                restoredDoc = withDocRevision rev3 (withDocBody (DocumentBody "初版本文") docAtRev2)

                -- Unmatched restoringRevisionDoc must be ignored
                sStaleRestore :: ClientState
                sStaleRestore = applyRevisionRestored jaMessages doc1.id (Right restoredDoc) sDetailLoaded

                sRestored :: ClientState
                sRestored = applyRevisionRestored
                    jaMessages
                    doc1.id
                    (Right restoredDoc)
                    (sDetailLoaded { restoringRevisionDoc = Just doc1.id })

                rhReset :: RevisionHistoryState
                rhReset = resetRevisionHistory sDetailLoaded.revisionHistory
            sStaleList.revisionHistory.revisions `shouldBe` []
            sClosedPanel.lastError `shouldBe` Just "keep-me"
            sListLoaded.revisionHistory.totalRevisions `shouldBe` 2
            sListLoaded.revisionHistory.selectedRevision `shouldBe` Just rev2
            sStaleDetail.revisionHistory.revisionDetail `shouldBe` Nothing
            sDetailLoaded.revisionHistory.revisionDetail `shouldBe` Just detail2
            fmap (.revision) sStaleRestore.selectedDocument `shouldBe` Just rev2
            fmap (.revision) sRestored.selectedDocument `shouldBe` Just rev3
            sRestored.restoringRevisionDoc `shouldBe` Nothing
            sRestored.draft.body `shouldBe` DocumentBody "初版本文"
            sRestored.statusNotice `shouldBe` Just jaMessages.noticeRevisionRestored
            rhReset.isHistoryOpen `shouldBe` False
            rhReset.listRequestSeq `shouldBe` sDetailLoaded.revisionHistory.listRequestSeq + 1
            rhReset.detailRequestSeq `shouldBe` sDetailLoaded.revisionHistory.detailRequestSeq + 1

        it "runs full MVU revision browse, detail inspect, restore, and save auto-refresh flow" $ do
            baseState <- mkAuthenticatedState
            docBase <- mkSampleDocument "doc-mvu-rev" "v2 タイトル"
            titleV1 <- expectRight (mkDocumentTitle "v1 タイトル")
            rev2 <- expectRight (mkRevisionNumber 2)
            rev3 <- expectRight (mkRevisionNumber 3)
            revId1 <- expectRight (mkRevisionId "rev-m1")
            revId2 <- expectRight (mkRevisionId "rev-m2")
            revId3 <- expectRight (mkRevisionId "rev-m3")
            hash1  <- expectRight (mkContentHash (Txt.replicate 64 "a"))
            hash2  <- expectRight (mkContentHash (Txt.replicate 64 "b"))
            hash3  <- expectRight (mkContentHash (Txt.replicate 64 "c"))

            let docV2 :: Document
                docV2 = withDocRevision rev2 (withDocBody (DocumentBody "line1\nline2-v2\n") docBase)

                docV3 :: Document
                docV3 = Document
                    { id        = docV2.id
                    , title     = titleV1
                    , body      = DocumentBody "line1\nline2-v1\n"
                    , format    = FormatMarkdown
                    , folder    = docV2.folder
                    , tags      = docV2.tags
                    , revision  = rev3
                    , createdAt = docV2.createdAt
                    , updatedAt = sampleLaterUtcTime
                    , deletedAt = Nothing
                    }

                detail1 :: RevisionDetail
                detail1 = RevisionDetail
                    { id               = revId1
                    , documentId       = docV2.id
                    , revision         = initialRevisionNumber
                    , parentRevisionId = Nothing
                    , title            = titleV1
                    , body             = DocumentBody "line1\nline2-v1\n"
                    , format           = FormatMarkdown
                    , diff             = generateUnifiedDiff "" "line1\nline2-v1\n"
                    , contentHash      = hash1
                    , createdAt        = sampleUtcTime
                    }

                detail2 :: RevisionDetail
                detail2 = RevisionDetail
                    { id               = revId2
                    , documentId       = docV2.id
                    , revision         = rev2
                    , parentRevisionId = Just revId1
                    , title            = docV2.title
                    , body             = docV2.body
                    , format           = FormatMarkdown
                    , diff             = generateUnifiedDiff "line1\nline2-v1\n" "line1\nline2-v2\n"
                    , contentHash      = hash2
                    , createdAt        = sampleLaterUtcTime
                    }

                detail3 :: RevisionDetail
                detail3 = RevisionDetail
                    { id               = revId3
                    , documentId       = docV2.id
                    , revision         = rev3
                    , parentRevisionId = Just revId2
                    , title            = titleV1
                    , body             = DocumentBody "line1\nline2-v1\n"
                    , format           = FormatMarkdown
                    , diff             = generateUnifiedDiff "line1\nline2-v2\n" "line1\nline2-v1\n"
                    , contentHash      = hash3
                    , createdAt        = sampleLaterUtcTime
                    }

            restoredFlagRef <- newIORef False
            let handler :: HttpRequest -> IO (Either Text HttpResponse)
                handler req
                    | req.method == MethodGet && req.url == "/api/documents/doc-mvu-rev/revisions" = do
                        wasRestored <- readIORef restoredFlagRef
                        let summaries :: [RevisionSummary]
                            summaries =
                                if wasRestored
                                    then [toRevisionSummary detail3, toRevisionSummary detail2, toRevisionSummary detail1]
                                    else [toRevisionSummary detail2, toRevisionSummary detail1]
                            res :: RevisionListResponse
                            res = RevisionListResponse
                                { revisions = summaries
                                , total     = length summaries
                                }
                        pure (Right (HttpResponse 200 (encodeStrict res)))
                    | req.method == MethodGet && req.url == "/api/documents/doc-mvu-rev/revisions/1" =
                        pure (Right (HttpResponse 200 (encodeStrict detail1)))
                    | req.method == MethodGet && req.url == "/api/documents/doc-mvu-rev/revisions/2" =
                        pure (Right (HttpResponse 200 (encodeStrict detail2)))
                    | req.method == MethodGet && req.url == "/api/documents/doc-mvu-rev/revisions/3" =
                        pure (Right (HttpResponse 200 (encodeStrict detail3)))
                    | req.method == MethodPost && req.url == "/api/documents/doc-mvu-rev/revisions/1/restore" = do
                        writeIORef restoredFlagRef True
                        pure (Right (HttpResponse 200 (encodeStrict docV3)))
                    | req.method == MethodPut && req.url == "/api/documents/doc-mvu-rev" =
                        pure (Right (HttpResponse 200 (encodeStrict docV3)))
                    | req.method == MethodGet && req.url == "/api/tags" =
                        pure (Right (HttpResponse 200 (encodeStrict (TagListResponse []))))
                    | req.method == MethodGet && req.url == "/api/folders" =
                        pure (Right (HttpResponse 200 (encodeStrict (FolderListResponse []))))
                    | otherwise =
                        pure (Left ("unexpected request: " <> showMethodUrl req))

                showMethodUrl :: HttpRequest -> Text
                showMethodUrl r = toText r.method <> " " <> r.url

            (env, reqRef, cmdRef, _, deletedDraftsRef, _) <-
                mkFullMockClientEnv handler (pure (Right [])) (pure FilePickCancelled)

            let s0 :: ClientState
                s0 = baseState
                    { documentSummaries = [toDocumentSummary docV2]
                    , totalDocuments    = 1
                    , selectedDocument  = Just docV2
                    , draft             = draftFromDocument docV2
                    , mobilePane        = PaneEditor
                    }

            -- 1. Open history panel -> loads list and auto-fetches latest revision detail (r2)
            (s1, _) <- runActionQueue env [ToggleRevisionHistory] s0
            s1.revisionHistory.isHistoryOpen `shouldBe` True
            s1.revisionHistory.totalRevisions `shouldBe` 2
            s1.revisionHistory.selectedRevision `shouldBe` Just rev2
            s1.revisionHistory.revisionDetail `shouldBe` Just detail2

            -- 2. Requesting restore on current revision (r2) is a no-op
            (s1Noop, _) <- runActionQueue env [RequestRestoreRevision rev2] s1
            s1Noop.revisionHistory.confirmRestoreRev `shouldBe` Nothing

            -- 3. Select older revision (r1) -> fetches detail1 and opens restore confirmation modal
            (s2, _) <- runActionQueue env
                [ SelectRevision initialRevisionNumber
                , SetRevisionDetailTab RevTabDiff
                , RequestRestoreRevision initialRevisionNumber
                ]
                s1
            s2.revisionHistory.selectedRevision `shouldBe` Just initialRevisionNumber
            s2.revisionHistory.revisionDetail `shouldBe` Just detail1
            s2.revisionHistory.activeTab `shouldBe` RevTabDiff
            s2.revisionHistory.confirmRestoreRev `shouldBe` Just initialRevisionNumber

            -- Verify view renders history panel, diff lines, and confirmation modal
            let htmlWithModal :: Text
                htmlWithModal =
                    TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (toHtmlWith () () s2 (viewClient jaMessages s2)))
            htmlWithModal `shouldSatisfy` Txt.isInfixOf jaMessages.historyHeading
            htmlWithModal `shouldSatisfy` Txt.isInfixOf jaMessages.historyCurrentBadge
            htmlWithModal `shouldSatisfy` Txt.isInfixOf "tt-diff-add"
            htmlWithModal `shouldSatisfy` Txt.isInfixOf jaMessages.confirmRestoreRevTitle
            htmlWithModal `shouldSatisfy` Txt.isInfixOf "r2"

            -- 4. Confirm restore -> calls POST restore, updates doc to r3, resets editor, reloads list (3 items)
            (s3, _) <- runActionQueue env [ConfirmRestoreRevision initialRevisionNumber] s2
            fmap (.revision) s3.selectedDocument `shouldBe` Just rev3
            s3.draft.body `shouldBe` DocumentBody "line1\nline2-v1\n"
            s3.draft.titleInput `shouldBe` "v1 タイトル"
            s3.revisionHistory.totalRevisions `shouldBe` 3
            s3.revisionHistory.selectedRevision `shouldBe` Just rev3
            s3.revisionHistory.revisionDetail `shouldBe` Just detail3
            s3.statusNotice `shouldBe` Just jaMessages.noticeRevisionRestored

            deletedIds <- readIORef deletedDraftsRef
            deletedIds `shouldSatisfy` elem docV2.id

            let isResetSyncCmd :: EditorCommand -> Bool
                isResetSyncCmd (CmdSyncDocument payload) =
                    payload.resetHistory && payload.body == "line1\nline2-v1\n"
                isResetSyncCmd _ = False
            cmds <- readIORef cmdRef
            cmds `shouldSatisfy` any isResetSyncCmd

            reqs <- readIORef reqRef
            reqs `shouldSatisfy` any (\ r ->
                r.method == MethodPost
                    && r.url == "/api/documents/doc-mvu-rev/revisions/1/restore"
                    && maybe False (Txt.isInfixOf "\"base_revision\":2" . TxtEnc.decodeUtf8Lenient) r.body)

            -- 5. CancelRestoreRevision and RefreshRevisionHistory work cleanly, and SaveCurrentDocument auto-refreshes open history
            (s3Cancel, _) <- runActionQueue env
                [ RequestRestoreRevision initialRevisionNumber
                , CancelRestoreRevision
                , RefreshRevisionHistory
                ]
                s3
            s3Cancel.revisionHistory.confirmRestoreRev `shouldBe` Nothing

            writeIORef reqRef []
            (sSaved, _) <- runActionQueue env
                [ UpdateDraftTitle "v3 保存タイトル"
                , SaveCurrentDocument
                ]
                s3Cancel
            reqsAfterSave <- readIORef reqRef
            reqsAfterSave `shouldSatisfy` any (\ r ->
                r.method == MethodGet && r.url == "/api/documents/doc-mvu-rev/revisions")

            -- 6. Close history panel
            (s4, _) <- runActionQueue env [ToggleRevisionHistory] sSaved
            s4.revisionHistory.isHistoryOpen `shouldBe` False

        it "handles pagination (LoadMoreRevisions), 409 conflict, 401 expiry, and in-flight restore doc-switch race safely" $ do
            baseState <- mkAuthenticatedState
            doc1 <- mkSampleDocument "doc-page" "ページ文書"
            doc2 <- mkSampleDocument "doc-other" "別文書"
            rev2 <- expectRight (mkRevisionNumber 2)
            rev3 <- expectRight (mkRevisionNumber 3)
            revId1 <- expectRight (mkRevisionId "rev-pg1")
            revId2 <- expectRight (mkRevisionId "rev-pg2")
            hash1  <- expectRight (mkContentHash (Txt.replicate 64 "e"))
            hash2  <- expectRight (mkContentHash (Txt.replicate 64 "f"))
            let doc1V2 :: Document
                doc1V2 = withDocRevision rev2 doc1

                detail1 :: RevisionDetail
                detail1 = RevisionDetail
                    { id               = revId1
                    , documentId       = doc1.id
                    , revision         = initialRevisionNumber
                    , parentRevisionId = Nothing
                    , title            = doc1.title
                    , body             = DocumentBody "v1-no-nl"
                    , format           = FormatMarkdown
                    , diff             = generateUnifiedDiff "" "v1-no-nl"
                    , contentHash      = hash1
                    , createdAt        = sampleUtcTime
                    }

                detail2 :: RevisionDetail
                detail2 = RevisionDetail
                    { id               = revId2
                    , documentId       = doc1.id
                    , revision         = rev2
                    , parentRevisionId = Just revId1
                    , title            = doc1.title
                    , body             = DocumentBody "lineA\nv2"
                    , format           = FormatMarkdown
                    , diff             = generateUnifiedDiff "lineA\nv1" "lineA\nv2"
                    , contentHash      = hash2
                    , createdAt        = sampleLaterUtcTime
                    }

                page1 :: RevisionListResponse
                page1 = RevisionListResponse { revisions = [toRevisionSummary detail2], total = 2 }

                page2 :: RevisionListResponse
                page2 = RevisionListResponse { revisions = [toRevisionSummary detail1], total = 2 }

                conflictErr :: ApiError
                conflictErr = ApiError ErrConflict "base_revision mismatch" Nothing

                unauthErr :: ApiError
                unauthErr = ApiError ErrUnauthorized "session expired" Nothing

            let pageHandler :: HttpRequest -> IO (Either Text HttpResponse)
                pageHandler req
                    | req.method == MethodGet && req.url == "/api/documents/doc-page/revisions" =
                        pure (Right (HttpResponse 200 (encodeStrict page1)))
                    | req.method == MethodGet && req.url == "/api/documents/doc-page/revisions?offset=1" =
                        pure (Right (HttpResponse 200 (encodeStrict page2)))
                    | req.method == MethodGet && req.url == "/api/documents/doc-page/revisions/2" =
                        pure (Right (HttpResponse 200 (encodeStrict detail2)))
                    | req.method == MethodPost && req.url == "/api/documents/doc-page/revisions/1/restore" =
                        pure (Right (HttpResponse 409 (encodeStrict conflictErr)))
                    | otherwise =
                        pure (Right (HttpResponse 401 (encodeStrict unauthErr)))

            (env, _, cmdRef, _, _, _) <- mkFullMockClientEnv pageHandler (pure (Right [])) (pure FilePickCancelled)

            let s0 :: ClientState
                s0 = baseState
                    { selectedDocument = Just doc1V2
                    , draft            = draftFromDocument doc1V2
                    }

            -- Page 1 loads 1/2 revisions and renders load-more button + diff lines (ctx/del/add)
            (s1, _) <- runActionQueue env [ToggleRevisionHistory, SetRevisionDetailTab RevTabDiff] s0
            length s1.revisionHistory.revisions `shouldBe` 1
            let htmlPage1 :: Text
                htmlPage1 = TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (toHtmlWith () () s1 (viewClient jaMessages s1)))
            htmlPage1 `shouldSatisfy` Txt.isInfixOf "1 / 2件"
            htmlPage1 `shouldSatisfy` Txt.isInfixOf jaMessages.actionLoadMoreHistory
            htmlPage1 `shouldSatisfy` Txt.isInfixOf "tt-diff-ctx"
            htmlPage1 `shouldSatisfy` Txt.isInfixOf "tt-diff-del"
            htmlPage1 `shouldSatisfy` Txt.isInfixOf "tt-diff-add"
            htmlPage1 `shouldSatisfy` Txt.isInfixOf "\\ No newline at end of file"

            -- LoadMoreRevisions appends page 2
            (s2, _) <- runActionQueue env [LoadMoreRevisions] s1
            length s2.revisionHistory.revisions `shouldBe` 2
            s2.revisionHistory.totalRevisions `shouldBe` 2

            -- 409 Conflict clears restoringRevisionDoc and reports conflict error
            (sConflict, _) <- runActionQueue env [ConfirmRestoreRevision initialRevisionNumber] s2
            sConflict.restoringRevisionDoc `shouldBe` Nothing
            sConflict.lastError `shouldBe` Just jaMessages.errConflictMessage

            -- In-flight restore arriving after switching to doc2 must NOT reset doc2's editor history
            writeIORef cmdRef []
            let sSwitched :: ClientState
                sSwitched = baseState
                    { selectedDocument     = Just doc2
                    , draft                = draftFromDocument doc2
                    , restoringRevisionDoc = Just doc1.id
                    }
                doc1V3 :: Document
                doc1V3 = withDocRevision rev3 doc1
            (sAfterLateRestore, _) <- runUpdateStep env (RevisionRestored baseState.authSessionSeq doc1.id (Right doc1V3)) sSwitched
            sAfterLateRestore.restoringRevisionDoc `shouldBe` Nothing
            fmap (.id) sAfterLateRestore.selectedDocument `shouldBe` Just doc2.id
            cmdsAfterLate <- readIORef cmdRef
            cmdsAfterLate `shouldBe` []

            -- 401 Unauthorized on revision detail expires session
            (s401, _) <- runActionQueue env [SelectRevision initialRevisionNumber] s2
            currentUserId s401 `shouldBe` Nothing
            s401.lastError `shouldBe` Just jaMessages.errSessionExpired

            -- Empty diff and malformed diff fallback rendering
            let detailEmptyDiff :: RevisionDetail
                detailEmptyDiff = detail2 { diff = emptyUnifiedDiff }

                detailBadDiff :: RevisionDetail
                detailBadDiff = detail2 { diff = UnifiedDiff "not-a-valid-unified-diff" }

                stEmptyDiff :: ClientState
                stEmptyDiff = s1 { revisionHistory = s1.revisionHistory { revisionDetail = Just detailEmptyDiff, activeTab = RevTabDiff } }

                stBadDiff :: ClientState
                stBadDiff = s1 { revisionHistory = s1.revisionHistory { revisionDetail = Just detailBadDiff, activeTab = RevTabDiff } }

                htmlEmptyDiff :: Text
                htmlEmptyDiff = TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (toHtmlWith () () stEmptyDiff (viewClient jaMessages stEmptyDiff)))

                htmlBadDiff :: Text
                htmlBadDiff = TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (toHtmlWith () () stBadDiff (viewClient jaMessages stBadDiff)))
            htmlEmptyDiff `shouldSatisfy` Txt.isInfixOf jaMessages.historyDiffEmpty
            htmlBadDiff `shouldSatisfy` Txt.isInfixOf "tt-diff-raw"

        it "shows read-only restore hint for trashed documents in revision history view" $ do
            baseState <- mkAuthenticatedState
            doc <- mkSampleDocument "doc-rev-trash" "ゴミ箱文書"
            rev2 <- expectRight (mkRevisionNumber 2)
            revId1 <- expectRight (mkRevisionId "rev-t1")
            hash1  <- expectRight (mkContentHash (Txt.replicate 64 "d"))
            let trashedDoc :: Document
                trashedDoc = withDocRevision rev2 (withDocDeletedAt (Just sampleUtcTime) doc)

                detail1 :: RevisionDetail
                detail1 = RevisionDetail
                    { id               = revId1
                    , documentId       = doc.id
                    , revision         = initialRevisionNumber
                    , parentRevisionId = Nothing
                    , title            = doc.title
                    , body             = doc.body
                    , format           = FormatMarkdown
                    , diff             = generateUnifiedDiff "" (toText doc.body)
                    , contentHash      = hash1
                    , createdAt        = sampleUtcTime
                    }

                st :: ClientState
                st = baseState
                    { selectedDocument = Just trashedDoc
                    , draft            = draftFromDocument trashedDoc
                    , revisionHistory  = emptyRevisionHistoryState
                        { isHistoryOpen    = True
                        , documentId       = Just doc.id
                        , revisions        = [toRevisionSummary detail1]
                        , totalRevisions   = 1
                        , selectedRevision = Just initialRevisionNumber
                        , revisionDetail   = Just detail1
                        }
                    }

                htmlText :: Text
                htmlText =
                    TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (toHtmlWith () () st (viewClient jaMessages st)))
            htmlText `shouldSatisfy` Txt.isInfixOf jaMessages.historyTrashedRestoreHint
            htmlText `shouldSatisfy` (not . Txt.isInfixOf jaMessages.actionRestoreRevision)

recordingTransport ::
       IORef (Maybe HttpRequest)
    -> Either Text HttpResponse
    -> HttpTransport
recordingTransport ref fixedResult = HttpTransport $ \ req -> do
    writeIORef ref (Just req)
    pure fixedResult

encodeStrict :: Aeson.ToJSON a => a -> ByteString
encodeStrict = LazyBS.toStrict . Aeson.encode

sampleUtcTime :: UTCTime
sampleUtcTime = UTCTime (fromGregorian 2026 11 20) (secondsToDiffTime (9 * 3600 + 30 * 60))

sampleLaterUtcTime :: UTCTime
sampleLaterUtcTime = UTCTime (fromGregorian 2026 11 20) (secondsToDiffTime (9 * 3600 + 35 * 60))

mkSampleDocument :: Text -> Text -> IO Document
mkSampleDocument rawId rawTitle = do
    docId    <- expectRight (mkDocumentId rawId)
    docTitle <- expectRight (mkDocumentTitle rawTitle)
    pure Document
        { id        = docId
        , title     = docTitle
        , body      = DocumentBody "# 見出し\n本文"
        , format    = FormatMarkdown
        , folder    = Nothing
        , tags      = emptyTagSet
        , revision  = initialRevisionNumber
        , createdAt = sampleUtcTime
        , updatedAt = sampleUtcTime
        , deletedAt = Nothing
        }

expectRight :: Show e => Either e a -> IO a
expectRight (Right val) = pure val
expectRight (Left err)  = do
    expectationFailure ("Expected Right, got Left: " <> show err)
    fail ("Expected Right, got Left: " <> show err)

isLeftEither :: Either a b -> Bool
isLeftEither (Left _)  = True
isLeftEither (Right _) = False

withDocBody :: DocumentBody -> Document -> Document
withDocBody nextBody doc = Document
    { id        = doc.id
    , title     = doc.title
    , body      = nextBody
    , format    = doc.format
    , folder    = doc.folder
    , tags      = doc.tags
    , revision  = doc.revision
    , createdAt = doc.createdAt
    , updatedAt = doc.updatedAt
    , deletedAt = doc.deletedAt
    }

withDocDeletedAt :: Maybe UTCTime -> Document -> Document
withDocDeletedAt mDel doc = Document
    { id        = doc.id
    , title     = doc.title
    , body      = doc.body
    , format    = doc.format
    , folder    = doc.folder
    , tags      = doc.tags
    , revision  = doc.revision
    , createdAt = doc.createdAt
    , updatedAt = doc.updatedAt
    , deletedAt = mDel
    }

withDocUpdatedAt :: UTCTime -> Document -> Document
withDocUpdatedAt ts doc = Document
    { id        = doc.id
    , title     = doc.title
    , body      = doc.body
    , format    = doc.format
    , folder    = doc.folder
    , tags      = doc.tags
    , revision  = doc.revision
    , createdAt = doc.createdAt
    , updatedAt = ts
    , deletedAt = doc.deletedAt
    }

withDocRevision :: RevisionNumber -> Document -> Document
withDocRevision rev doc = Document
    { id        = doc.id
    , title     = doc.title
    , body      = doc.body
    , format    = doc.format
    , folder    = doc.folder
    , tags      = doc.tags
    , revision  = rev
    , createdAt = doc.createdAt
    , updatedAt = doc.updatedAt
    , deletedAt = doc.deletedAt
    }

expectPureTagSet :: Tag -> TagSet
expectPureTagSet t = case mkTagSet [t] of
    Right ts -> ts
    Left _   -> emptyTagSet

withDraftTitle :: Text -> EditorDraft -> EditorDraft
withDraftTitle nextTitle d = EditorDraft
    { titleInput  = nextTitle
    , body        = d.body
    , format      = d.format
    , folderInput = d.folderInput
    , tagsInput   = d.tagsInput
    }

withDraftBody :: DocumentBody -> EditorDraft -> EditorDraft
withDraftBody nextBody d = EditorDraft
    { titleInput  = d.titleInput
    , body        = nextBody
    , format      = d.format
    , folderInput = d.folderInput
    , tagsInput   = d.tagsInput
    }

withDraftFormat :: DocumentFormat -> EditorDraft -> EditorDraft
withDraftFormat nextFmt d = EditorDraft
    { titleInput  = d.titleInput
    , body        = d.body
    , format      = nextFmt
    , folderInput = d.folderInput
    , tagsInput   = d.tagsInput
    }

withDraftFolder :: Text -> EditorDraft -> EditorDraft
withDraftFolder nextFolder d = EditorDraft
    { titleInput  = d.titleInput
    , body        = d.body
    , format      = d.format
    , folderInput = nextFolder
    , tagsInput   = d.tagsInput
    }

withDraftTags :: Text -> EditorDraft -> EditorDraft
withDraftTags nextTags d = EditorDraft
    { titleInput  = d.titleInput
    , body        = d.body
    , format      = d.format
    , folderInput = d.folderInput
    , tagsInput   = nextTags
    }

withFilterStatus :: DocumentStatusFilter -> FilterState -> FilterState
withFilterStatus nextStatus flt = FilterState
    { status            = nextStatus
    , searchInput       = flt.searchInput
    , selectedTag       = flt.selectedTag
    , selectedFolder    = flt.selectedFolder
    , includeSubfolders = flt.includeSubfolders
    }

mkTestSearchPanel :: Text -> Int -> Int -> Maybe Text -> SearchPanelState
mkTestSearchPanel q cnt act err = SearchPanelState
    { isOpen        = True
    , showReplace   = False
    , query         = q
    , replacement   = ""
    , caseSensitive = False
    , useRegex      = False
    , matchCount    = cnt
    , activeMatch   = act
    , regexError    = err
    }

mkMockClientEnv ::
       (HttpRequest -> IO (Either Text HttpResponse))
    -> IO (ClientEnv, IORef [HttpRequest], IORef [EditorCommand])
mkMockClientEnv handler = do
    (env, reqRef, cmdRef, _, _, _) <- mkFullMockClientEnv handler (pure (Right [])) (pure FilePickCancelled)
    pure (env, reqRef, cmdRef)

mkFullMockClientEnv ::
       (HttpRequest -> IO (Either Text HttpResponse))
    -> IO (Either Text [StoredDraft])
    -> IO FilePickResult
    -> IO
        ( ClientEnv
        , IORef [HttpRequest]
        , IORef [EditorCommand]
        , IORef [StoredDraft]
        , IORef [DocumentId]
        , IORef [ExportSpec]
        )
mkFullMockClientEnv handler listDraftsIo pickFileIo = do
    reqRef <- newIORef []
    cmdRef <- newIORef []
    savedDraftsRef <- newIORef []
    deletedIdsRef <- newIORef []
    exportsRef <- newIORef []
    let transport :: HttpTransport
        transport = HttpTransport $ \ req -> do
            modifyIORef' reqRef (\ xs -> xs <> [req])
            handler req

        client :: ApiClient
        client = ApiClient
            { baseUrl   = defaultApiBaseUrl
            , transport = transport
            }

        storage :: DraftStorage
        storage = DraftStorage
            { scheduleSaveDraft = \ sd -> modifyIORef' savedDraftsRef (\ xs -> xs <> [sd])
            , deleteStoredDraft = \ did -> modifyIORef' deletedIdsRef (\ xs -> xs <> [did])
            , listStoredDrafts  = listDraftsIo
            }

        transfer :: FileTransferClient
        transfer = FileTransferClient
            { exportFile = \ exportSpec -> do
                modifyIORef' exportsRef (\ xs -> xs <> [exportSpec])
                pure (Right ())
            , pickImportFile = pickFileIo
            }

        env :: ClientEnv
        env = ClientEnv
            { apiClient         = client
            , extensions        = bundledExtensions
            , messages          = jaMessages
            , sendEditorCmd     = \ cmd -> modifyIORef' cmdRef (\ xs -> xs <> [cmd])
            , draftStorage      = storage
            , fileTransfer      = transfer
            , getCurrentUtcTime = pure sampleLaterUtcTime
            }
    pure (env, reqRef, cmdRef, savedDraftsRef, deletedIdsRef, exportsRef)

runUpdateStep ::
       ClientEnv
    -> ClientAction
    -> ClientState
    -> IO (ClientState, [ClientAction])
runUpdateStep env action state = do
    let info = mkComponentInfo 0 0 jsNull () ()
        (nextState, schedules) = runEffect (updateClient env action) info state
    emittedRef <- newIORef []
    let sink :: ClientAction -> IO ()
        sink act = modifyIORef' emittedRef (\ xs -> xs <> [act])
    mapM_ (runSchedule sink) schedules
    emitted <- readIORef emittedRef
    pure (nextState, emitted)
  where
    runSchedule :: (ClientAction -> IO ()) -> Schedule () ClientAction -> IO ()
    runSchedule sink (Schedule _ actionIo) = actionIo sink
    runSchedule _    _                     = pure ()

runActionQueue ::
       ClientEnv
    -> [ClientAction]
    -> ClientState
    -> IO (ClientState, [ClientAction])
runActionQueue _ [] state = pure (state, [])
runActionQueue env (act : rest) state = do
    (nextState, emitted) <- runUpdateStep env act state
    runActionQueue env (rest <> emitted) nextState

mkSampleUserId :: Text -> IO UserId
mkSampleUserId = expectRight . mkUserId

mkSampleCsrfToken :: IO CsrfToken
mkSampleCsrfToken = expectRight (mkCsrfToken "47DEQpj8HBSa-_TImW-5JCeuQeRkm5NMpJWZG3hSuFU")

mkSampleAuthSession :: Text -> Text -> Maybe Text -> IO AuthSessionResponse
mkSampleAuthSession rawUid rawUsername mRawDisplay = do
    uid   <- mkSampleUserId rawUid
    uname <- expectRight (mkUsername rawUsername)
    mDisp <- traverse (expectRight . mkDisplayName) mRawDisplay
    csrf  <- mkSampleCsrfToken
    pure AuthSessionResponse
        { user = UserProfile
            { id          = uid
            , username    = uname
            , displayName = mDisp
            , createdAt   = sampleUtcTime
            }
        , csrfToken = csrf
        }

mkAuthenticatedState :: IO ClientState
mkAuthenticatedState = do
    session <- mkSampleAuthSession "user-alice" "alice" (Just "アリス")
    pure (mkAuthenticatedStateFor session)

mkAuthenticatedStateFor :: AuthSessionResponse -> ClientState
mkAuthenticatedStateFor session =
    (initialClientState bundledExtensions)
        { connectionStatus = ConnOnline
        , authState        = AuthAuthenticated session
        , authSessionSeq   = 1
        }
