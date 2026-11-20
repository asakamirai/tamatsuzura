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
    , buildGetDocumentRequest
    , buildHealthRequest
    , buildListDocumentsRequest
    , buildListFoldersRequest
    , buildListTagsRequest
    , buildPermanentDeleteDocumentRequest
    , buildRestoreDocumentRequest
    , buildSoftDeleteDocumentRequest
    , buildUpdateDocumentRequest
    , createDocument
    , defaultApiBaseUrl
    , fetchHealth
    , getDocument
    , listDocuments
    , listFolders
    , listTags
    , mkApiBaseUrl
    , parseJsonResponse
    , parseNoContentResponse
    , percentEncodeQueryComponent
    , permanentDeleteDocument
    , restoreDocument
    , softDeleteDocument
    , updateDocument
    )
import           TamaTsuzura.Client.App
    ( ClientAction(..)
    , ClientEnv(..)
    , ClientState(..)
    , ConnectionStatus(..)
    , EditorDraft(..)
    , EditorViewMode(..)
    , FilterState(..)
    , MobilePane(..)
    , PendingNavigation(..)
    , SearchPanelState(..)
    , applyDocumentCreated
    , applyDocumentLoaded
    , applyDocumentPermanentlyDeleted
    , applyDocumentRestored
    , applyDocumentSaved
    , applyDocumentSoftDeleted
    , applyDocumentsResult
    , applyEditorEvent
    , applyExtensionById
    , applyFoldersResult
    , applyHealthResult
    , applyTagsResult
    , buildEditorSyncPayload
    , buildListQueryFromFilter
    , defaultFilterState
    , dismissError
    , dismissNotice
    , draftFromDocument
    , emptyEditorDraft
    , emptySearchPanelState
    , formatTagsInput
    , hasActiveFilters
    , initialClientState
    , isCurrentDocumentDirty
    , isDraftDirty
    , mergeDraftAfterSave
    , parseFolderInput
    , parseTagsInput
    , updateClient
    , validateEditorDraft
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
import           TamaTsuzura.Client.I18n
    ( UiMessages(..)
    , formatApiClientError
    , formatDocumentFormat
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
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody(..)
    , DocumentFormat(..)
    , DocumentSummary(..)
    , emptyDocumentTitle
    , emptyTagSet
    , mkDocumentId
    , mkDocumentTitle
    , mkFolderPath
    , mkTag
    , toDocumentSummary
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

        it "builds accurate HTTP requests for all 10 M1 endpoints" $ do
            docId <- expectRight (mkDocumentId "doc-123")
            let base :: ApiBaseUrl
                base = mkApiBaseUrl "/app/"

                healthReq :: HttpRequest
                healthReq = buildHealthRequest base

                listReq :: HttpRequest
                listReq = buildListDocumentsRequest base defaultDocumentListQuery

                getReq :: HttpRequest
                getReq = buildGetDocumentRequest base docId

                softDelReq :: HttpRequest
                softDelReq = buildSoftDeleteDocumentRequest base docId

                restoreReq :: HttpRequest
                restoreReq = buildRestoreDocumentRequest base docId

                permDelReq :: HttpRequest
                permDelReq = buildPermanentDeleteDocumentRequest base docId

                tagsReq :: HttpRequest
                tagsReq = buildListTagsRequest base

                foldersReq :: HttpRequest
                foldersReq = buildListFoldersRequest base

            (healthReq.method, healthReq.url) `shouldBe` (MethodGet, "/app/api/health")
            (listReq.method, listReq.url) `shouldBe` (MethodGet, "/app/api/documents")
            (getReq.method, getReq.url) `shouldBe` (MethodGet, "/app/api/documents/doc-123")
            (softDelReq.method, softDelReq.url) `shouldBe` (MethodDelete, "/app/api/documents/doc-123")
            (restoreReq.method, restoreReq.url) `shouldBe` (MethodPost, "/app/api/documents/doc-123/restore")
            (permDelReq.method, permDelReq.url) `shouldBe` (MethodDelete, "/app/api/documents/doc-123/permanent")
            (tagsReq.method, tagsReq.url) `shouldBe` (MethodGet, "/app/api/tags")
            (foldersReq.method, foldersReq.url) `shouldBe` (MethodGet, "/app/api/folders")

            createReq <- expectRight (buildCreateDocumentRequest base defaultCreateDocumentRequest)
            createReq.method `shouldBe` MethodPost
            createReq.url `shouldBe` "/app/api/documents"
            createReq.body `shouldSatisfy` (/= Nothing)

            updateReq <- expectRight $ buildUpdateDocumentRequest base docId UpdateDocumentRequest
                { title  = emptyDocumentTitle
                , body   = DocumentBody "updated"
                , format = FormatPlainText
                , folder = Nothing
                , tags   = emptyTagSet
                }
            updateReq.method `shouldBe` MethodPut
            updateReq.url `shouldBe` "/app/api/documents/doc-123"

        it "rejects oversized document body before hitting transport" $ do
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
            res <- createDocument client req
            case res of
                Left (ClientValidationError BodyTooLarge{}) -> pure ()
                other -> expectationFailure ("Expected ClientValidationError BodyTooLarge, got: " <> show other)

            docId <- expectRight (mkDocumentId "doc-huge")
            let updReq :: UpdateDocumentRequest
                updReq = UpdateDocumentRequest
                    { title  = emptyDocumentTitle
                    , body   = hugeBody
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = emptyTagSet
                    }
            updRes <- updateDocument client docId updReq
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

        it "executes all 10 high-level API operations against injected HttpTransport" $ do
            sampleDoc <- mkSampleDocument "doc-1" "執筆メモ"
            sampleTag <- expectRight (mkTag "メモ")
            sampleFolder <- expectRight (mkFolderPath "diary/2026")
            let sampleSummary :: DocumentSummary
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

            eList <- listDocuments (mkClient (HttpResponse 200 (encodeStrict sampleList))) defaultDocumentListQuery
            eList `shouldBe` Right sampleList

            eCreated <- createDocument (mkClient (HttpResponse 201 (encodeStrict sampleDoc))) defaultCreateDocumentRequest
            eCreated `shouldBe` Right sampleDoc

            eGot <- getDocument (mkClient (HttpResponse 200 (encodeStrict sampleDoc))) sampleDoc.id
            eGot `shouldBe` Right sampleDoc

            let updReq :: UpdateDocumentRequest
                updReq = UpdateDocumentRequest
                    { title  = sampleDoc.title
                    , body   = DocumentBody "更新本文"
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = emptyTagSet
                    }
            eUpdated <- updateDocument (mkClient (HttpResponse 200 (encodeStrict sampleDoc))) sampleDoc.id updReq
            eUpdated `shouldBe` Right sampleDoc

            eSoftDel <- softDeleteDocument (mkClient (HttpResponse 200 (encodeStrict sampleDoc))) sampleDoc.id
            eSoftDel `shouldBe` Right sampleDoc

            eRestored <- restoreDocument (mkClient (HttpResponse 200 (encodeStrict sampleDoc))) sampleDoc.id
            eRestored `shouldBe` Right sampleDoc

            ePermDel <- permanentDeleteDocument (mkClient (HttpResponse 204 "")) sampleDoc.id
            ePermDel `shouldBe` Right ()

            eTags <- listTags (mkClient (HttpResponse 200 (encodeStrict sampleTags)))
            eTags `shouldBe` Right sampleTags

            eFolders <- listFolders (mkClient (HttpResponse 200 (encodeStrict sampleFolders)))
            eFolders `shouldBe` Right sampleFolders

            mLastReq <- readIORef lastReqRef
            fmap (\ r -> r.url) mLastReq `shouldBe` Just "/api/folders"

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
                invalidTitleDraft = validDraft { titleInput = "bad\ntitle" }
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
            isDraftDirty doc (cleanDraft { titleInput = "  元のタイトル  " }) `shouldBe` False
            isDraftDirty doc (withDraftBody (DocumentBody "変更後") cleanDraft) `shouldBe` True
            isDraftDirty doc (withDraftFormat FormatPlainText cleanDraft) `shouldBe` True
            isDraftDirty doc (cleanDraft { folderInput = "work/sub" }) `shouldBe` True
            isDraftDirty doc (cleanDraft { folderInput = "../invalid" }) `shouldBe` True
            isDraftDirty doc (cleanDraft { tagsInput = "新規タグ" }) `shouldBe` True
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
                sentDraft = (draftFromDocument doc) { titleInput = "  送信タイトル  " }

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
            (env, reqRef, cmdRef) <- mkMockClientEnv (\ _ -> pure (Right (HttpResponse 200 (encodeStrict (DocumentListResponse [] 0)))))
            let dirtyState :: ClientState
                dirtyState = (initialClientState bundledExtensions)
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
                badDraftState = (initialClientState bundledExtensions)
                    { selectedDocument = Just doc
                    , draft            = (draftFromDocument doc) { titleInput = "bad\ntitle" }
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
                trashState = (initialClientState bundledExtensions)
                    { filterState = withFilterStatus StatusTrashed defaultFilterState
                    }
            writeIORef reqRef []
            (sCreating, createActs) <- runUpdateStep env RequestCreateDocument trashState
            sCreating.pendingListReloadAfterCreate `shouldBe` True
            sCreating.filterState.status `shouldBe` StatusActive
            reqsBeforeCreated <- readIORef reqRef
            map (.method) reqsBeforeCreated `shouldBe` [MethodPost]

            writeIORef reqRef []
            writeIORef cmdRef []
            (sCreated, _) <- case createActs of
                (act : _) -> runUpdateStep env act sCreating
                []        -> expectationFailure "Expected DocumentCreated action" >> pure (sCreating, [])
            sCreated.pendingListReloadAfterCreate `shouldBe` False
            reqsAfterCreated <- readIORef reqRef
            any (\ r -> r.method == MethodGet && r.url == "/api/documents") reqsAfterCreated `shouldBe` True
            cmdsAfterCreated <- readIORef cmdRef
            cmdsAfterCreated `shouldSatisfy` elem CmdFocus

        it "dispatches search, format normalization, extension sync, and clears openingDocumentId on same-doc select" $ do
            doc <- mkSampleDocument "doc-mvu-ops" "操作テスト"
            otherId <- expectRight (mkDocumentId "doc-other")
            (env, _reqRef, cmdRef) <- mkMockClientEnv (\ _ -> pure (Right (HttpResponse 200 "{}")))
            let base :: ClientState
                base = (initialClientState bundledExtensions)
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
            let onlineState :: ClientState
                onlineState =
                    (applyDocumentCreated jaMessages (Right doc)
                        (applyHealthResult jaMessages (Right healthOk) (initialClientState bundledExtensions)))
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
    , createdAt = doc.createdAt
    , updatedAt = doc.updatedAt
    , deletedAt = mDel
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
    reqRef <- newIORef []
    cmdRef <- newIORef []
    let transport :: HttpTransport
        transport = HttpTransport $ \ req -> do
            modifyIORef' reqRef (\ xs -> xs <> [req])
            handler req

        client :: ApiClient
        client = ApiClient
            { baseUrl   = defaultApiBaseUrl
            , transport = transport
            }

        env :: ClientEnv
        env = ClientEnv
            { apiClient     = client
            , extensions    = bundledExtensions
            , messages      = jaMessages
            , sendEditorCmd = \ cmd -> modifyIORef' cmdRef (\ xs -> xs <> [cmd])
            }
    pure (env, reqRef, cmdRef)

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
