{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main
    ( main
    ) where

import qualified Data.Aeson                           as Aeson
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy                 as LazyBS
import           Data.IORef                           (IORef, newIORef, readIORef, writeIORef)
import           Data.Text                            (Text)
import qualified Data.Text                            as Txt
import qualified Data.Text.Encoding                   as TxtEnc
import           Data.Time.Calendar                   (fromGregorian)
import           Data.Time.Clock                      (UTCTime(..), secondsToDiffTime)
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
    ( ClientState(..)
    , ConnectionStatus(..)
    , applyDocumentCreated
    , applyDocumentsResult
    , applyExtensionById
    , applyHealthResult
    , dismissError
    , initialClientState
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
    , jaMessages
    )
import           TamaTsuzura.Client.View              (displayTitle, formatUtcTimestamp, viewClient)
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
    , emptyDocumentBody
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
                query = defaultDocumentListQuery { includeSubfolders = True }
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
            state.draftTitle `shouldBe` emptyDocumentTitle
            state.draftBody `shouldBe` emptyDocumentBody
            length state.extensionMetadatas `shouldBe` 1

        it "applies a bundled extension by id and ignores unknown ids" $ do
            let base :: ClientState
                base = (initialClientState bundledExtensions)
                    { draftBody = DocumentBody "line1   \nline2\t\n"
                    }
                updated :: ClientState
                updated = applyExtensionById bundledExtensions (ExtensionId "trim-trailing-spaces") base

                unchanged :: ClientState
                unchanged = applyExtensionById bundledExtensions (ExtensionId "non-existent") base
            updated.draftBody `shouldBe` DocumentBody "line1\nline2\n"
            unchanged.draftBody `shouldBe` base.draftBody

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
                s2 = applyDocumentsResult jaMessages (Right listRes) s1

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
                afterListOk = applyDocumentsResult jaMessages (Right (DocumentListResponse [] 0)) afterHealthErr

                afterDismiss :: ClientState
                afterDismiss = dismissError afterListOk

                afterListErr :: ClientState
                afterListErr = applyDocumentsResult jaMessages (Left (ClientHttpError 503 "Unavailable")) afterDismiss

                afterCreateErr :: ClientState
                afterCreateErr = applyDocumentCreated jaMessages (Left (ClientApiError 409 (ApiError ErrConflict "conflict" Nothing))) afterListErr

            afterListOk.isLoadingDocuments `shouldBe` False
            afterListOk.lastError `shouldBe` afterHealthErr.lastError
            afterDismiss.lastError `shouldBe` Nothing
            afterListErr.isLoadingDocuments `shouldBe` False
            afterListErr.lastError `shouldSatisfy` (/= Nothing)
            afterCreateErr.isCreatingDocument `shouldBe` False
            afterCreateErr.lastError `shouldBe` Just jaMessages.errConflictMessage

    describe "TamaTsuzura.Client.I18n and View helpers" $ do
        it "formats titles, formats, timestamps, and Japanese error messages" $ do
            displayTitle jaMessages emptyDocumentTitle `shouldBe` jaMessages.untitledDocument
            titled <- expectRight (mkDocumentTitle "議事録")
            displayTitle jaMessages titled `shouldBe` "議事録"
            formatDocumentFormat jaMessages FormatMarkdown `shouldBe` "Markdown"
            formatDocumentFormat jaMessages FormatPlainText `shouldBe` "テキスト"
            formatUtcTimestamp sampleUtcTime `shouldBe` "2026-11-20 09:30 UTC"
            formatApiClientError jaMessages (ClientApiError 404 (ApiError ErrNotFound "missing" Nothing))
                `shouldBe` jaMessages.errNotFoundMessage

        it "renders mobile-first Japanese Miso view HTML for online and error states" $ do
            doc <- mkSampleDocument "doc-view-1" "設計メモ"
            let onlineState :: ClientState
                onlineState =
                    applyDocumentCreated jaMessages (Right doc)
                        (applyHealthResult jaMessages (Right healthOk) (initialClientState bundledExtensions))

                onlineHtml :: Text
                onlineHtml = TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (toHtmlWith () () onlineState (viewClient jaMessages onlineState)))

                errState :: ClientState
                errState = applyHealthResult jaMessages (Left (ClientNetworkError "offline")) onlineState

                errHtml :: Text
                errHtml = TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (toHtmlWith () () errState (viewClient jaMessages errState)))

            onlineHtml `shouldSatisfy` Txt.isInfixOf jaMessages.appTitle
            onlineHtml `shouldSatisfy` Txt.isInfixOf jaMessages.statusOnline
            onlineHtml `shouldSatisfy` Txt.isInfixOf "設計メモ"
            onlineHtml `shouldSatisfy` Txt.isInfixOf jaMessages.actionNewDocument
            errHtml `shouldSatisfy` Txt.isInfixOf jaMessages.statusError
            errHtml `shouldSatisfy` Txt.isInfixOf jaMessages.errNetworkPrefix

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
