{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.FileTransfer
    ( -- * エクスポート・インポート型
      ExportSpec(..)
    , ImportedFile(..)
    , FileImportError(..)
    , FilePickResult(..)
    , FileTransferClient(..)
    , importFileAcceptFilter
      -- * 純粋なファイル名生成・形式推論・インポート変換関数
    , exportMimeType
    , sanitizeExportBaseName
    , buildExportFileName
    , buildExportSpec
    , inferDocumentFormatFromFileName
    , deriveTitleFromFileName
    , stripUtf8Bom
    , prepareImportedDocumentRequest
      -- * 純粋な JSFFI 応答デコード関数
    , decodeExportResultJson
    , decodeFilePickResultJson
      -- * ブラウザ実装
    , browserFileTransferClient
    ) where

import           Data.Aeson                    (FromJSON(..))
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types              (Parser)
import qualified Data.Char                     as Char
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as Txt
import qualified Data.Text.Encoding            as TxtEnc
import           TamaTsuzura.Common.Api        (CreateDocumentRequest(..))
import           TamaTsuzura.Common.Document
    ( DocumentBody(..)
    , DocumentFormat(..)
    , FolderPath
    , TagSet
    , mkDocumentTitle
    , validateDocumentBody
    )
import           TamaTsuzura.Common.Text       (ToText(..))
import           TamaTsuzura.Common.Validation
    ( ValidationError
    , maxDocumentBodyBytes
    , maxDocumentTitleLength
    )
#ifdef WASM
import           GHC.Wasm.Prim                 (JSVal)
import           Miso.DSL                      (fromJSVal, toJSVal)
#endif

-- | ダウンロード対象テキストファイルの出力仕様。
data ExportSpec = ExportSpec
    { fileName :: !Text
      -- ^ OS 禁止文字を除去済みのダウンロードファイル名
    , mimeType :: !Text
      -- ^ Blob 生成時の MIME タイプ
    , content  :: !Text
      -- ^ UTF-8 本文テキスト
    } deriving (Eq, Show)

-- | ブラウザのファイル選択から読み込まれたテキストファイル。
data ImportedFile = ImportedFile
    { fileName :: !Text
      -- ^ 元のファイル名
    , content  :: !Text
      -- ^ UTF-8 デコード済みのファイル内容
    } deriving (Eq, Show)

-- | ファイルインポート処理で発生しうる失敗理由。
data FileImportError
    = ImportFileTooLarge !Int !Int
      -- ^ ファイルサイズが上限 (5 MiB) を超えている (実際バイト数, 上限バイト数)
    | ImportInvalidUtf8 !Text
      -- ^ 有効な UTF-8 テキストではない (バイナリファイル等)
    | ImportValidationFailed !ValidationError
      -- ^ ドメイン検証 (タイトル・本文上限等) に違反した
    | ImportReadFailed !Text
      -- ^ ブラウザ側のファイル読み込みエラー
    deriving (Eq, Show)

-- | ファイル選択ダイアログの実行結果。
data FilePickResult
    = FilePickSelected !ImportedFile
      -- ^ ファイルが選択され UTF-8 読み込みに成功した
    | FilePickCancelled
      -- ^ ユーザーがファイル選択をキャンセルした
    | FilePickError !FileImportError
      -- ^ 読み込みまたは事前検証でエラーが発生した
    deriving (Eq, Show)

-- | 差し替え可能なローカルファイル入出力クライアント抽象。
data FileTransferClient = FileTransferClient
    { exportFile     :: !(ExportSpec -> IO (Either Text ()))
      -- ^ テキストファイルをブラウザからダウンロードする
    , pickImportFile :: !(IO FilePickResult)
      -- ^ ファイル選択ダイアログを開き、選択されたテキストファイルを読み込む
    }

-- | ファイル選択ダイアログの @accept@ 属性値。
importFileAcceptFilter :: Text
importFileAcceptFilter = ".md,.markdown,.mkd,.mdown,.txt,.text,text/markdown,text/plain"

-- | エクスポートファイル名のベース部最大文字数 (拡張子を含めて各 OS の 255 バイト制限に余裕を持たせる)。
maxExportBaseNameLength :: Int
maxExportBaseNameLength = 80

-- | 'DocumentFormat' に対応する UTF-8 MIME タイプを返す。
exportMimeType :: DocumentFormat -> Text
exportMimeType FormatMarkdown  = "text/markdown;charset=utf-8"
exportMimeType FormatPlainText = "text/plain;charset=utf-8"

-- | 任意のタイトル文字列を各 OS で安全に保存可能なファイル名ベース文字列へ正規化する。
-- パス区切り・OS 禁止文字・制御文字を @_@ へ置換し、前後空白と先頭/末尾ドットを取り除いたうえで、
-- 空になる場合は @"untitled"@ へフォールバックする。
sanitizeExportBaseName :: Text -> Text
sanitizeExportBaseName raw =
    let replaced :: Text
        replaced = Txt.map replaceForbidden raw

        trimmed :: Text
        trimmed = Txt.dropWhileEnd isEdgeNoise (Txt.dropWhile isEdgeNoise replaced)

        collapsed :: Text
        collapsed = collapseUnderscores trimmed

        shortened :: Text
        shortened = Txt.dropWhileEnd isEdgeNoise (Txt.take maxExportBaseNameLength collapsed)
    in  if Txt.null shortened then "untitled" else shortened
  where
    replaceForbidden :: Char -> Char
    replaceForbidden ch
        | Char.isControl ch || isForbiddenFileChar ch = '_'
        | otherwise                                   = ch

    isForbiddenFileChar :: Char -> Bool
    isForbiddenFileChar ch =
        ch `elem` ['/', '\\', ':', '*', '?', '"', '<', '>', '|']

    isEdgeNoise :: Char -> Bool
    isEdgeNoise ch = Char.isSpace ch || ch == '.'

    collapseUnderscores :: Text -> Text
    collapseUnderscores txt =
        case Txt.uncons txt of
            Nothing -> ""
            Just _  -> Txt.intercalate "_" (filter (not . Txt.null) (Txt.splitOn "_" txt))

-- | タイトルとフォーマットから拡張子付きのダウンロードファイル名を構築する。
-- すでにフォーマットと一致する拡張子で終わっている場合は二重付与しない。
buildExportFileName :: Text -> DocumentFormat -> Text
buildExportFileName titleInput fmt =
    let base :: Text
        base = sanitizeExportBaseName titleInput

        lowerBase :: Text
        lowerBase = Txt.toLower base
    in  case fmt of
            FormatMarkdown
                | Txt.isSuffixOf ".md" lowerBase || Txt.isSuffixOf ".markdown" lowerBase -> base
                | otherwise -> base <> ".md"
            FormatPlainText
                | Txt.isSuffixOf ".txt" lowerBase -> base
                | otherwise -> base <> ".txt"

-- | タイトル・本文・フォーマットから 'ExportSpec' を構築する。
buildExportSpec :: Text -> DocumentBody -> DocumentFormat -> ExportSpec
buildExportSpec titleInput body fmt = ExportSpec
    { fileName = buildExportFileName titleInput fmt
    , mimeType = exportMimeType fmt
    , content  = toText body
    }

-- | インポートされたファイル名の拡張子から 'DocumentFormat' を推論する。
inferDocumentFormatFromFileName :: Text -> DocumentFormat
inferDocumentFormatFromFileName fileName =
    let lower :: Text
        lower = Txt.toLower (Txt.strip fileName)
    in  if any (`Txt.isSuffixOf` lower) markdownExtensions
            then FormatMarkdown
            else FormatPlainText

-- | インポートされたファイル名から文書タイトル候補を生成する。
-- パス区切りと既知拡張子を取り除き、制御文字を空白へ置換して 'maxDocumentTitleLength' 以内に収める。
deriveTitleFromFileName :: Text -> Text
deriveTitleFromFileName rawName =
    let lastSeg :: Text
        lastSeg = Txt.strip (extractFileNameSegment rawName)

        stem :: Text
        stem = stripKnownExtension lastSeg

        cleaned :: Text
        cleaned = Txt.strip (Txt.map replaceControl stem)
    in  Txt.take maxDocumentTitleLength cleaned
  where
    extractFileNameSegment :: Text -> Text
    extractFileNameSegment txt =
        case reverse (Txt.split isPathSep txt) of
            (seg : _) -> seg
            []        -> txt

    isPathSep :: Char -> Bool
    isPathSep ch = ch == '/' || ch == '\\'

    replaceControl :: Char -> Char
    replaceControl ch
        | Char.isControl ch = ' '
        | otherwise         = ch

    stripKnownExtension :: Text -> Text
    stripKnownExtension name =
        let lower :: Text
            lower = Txt.toLower name
        in  case filter (`Txt.isSuffixOf` lower) knownImportExtensions of
                (ext : _) -> Txt.take (Txt.length name - Txt.length ext) name
                []        -> name

-- | UTF-8 BOM (@U+FEFF@) が先頭にある場合のみ取り除く。
stripUtf8Bom :: Text -> Text
stripUtf8Bom txt = fromMaybe txt (Txt.stripPrefix "\xFEFF" txt)

-- | 読み込まれたファイルと現在のフォルダ・タグ選択から、
-- ドメイン検証済みの @POST /api/documents@ 用 'CreateDocumentRequest' を構築する。
prepareImportedDocumentRequest ::
       ImportedFile
    -> Maybe FolderPath
    -> TagSet
    -> Either FileImportError CreateDocumentRequest
prepareImportedDocumentRequest imported mFolder tags = do
    let derivedTitle :: Text
        derivedTitle = deriveTitleFromFileName imported.fileName

        cleanBody :: DocumentBody
        cleanBody = DocumentBody (stripUtf8Bom imported.content)

        inferredFormat :: DocumentFormat
        inferredFormat = inferDocumentFormatFromFileName imported.fileName

    validTitle <- mapValidation (mkDocumentTitle derivedTitle)
    validBody  <- mapValidation (validateDocumentBody cleanBody)
    Right CreateDocumentRequest
        { title  = validTitle
        , body   = validBody
        , format = inferredFormat
        , folder = mFolder
        , tags   = tags
        }
  where
    mapValidation :: Either ValidationError a -> Either FileImportError a
    mapValidation (Left err)  = Left (ImportValidationFailed err)
    mapValidation (Right val) = Right val

data ExportJsPayload = ExportJsPayload
    { ok     :: !Bool
    , errMsg :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON ExportJsPayload where
    parseJSON = Aeson.withObject "ExportJsPayload" $ \ obj ->
        ExportJsPayload
            <$> obj Aeson..:  "ok"
            <*> obj Aeson..:? "error"

-- | JSFFI の @exportTextFile()@ 応答 JSON をデコードする。
decodeExportResultJson :: Text -> Either Text ()
decodeExportResultJson rawJson =
    case ePayload of
        Left parseErr ->
            Left ("Malformed JSFFI export payload: " <> Txt.pack parseErr)
        Right payload
            | payload.ok -> Right ()
            | otherwise  -> Left (fromMaybe "File export failed" payload.errMsg)
  where
    ePayload :: Either String ExportJsPayload
    ePayload = Aeson.eitherDecodeStrict' (TxtEnc.encodeUtf8 rawJson)

instance FromJSON FilePickResult where
    parseJSON = Aeson.withObject "FilePickResult" $ \ obj -> do
        statusText <- obj Aeson..: "status" :: Parser Text
        case statusText of
            "cancelled" -> pure FilePickCancelled
            "selected"  -> do
                name <- obj Aeson..: "fileName"
                body <- obj Aeson..: "content"
                pure $ FilePickSelected ImportedFile
                    { fileName = name
                    , content  = body
                    }
            "error" -> do
                code <- obj Aeson..:? "code" Aeson..!= ("read_failed" :: Text)
                case code of
                    "too_large" -> do
                        actual <- obj Aeson..:? "actualBytes" Aeson..!= (maxDocumentBodyBytes + 1)
                        limit  <- obj Aeson..:? "maxBytes"    Aeson..!= maxDocumentBodyBytes
                        pure (FilePickError (ImportFileTooLarge actual limit))
                    "invalid_utf8" -> do
                        msg <- obj Aeson..:? "message" Aeson..!= "Invalid UTF-8 file"
                        pure (FilePickError (ImportInvalidUtf8 msg))
                    _ -> do
                        msg <- obj Aeson..:? "message" Aeson..!= "Failed to read file"
                        pure (FilePickError (ImportReadFailed msg))
            other ->
                fail ("Unknown file pick status: " <> Txt.unpack other)

-- | JSFFI の @pickTextFile()@ 応答 JSON を 'FilePickResult' へ純粋にデコードする。
decodeFilePickResultJson :: Text -> FilePickResult
decodeFilePickResultJson rawJson =
    case Aeson.eitherDecodeStrict' (TxtEnc.encodeUtf8 rawJson) of
        Left parseErr ->
            FilePickError (ImportReadFailed ("Malformed JSFFI file picker payload: " <> Txt.pack parseErr))
        Right res -> res

markdownExtensions :: [Text]
markdownExtensions =
    [ ".markdown"
    , ".mdown"
    , ".mkd"
    , ".md"
    ]

knownImportExtensions :: [Text]
knownImportExtensions =
    markdownExtensions <>
    [ ".text"
    , ".txt"
    ]

#ifdef WASM
foreign import javascript unsafe
    "(() => { if (globalThis.__tamatsuzuraFiles && typeof globalThis.__tamatsuzuraFiles.exportTextFile === 'function') { return globalThis.__tamatsuzuraFiles.exportTextFile($1, $2, $3); } return JSON.stringify({ ok: false, error: 'File transfer bridge is unavailable' }); })()"
    jsExportTextFile :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript safe
    "(async () => { if (globalThis.__tamatsuzuraFiles && typeof globalThis.__tamatsuzuraFiles.pickTextFile === 'function') { return await globalThis.__tamatsuzuraFiles.pickTextFile($1, $2); } return JSON.stringify({ status: 'error', code: 'read_failed', message: 'File transfer bridge is unavailable' }); })()"
    jsPickTextFile :: JSVal -> Int -> IO JSVal
#endif

-- | ブラウザの Blob / File API ブリッジを用いる標準 'FileTransferClient'。
-- ネイティブ (非 Wasm) ビルド時は安全なフォールバック実装を返す。
browserFileTransferClient :: FileTransferClient
browserFileTransferClient = FileTransferClient
    { exportFile     = exportFileBrowser
    , pickImportFile = pickImportFileBrowser
    }

exportFileBrowser :: ExportSpec -> IO (Either Text ())
#ifdef WASM
exportFileBrowser spec = do
    nameVal <- toJSVal spec.fileName
    mimeVal <- toJSVal spec.mimeType
    bodyVal <- toJSVal spec.content
    rawVal  <- jsExportTextFile nameVal mimeVal bodyVal
    mRawTxt <- fromJSVal rawVal :: IO (Maybe Text)
    pure $ case mRawTxt of
        Nothing     -> Left "JSFFI exportTextFile returned a non-string value"
        Just rawTxt -> decodeExportResultJson rawTxt
#else
exportFileBrowser _spec =
    pure (Left "Browser file export is only available in the WebAssembly runtime")
#endif

pickImportFileBrowser :: IO FilePickResult
#ifdef WASM
pickImportFileBrowser = do
    acceptVal <- toJSVal importFileAcceptFilter
    rawVal    <- jsPickTextFile acceptVal maxDocumentBodyBytes
    mRawTxt   <- fromJSVal rawVal :: IO (Maybe Text)
    pure $ case mRawTxt of
        Nothing     -> FilePickError (ImportReadFailed "JSFFI pickTextFile returned a non-string value")
        Just rawTxt -> decodeFilePickResultJson rawTxt
#else
pickImportFileBrowser =
    pure FilePickCancelled
#endif
