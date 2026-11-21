{-# LANGUAGE OverloadedStrings #-}

module TamaTsuzura.Server.Static
    ( staticFilesMiddleware
    , resolveStaticAssetPath
    , contentTypeForOsPath
    , cacheControlForOsPath
    ) where

import           Control.Exception               (IOException, try)
import           Data.ByteString                 (ByteString)
import qualified Data.Char                       as Char
import           Data.Text                       (Text)
import qualified Data.Text                       as Txt
import qualified Network.HTTP.Types              as Http
import qualified Network.Wai                     as Wai
import qualified System.Directory.OsPath         as Dir
import qualified System.OsPath                   as OS

-- | Warp 開発・スタンドアロン運用時に静的アセット (@dist/web/@ 等) を配信するミドルウェア。
-- @/api@ および @/api/...@ へのリクエストは一切介入せず内側の REST API アプリへ委譲する。
-- パストラバーサルおよびシンボリックリンクによるルート外脱出を厳格に拒否する。
staticFilesMiddleware :: OS.OsPath -> Wai.Middleware
staticFilesMiddleware rootDir apiApp req respond
    | isApiPath (Wai.pathInfo req) = apiApp req respond
    | method /= Http.methodGet && method /= Http.methodHead =
        respond methodNotAllowedResponse
    | otherwise = do
        mTarget <- resolveStaticAssetPath rootDir (Wai.pathInfo req)
        case mTarget of
            Nothing ->
                respond notFoundResponse
            Just filePath -> do
                let mime :: ByteString
                    mime = contentTypeForOsPath filePath

                    cacheControl :: ByteString
                    cacheControl = cacheControlForOsPath filePath

                    headers :: Http.ResponseHeaders
                    headers =
                        [ (Http.hContentType, mime)
                        , (Http.hCacheControl, cacheControl)
                        , ("X-Content-Type-Options", "nosniff")
                        ]
                if method == Http.methodHead
                    then respond (Wai.responseLBS Http.status200 headers "")
                    else do
                        -- Wai.responseFile は String ベースの FilePath のみ受け付けるため境界でデコードする
                        eFilePath <- try (OS.decodeFS filePath) :: IO (Either IOException FilePath)
                        case eFilePath of
                            Left _        -> respond notFoundResponse
                            Right rawPath -> respond (Wai.responseFile Http.status200 headers rawPath Nothing)
  where
    method :: Http.Method
    method = Wai.requestMethod req

    isApiPath :: [Text] -> Bool
    isApiPath ("api" : _) = True
    isApiPath _           = False

    notFoundResponse :: Wai.Response
    notFoundResponse = Wai.responseLBS
        Http.status404
        [ (Http.hContentType, "text/plain; charset=utf-8")
        , ("X-Content-Type-Options", "nosniff")
        ]
        "Not Found"

    methodNotAllowedResponse :: Wai.Response
    methodNotAllowedResponse = Wai.responseLBS
        Http.status405
        [ (Http.hContentType, "text/plain; charset=utf-8")
        , ("Allow", "GET, HEAD")
        , ("X-Content-Type-Options", "nosniff")
        ]
        "Method Not Allowed"

-- | リクエストのパスセグメント列を安全に検証し、配信ルート配下の正規化済みファイルパスへ解決する。
-- ルートパス (@[]@) は @index.html@ へマップする。不正セグメント・ディレクトリ外脱出・非存在ファイルは 'Nothing' を返す。
resolveStaticAssetPath :: OS.OsPath -> [Text] -> IO (Maybe OS.OsPath)
resolveStaticAssetPath rootDir rawSegments =
    case sanitizeSegments rawSegments of
        Nothing -> pure Nothing
        Just cleanSegments -> do
            eRootCanon <- try (Dir.canonicalizePath rootDir) :: IO (Either IOException OS.OsPath)
            case eRootCanon of
                Left _          -> pure Nothing
                Right rootCanon -> do
                    eRelPaths <- traverse encodeSegment cleanSegments
                    case sequence eRelPaths of
                        Left _        -> pure Nothing
                        Right relSegs -> do
                            let candidate :: OS.OsPath
                                candidate = foldl' (OS.</>) rootCanon relSegs
                            exists <- Dir.doesFileExist candidate
                            if not exists
                                then pure Nothing
                                else do
                                    eCanon <- try (Dir.canonicalizePath candidate) :: IO (Either IOException OS.OsPath)
                                    case eCanon of
                                        Left _      -> pure Nothing
                                        Right canon ->
                                            if isContainedIn rootCanon canon
                                                then pure (Just canon)
                                                else pure Nothing
  where
    sanitizeSegments :: [Text] -> Maybe [Text]
    sanitizeSegments []   = Just ["index.html"]
    sanitizeSegments [""] = Just ["index.html"]
    sanitizeSegments segs
        | all isSafeSegment segs = Just segs
        | otherwise              = Nothing

    isSafeSegment :: Text -> Bool
    isSafeSegment seg =
        not (Txt.null seg)
            && seg /= "."
            && seg /= ".."
            && not (Txt.any isForbiddenChar seg)

    isForbiddenChar :: Char -> Bool
    isForbiddenChar ch =
        Char.isControl ch || ch == '/' || ch == '\\'

    -- WAI の pathInfo は Text セグメントで渡されるため、境界で OsPath へエンコードする
    encodeSegment :: Text -> IO (Either IOException OS.OsPath)
    encodeSegment seg = try (OS.encodeFS (Txt.unpack seg))

    isContainedIn :: OS.OsPath -> OS.OsPath -> Bool
    isContainedIn base target =
        let baseParts :: [OS.OsPath]
            baseParts = OS.splitDirectories base

            targetParts :: [OS.OsPath]
            targetParts = OS.splitDirectories target
        in  length targetParts > length baseParts
                && take (length baseParts) targetParts == baseParts

-- | 静的ファイルの拡張子からブラウザ向けの安全な @Content-Type@ を判定する。
-- WebAssembly ストリーミングコンパイル (@WebAssembly.instantiateStreaming@) に必須な @application/wasm@ を明示する。
contentTypeForOsPath :: OS.OsPath -> ByteString
contentTypeForOsPath path =
    -- 拡張子の大文字・小文字正規化と MIME マップ照合のため UTF-8 デコード境界を経由する
    case OS.decodeUtf (OS.takeExtension path) of
        Left _    -> "application/octet-stream"
        Right ext -> case Txt.toLower (Txt.pack ext) of
            ".html"        -> "text/html; charset=utf-8"
            ".htm"         -> "text/html; charset=utf-8"
            ".js"          -> "text/javascript; charset=utf-8"
            ".mjs"         -> "text/javascript; charset=utf-8"
            ".css"         -> "text/css; charset=utf-8"
            ".wasm"        -> "application/wasm"
            ".json"        -> "application/json; charset=utf-8"
            ".webmanifest" -> "application/manifest+json; charset=utf-8"
            ".svg"         -> "image/svg+xml"
            ".png"         -> "image/png"
            ".ico"         -> "image/x-icon"
            ".txt"         -> "text/plain; charset=utf-8"
            _              -> "application/octet-stream"

-- | 静的アセットに付与する @Cache-Control@ ヘッダ値を返す。
-- @sw.js@・HTML・Web App Manifest はブラウザの再検証を妨げないよう @no-cache@ とする。
-- 現行アセットはファイル名にコンテンツハッシュを含まないため、その他アセットも更新漏れ防止を優先し @no-cache@ を返す（オフライン高速起動は Service Worker のバージョン付きキャッシュが担う）。
cacheControlForOsPath :: OS.OsPath -> ByteString
cacheControlForOsPath path =
    case OS.decodeUtf (OS.takeFileName path) of
        Right "sw.js" -> "no-cache"
        _             -> case OS.decodeUtf (OS.takeExtension path) of
            Right ext -> case Txt.toLower (Txt.pack ext) of
                ".html"        -> "no-cache"
                ".htm"         -> "no-cache"
                ".webmanifest" -> "no-cache"
                _              -> "no-cache"
            Left _    -> "no-cache"
