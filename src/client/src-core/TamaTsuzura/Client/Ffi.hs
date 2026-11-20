{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.Ffi
    ( -- * Fetch JSFFI 型・純粋エンコード/デコード
      RawFetchResult(..)
    , encodeFetchOptionsJson
    , decodeFetchResultJson
      -- * ブラウザ JSFFI エンドポイント
    , fetchRaw
    , readConfiguredApiBaseUrl
    ) where

import           Data.Aeson            (FromJSON(..), ToJSON(..))
import qualified Data.Aeson            as Aeson
import qualified Data.Aeson.Key        as AesonKey
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Lazy  as LazyBS
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as Txt
import qualified Data.Text.Encoding    as TxtEnc
#ifdef WASM
import           GHC.Wasm.Prim         (JSVal)
import           Miso.DSL              (fromJSVal, toJSVal)
#endif

-- | ブラウザの Fetch API 呼び出しから返却される低レベル結果。
-- HTTP 4xx/5xx も含めてサーバが返したレスポンス本文を失わずに上位 API クライアント層へ渡すため、
-- ネットワーク失敗と HTTP レスポンス受信を明確に分離して表現する。
data RawFetchResult
    = RawFetchOk !Int !ByteString
      -- ^ HTTP レスポンスを受信した (HTTP ステータスコード, UTF-8 レスポンス本文)
    | RawFetchFailed !Text
      -- ^ ネットワーク切断・CORS/TLS 失敗・JS 実行時エラー等によりレスポンスを得られなかった
    deriving (Eq, Show)

-- | JSFFI 経由でブラウザ @fetch@ へ渡すシリアライズ済みリクエスト設定。
data FetchJsOptions = FetchJsOptions
    { method  :: !Text
    , headers :: ![(Text, Text)]
    , body    :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON FetchJsOptions where
    toJSON opts = Aeson.object
        [ "method"  Aeson..= opts.method
        , "headers" Aeson..= Aeson.object
            [ AesonKey.fromText k Aeson..= v
            | (k, v) <- opts.headers
            ]
        , "body"    Aeson..= opts.body
        ]

-- | JSFFI 側の @fetch@ 実行結果を安全に Haskell 側へ戻すための JSON ラッパー。
data FetchJsPayload = FetchJsPayload
    { ok     :: !Bool
    , status :: !(Maybe Int)
    , body   :: !(Maybe Text)
    , errMsg :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON FetchJsPayload where
    parseJSON = Aeson.withObject "FetchJsPayload" $ \ obj ->
        FetchJsPayload
            <$> obj Aeson..:  "ok"
            <*> obj Aeson..:? "status"
            <*> obj Aeson..:? "body"
            <*> obj Aeson..:? "error"

-- | HTTP リクエスト設定を JSFFI 受け渡し用の JSON テキストへ純粋にエンコードする。
-- 空文字列ボディ (@Just ""@) とボディなし (@Nothing@) を JSON の @""@ と @null@ で厳密に区別する。
encodeFetchOptionsJson :: Text -> [(Text, Text)] -> Maybe ByteString -> Text
encodeFetchOptionsJson methodName reqHeaders mBodyBytes =
    let mBodyText :: Maybe Text
        mBodyText = fmap TxtEnc.decodeUtf8Lenient mBodyBytes

        opts :: FetchJsOptions
        opts = FetchJsOptions
            { method  = methodName
            , headers = reqHeaders
            , body    = mBodyText
            }
    in  TxtEnc.decodeUtf8Lenient (LazyBS.toStrict (Aeson.encode opts))

-- | JSFFI から返却された JSON テキストを 'RawFetchResult' へ純粋にデコードする。
decodeFetchResultJson :: Text -> RawFetchResult
decodeFetchResultJson rawJson =
    case ePayload of
        Left parseErr ->
            RawFetchFailed ("Malformed JSFFI fetch response: " <> Txt.pack parseErr)
        Right payload
            | payload.ok -> case payload.status of
                Nothing -> RawFetchFailed "Missing HTTP status in JSFFI fetch response"
                Just sc ->
                    let bodyText :: Text
                        bodyText = fromMaybe "" payload.body
                    in  RawFetchOk sc (TxtEnc.encodeUtf8 bodyText)
            | otherwise ->
                let detail :: Text
                    detail = fromMaybe "Network request failed" payload.errMsg
                in  RawFetchFailed detail
  where
    ePayload :: Either String FetchJsPayload
    ePayload = Aeson.eitherDecodeStrict' (TxtEnc.encodeUtf8 rawJson)

#ifdef WASM
foreign import javascript safe
    "try { const parsed = JSON.parse($2); const init = { method: parsed.method, headers: parsed.headers, credentials: 'same-origin' }; if (parsed.body !== null && parsed.body !== undefined) { init.body = parsed.body; } const res = await fetch($1, init); const bodyText = await res.text(); return JSON.stringify({ ok: true, status: res.status, body: bodyText }); } catch (err) { const msg = (err && err.message) ? String(err.message) : String(err); return JSON.stringify({ ok: false, error: msg }); }"
    jsFetchRaw :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe
    "(() => { if (typeof globalThis.__TAMATSUZURA_API_BASE__ === 'string') { return globalThis.__TAMATSUZURA_API_BASE__; } if (typeof document !== 'undefined' && document.querySelector) { const meta = document.querySelector('meta[name=\"tamatsuzura-api-base\"]'); if (meta && typeof meta.content === 'string') { return meta.content; } } return ''; })()"
    jsReadApiBaseUrl :: IO JSVal
#endif

-- | ブラウザの Fetch API 経由で生 HTTP リクエストを実行する。
-- @credentials: 'same-origin'@ を付与することで、M3 以降の CGI 互換 Cookie セッション認証にも対応する。
-- ネイティブ GHC ビルド時 (非 Wasm) はテスト用のトランスポート注入を前提とし、明示的な失敗結果を返す。
fetchRaw :: Text -> Text -> [(Text, Text)] -> Maybe ByteString -> IO RawFetchResult
#ifdef WASM
fetchRaw url methodName reqHeaders mBodyBytes = do
    let optsJson :: Text
        optsJson = encodeFetchOptionsJson methodName reqHeaders mBodyBytes
    urlVal  <- toJSVal url
    optsVal <- toJSVal optsJson
    rawVal  <- jsFetchRaw urlVal optsVal
    mRawTxt <- fromJSVal rawVal :: IO (Maybe Text)
    pure $ case mRawTxt of
        Nothing     -> RawFetchFailed "JSFFI fetch returned a non-string value"
        Just rawTxt -> decodeFetchResultJson rawTxt
#else
fetchRaw _url _methodName _reqHeaders _mBodyBytes =
    pure (RawFetchFailed "Browser Fetch API is only available in the WebAssembly runtime")
#endif

-- | HTML の @\<meta name="tamatsuzura-api-base"\>@ または @globalThis.__TAMATSUZURA_API_BASE__@ から
-- API ベース URL 設定を読み取る。未指定またはネイティブビルド時は空文字 (同一オリジン) を返す。
readConfiguredApiBaseUrl :: IO Text
#ifdef WASM
readConfiguredApiBaseUrl = do
    rawVal  <- jsReadApiBaseUrl
    mRawTxt <- fromJSVal rawVal :: IO (Maybe Text)
    pure $ case mRawTxt of
        Nothing  -> ""
        Just txt -> txt
#else
readConfiguredApiBaseUrl = pure ""
#endif
