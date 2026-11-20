{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.Auth.Crypto
    ( -- * 暗号処理エラー型
      AuthCryptoError(..)
      -- * Argon2id パスワードハッシュパラメータ
    , PasswordHashParams
    , mkPasswordHashParams
    , defaultPasswordHashParams
    , fastTestPasswordHashParams
    , passwordHashMemoryKiB
    , passwordHashIterations
    , passwordHashParallelism
    , passwordHashOutputLength
    , passwordHashSaltLength
      -- * PHC 形式パスワードハッシュ
    , PasswordHash
    , passwordHashToText
    , parsePasswordHash
    , hashPassword
    , hashPasswordWithSalt
    , verifyPassword
      -- * セッション・CSRF トークンおよびハッシュ
    , sessionTokenEntropyBytes
    , SessionToken
    , generateSessionToken
    , mkSessionToken
    , sessionTokenRawText
    , SessionTokenHash
    , mkSessionTokenHash
    , hashSessionToken
    , generateCsrfToken
    , verifyCsrfToken
    , constantTimeTextEq
    ) where

import           Control.Exception       (Exception)
import qualified Crypto.Error            as CryptoErr
import           Crypto.Hash             (Digest, SHA256(..))
import qualified Crypto.Hash             as Hash
import qualified Crypto.KDF.Argon2       as Argon2
import qualified Crypto.Random           as Random
import qualified Data.ByteArray          as BA
import           Data.ByteArray.Encoding (Base(..))
import qualified Data.ByteArray.Encoding as BAEnc
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.Char               as Char
import           Data.Text               (Text)
import qualified Data.Text               as Txt
import qualified Data.Text.Encoding      as TxtEnc
import           Data.Word               (Word32)
import           TamaTsuzura.Common.Auth
    ( CsrfToken
    , Password
    , mkCsrfToken
    , unsafePasswordText
    )
import           TamaTsuzura.Common.Text (ToText(..))
import           Text.Read               (readMaybe)

-- | パスワードハッシュおよび認証トークン暗号処理の失敗理由。
data AuthCryptoError
    = InvalidHashParams !Text
      -- ^ Argon2id のコスト・並列度・出力長・ソルト長パラメータが安全範囲外である
    | InvalidSaltLength !Int !Int
      -- ^ ソルト長が下限/上限を満たしていない (実際値, 最小または最大値)
    | InvalidPasswordHashFormat !Text
      -- ^ PHC 形式のパスワードハッシュ文字列が破損している、または未対応形式である
    | Argon2ComputationFailed !Text
      -- ^ Argon2id 計算ライブラリがエラーを返した
    | TokenGenerationFailed !Text
      -- ^ 乱数トークンの生成または検証に失敗した
    deriving (Eq)

instance ToText AuthCryptoError where
    toText (InvalidHashParams detail) =
        "Invalid Argon2id parameters: " <> detail
    toText (InvalidSaltLength actual expected) =
        "Invalid salt byte length " <> showInt actual <> " (limit: " <> showInt expected <> ")"
    toText (InvalidPasswordHashFormat detail) =
        "Invalid password hash format: " <> detail
    toText (Argon2ComputationFailed detail) =
        "Argon2id computation failed: " <> detail
    toText (TokenGenerationFailed detail) =
        "Token generation failed: " <> detail

instance Show AuthCryptoError where
    show = Txt.unpack . toText

instance Exception AuthCryptoError

-- | Argon2id パスワードハッシュ化コストパラメータ。
-- 検証済み範囲内の安全な値のみを保持するためコンストラクタを非公開とする。
data PasswordHashParams = PasswordHashParams
    { memoryKiB   :: !Word32
      -- ^ 使用メモリ量 (KiB)
    , iterations  :: !Word32
      -- ^ 反復回数 (time cost)
    , parallelism :: !Word32
      -- ^ 並列レーン数 (CGI 単一プロセス運用に適した 1 以上)
    , hashLength  :: !Int
      -- ^ 導出ハッシュのバイト長
    , saltLength  :: !Int
      -- ^ ランダムソルトのバイト長
    } deriving (Eq, Show)

minMemoryKiB :: Word32
minMemoryKiB = 8

maxMemoryKiB :: Word32
maxMemoryKiB = 1024 * 1024

minIterations :: Word32
minIterations = 1

maxIterations :: Word32
maxIterations = 100

minParallelism :: Word32
minParallelism = 1

maxParallelism :: Word32
maxParallelism = 16

minHashLength :: Int
minHashLength = 16

maxHashLength :: Int
maxHashLength = 64

minSaltLength :: Int
minSaltLength = 8

maxSaltLength :: Int
maxSaltLength = 64

-- | 各パラメータの安全上下限および Argon2 仕様要件 (@memoryKiB >= 8 * parallelism@) を検証して
-- 'PasswordHashParams' を構築する。
mkPasswordHashParams ::
       Word32
    -> Word32
    -> Word32
    -> Int
    -> Int
    -> Either AuthCryptoError PasswordHashParams
mkPasswordHashParams memKiB iters par outLen sLen
    | par < minParallelism || par > maxParallelism =
        Left (InvalidHashParams "parallelism must be between 1 and 16")
    | memKiB < max minMemoryKiB (8 * par) || memKiB > maxMemoryKiB =
        Left (InvalidHashParams "memoryKiB must be >= 8 * parallelism and <= 1048576")
    | iters < minIterations || iters > maxIterations =
        Left (InvalidHashParams "iterations must be between 1 and 100")
    | outLen < minHashLength || outLen > maxHashLength =
        Left (InvalidHashParams "hashLength must be between 16 and 64 bytes")
    | sLen < minSaltLength || sLen > maxSaltLength =
        Left (InvalidHashParams "saltLength must be between 8 and 64 bytes")
    | otherwise =
        Right PasswordHashParams
            { memoryKiB   = memKiB
            , iterations  = iters
            , parallelism = par
            , hashLength  = outLen
            , saltLength  = sLen
            }

-- | 本番環境向けの既定 Argon2id パラメータ。
-- OWASP Password Storage Cheat Sheet の推奨最小設定 (@m=19456 KiB (19 MiB), t=2, p=1@) に準拠し、
-- レンタルサーバ CGI 環境でのメモリ制約とブルートフォース耐性を両立する。
defaultPasswordHashParams :: PasswordHashParams
defaultPasswordHashParams = PasswordHashParams
    { memoryKiB   = 19456
    , iterations  = 2
    , parallelism = 1
    , hashLength  = 32
    , saltLength  = 16
    }

-- | 自動テスト高速化用の軽量 Argon2id パラメータ。
-- テストスイートの実行時間を抑える用途限定であり、本番認証設定には使用しないこと。
fastTestPasswordHashParams :: PasswordHashParams
fastTestPasswordHashParams = PasswordHashParams
    { memoryKiB   = 64
    , iterations  = 1
    , parallelism = 1
    , hashLength  = 32
    , saltLength  = 16
    }

-- | 使用メモリ量 (KiB) を返す。
passwordHashMemoryKiB :: PasswordHashParams -> Word32
passwordHashMemoryKiB params = params.memoryKiB

-- | 反復回数 (time cost) を返す。
passwordHashIterations :: PasswordHashParams -> Word32
passwordHashIterations params = params.iterations

-- | 並列レーン数を返す。
passwordHashParallelism :: PasswordHashParams -> Word32
passwordHashParallelism params = params.parallelism

-- | 導出ハッシュ長 (バイト) を返す。
passwordHashOutputLength :: PasswordHashParams -> Int
passwordHashOutputLength params = params.hashLength

-- | ソルト長 (バイト) を返す。
passwordHashSaltLength :: PasswordHashParams -> Int
passwordHashSaltLength params = params.saltLength

-- | PHC 文字列形式 (@$argon2id$v=19$m=...,t=...,p=...$<salt>$<hash>@) で検証済みのパスワードハッシュ。
-- ログやエラー出力への機密情報露出を防ぐため 'Show' は伏せ字とし、
-- 同値比較はタイミング攻撃を避けるため定数時間比較を行う。
newtype PasswordHash = PasswordHash Text

instance Eq PasswordHash where
    PasswordHash a == PasswordHash b = constantTimeTextEq a b

instance Show PasswordHash where
    show _ = "<PasswordHash>"

-- | 永続化層へ保存するために検証済み 'PasswordHash' の PHC 文字列を取り出す。
passwordHashToText :: PasswordHash -> Text
passwordHashToText (PasswordHash raw) = raw

data ParsedPhc = ParsedPhc
    { params :: !PasswordHashParams
    , salt   :: !ByteString
    , digest :: !ByteString
    }

-- | DB から読み込んだ PHC 形式文字列を厳格に検証し、'PasswordHash' を復元する。
parsePasswordHash :: Text -> Either AuthCryptoError PasswordHash
parsePasswordHash raw = do
    _ <- parsePhcDetails raw
    Right (PasswordHash raw)

-- | 暗号論的安全な乱数ソルトを生成してパスワードを Argon2id ハッシュ化する。
hashPassword :: PasswordHashParams -> Password -> IO (Either AuthCryptoError PasswordHash)
hashPassword params pw = do
    saltBytes <- Random.getRandomBytes params.saltLength
    pure (hashPasswordWithSalt params saltBytes pw)

-- | 明示的に指定したソルトを用いてパスワードを純粋に Argon2id ハッシュ化する。
-- 決定的な単体テストおよび PHC エンコード検証に使用する。
hashPasswordWithSalt ::
       PasswordHashParams
    -> ByteString
    -> Password
    -> Either AuthCryptoError PasswordHash
hashPasswordWithSalt params saltBytes pw
    | BS.length saltBytes < minSaltLength =
        Left (InvalidSaltLength (BS.length saltBytes) minSaltLength)
    | BS.length saltBytes > maxSaltLength =
        Left (InvalidSaltLength (BS.length saltBytes) maxSaltLength)
    | otherwise = do
        digestBytes <- computeArgon2id params saltBytes (TxtEnc.encodeUtf8 (unsafePasswordText pw))
        Right (PasswordHash (formatPhc params saltBytes digestBytes))

-- | 入力パスワードが保存済み 'PasswordHash' と一致するかを定数時間比較で検証する。
-- ハッシュ文字列内に記録されたパラメータ (m, t, p, salt, hash 長) を用いて再計算するため、
-- 将来既定パラメータを引き上げても既存ユーザーのログイン互換性を維持できる。
verifyPassword :: Password -> PasswordHash -> Bool
verifyPassword pw (PasswordHash rawPhc) =
    case parsePhcDetails rawPhc of
        Left _       -> False
        Right parsed ->
            case computeArgon2id parsed.params parsed.salt (TxtEnc.encodeUtf8 (unsafePasswordText pw)) of
                Left _         -> False
                Right computed ->
                    BS.length computed == BS.length parsed.digest
                        && BA.constEq computed parsed.digest

-- | セッション・CSRF トークン生成に使用する暗号乱数エントロピー長 (32 バイト = 256 bit)。
sessionTokenEntropyBytes :: Int
sessionTokenEntropyBytes = 32

minSessionTokenChars :: Int
minSessionTokenChars = 32

maxSessionTokenChars :: Int
maxSessionTokenChars = 128

-- | クライアントの @HttpOnly@ Cookie へ設定する生のセッショントークン。
-- ログ漏洩を防ぐため 'Show' は伏せ字とし、比較は定数時間で行う。
newtype SessionToken = SessionToken Text

instance Eq SessionToken where
    SessionToken a == SessionToken b = constantTimeTextEq a b

instance Show SessionToken where
    show _ = "<SessionToken>"

-- | 暗号論的安全な 256 bit 乱数から URL セーフ (Base64URL パディング無し 43 文字) の
-- 'SessionToken' を生成する。
generateSessionToken :: IO SessionToken
generateSessionToken = do
    rawBytes <- Random.getRandomBytes sessionTokenEntropyBytes
    pure (SessionToken (encodeBase64UrlUnpadded rawBytes))

-- | Cookie ヘッダ等から受け取った文字列を書式検証して 'SessionToken' を構築する。
mkSessionToken :: Text -> Either Text SessionToken
mkSessionToken raw
    | len < minSessionTokenChars || len > maxSessionTokenChars =
        Left "Invalid session token length"
    | not (Txt.all isUrlSafeTokenChar raw) =
        Left "Session token contains invalid characters"
    | otherwise =
        Right (SessionToken raw)
  where
    len :: Int
    len = Txt.length raw

-- | Cookie 設定ヘッダ生成などの境界処理専用に生セッショントークン文字列を取り出す。
sessionTokenRawText :: SessionToken -> Text
sessionTokenRawText (SessionToken raw) = raw

-- | SQLite の @sessions.token_hash@ 列に保存する SHA-256 ハッシュ済みセッショントークン (小文字 16 進 64 文字)。
-- DB ファイル漏洩時に生セッショントークンを復元・悪用できないよう、DB には本型のみを保存する。
newtype SessionTokenHash = SessionTokenHash Text
    deriving (Eq, Ord)

instance ToText SessionTokenHash where
    toText (SessionTokenHash raw) = raw

instance Show SessionTokenHash where
    show = Txt.unpack . toText

-- | DB から読み込んだ 64 文字小文字 16 進 SHA-256 ダイジェスト文字列を検証して 'SessionTokenHash' を構築する。
mkSessionTokenHash :: Text -> Either Text SessionTokenHash
mkSessionTokenHash raw
    | Txt.length raw /= 64                = Left "Session token hash must be 64 hex characters"
    | not (Txt.all isLowerHexChar raw)    = Left "Session token hash must be lowercase hexadecimal"
    | otherwise                           = Right (SessionTokenHash raw)
  where
    isLowerHexChar :: Char -> Bool
    isLowerHexChar ch = Char.isDigit ch || (ch >= 'a' && ch <= 'f')

-- | 生の 'SessionToken' を SHA-256 で一方向ハッシュ化し、DB 検索キー用の 'SessionTokenHash' を算出する。
hashSessionToken :: SessionToken -> SessionTokenHash
hashSessionToken token =
    let digest :: Digest SHA256
        digest = Hash.hashWith SHA256 (TxtEnc.encodeUtf8 (sessionTokenRawText token))

        hexBytes :: ByteString
        hexBytes = BAEnc.convertToBase Base16 digest
    in  SessionTokenHash (TxtEnc.decodeLatin1 hexBytes)

-- | 暗号論的安全な 256 bit 乱数から 'CsrfToken' (Base64URL パディング無し 43 文字) を生成する。
generateCsrfToken :: IO (Either AuthCryptoError CsrfToken)
generateCsrfToken = do
    rawBytes <- Random.getRandomBytes sessionTokenEntropyBytes
    let tokenText :: Text
        tokenText = encodeBase64UrlUnpadded rawBytes
    pure $ case mkCsrfToken tokenText of
        Right token -> Right token
        Left err    -> Left (TokenGenerationFailed (toText err))

-- | 2 つの 'CsrfToken' をバイト列レベルの定数時間比較で照合する。
verifyCsrfToken :: CsrfToken -> CsrfToken -> Bool
verifyCsrfToken expected provided = constantTimeTextEq (toText expected) (toText provided)

-- | 2 つの 'Text' の UTF-8 表現を定数時間で比較する。
-- CSRF トークン照合などの秘密値比較でタイミングサイドチャネルを抑制するために使用する。
constantTimeTextEq :: Text -> Text -> Bool
constantTimeTextEq a b = BA.constEq (TxtEnc.encodeUtf8 a) (TxtEnc.encodeUtf8 b)

computeArgon2id ::
       PasswordHashParams
    -> ByteString
    -> ByteString
    -> Either AuthCryptoError ByteString
computeArgon2id params saltBytes passwordBytes =
    let opts :: Argon2.Options
        opts = Argon2.Options
            { Argon2.iterations  = params.iterations
            , Argon2.memory      = params.memoryKiB
            , Argon2.parallelism = params.parallelism
            , Argon2.variant     = Argon2.Argon2id
            , Argon2.version     = Argon2.Version13
            }
    in  case Argon2.hash opts passwordBytes saltBytes params.hashLength of
            CryptoErr.CryptoPassed digest -> Right digest
            CryptoErr.CryptoFailed err    -> Left (Argon2ComputationFailed (Txt.pack (show err)))

formatPhc :: PasswordHashParams -> ByteString -> ByteString -> Text
formatPhc params saltBytes digestBytes =
    "$argon2id$v=19$m="
        <> showWord32 params.memoryKiB
        <> ",t="
        <> showWord32 params.iterations
        <> ",p="
        <> showWord32 params.parallelism
        <> "$"
        <> encodeBase64Unpadded saltBytes
        <> "$"
        <> encodeBase64Unpadded digestBytes

parsePhcDetails :: Text -> Either AuthCryptoError ParsedPhc
parsePhcDetails raw =
    case Txt.splitOn "$" raw of
        ["", "argon2id", "v=19", paramsPart, saltPart, hashPart] -> do
            (memKiB, iters, par) <- parseParamsPart paramsPart
            saltBytes <- decodeBase64Unpadded "salt" saltPart
            digestBytes <- decodeBase64Unpadded "digest" hashPart
            validatedParams <-
                mkPasswordHashParams
                    memKiB
                    iters
                    par
                    (BS.length digestBytes)
                    (BS.length saltBytes)
            Right ParsedPhc
                { params = validatedParams
                , salt   = saltBytes
                , digest = digestBytes
                }
        _ ->
            Left (InvalidPasswordHashFormat "Expected PHC format $argon2id$v=19$m=...,t=...,p=...$<salt>$<hash>")
  where
    parseParamsPart :: Text -> Either AuthCryptoError (Word32, Word32, Word32)
    parseParamsPart part =
        case Txt.splitOn "," part of
            [mPart, tPart, pPart] -> do
                memVal  <- parseKeyWord32 "m=" mPart
                iterVal <- parseKeyWord32 "t=" tPart
                parVal  <- parseKeyWord32 "p=" pPart
                Right (memVal, iterVal, parVal)
            _ ->
                Left (InvalidPasswordHashFormat "Malformed Argon2id parameter section")

    parseKeyWord32 :: Text -> Text -> Either AuthCryptoError Word32
    parseKeyWord32 prefix kv =
        case Txt.stripPrefix prefix kv of
            Nothing ->
                Left (InvalidPasswordHashFormat ("Missing parameter prefix " <> prefix))
            Just rawNum
                | Txt.null rawNum || Txt.length rawNum > 10 || not (Txt.all Char.isDigit rawNum) ->
                    Left (InvalidPasswordHashFormat ("Malformed numeric parameter value for " <> prefix))
                | Txt.length rawNum > 1 && Txt.isPrefixOf "0" rawNum ->
                    Left (InvalidPasswordHashFormat ("Leading zeros are not allowed in PHC parameter " <> prefix))
                | otherwise ->
                    case readMaybe (Txt.unpack rawNum) :: Maybe Integer of
                        Just n
                            | n >= 0 && n <= toInteger (maxBound :: Word32) ->
                                Right (fromInteger n)
                        _ ->
                            Left (InvalidPasswordHashFormat ("Parameter out of Word32 range for " <> prefix))

encodeBase64Unpadded :: ByteString -> Text
encodeBase64Unpadded rawBytes =
    let encoded :: ByteString
        encoded = BAEnc.convertToBase Base64 rawBytes
    in  Txt.dropWhileEnd (== '=') (TxtEnc.decodeLatin1 encoded)

decodeBase64Unpadded :: Text -> Text -> Either AuthCryptoError ByteString
decodeBase64Unpadded label raw
    | Txt.null raw =
        Left (InvalidPasswordHashFormat ("Empty Base64 " <> label))
    | Txt.any (not . isBase64UnpaddedChar) raw =
        Left (InvalidPasswordHashFormat ("Invalid Base64 characters in " <> label))
    | remLen == 1 =
        Left (InvalidPasswordHashFormat ("Invalid Base64 length in " <> label))
    | otherwise =
        let padding :: Text
            padding = case remLen of
                0 -> ""
                2 -> "=="
                _ -> "="

            paddedBytes :: ByteString
            paddedBytes = TxtEnc.encodeUtf8 (raw <> padding)
        in  case BAEnc.convertFromBase Base64 paddedBytes of
                Left errMsg   -> Left (InvalidPasswordHashFormat ("Failed to decode " <> label <> ": " <> Txt.pack errMsg))
                Right decoded ->
                    -- 正規形チェック: 非ゼロの余剰パディングビットを含む非標準エンコードを拒否する
                    if encodeBase64Unpadded decoded == raw
                        then Right decoded
                        else Left (InvalidPasswordHashFormat ("Non-canonical Base64 encoding in " <> label))
  where
    remLen :: Int
    remLen = Txt.length raw `mod` 4

    isBase64UnpaddedChar :: Char -> Bool
    isBase64UnpaddedChar ch =
        (Char.isAscii ch && Char.isAlphaNum ch) || ch == '+' || ch == '/'

encodeBase64UrlUnpadded :: ByteString -> Text
encodeBase64UrlUnpadded rawBytes =
    TxtEnc.decodeLatin1 (BAEnc.convertToBase Base64URLUnpadded rawBytes)

isUrlSafeTokenChar :: Char -> Bool
isUrlSafeTokenChar ch =
    (Char.isAscii ch && Char.isAlphaNum ch) || ch == '-' || ch == '_'

showInt :: Int -> Text
showInt = Txt.pack . show

showWord32 :: Word32 -> Text
showWord32 = Txt.pack . show
