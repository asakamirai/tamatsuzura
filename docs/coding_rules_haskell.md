# コーディングルール（Haskell 一般）

> **読むべきタイミング**: Haskell コードを書く／直す前。
> ここには「本プロジェクトの事情に依存しない」Haskell の書き方を定めます。
> プロジェクト固有の設計方針やアーキテクチャ境界は [coding_rules_project.md](coding_rules_project.md) を参照してください。

本プロジェクト全体で一貫性を保つための標準スタイルです。新しく記述するコードは本規約に統一してください。

---

## 1. レイアウト・スタイル

### 1-1. インデント

- **スペース4つ**。タブ文字は使用しない。
- `where` は本体より2スペース浅く置き、その中身は `where` から4スペース下げる。

```haskell
renderMessage :: Config -> Message -> Text
renderMessage config msg = prefix <> ": " <> msg.body
  where
    prefix = formatTag config.tag
```

### 1-2. 縦の揃え

**揃えられるものは縦位置を揃える**。視認性とコードの一貫性を高めるための重要なルールです。

- ガードの `=`

```haskell
clamp :: Int -> Int -> Int -> Int
clamp low high val | val < low  = low
                   | val > high = high
                   | otherwise  = val
```

- パターンマッチの `=`

```haskell
toText :: Status -> Text
toText Changed{}             = "changed"
toText Unchanged{}           = "unchanged"
toText res@Error{}           = "error: " <> toText res.errorDetail
toText (Skipped Nothing)     = "skipped"
toText (Skipped (Just prev)) = "skipped [" <> toText prev <> "]"
```

- レコードのフィールド定義と構築

```haskell
data OwnerDetail = OwnerDetail
    { userName  :: UserName
    , userId    :: Posix.UserID
    , groupName :: GroupName
    , groupId   :: Posix.GroupID
    } deriving (Eq)
```

- `case` 式の `->`

```haskell
checkLogLevel :: LogLevel -> LogType -> Bool
checkLogLevel level logType = case (level, logType) of
    (LogLevelDetail, _)     -> True
    (LogLevelInfo,   Info)  -> True
    (LogLevelInfo,   Error) -> True
    (LogLevelInfo,   _)     -> False
    (LogLevelError,  Error) -> True
    _                       -> False
```

### 1-3. リストとレコードは先頭カンマ

複数行にわたるリストやレコード構文では、カンマを行頭に置きます。

```haskell
packages :: [PackageName]
packages =
    [ "build-essential"
    , "autoconf"
    , "git"
    ]
```

### 1-4. ラムダはバックスラッシュの後ろに空白を置く

```haskell
forM items $ \ item -> ...
```

---

## 2. モジュール構成・import・言語拡張

### 2-1. ファイルの構成順

1. `{-# LANGUAGE ... #-}` を1行に1つ記述
2. `module X.Y (...) where` — **エクスポートリストは必ず明示する**
3. import 群
4. モジュール本体

エクスポートリストは先頭カンマで1行1項目とします。再エクスポート項目は末尾にコメントで区切ります。

```haskell
module TamaTsuzura.Document.Sync
    ( DocumentSpec(..)
    , SyncStatus(..)
    , syncDocument
    -- re-export
    , System.Permissions(..)
    ) where
```

### 2-2. import の書き方

- `qualified` の有無にかかわらず**モジュール名の開始列を揃える**（非 qualified 側は `import` の後ろにスペースを詰める）。
- `as` の別名開始位置も揃える。
- 非 qualified import は**インポートリストを明示**する（意図的なプレリュード的「まとめ」モジュールを除く）。

```haskell
import qualified Control.Monad.Reader as M
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as BS
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as Txt
```

- 別名 (qualified as) は短く一貫させる。

| モジュール | 推奨別名 |
| --- | --- |
| `Data.Text` / `Data.Text.IO` | `Txt` |
| `Data.ByteString*` | `BS` / `LazyBS` / `ShortBS` |
| `Control.Monad.*`（mtl 系） | `M` |
| `System.*`（ファイル・OS系） | `Sys` |
| `System.Posix.*` | `Posix` |
| `Data.Aeson*` | `Aeson` |

### 2-3. 言語拡張

必要なものだけをファイル冒頭に列挙します。

| 言語拡張 | 用途 |
| --- | --- |
| `OverloadedStrings` | `Text` / `ByteString` / newtype の文字列リテラル記述 |
| `OverloadedRecordDot` | `x.field` 形式による明快なレコードアクセス |
| `DuplicateRecordFields` + `NoFieldSelectors` | 別の型同士で同名のフィールド名を安全に共有 |
| `ScopedTypeVariables` | `where` 内の補助関数にスコープ付き型変数の注釈を付与 |
| `DataKinds` + `TypeFamilies` | 型レベルで状態遷移や検証ステータスを区別 |
| `DefaultSignatures` | 型クラスのデフォルト実装を導出 (`coerce` 活用) |

**危険な言語拡張の禁止**:
安全性を損なう言語拡張 (例: `IncoherentInstances`, `UndecidableInstances`, 非推奨の型システムハック等) は原則として使用しません。

---

## 3. 型シグネチャ

### 3-1. 必ず書く

トップレベル関数はもちろん、**`where` の中の補助関数にも型シグネチャを必ず付ける**のが基本原則です。
`where` 内で外側の型変数を参照する場合は `ScopedTypeVariables` と `forall` を併用します。

```haskell
processDocument :: forall m. MonadIO m => DocumentId -> m Result
processDocument docId = ...
  where
    validateAction :: DocConfig -> m Bool
    validateAction cfg = ...
```

### 3-2. 長いシグネチャの改行

制約 (`=>`) の後ろで改行し、引数を4スペース下げて `->` を行頭に揃えます。

```haskell
ensureDirectoryFromSource :: MonadIO m =>
       Text
    -> OsPath
    -> OsPath
    -> Permissions
    -> UserName
    -> m Result
```

短ければ1行、あるいは型クラス制約の後ろで1回だけ改行する形でも構いません。

---

## 4. 型設計

### 4-1. 型によるコンパイル時保証と可読性のバランス

- **型を品質向上に最大限活用する**:
  - テストコードだけに頼るのではなく、型システムによって不変条件やドメインの制約を表現し、できる限り**コンパイル時に多くの正しさを保証**できるように設計します。
  - テストコードに頼らずに型で保証できるものは、積極的に型で保証します（「Make Illegal States Unrepresentable / 不正な状態を表現不可能にする」）。
- **可読性とのトレードオフ（可読性を優先）**:
  - ただし、型を用いることで**コードの可読性や見通しの良さが犠牲になる場合は、可読性を優先**します。
  - 過度に難解な型レベルプログラミングや複雑すぎる型クラスの多用によってコードの意図が伝わりにくくなることは避け、チーム全体にとっての保守性・理解しやすさとのバランスを保ちます。

### 4-2. プリミティブを裸で引き回さない

意味の異なる値は `newtype` で厳密に区別します。`deriving` で必要なインスタンスだけを継承します。

```haskell
newtype UserName = UserName ByteString
    deriving (Show, Eq, Ord, IsString, Semigroup, Monoid, ToText)
```

`Text` や `ByteString` 等の生プリミティブをそのまま引数に取る関数は、境界での変換処理や起点・終点だけに留めます。

### 4-3. 変換は型クラスに集約する

`ToText` / `ToByteString` のように「変換の意味」ごとに型クラスを作り、`DefaultSignatures` と `coerce` で newtype 用の既定実装を与えます。

```haskell
class ToText a where
    toText :: a -> Text
    default toText :: Coercible a Text => a -> Text
    toText = coerce
```

### 4-4. `Show` は `ToText` に委譲する

表示形式の定義が分散するのを防ぐため、文字列表現を持つ型では `ToText` に委譲して定義することを推奨します。

```haskell
instance Show OwnerDetail where
    show = Txt.unpack . toText
```

### 4-5. レコードアクセスは `OverloadedRecordDot`

従来のトップレベルフィールドセレクタ関数（`fieldName record`）ではなく、`record.fieldName` を使用します。
名前空間の汚染を防ぎ、他型と同名フィールドを持てるよう `DuplicateRecordFields` と `NoFieldSelectors` を併用します。

### 4-6. 「あり得ない状態」は型で消す

`DataKinds` + `TypeFamilies` を活用し、検証前後・取得前後の状態を型レベルで厳密に区別します。

```haskell
data CheckStatus = Unchecked | Checked

type family Payload (status :: CheckStatus) :: Type where
    Payload 'Unchecked = RawInput
    Payload 'Checked   = ValidatedData
```

この記法を採用した場合、自動で `deriving` されないため、必要に応じて `deriving instance Show ...` を型引数ごとに明示します。

### 4-7. パスは `OsPath` 系で扱う（`FilePath` を避ける）

`FilePath` は `String` (`[Char]`) の型別名にすぎず、パスをただの文字列として扱う古い表現です。

- **エンコーディングの欠落**: OS が実際に扱うバイト列 (POSIX) や UTF-16 (Windows) を `String` へ変換する過程で、不正な文字・バイト列を含むファイル名が破壊されます。
- **型の曖昧性**: パスなのか任意の文字列なのかがコンパイラから識別できません。
- **リソース効率**: `[Char]` はヒープとGCへの負荷が大きくなります。

したがって、**`OsPath` ベースの API を原則として優先**してください。

```haskell
-- ○ OsPath ベース
import qualified System.OsPath as OS
OS.takeDirectory path

-- × FilePath（String）ベース
import qualified System.FilePath as FilePath
FilePath.takeDirectory path
```

主な対応関係:

| 従来の `FilePath` 系 | 推奨 `OsPath` 系 |
| --- | --- |
| `System.FilePath` | `System.OsPath` |
| `System.Directory` | `System.Directory.OsPath` |
| `System.IO`（`readFile` など） | `System.File.OsPath` |
| `System.Posix.Files` | `System.Posix.Files.PosixString` |
| `System.Posix.Process` | `System.Posix.Process.PosixString` |
| `System.Posix.Env` | `System.Posix.Env.PosixString` |

**`OsPath` 版が存在しない外部ライブラリとの連携時のみ** `String` を経由します。その際は、**なぜ経由するのかをコードコメントに必ず残してください**。

```haskell
-- getExecutablePath だけは OsPath 版が base に無いため String を経由してエンコードする
exePath <- OS.encodeFS =<< Env.getExecutablePath
```

`OsPath` から `PosixPath` へキャストする際は `coerce` を利用します（コンストラクタ参照のため `System.OsString.Internal.Types` を import）。

---

## 5. 純粋性と副作用

### 5-1. 純粋関数の徹底

- 可能な限り純粋関数 (`pure`) でロジックを構成し、`IO` などのモナドによる副作用はシステムの端点 (境界) に閉じ込めます。
- ビジネスロジックを純粋関数として記述することで、推論の容易性とテスト性を最大化します。

### 5-2. パターンマッチで引数数に依存させない `{}` 構文

「どのコンストラクタか」だけを判別したく、フィールド値に用がないときは、`_` を引数の数だけ並べるのではなく `{}` で受けます。

```haskell
merge res@Error{}     _               = res
merge _               res@Error{}     = res
merge res@Changed{}   _               = res
merge _               res@Changed{}   = res
```

`(Error _)` のように記述すると、後からコンストラクタにフィールドを追加した際、**値を使っていない無関係な箇所まで一斉にコンパイルエラー**になります。`{}` を使えば定義変更に強いコードになります。

フィールド値を利用する場合は、`{}` で受けてから `OverloadedRecordDot` で取り出します。

```haskell
formatResult res@Error{} = "error: " <> toText res.errorDetail
```

中身のパターンで分岐する場合は、通常どおり記述して構いません。

```haskell
formatResult (Skipped Nothing)     = "skipped"
formatResult (Skipped (Just prev)) = "skipped [" <> toText prev <> "]"
```

---

## 6. メモリとパフォーマンス

### 6-1. スペースリーク (Space Leak) の防止

- Haskell では不要なサンクの蓄積によるメモリ肥大化が起こりやすいため、正格評価 (`seq`, `BangPatterns`, 正格データ型) を適切に使用します。
- データ型のレコードフィールドには原則として正格性フラグ (`!`) を付与することを検討してください。
- リストの畳み込みには遅延評価の `foldl` を厳禁とし、正格な `foldl'` または適切なストリーミング処理を用います。

### 6-2. 計算量とデータ構造の選択

差分計算や大量テキスト・バイナリ処理では、適切なデータ構造を選択します。

- テキストデータ: `Data.Text` (UTF-8)
- バイナリデータ: `Data.ByteString`
- 連続メモリ領域・高速添字アクセス: `Data.Vector` (非ボックス化可能な場合は `Data.Vector.Unboxed`)
- キー・値マップ: `Data.Map.Strict` または `Data.HashMap.Strict`

---

## 7. エラー処理

### 7-1. 例外型はモジュールごとに定義する

```haskell
data SyncError
    = SourceFileNotFound OsPath
      -- ^ コピー元のファイルが存在しない
    | DocumentNotFound DocumentId
      -- ^ 指定された文書が見つからない
    deriving (Show, Eq)

instance Exception SyncError
```

### 7-2. `error` / 部分関数の原則禁止

- `error` を使ってよいのは「型では表現しきれないが、呼び出し規約上絶対にあり得ない」場合のみです。その場合も、なぜ起こらないかをコードコメントに明記してください。
- `head`, `tail`, `fromJust`, 不完全なパターンマッチ等の部分関数 (Partial Functions) は一切使用せず、`Maybe` / `Either` で安全にハンドリングします。

### 7-3. 失敗は戻り値の型で表す

例外を無秩序に投げず、失敗可能性を `Either` やドメイン固有の結果型 (`Result`) として関数の戻り値で明示する設計を最優先します。

---

## 8. 命名規則

| 対象 | 命名規則 | 例 |
| --- | --- | --- |
| 関数・変数 | lowerCamelCase | `ensureDirectoryExist` |
| 型・コンストラクタ | UpperCamelCase | `PresentStatus`, `DocumentId` |
| `Maybe a` の値 | `m` または `maybe` を前置 | `mUser`, `maybePreviousResult` |
| `Either e a` の値 | `e` を前置（二重なら `ee`） | `eRet`, `eeResult` |
| `IORef` | `Ref` を後置 | `ctxRef`, `logBufferRef` |
| 低レベル版・生の値 | `Raw` を後置 | `putLogRaw`, `rawFileMode` |
| newtype の展開フィールド | `unWrap` | `newtype Stack = Stack { unWrap :: [Item] }` |
| 未使用の引数 | `_` を前置 | `_userName`, `_key` |

---

## 9. コメント

- 型・コンストラクタ・フィールドの説明は Haddock 記法（`-- |` / `-- ^`）で記述する。
- **日本語で記述してよい**。
- 「何をしているか」ではなく、「**なぜそうしたか**（設計意図、アルゴリズム選定理由、制約事項）」を記述する。
- **コメントアウトした不要コードを残さない**（履歴は git に保持されるため、削除する）。

---

## 10. テスト規約

- **型による保証との役割分担**:
  - 型システムで静的に保証できる制約は型に委ね、テストコードは型では表現しきれないビジネスロジックの複雑な振る舞いやエッジケース、入出力の相互作用の検証に集中させます。
- **テストフレームワーク**:
  - `hspec` を標準とし、仕様を記述する単体テストを整備する。
  - プロパティテストには `QuickCheck` を組み合わせ、不変条件やエッジケースを網羅する。
- **テストケースの自己文書化**:
  - `describe` や `it` のブロック名には、期待される振る舞いや前提仕様を自然言語で明確に表現する。

---

## 11. コンパイラ警告の扱い (-Werror)

本プロジェクトのコードは `-Wall -Werror` (警告をエラーとして扱う) でビルドされます。一切のコンパイラ警告を残してはなりません。

特に踏みやすい警告:

- **未使用の束縛** (`-Wunused-top-binds`, `-Wunused-matches`):
  エクスポートしていないトップレベル関数を記述して呼び出さない場合や、使っていない引数がある場合に発生。
- **名前の隠蔽** (`-Wname-shadowing`):
  `where` 節やラムダの内側で、外側のスコープやトップレベルと同じ変数名を再定義した場合に発生。
- **不完全なパターンマッチ** (`-Wincomplete-patterns`):
  すべてのコンストラクタやガードケースを網羅していない場合に発生。

どうしても局所的に特定の警告を抑止せざるを得ない場合は、その理由を必ずコードコメントに明記してください。
