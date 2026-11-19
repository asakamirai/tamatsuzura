# src/AGENTS.md

`src/` 配下のコードを作成・修正・レビューする際に適用される実装ガードレールです。
詳細は [docs/coding_rules_project.md](../docs/coding_rules_project.md) および [docs/coding_rules_haskell.md](../docs/coding_rules_haskell.md) を参照してください。

## パッケージ境界 (依存ルール)

- `src/common/`: **外部フレームワーク (Warp, Miso, SQLite 等) および `IO` 副作用を一切禁止**。完全な純粋関数として実装し、`hspec` + `QuickCheck` でテストする。
- `src/server/`: 内部パッケージは `src/common/` のみに依存。`src/client/` への依存禁止。
- `src/client/`: コア UI・ロジックの内部依存は `src/common/` と `src/extension-api/` のみ。同梱拡張を登録するエントリポイント／レジストリ結線層に限定して `src/extensions/` への片方向依存を許可する。`src/server/` 内部実装への直接依存禁止（Web API 経由）。
- `src/extension-api/`: 内部パッケージは `src/common/` のみに依存。後方互換性を厳格に維持する。
- `src/extensions/`: `src/extension-api/`（および必要に応じ `src/common/`）のみに依存し、`src/client/`・`src/server/` への依存を禁止する。

## Haskell 必須チェックポイント

- **警告ゼロ (`-Wall -Werror`)**: 未使用束縛・名前隠蔽・不完全パターンマッチを含むあらゆる警告を残さない。
- **名前空間・モジュール構成**: ルート名前空間は `TamaTsuzura.*`。エクスポートリストおよび非 qualified import のインポートリストを明示する。
- **スタイル**: スペース 4 つ（`where` は 2 スペース浅く中身は +4）、縦揃え（`=`, `->`, レコード、import）、先頭カンマ、ラムダは `\ x ->`（バックスラッシュ後に空白）。
- **型シグネチャと型設計**: トップレベルおよび `where` 内の補助関数にも型シグネチャを必須とする。生プリミティブを引き回さず `newtype` / `DataKinds` / `TypeFamilies` で不正状態を型で排除する（ただし過度な難解化より可読性を優先）。
- **パスとレコード**: `FilePath` (`String`) を避け `OsPath` 系 API を優先する。レコードアクセスは `OverloadedRecordDot`（`DuplicateRecordFields` + `NoFieldSelectors`）、コンストラクタ判別のみのパターンマッチは `{}` を使用する。
- **安全性と性能**: `error`・部分関数 (`head`, `tail`, `fromJust` 等)・遅延 `foldl` を禁止（`Either`/`Maybe` と正格な `foldl'` を使用）。レコードフィールドへの `!` 付与を検討する。
- **文書化**: エクスポートするすべての公開型・関数・クラスに Haddock (`-- |` / `-- ^`) を付与し、設計意図や不変条件を記述する。
