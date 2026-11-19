# 技術スタック選定方針

本ドキュメントは、TamaTsuzura で採用する技術スタック、選定理由、および開発前提を定義します。

---

## 1. コア技術一覧

| レイヤー | 技術 / ツール | 選定理由・目的 |
| --- | --- | --- |
| **主要言語** | Haskell (GHC 9.x 系列) | 強力な静的型付け、純粋関数型モデルによる不変性、サーバ・クライアント・拡張間の型共有 |
| **ビルドツール（サーバ / 共通）** | Stack / Cabal | 再現性の高いパッケージ管理と依存関係の固定（M0 Phase 1 で詳細構成を確定） |
| **ビルドツール（クライアント Wasm）** | GHC Wasm バックエンド (`wasm32-wasi-ghc` / `wasm32-wasi-cabal` / `ghc-wasm-meta`) + `wasm-opt` | Miso および共通 Haskell コードの WebAssembly コンパイルとバイナリ最適化 |
| **バックエンド** | WAI (`wai-extra` の `Network.Wai.Handler.CGI` + `warp`) / Servant | **CGI 最優先**かつ Warp スタンドアロン両対応の型安全な Web API 定義 |
| **データベース** | SQLite (`direct-sqlite` 等、WAL + `busy_timeout`) | レンタルサーバ・CGI 環境で単一ファイル運用でき、ACID トランザクションと並行読み書きに強い |
| **フロントエンド UI** | Miso (Haskell / GHC Wasm) + 薄い JSFFI グルー | `src/common/` の型をクライアント UI・同梱拡張までエンドツーエンド共有 |
| **エディタコア** | CodeMirror 6（薄い JSFFI 経由） | モバイルブラウザ対応、高い編集性能、拡張性、軽量性 |
| **コンテナ環境** | Docker / Docker Compose | 開発者およびエージェントの環境差異をなくす再現性の高い実行基盤 |
| **AI エージェント** | Antigravity (AGY) | ルール (`AGENTS.md` / `src/AGENTS.md`)、スキル、フック、サンドボックス実行による開発自動化 |

---

## 2. 開発・実行方針

- `scripts/` 配下のラッパースクリプト (`scripts/check.sh` 等) を統一窓口とし、ホスト側からは単一コマンドでビルド・テスト・Lint を完結できるようにします。
- ホストに `stack` / `hlint` 等のツールが導入されている環境ではホスト側ツールを優先して高速かつ Antigravity 標準サンドボックス内で実行し、未導入環境やクリーンな再現性検証時 (`USE_DOCKER=1`) には Docker コンテナへフォールバックします。
- Wasm クライアントのビルドツールチェーン（`wasm32-wasi-cabal` / `wasm-opt` 等）のホスト・Docker 統合方法は M0 Phase 1〜2 で具体的に整備します。
