# DEV.md — 開発者向けガイド

本ドキュメントは、本リポジトリでの環境構築、日常の開発サイクル、および各種コマンドの利用方法を説明します。

---

## 1. 前提条件

以下のいずれかの実行環境を用意してください。

- **ホスト環境 (推奨: Antigravity 標準サンドボックス対応)**: `stack`（またはホスト単独利用時の `cabal`）、`hlint`、Wasm ビルド用 `wasm32-wasi-cabal`（`~/.ghc-wasm/env` 自動読込対応）、およびフロントエンド CodeMirror 6 バンドル用の `node` / `pnpm` がインストールされていること。
- **Docker 環境 (再現性重視・ホスト非汚染)**: **Docker** および **Docker Compose** がインストールされていること（コンテナ内は `stack` + `hlint` + GHC 9.12 Wasm `ghc-wasm-meta` + `node`/`pnpm` 標準構成。成果物・依存は `.stack-work-docker/` / `dist-newstyle-docker/` および Docker 名前付きボリューム `web-node-modules` へ分離されるためホスト環境と競合しない）。
  - `scripts/*.sh` はホストに Haskell ツールがあればホスト側を優先し、未導入環境または `USE_DOCKER=1` 指定時に自動で Docker Compose (`docker/docker-compose.yml`) を使用します。
  - Antigravity で Docker 経由実行を行う場合は、`/var/run/docker.sock` アクセスのため標準サンドボックス外 (`BypassSandbox: true`) での実行が必要になります。

---

## 2. 日常の開発サイクル

開発は以下のサイクルを基本とします。

1. **タスクの確認**: [PROJECT.md](./PROJECT.md) で現在地と着手する Phase を確認。
2. **コードの変更**: `src/` 配下のソースコードやテストコードを修正。
3. **検証**:
   ```bash
   scripts/check.sh
   ```
   - ビルド、テスト、Lint が順に実行されます。
   - 失敗した場合は `.check.log` を確認して原因を解消してください。
4. **完了条件の確認**: [AGENTS.md](../AGENTS.md) に従い、AI レビュー（サブエージェント活用）、ドキュメント同期、コミットを行います。

---

## 3. スクリプト一覧

| コマンド | 説明 |
| --- | --- |
| `scripts/check.sh` | build（ネイティブ + Wasm）→ test → lint を一括実行 |
| `scripts/build.sh` | プロジェクト全体のビルド（ネイティブ + Wasm。`SKIP_WASM=1` でネイティブのみ可） |
| `scripts/build-wasm.sh` | GHC Wasm クライアント（`cabal.project.wasm`）単体ビルド、`post-link.mjs` 生成、CodeMirror 6 バンドル (`editor-bridge.js`) 出力、および `dist/web/` への配信アセット一式出力 |
| `scripts/test.sh` | テストスイートの実行 |
| `scripts/lint.sh` | 静的解析 (Lint) の実行 |
| `scripts/run-server.sh` | ローカル開発用サーバの起動 (ポート 8080、`dist/web/index.html` 存在時は自動的に静的アセット配信を有効化) |

---

## 4. サーバ起動オプションと環境変数

Warp サーバ (`tamatsuzura-server`) および CGI バイナリ (`tamatsuzura-cgi`) は以下の環境変数・CLI オプションをサポートします。同一項目が両方指定された場合は CLI 引数が優先されます。

| 項目 | 環境変数 | Warp CLI 引数 | 既定値 |
| --- | --- | --- | --- |
| 待受ポート | `PORT` | `--port PORT` | `8080` |
| SQLite DB パス | `TAMATSUZURA_DB_PATH` | `--db-path PATH` | `data/tamatsuzura.sqlite3` |
| SQLite `busy_timeout` (ms) | `TAMATSUZURA_DB_BUSY_TIMEOUT_MS` | `--busy-timeout-ms MS` | `5000` (範囲: `0..300000`) |
| 静的アセット配信ディレクトリ (Warp 専用) | `TAMATSUZURA_STATIC_DIR` | `--static-dir DIR` | 未指定 (`scripts/run-server.sh` は `dist/web/index.html` 存在時に `dist/web` を自動設定) |
