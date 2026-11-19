# DEV.md — 開発者向けガイド

本ドキュメントは、本リポジトリでの環境構築、日常の開発サイクル、および各種コマンドの利用方法を説明します。

---

## 1. 前提条件

以下のいずれかの実行環境を用意してください。

- **ホスト環境 (推奨: Antigravity 標準サンドボックス対応)**: `stack`（またはホスト単独利用時の `cabal`）および `hlint` がインストールされていること。
- **Docker 環境 (再現性重視・ホスト非汚染)**: **Docker** および **Docker Compose** がインストールされていること（コンテナ内は `stack` + `hlint` 標準構成）。
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
| `scripts/check.sh` | build → test → lint を一括実行 |
| `scripts/build.sh` | ビルドの実行 |
| `scripts/test.sh` | テストスイートの実行 |
| `scripts/lint.sh` | 静的解析 (Lint) の実行 |
| `scripts/run-server.sh` | ローカル開発用サーバの起動 (ポート 8080) |
