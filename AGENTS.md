# AGENTS.md

TamaTsuzura リポジトリで作業する Antigravity AI エージェントおよび開発者向けの基本指示書です。
セッション開始時に自動的に読み込まれます。運用上の変更が必要になった場合は適宜更新してください。

---

## 1. 最初に読むもの

| ドキュメント | 用途・タイミング |
| --- | --- |
| [docs/PROJECT.md](docs/PROJECT.md) | 現在地・進行中のマイルストーン・次の Phase。**毎回最初に必ず読むこと** |
| [docs/CODING_RULES.md](docs/CODING_RULES.md) | 規約の案内（[プロジェクト規約](docs/coding_rules_project.md) / [Haskell規約](docs/coding_rules_haskell.md)）。**コード変更前に読むこと**（`src/` 配下の作業時は [src/AGENTS.md](src/AGENTS.md) も自動適用される） |

---

## 2. オンデマンド参照

全文を無差別に読まず、タスクに必要な節だけを参照してください。

| ドキュメント | 参照するタイミング |
| --- | --- |
| [docs/REQUIREMENTS.md](docs/REQUIREMENTS.md) | プロダクト全体の機能要件・制約を確認・調整するとき |
| [docs/project/milestones.md](docs/project/milestones.md) | 各マイルストーンの詳細スコープ・完了条件を確認するとき |
| [docs/project/backlog.md](docs/project/backlog.md) | 意図的な先送り課題・技術的負債を確認または追記するとき |
| [docs/project/architecture.md](docs/project/architecture.md) | 全体アーキテクチャ・データフローを確認・改定するとき |
| [docs/project/features.md](docs/project/features.md) | 各機能の設計方針・データ構造を確認・改定するとき |
| [docs/project/tech_stack.md](docs/project/tech_stack.md) | 採用技術や選定理由を確認するとき |
| [docs/MANUAL_TESTS.md](docs/MANUAL_TESTS.md) | 自動テスト不可領域（UI/Wasm/オフライン/デプロイ等）を検証・手順化するとき |
| [docs/DEV.md](docs/DEV.md) / [docs/DEPLOY.md](docs/DEPLOY.md) | 開発環境・ビルド実行・デプロイ手順を確認するとき |
| `git log` | 過去の設計意図やトレードオフを調査するとき |

---

## 3. コマンド体系と実行方針

ビルド・テスト・Lint は `scripts/` 配下のラッパースクリプトを**単独コマンド**で実行します（パイプラインやコマンド置換を繋がない）。

| コマンド | 用途 |
| --- | --- |
| `scripts/check.sh` | build → test → lint を一括実行。全ログは `.check.log` に出力し、失敗時は末尾 30 行を表示する（詳細は `.check.log` を確認） |
| `scripts/build.sh` | プロジェクト全体のビルド |
| `scripts/test.sh` | テストスイートの実行 |
| `scripts/lint.sh` | 静的解析 (`hlint`) の実行 |
| `scripts/run-server.sh` | 開発用サーバの起動 |

### サンドボックス / Docker 実行方針
- 各スクリプトはホストに `stack` / `hlint` がある場合はホスト側ツールを優先し、未導入環境または `USE_DOCKER=1` 指定時に Docker Compose (`docker/docker-compose.yml`) へフォールバックします。
- まず標準サンドボックス (`BypassSandbox: false`) で実行し、Docker ソケット (`/var/run/docker.sock`) へのアクセスや依存取得でブロックされた場合のみ最小範囲で `BypassSandbox: true` を使用します。

---

## 4. ディレクトリ構造 (構成方針)

M0 完了時に実態に合わせて更新してください（詳細は [docs/project/architecture.md](docs/project/architecture.md)）。

- `src/common/`: クライアント・サーバ共通の純粋データ型、差分計算、バリデーション
- `src/server/`: バックエンド HTTP サーバ、SQLite 永続化、認証
- `src/client/`: フロントエンド UI、エディタコア統合、オフライン同期 (Wasm/Miso)
- `src/extension-api/`: 拡張機能開発者向けの公開インターフェース型定義
- `src/extensions/`: サンプル拡張機能の実装
- `src/AGENTS.md`: `src/` 配下編集時に自動適用される実装ガードレール
- `docs/`: 要件・計画・コーディング規約・API 仕様・手動検証手順
- `scripts/`: ビルド・テスト・Lint 等の共通ラッパースクリプト
- `docker/`: 開発用 Dockerfile および `docker-compose.yml`
- `.agents/`: Antigravity カスタマイズ（`skills/`, `hooks.json`, `hooks/`）

---

## 5. 作業の進め方

1. **現在地確認**: [docs/PROJECT.md](docs/PROJECT.md) を読み、対象マイルストーンと次の Phase を把握する。
2. **Phase 分割**: タスクが大きい場合は複数の Phase に分解して [docs/PROJECT.md](docs/PROJECT.md) の「次のタスク」にチェックリスト化する。マイルストーンの追加・再編はスキル `add-milestone` を用いる。
3. **実装・作業**: 各 Phase の作業を実施する。要件の曖昧さや設計の分岐点は独断で進めず、`ask_question` 等で選択肢を整理してユーザーへ確認する。
4. **完了条件の充足**: 下記「6. 完了条件」をすべて満たすまで完了とみなさない。
5. **進捗更新・記録**: [docs/PROJECT.md](docs/PROJECT.md) を更新し、Phase 単位でコミットする。

---

## 6. 完了条件

### 6-1. 実装完了条件 (コード変更を伴う Phase)

以下 6 条件をすべて満たした時点で完了とします。

1. **テスト**: `scripts/test.sh` がエラーゼロで全件パスすること。
2. **Lint**: `scripts/lint.sh` が警告ゼロでパスすること（1〜2 は `scripts/check.sh` で一括検証可）。
3. **AI コードレビュー**: スキル `code-review` を実行し、客観的な品質・設計・規約チェックを通過すること（下記 6-3 参照）。
4. **再検証**: 指摘修正でコードを変更した場合は、必ず 1〜3 を再通過すること。
5. **ドキュメント同期**:
   - Web API 変更時: [docs/api/openapi.yaml](docs/api/openapi.yaml) を同期更新。
   - 先送り課題・技術的負債発生時: [docs/project/backlog.md](docs/project/backlog.md) に追記。
   - 自動テスト不可の挙動を追加・変更・廃止時: [docs/MANUAL_TESTS.md](docs/MANUAL_TESTS.md) を同一コミットで同期更新。
   - タスク進捗: [docs/PROJECT.md](docs/PROJECT.md) を更新。
6. **コミット**: 下記「8. コミット規約」に従って作成すること。

### 6-2. 実装以外の成果物（設計・仕様書・手順書・設定等）

1. **整合性確認**: 既存の要件・アーキテクチャ・関連ドキュメントと矛盾がないこと。
2. **AI サブエージェントレビュー**: 必要に応じて 6-3 に従い独立コンテキストの客観レビューを受けること。
3. **指摘反映・再検証**: 指摘事項を反映し、品質を確認すること。
4. **進捗更新とコミット**: [docs/PROJECT.md](docs/PROJECT.md) を更新し、規約に沿ってコミットすること。

### 6-3. AI レビュー共通ルール (サブエージェント運用・モデル選定)

- **読み取り専用サブエージェントによる客観性の担保**:
  - 作成者バイアスを排除するため、独立したコンテキストを持つ読み取り専用サブエージェント（組み込み `research`、または `define_subagent` で `EnableWriteTools: false` とした専用レビュアー）でレビューを実施する。
- **モデル選定**:
  - 論理矛盾・エッジケース・潜在的欠陥を見抜くため、利用可能な選択肢の中に現在より高度な思考を得意とするモデルがある場合はそちらを指定し、それ以外は既定の `inherit` を使用する（クォータ制限等で使用できない場合は利用可能な範囲で最適なモデルを使用する）。

---

## 7. 手動検証の原則

ブラウザ UI・Wasm 実行・オフライン同期・実デプロイなど自動テストで網羅できない挙動は、[docs/MANUAL_TESTS.md](docs/MANUAL_TESTS.md) に第三者がそのまま再現できる手順として保守します。

- 「目視確認が必要」とだけ書いて放置することは禁止。
- 追加・更新（変更と**同一コミット**）・廃止時削除を徹底する。エージェントが実機確認できない場合でも、正確な手順書の作成・更新までを責務とする。

---

## 8. コミット規約

- **粒度**: 原則として Phase 単位（または独立した 1 タスク単位）。
- **Subject**: 英語 1 行で簡潔に記述し、先頭に対象マイルストーンまたは領域を明記する。
  - 例: `M1: Implement document CRUD REST API on Warp server`
  - 例: `docs: Add milestone M4 for revision diff management`
- **Body**: 「何をしたか」の羅列ではなく、**なぜその設計にしたか・何を割り切ったか（トレードオフ）** を中心に記述する。
- **クリーンなツリー**: コミット前に `git status` で一時ファイルや不要な未追跡ファイルがないことを確認する。

---

## 9. Antigravity カスタマイズ

- **スキル (`.agents/skills/`)**:
  - `code-review`: 実装・成果物の体系的な客観レビュー手順。
  - `add-milestone`: マイルストーン追加・変更・完了アーカイブ時のドキュメント同期手順。
- **フック (`.agents/hooks.json`)**:
  - `SessionStart` で現在のマイルストーン・残タスクを軽量通知する。ビルド等の重い処理は同期ブロックを避けるためフックには組み込まない。
