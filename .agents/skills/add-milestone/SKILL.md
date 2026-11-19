---
name: add-milestone
description: >-
  Adds, splits, reorders, updates, or archives project milestones in TamaTsuzura
  while keeping docs/project/milestones.md, docs/PROJECT.md, docs/project/backlog.md,
  and docs/project/history/ consistent. Use whenever the user asks to add a new
  milestone, change milestone scope or ordering, or record a completed milestone.
---

# Add / Update Milestone

TamaTsuzura のマイルストーン計画を追加・変更・完了アーカイブする際、関連ドキュメント間の整合性を漏れなく維持するための手順です。

## 手順

### 1. 現状把握

以下のドキュメントを読み、現在のマイルストーン一覧・進捗・先送り課題を確認する。

- [docs/PROJECT.md](../../../docs/PROJECT.md)
- [docs/project/milestones.md](../../../docs/project/milestones.md)
- [docs/project/backlog.md](../../../docs/project/backlog.md)
- 必要に応じて [docs/REQUIREMENTS.md](../../../docs/REQUIREMENTS.md) および [docs/project/architecture.md](../../../docs/project/architecture.md)

### 2. スコープと要件の確認

マイルストーン番号・目的・完了条件・既存マイルストーンとの依存関係・バックログ項目の取り込み有無に曖昧な点があれば、`ask_question` を使ってユーザーへ確認する。

### 3. 関連ドキュメントの同期更新

1. **`docs/project/milestones.md`**:
   - 既存フォーマット（`目的` / `作業項目` / `完了条件`）に沿ってマイルストーンを追加または更新する。
   - 番号の連番と依存順序に矛盾がないようにする。
2. **`docs/PROJECT.md`**:
   - マイルストーン一覧テーブルを更新する。
   - 本ファイルは **目安 80 行以内** を厳守する。
   - 着手対象が変わる場合は「現在地」「次のタスク（Phase チェックリスト）」も更新する。
3. **`docs/project/backlog.md`**:
   - 新規マイルストーンで解消対象となる課題があれば整理・紐付けを行う。
   - スコープ調整により先送りした項目があれば「課題 / 出所 / 再検討のトリガ」を追記する。
4. **`docs/project/history/`（完了アーカイブ時のみ）**:
   - 完了したマイルストーンの詳細記録を `docs/project/history/M<N>.md` として作成し、`docs/PROJECT.md` の「完了記録」列から相対リンクする。

### 4. 整合性検証

1. `docs/PROJECT.md` の行数が目安（80行）を大きく超過していないか確認する。
2. 変更したドキュメント内の相対リンク・マイルストーン番号・表記（プロジェクト名は `TamaTsuzura`）に誤りがないか確認する。
3. スコープ変更が大きい場合は `code-review` スキルに従い読み取り専用サブエージェントによる整合性レビューを実施する。

### 5. コミット準備

`git status` と `git diff` を確認し、`AGENTS.md` のコミット規約（例: `docs: Add milestone M8 for ...`、Body に追加・変更理由とトレードオフを明記）に従ってコミットを作成する。
