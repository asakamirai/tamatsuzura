---
name: code-review
description: >-
  Performs an objective, systematic review of code changes or design/documentation
  deliverables in TamaTsuzura using an independent read-only subagent. Use before
  completing any implementation Phase, after fixing review feedback, or when
  verifying significant design, architecture, or rule documents.
---

# Code & Deliverable Review

実装タスク（Phase）および設計・ドキュメント成果物の完了条件を満たすため、独立コンテキストの読み取り専用サブエージェントによる客観レビューを実施する手順です。

## 手順

### 1. レビュー対象と前提検証の確認

1. `git status` および `git diff`（未追跡ファイルがある場合はその一覧）で変更範囲を特定する。
2. コード変更を含む場合は、事前に `scripts/check.sh` がエラーゼロ・警告ゼロで通過していることを確認する（未実行なら先に実行する）。

### 2. 読み取り専用サブエージェントによるレビュー実行

作成者バイアスを排除するため、`invoke_subagent` で独立した読み取り専用サブエージェントを起動する。

- **種別 (`TypeName`)**: 組み込みの `research`（読み取り専用）、または `define_subagent` で `EnableWriteTools: false` を指定した専用レビュアーを使用する。
- **モデル (`Model`)**: 利用可能な選択肢の中に現在より高度な思考を得意とするモデルがある場合はそちらを指定し、それ以外は既定の `inherit` を使用する（クォータ制限等で使用できない場合は利用可能な範囲で最適なモデルを使用する）。
- **プロンプトに含める情報**:
  - 変更の目的（対象マイルストーン / Phase）
  - 変更・追加されたファイルパス一覧と `git diff` 要約
  - 参照すべき規約ファイル（コード変更時: `docs/coding_rules_project.md`, `docs/coding_rules_haskell.md`, `src/AGENTS.md` / 非コード時: `AGENTS.md`, `docs/PROJECT.md`, 関連設計書）
  - 下記のチェックリストと重要度分類（`Critical` / `Major` / `Minor`）での報告形式

### 3. レビュー観点チェックリスト

#### コード変更時
- **アーキテクチャ境界**: `src/common/` に外部フレームワークや `IO` 副作用が混入していないか。パッケージ間の依存方向が `docs/coding_rules_project.md` に従っているか。
- **Haskell 規約**:
  - `-Wall -Werror` 準拠（警告抑止がある場合は理由コメントがあるか）。
  - レイアウト（4スペース、縦揃え、先頭カンマ、`\ x ->`）、明示的エクスポート・import。
  - トップレベルおよび `where` 内の型シグネチャ、`newtype` 活用、`OsPath` 優先、`{}` パターンマッチ + `OverloadedRecordDot`。
  - `error`・部分関数・`foldl` の排除、正格評価によるスペースリーク防止。
  - 公開 API への Haddock コメント。
- **正当性・テスト**: エッジケース、境界値、並行/エラー処理、単体テスト (`hspec`)・プロパティテスト (`QuickCheck`) の網羅性。
- **同期更新**: Web API 変更時の `docs/api/openapi.yaml`、手動確認項目がある場合の `docs/MANUAL_TESTS.md`、先送り課題の `docs/project/backlog.md`、`docs/PROJECT.md`。

#### 設計・ドキュメント・設定変更時
- **整合性**: 既存要件 (`docs/REQUIREMENTS.md`)、アーキテクチャ、関連ドキュメントおよびスクリプトとの矛盾がないか。
- **正確性**: リンク切れ、プロジェクト名表記ゆれ（正: `TamaTsuzura`）、未存在ファイル・ツールへの誤参照がないか。
- **明瞭さ・実現可能性**: 第三者や後続セッションのエージェントが誤解なく実行できるか。

### 4. 指摘の分類と対応

サブエージェントからの指摘を以下の基準で整理して対応する。

| 重要度 | 基準 | 対応 |
| --- | --- | --- |
| **Critical** | バグ、型/境界違反、仕様矛盾、リンク切れ、完了条件未達 | **必ず修正** |
| **Major** | 規約違反、テスト不足、保守性・安全性の懸念 | **原則修正**（やむを得ず先送りする場合は理由と再検討トリガを `docs/project/backlog.md` に記録） |
| **Minor** | 軽微な表現改善や任意のリファクタ提案 | 妥当なものを反映 |

### 5. 再検証と完了判定

1. 指摘を受けてファイルを変更した場合:
   - コード変更あり: `scripts/check.sh` を再実行し、再度サブエージェントレビューで解消を確認する。
   - 非コード変更のみ: 変更箇所の整合性と関連検証（スクリプト実行や JSON 検証等）を再確認する。
2. 未解決の `Critical` / `Major` 指摘がゼロになった時点でレビュー完了とする。
