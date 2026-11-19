# PROJECT.md

プロジェクトの**現在地**と**次に行うべきタスク**を記録するドキュメントです。
本ファイルは常に短く簡潔に保ちます (目安 80 行以内)。
完了したタスクの詳細な記録は [project/history/](./project/history/) にマイルストーン単位で切り出します。

作業の進め方および実装完了条件は [AGENTS.md](../AGENTS.md) を参照してください。

---

## 現在地

**M0 Phase 0（要件定義の確定）完了。次は M0 Phase 1（リポジトリ構成とビルドツールの初期設定）へ着手予定。**

| マイルストーン | 内容 | 状態 | 完了記録 |
| --- | --- | --- | --- |
| **M0** | **プロジェクト骨格と開発基盤の整備** | **進行中** | — |
| M1 | MVP サーバ (文書 CRUD + CGI / Warp 両対応) | 未着手 | — |
| M2 | MVP クライアント (モバイルファースト Miso/Wasm UI) | 未着手 | — |
| M3 | 認証・ユーザ管理 (CGI 互換 SQLite セッション) | 未着手 | — |
| M4 | 差分管理 Phase 1 (リビジョン履歴) | 未着手 | — |
| M5 | Haskell 拡張機構 (MVP: 同梱拡張) | 未着手 | — |
| M6 | PWA / オフライン対応と同期 | 未着手 | — |
| M7 | 差分管理 Phase 2 (リビジョン比較・マージ) | 未着手 | — |

※ 各マイルストーンの詳細な目的・完了条件は [project/milestones.md](./project/milestones.md) を参照してください。

---

## 次のタスク

**M0: プロジェクト骨格と開発基盤の整備**

- [x] Phase 0: 要件定義の確定 ([REQUIREMENTS.md](./REQUIREMENTS.md) および関連設計書の同期)
- [ ] Phase 1: リポジトリ構成とビルドツールの初期設定 (サーバ向け Haskell + Wasm クライアント向け `wasm32-wasi-cabal` 設定)
- [ ] Phase 2: Docker 開発環境および Wasm 最小ビルドの疎通確認 (`scripts/build.sh`, `scripts/test.sh`, `scripts/lint.sh`)
- [ ] Phase 3: 自動検証・CI スクリプト (`scripts/check.sh`) の稼働確認

※ マイルストーンの新規追加や調整は Antigravity スキル `add-milestone` を利用してください。
完了時は記録を [project/history/](./project/history/) へ移行し、この節を更新します。

---

## 未消化の課題 (Backlog)

意図的に先送りした課題や技術的負債は [project/backlog.md](./project/backlog.md) に集約しています。
マイルストーンの計画策定やタスク着手時には必ず確認してください。

---

## 品質と完了条件

各 Phase の実装および開発成果物は、[AGENTS.md](../AGENTS.md) に定める完了条件 (実装6条件、成果物のサブエージェントレビュー等) を満たすことで完了とします。
手動検証が必要な項目は [MANUAL_TESTS.md](./MANUAL_TESTS.md) に手順化してください。
