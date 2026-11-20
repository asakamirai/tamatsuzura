# PROJECT.md

プロジェクトの**現在地**と**次に行うべきタスク**を記録するドキュメントです。
本ファイルは常に短く簡潔に保ちます (目安 80 行以内)。
完了したタスクの詳細な記録は [project/history/](./project/history/) にマイルストーン単位で切り出します。

作業の進め方および実装完了条件は [AGENTS.md](../AGENTS.md) を参照してください。

---

## 現在地

**M3（認証・ユーザ管理: CGI 互換 SQLite セッション）進行中。Phase 2 完了、次は Phase 3（Miso クライアントへの認証画面統合と疎通検証）へ着手予定。**

| マイルストーン | 内容 | 状態 | 完了記録 |
| --- | --- | --- | --- |
| M0 | プロジェクト骨格と開発基盤の整備 | **完了** | [M0.md](./project/history/M0.md) |
| M1 | MVP サーバ (文書 CRUD + CGI / Warp 両対応) | **完了** | [M1.md](./project/history/M1.md) |
| M2 | MVP クライアント (モバイルファースト Miso/Wasm UI) | **完了** | [M2.md](./project/history/M2.md) |
| **M3** | **認証・ユーザ管理 (CGI 互換 SQLite セッション)** | **進行中** | — |
| M4 | 差分管理 Phase 1 (リビジョン履歴) | 未着手 | — |
| M5 | Haskell 拡張機構 (MVP: 同梱拡張) | 未着手 | — |
| M6 | PWA / オフライン対応と同期 | 未着手 | — |
| M7 | 差分管理 Phase 2 (リビジョン比較・マージ) | 未着手 | — |

※ 各マイルストーンの詳細な目的・完了条件は [project/milestones.md](./project/milestones.md) を参照してください。

---

## 次のタスク

**M3: 認証・ユーザ管理 (CGI 互換 SQLite セッション)**

- [x] Phase 1: 認証ドメイン型・パスワードハッシュ・SQLite ユーザー/セッション永続化および OpenAPI 仕様の拡張
- [x] Phase 2: 登録・ログイン・ログアウト API、Cookie セッション認証ミドルウェア、CSRF 対策および文書所有者分離の実装（CGI / Warp 両検証）
- [ ] Phase 3: Miso クライアントへの認証画面（新規登録・ログイン・ログアウト・401 連携）統合と疎通検証

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
