# DEPLOY.md — デプロイメントガイド

本ドキュメントは、TamaTsuzura の本番・検証環境へのデプロイ形態および検証手順の概要を定めます。

---

## 1. デプロイ形態

本システムは以下の 2 つの形態をサポートし、**レンタルサーバ等の CGI 環境での運用を最優先** とします。

1. **CGI + 静的配信環境（最優先・主軸）**:
   - C 言語互換 CGI として動作する Haskell バイナリ（`wai-extra` の `Network.Wai.Handler.CGI` ベース）を `/api/*` に配置。
   - HTML / JS / CSS / Wasm の静的フロントエンドアセットは Web サーバ（Apache / Nginx 等）から直接配信。
   - SQLite DB ファイルは Web 公開ディレクトリ外に配置し、WAL モード + `busy_timeout` で運用。
2. **スタンドアロン HTTP サーバ / Docker コンテナ**:
   - Warp HTTP サーバ（`warp` ベース）と静的アセットを同梱した構成。ローカル開発や VPS / リバースプロキシ（Nginx / Cloudflare 等）背後での常駐運用に使用。

---

## 2. デプロイ後の手動検証

デプロイ完了後は、必ず [MANUAL_TESTS.md](./MANUAL_TESTS.md) に基づき、最低限 MT-1（サインインと文書保存）の検証を実施してください。
検証結果は `MANUAL_TESTS.md` の実施記録テーブルに追記します。
