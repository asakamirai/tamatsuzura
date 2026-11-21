# DEPLOY.md — デプロイメントガイド

本ドキュメントは、TamaTsuzura の本番・検証環境へのデプロイ形態および検証手順の概要を定めます。

---

## 1. デプロイ形態

本システムは以下の 2 つの形態をサポートし、**レンタルサーバ等の CGI 環境での運用を最優先** とします。

1. **CGI + 静的配信環境（最優先・主軸）**:
   - C 言語互換 CGI として動作する Haskell バイナリ（`tamatsuzura-cgi`、`wai-extra` の `Network.Wai.Handler.CGI` ベース）を `/api/*` に配置。
   - HTML / JS / CSS / Wasm / PWA（`manifest.webmanifest`, `sw.js`, `icons/*`）の静的フロントエンドアセットは Web サーバ（Apache / Nginx 等）から直接配信。
     - **PWA / Service Worker 配信要件**:
       - HTTPS で配信すること（ローカル検証の `localhost` / `127.0.0.1` を除く）。
       - `.wasm` に `application/wasm`、`.webmanifest` に `application/manifest+json; charset=utf-8` の MIME タイプを設定すること。
       - `sw.js`・`index.html`・`manifest.webmanifest` には `Cache-Control: no-cache` を付与し、ブラウザの Service Worker 更新チェックを阻害しないこと。また現行アセット（`.js` / `.css` / `.wasm` 等）はファイル名にコンテンツハッシュを含まないため、更新時のバージョン不整合を防ぐため全静的アセットに `Cache-Control: no-cache`（または ETag による再検証）を設定することを推奨する。
   - SQLite DB ファイルは Web 公開ディレクトリ外に配置し、環境変数で指定する（WAL モード + `busy_timeout` は初回接続時に自動適用）。
   - ルーティング配置は `SCRIPT_NAME=/cgi-bin/tamatsuzura.cgi, PATH_INFO=/api/...` 方式および `ScriptAlias /api /path/to/tamatsuzura-cgi`（`SCRIPT_NAME=/api, PATH_INFO=/...`）方式の双方に対応。
   - 必須・任意環境変数:
     - `TAMATSUZURA_DB_PATH`: SQLite DB ファイルパス（例: `/var/lib/tamatsuzura/tamatsuzura.sqlite3`、未指定時は `data/tamatsuzura.sqlite3`）。
     - `TAMATSUZURA_DB_BUSY_TIMEOUT_MS`: SQLite ロック待機時間ミリ秒（`0..300000`、既定値 `5000`）。
     - `TAMATSUZURA_ALLOW_REGISTRATION`: ブラウザからの新規ユーザー登録許可（`true` / `false`、既定値 `true`。クローズド運用時は `false`）。
     - `TAMATSUZURA_SESSION_TTL_SECONDS`: SQLite セッションおよび Cookie `Max-Age` 有効期間秒数（`60..31536000`、既定値 `604800` = 7 日）。
     - `TAMATSUZURA_COOKIE_SECURE`: セッション Cookie の `Secure` 属性方針（`auto` / `always` / `never`、既定値 `auto`。CGI の `HTTPS=on` / `REQUEST_SCHEME=https` を自動検出）。
     - `TAMATSUZURA_TRUST_PROXY_HEADERS`: リバースプロキシ配下で `X-Forwarded-Proto` / `X-Forwarded-Host` を信頼するか（`true` / `false`、既定値 `false`）。
2. **スタンドアロン HTTP サーバ / Docker コンテナ**:
   - Warp HTTP サーバ（`tamatsuzura-server`、`warp` ベース）と静的アセットを同梱した構成。ローカル開発や VPS / リバースプロキシ（Nginx / Cloudflare 等）背後での常駐運用に使用。
   - 起動例: `tamatsuzura-server --port 8080 --db-path /var/lib/tamatsuzura/tamatsuzura.sqlite3 --busy-timeout-ms 5000`（環境変数 `PORT` / `TAMATSUZURA_DB_PATH` / `TAMATSUZURA_DB_BUSY_TIMEOUT_MS` も使用可能）。

---

## 2. デプロイ後の手動検証

デプロイ完了後は、必ず [MANUAL_TESTS.md](./MANUAL_TESTS.md) に基づき、実機サーバの API 疎通（MT-2）および UI 統合後の基本保存（MT-1）を検証してください。
検証結果は `MANUAL_TESTS.md` の実施記録テーブルに追記します。
