# MANUAL_TESTS.md — 手動検証手順書

本ドキュメントは、自動テスト (`scripts/test.sh`) では検証できない挙動（ブラウザ UI、WebAssembly 実行、オフライン同期、実際のデプロイ環境など）の手動検証手順を定めたものです。

---

## 運用の原則

1. **第三者が迷わず実行できること**:
   - 本システムの内部実装やコードを知らない人でも、画面に表示されるボタンやテキストだけを頼りに検証できる具体性で記述します。
   - 専門用語や曖昧な指示（例:「適切に保存されること」）は避け、「status 欄に `saved at ...` と表示されること」のように画面上の変化を明記します。
2. **手順と実装の完全同期**:
   - UI の文言、操作フロー、レイアウトを変更した場合は、**その変更と同じコミットで本手順書を修正** してください。
   - 機能が廃止された場合は手順を削除し、新しい手動検証が必要な挙動を追加した場合は手順を追記します。

---

## 項目の追加・更新・削除のルール

- **追加**: 自動テストで確認できない挙動を実装した Phase の完了時に、項目を追加します。
- **更新**: ボタン名、入力欄のプレースホルダー、画面遷移、期待される表示が変わった場合、即時更新します。
- **削除**: 仕様変更で機能がなくなった場合、該当項目を削除します。

---

## 検証項目フォーマット

各テストケースは以下の構造で記述します。

```markdown
### MT-X: <検証項目のタイトル>

**確認したいこと**: 何を確かめるための手順か (1〜2文)

**手順**:
1. 操作内容を具体的に記述
   - 確認項目: 画面のどこに何が出るか
2. 次の操作...
   - 確認項目: ...

**再実施が必要になる変更**: どのようなコード変更を行った際にこのテストを再実施すべきか
```

---

## 検証項目一覧

### MT-1: サインインと文書の基本保存

> [!NOTE]
> ※ 本項目は、M1 (MVP サーバ: CGI/Warp)、M2 (MVP クライアント: モバイルファースト日本語 UI)、M3 (認証・ユーザ管理) の実装完了後に実施可能となるエンドツーエンドの検証テンプレートです。実装時の実際の日本語ラベル・モバイル表示および CGI 環境確認に合わせて更新してください。

**確認したいこと**: ブラウザからサインインし、新規文書を作成・編集してサーバへ永続化できること。

**手順**:
1. ブラウザでエディタの URL を開く。
   - 確認項目: サインイン画面または初期画面が表示されること。
2. ユーザー名とパスワードを入力して **Sign in** (または新規登録) を行う。
   - 確認項目: 認証が成功し、エディタの一覧画面が表示されること。
3. **+ new** (新規作成ボタン) を押す。
   - 確認項目: 新しい文書が作成され、編集画面が開くこと。
4. エディタ領域に任意のテキスト (例: `Hello TamaTsuzura`) を入力する。
   - 確認項目: 未保存を示すインジケータ (例: `(modified)`) が表示されること。
5. タイトルを入力し、**Save** を押す。
   - 確認項目: 保存完了メッセージが表示され、未保存インジケータが消えること。
6. ブラウザを再読み込み (F5) する。
   - 確認項目: 直前に保存した文書と内容が正しく復元されること。

**再実施が必要になる変更**: 認証機能、文書 CRUD API、DB ストレージ接続、エディタ基本 UI の変更時。

---

### MT-2: Warp / CGI 実バイナリでの REST API 疎通と SQLite 永続化検証

**確認したいこと**: ビルド済み実バイナリ (`tamatsuzura-server` および `tamatsuzura-cgi`) をそれぞれ独立プロセスとして起動し、同一 SQLite ファイルに対して文書 CRUD・ゴミ箱移動・復元・完全削除が一貫して動作すること。

**手順**:
1. リポジトリルートで `scripts/build.sh` を実行し、ワークスペース直下に一時 DB パスを指定する（ホスト実行・Docker 実行の両方から同一ファイルを参照でき、かつ `.gitignore` 対象となる相対パスを使用する）:
   ```bash
   export TAMATSUZURA_DB_PATH=./tmp-mt2.sqlite3
   ```
   - 確認項目: ビルドがエラーなく完了すること。
2. Warp サーバを起動する（Docker 経由で実行する場合は `USE_DOCKER=1` を付与）:
   ```bash
   PORT=18080 scripts/run-server.sh
   ```
   別ターミナルからヘルスチェックと文書作成を実行する:
   ```bash
   curl -i http://127.0.0.1:18080/api/health
   curl -i -X POST http://127.0.0.1:18080/api/documents \
     -H 'Content-Type: application/json' \
     -d '{"title":"MT-2確認","body":"Warp作成本文","folder":"manual/test","tags":["mt2"]}'
   ```
   - 確認項目: `/api/health` が `HTTP/1.1 200 OK` と `{"status":"ok"}` を返すこと。
   - 確認項目: `POST /api/documents` が `HTTP/1.1 201 Created` と生成された `id` を含む JSON を返すこと（返却された `id` を `<DOC_ID>` として控える）。
3. Warp サーバを `Ctrl+C` で停止し、同じ DB ファイルに対して CGI バイナリ (`tamatsuzura-cgi`) を CGI/1.1 環境変数で直接呼び出す:
   - ホストに `stack` がある場合:
     ```bash
     GATEWAY_INTERFACE=CGI/1.1 REQUEST_METHOD=GET SCRIPT_NAME=/cgi-bin/tamatsuzura.cgi \
       PATH_INFO="/api/documents/<DOC_ID>" SERVER_PROTOCOL=HTTP/1.1 SERVER_NAME=localhost \
       SERVER_PORT=80 REMOTE_ADDR=127.0.0.1 TAMATSUZURA_DB_PATH=./tmp-mt2.sqlite3 \
       stack exec tamatsuzura-cgi
     ```
   - Docker 環境 (`USE_DOCKER=1` / ホスト `stack` 未導入) の場合:
     ```bash
     docker compose -f docker/docker-compose.yml run --rm -T \
       -e GATEWAY_INTERFACE=CGI/1.1 -e REQUEST_METHOD=GET -e SCRIPT_NAME=/cgi-bin/tamatsuzura.cgi \
       -e "PATH_INFO=/api/documents/<DOC_ID>" -e SERVER_PROTOCOL=HTTP/1.1 -e SERVER_NAME=localhost \
       -e SERVER_PORT=80 -e REMOTE_ADDR=127.0.0.1 -e TAMATSUZURA_DB_PATH=./tmp-mt2.sqlite3 \
       dev stack --allow-different-user exec tamatsuzura-cgi
     ```
   - 確認項目: `Status: 200 OK`、`Content-Type: application/json;charset=utf-8`、および手順 2 で作成した文書の JSON が標準出力に返ること。
4. 同様に CGI バイナリ経由で論理削除 (`REQUEST_METHOD=DELETE`, `PATH_INFO="/api/documents/<DOC_ID>"`)、復元 (`REQUEST_METHOD=POST`, `PATH_INFO="/api/documents/<DOC_ID>/restore"`)、再度の論理削除、完全削除 (`REQUEST_METHOD=DELETE`, `PATH_INFO="/api/documents/<DOC_ID>/permanent"`) を順に実行する。
   - 確認項目: 論理削除で `Status: 200 OK` かつ `deleted_at` が非 null、復元で `Status: 200 OK` かつ `deleted_at` が `null`、完全削除で `Status: 204 No Content` かつボディ空であること。
5. 検証終了後、一時 DB ファイル (`./tmp-mt2.sqlite3`, `./tmp-mt2.sqlite3-wal`, `./tmp-mt2.sqlite3-shm`) を削除する。

**再実施が必要になる変更**: `src/server/app/{WarpMain,CgiMain}.hs`、`TamaTsuzura.Server.{Config,Api,App,Storage}`、`scripts/run-server.sh`、または本番 CGI / Web サーバ設定の変更時。

---

### MT-3: Miso/Wasm クライアントのブラウザ起動と M1 REST API 疎通確認

**確認したいこと**: `scripts/build-wasm.sh` で生成した `dist/web/` の Wasm バンドルがモダンブラウザ（スマートフォン幅および PC 幅）で正常に起動し、Warp 開発サーバ経由で M1 REST API（ヘルスチェック・文書一覧取得・新規メモ作成）と疎通できること。

**手順**:
1. リポジトリルートでネイティブおよび Wasm バンドルをビルドする（Docker 経由で実行する場合は `USE_DOCKER=1` を付与）:
   ```bash
   scripts/build.sh
   ```
   - 確認項目: `dist/web/` 配下に `index.html`, `index.js`, `wasi-shim.js`, `ghc_wasm_jsffi.js`, `styles.css`, `tamatsuzura-client.wasm` の 6 ファイルが生成され、Wasm インポート検証ログが表示されること。
2. 一時 SQLite DB を指定して開発用 Warp サーバを起動する（`dist/web/index.html` が存在するため静的アセット配信が自動で有効になる。Docker 経由で実行する場合は `USE_DOCKER=1` を付与）:
   ```bash
   TAMATSUZURA_DB_PATH=./tmp-mt3.sqlite3 PORT=18080 scripts/run-server.sh
   ```
   - 確認項目: 標準出力に `Serving static client assets from dist/web` と表示されること。
3. ブラウザ（Chromium 系または Firefox）で `http://127.0.0.1:18080/` を開き、開発者ツールでモバイル表示（例: 幅 390px）も確認する。
   - 確認項目: 「TamaTsuzura を起動しています...」の表示から Miso 画面へ切り替わり、ヘッダに **TamaTsuzura** および緑色のバッジ **サーバ接続済み** が表示されること。
   - 確認項目: ブラウザコンソールに未捕捉のエラー（WASI / JSFFI LinkError や 404）が出ないこと。
   - 確認項目: 文書一覧セクションに **0件** および「文書はまだありません。「新規メモ作成」から最初のメモを作成できます。」が表示され、読み込み済み拡張機能セクションに **行末空白の除去** が表示されること。
4. **新規メモ作成** ボタンを押す。
   - 確認項目: ボタンが一時的に **作成中...** に切り替わった後、文書一覧が **1件** になり、タイトル **無題**・形式 **Markdown** の行が追加され、「最後に作成した文書」カードに作成された文書 ID が表示されること。
5. ブラウザを再読み込み (F5) し、さらに **再読込** ボタンを押す。
   - 確認項目: 再読み込み後も **サーバ接続済み** となり、手順 4 で作成した文書（**1件** / **無題**）が SQLite から復元表示されること。
6. Warp サーバを停止した状態で **再読込** ボタンを押す。
   - 確認項目: 接続バッジが赤色の **接続エラー** に変わり、「サーバへ接続できませんでした (...)」を含むエラーバナーと **閉じる** ボタンが表示されること。**閉じる** を押すとバナーが消えること。
7. 検証終了後、一時 DB ファイル (`./tmp-mt3.sqlite3`, `./tmp-mt3.sqlite3-wal`, `./tmp-mt3.sqlite3-shm`) を削除する。

**再実施が必要になる変更**: `src/client/src-core/TamaTsuzura/Client/{Ffi,Api,App,View,I18n}.hs`、`src/client/app/Main.hs`、`src/client/web/*`、`scripts/build-wasm.sh`、`TamaTsuzura.Server.Static` の変更時。

---

## 実施記録

| 日付 | 項目番号 | 対象コミット / バージョン | 実施者 | 結果 | 備考 |
| --- | --- | --- | --- | --- | --- |
| — | — | — | — | — | 初期構築 |
