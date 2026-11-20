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

### MT-2: Warp / CGI 実バイナリでの認証・所有者分離・REST API 疎通と SQLite 永続化検証

**確認したいこと**: ビルド済み実バイナリ (`tamatsuzura-server` および `tamatsuzura-cgi`) をそれぞれ独立プロセスとして起動し、同一 SQLite ファイルに対してユーザー登録・Cookie セッション認証・CSRF 検証・ユーザー間文書分離・文書 CRUD・ゴミ箱移動・復元・完全削除・ログアウトが一貫して動作すること。

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
   別ターミナルからヘルスチェック・未認証拒否・ユーザー登録 (Alice)・文書作成を実行する:
   ```bash
   curl -i http://127.0.0.1:18080/api/health
   curl -i http://127.0.0.1:18080/api/documents
   curl -i -c ./tmp-mt2-alice.cookies -X POST http://127.0.0.1:18080/api/auth/register \
     -H 'Content-Type: application/json' \
     -d '{"username":"alice","display_name":"アリス","password":"alice-password-123"}'
   ```
   返却 JSON の `csrf_token` を `<ALICE_CSRF>` として控え、CSRF なしの拒否および CSRF ありの文書作成を実行する:
   ```bash
   curl -i -b ./tmp-mt2-alice.cookies -X POST http://127.0.0.1:18080/api/documents \
     -H 'Content-Type: application/json' -d '{}'
   curl -i -b ./tmp-mt2-alice.cookies -X POST http://127.0.0.1:18080/api/documents \
     -H 'Content-Type: application/json' \
     -H "X-CSRF-Token: <ALICE_CSRF>" \
     -d '{"title":"MT-2確認","body":"Warp作成本文","folder":"manual/test","tags":["mt2"]}'
   ```
   - 確認項目: `/api/health` が `HTTP/1.1 200 OK` と `{"status":"ok"}`、未認証 `/api/documents` が `401 Unauthorized` を返すこと。
   - 確認項目: `/api/auth/register` が `HTTP/1.1 201 Created`、および `Set-Cookie: tamatsuzura_session=...; Path=/; HttpOnly; SameSite=Lax; ...` を返すこと。
   - 確認項目: `X-CSRF-Token` なしの `POST /api/documents` が `403 Forbidden` を返し、付与した場合は `HTTP/1.1 201 Created` と生成された `id` を含む JSON を返すこと（返却された `id` を `<DOC_ID>`、`./tmp-mt2-alice.cookies` の `tamatsuzura_session` 値を `<ALICE_TOKEN>` として控える）。
3. 続けて別ユーザー Bob を登録し、Alice の文書が不可視であることを確認する:
   ```bash
   curl -i -c ./tmp-mt2-bob.cookies -X POST http://127.0.0.1:18080/api/auth/register \
     -H 'Content-Type: application/json' \
     -d '{"username":"bob","password":"bob-password-123"}'
   curl -i -b ./tmp-mt2-bob.cookies http://127.0.0.1:18080/api/documents
   curl -i -b ./tmp-mt2-bob.cookies "http://127.0.0.1:18080/api/documents/<DOC_ID>"
   ```
   - 確認項目: Bob の一覧は `{"documents":[],"total":0}`、Alice の `<DOC_ID>` 取得は `404 Not Found` となること。
4. Warp サーバを `Ctrl+C` で停止し、同じ DB ファイルに対して CGI バイナリ (`tamatsuzura-cgi`) を CGI/1.1 環境変数で直接呼び出し、Warp で発行した Cookie セッションが CGI でも有効であることを確認する:
   - ホストに `stack` がある場合:
     ```bash
     GATEWAY_INTERFACE=CGI/1.1 REQUEST_METHOD=GET SCRIPT_NAME=/cgi-bin/tamatsuzura.cgi \
       PATH_INFO="/api/documents/<DOC_ID>" SERVER_PROTOCOL=HTTP/1.1 SERVER_NAME=localhost \
       SERVER_PORT=80 REMOTE_ADDR=127.0.0.1 HTTP_COOKIE="tamatsuzura_session=<ALICE_TOKEN>" \
       TAMATSUZURA_DB_PATH=./tmp-mt2.sqlite3 stack exec tamatsuzura-cgi
     ```
   - Docker 環境 (`USE_DOCKER=1` / ホスト `stack` 未導入) の場合:
     ```bash
     docker compose -f docker/docker-compose.yml run --rm -T \
       -e GATEWAY_INTERFACE=CGI/1.1 -e REQUEST_METHOD=GET -e SCRIPT_NAME=/cgi-bin/tamatsuzura.cgi \
       -e "PATH_INFO=/api/documents/<DOC_ID>" -e SERVER_PROTOCOL=HTTP/1.1 -e SERVER_NAME=localhost \
       -e SERVER_PORT=80 -e REMOTE_ADDR=127.0.0.1 -e "HTTP_COOKIE=tamatsuzura_session=<ALICE_TOKEN>" \
       -e TAMATSUZURA_DB_PATH=./tmp-mt2.sqlite3 \
       dev stack --allow-different-user exec tamatsuzura-cgi
     ```
   - 確認項目: `Status: 200 OK`、`Content-Type: application/json; charset=utf-8`、`Cache-Control: no-store`、および手順 2 で作成した Alice の文書 JSON が標準出力に返ること。
5. 同様に CGI バイナリ経由で `HTTP_COOKIE="tamatsuzura_session=<ALICE_TOKEN>"` および `HTTP_X_CSRF_TOKEN="<ALICE_CSRF>"` を指定し、論理削除 (`REQUEST_METHOD=DELETE`, `PATH_INFO="/api/documents/<DOC_ID>"`)、復元 (`REQUEST_METHOD=POST`, `PATH_INFO="/api/documents/<DOC_ID>/restore"`)、再度の論理削除、完全削除 (`REQUEST_METHOD=DELETE`, `PATH_INFO="/api/documents/<DOC_ID>/permanent"`)、最後にログアウト (`REQUEST_METHOD=POST`, `PATH_INFO="/api/auth/logout"`) を順に実行する。
   - 確認項目: 論理削除で `Status: 200 OK` かつ `deleted_at` が非 null、復元で `Status: 200 OK` かつ `deleted_at` が `null`、完全削除で `Status: 204 No Content` かつボディ空、ログアウトで `Status: 204 No Content` と `Set-Cookie: tamatsuzura_session=; ... Max-Age=0` が返り、ログアウト後の `GET /api/auth/me` が `Status: 401 Unauthorized` となること。
6. 検証終了後、一時 DB ファイル (`./tmp-mt2.sqlite3`, `./tmp-mt2.sqlite3-wal`, `./tmp-mt2.sqlite3-shm`) および一時 Cookie ファイル (`./tmp-mt2-alice.cookies`, `./tmp-mt2-bob.cookies`) を削除する。

**再実施が必要になる変更**: `src/server/app/{WarpMain,CgiMain}.hs`、`TamaTsuzura.Server.{Config,Api,App,Storage}`、`TamaTsuzura.Server.Auth.*`、`TamaTsuzura.Server.Storage.Auth`、`scripts/run-server.sh`、または本番 CGI / Web サーバ設定の変更時。

---

### MT-3: Miso/Wasm クライアントのブラウザ起動と M1 REST API 疎通確認

**確認したいこと**: `scripts/build-wasm.sh` で生成した `dist/web/` の Wasm バンドルおよび CodeMirror 6 バンドルがモダンブラウザ（スマートフォン幅および PC 幅）で正常に起動し、Warp 開発サーバ経由で M1 REST API（ヘルスチェック・文書一覧取得・新規作成）と疎通できること。

**手順**:
1. リポジトリルートでネイティブおよび Wasm バンドルをビルドする（Docker 経由で実行する場合は `USE_DOCKER=1` を付与）:
   ```bash
   scripts/build.sh
   ```
   - 確認項目: `dist/web/` 配下に `index.html`, `index.js`, `editor-bridge.js`, `browser-bridge.js`, `wasi-shim.js`, `ghc_wasm_jsffi.js`, `styles.css`, `tamatsuzura-client.wasm` の 8 ファイルが生成され、Wasm インポート検証ログが表示されること。
2. 一時 SQLite DB を指定して開発用 Warp サーバを起動する（`dist/web/index.html` が存在するため静的アセット配信が自動で有効になる。Docker 経由で実行する場合は `USE_DOCKER=1` を付与）:
   ```bash
   TAMATSUZURA_DB_PATH=./tmp-mt3.sqlite3 PORT=18080 scripts/run-server.sh
   ```
   - 確認項目: 標準出力に `Serving static client assets from dist/web` と表示されること。
3. ブラウザ（Chromium 系または Firefox）で `http://127.0.0.1:18080/` を開き、開発者ツールでモバイル表示（例: 幅 390px）と PC 表示（幅 1200px）の双方を確認する。
   - 確認項目: 「TamaTsuzura を起動しています...」の表示から Miso 画面へ切り替わり、ヘッダに **TamaTsuzura** および緑色のバッジ **サーバ接続済み** が表示されること。
   - 確認項目: ブラウザコンソールに未捕捉のエラー（WASI / JSFFI LinkError、CodeMirror 6 読み込み失敗、404）が出ないこと。
   - 確認項目: 一覧ペインに **文書一覧**・**0件** および「文書はまだありません。「新規作成」から最初のメモを作成できます。」が表示されること。PC 幅では 2 ペイン、モバイル幅では一覧単一ペインで表示されること。
4. ヘッダの **新規作成** ボタンを押す。
   - 確認項目: ボタンが一時的に **作成中...** に切り替わった後、エディタ画面が開き（モバイル幅では一覧からエディタへ遷移）、一覧が **1件** になり、タイトル **無題**・形式 **Markdown** の行が追加されること。ステータスバー右側の拡張機能欄に **行末空白の除去** ボタンが表示されること。
5. ブラウザを再読み込み (F5) し、さらに **再読込** ボタンを押す。
   - 確認項目: 再読み込み後も **サーバ接続済み** となり、手順 4 で作成した文書（**1件** / **無題**）が SQLite から復元表示されること。
6. Warp サーバを停止した状態で **再読込** ボタンを押す。
   - 確認項目: 接続バッジが赤色の **接続エラー** に変わり、「サーバへ接続できませんでした (...)」を含むエラーバナーと **閉じる** ボタンが表示されること。**閉じる** を押すとバナーが消えること。
7. 検証終了後、一時 DB ファイル (`./tmp-mt3.sqlite3`, `./tmp-mt3.sqlite3-wal`, `./tmp-mt3.sqlite3-shm`) を削除する。

**再実施が必要になる変更**: `src/client/src-core/TamaTsuzura/Client/{Ffi,Api,App,View,I18n,EditorBridge}.hs`、`src/client/app/Main.hs`、`src/client/web/*`、`scripts/build-wasm.sh`、`TamaTsuzura.Server.Static` の変更時。

---

### MT-4: CodeMirror 6 編集・明示的保存・Markdown プレビュー・検索/置換・整理・ゴミ箱操作

**確認したいこと**: スマートフォン幅（例: 390px）および PC 幅（例: 1200px）のブラウザで、CodeMirror 6 を用いた本文編集、Undo/Redo、明示的保存 (`Ctrl+S` / **保存** ボタン)、未保存変更ガード、Markdown プレビュー（編集/分割/プレビュー切替）、エディタ内検索/置換、フォルダ・タグ整理フィルタ、およびゴミ箱への移動・復元・完全削除が一連の操作として正常に動作すること。

**手順**:
1. MT-3 の手順 1〜2 と同様にビルドおよび Warp サーバ起動を行い、ブラウザで `http://127.0.0.1:18080/` を開く（DB は `./tmp-mt4.sqlite3` を使用）。
2. **新規作成** を押し、タイトル欄に `議事録テスト`、エディタ本文に以下を入力する:
   ```markdown
   # 週次メモ

   - [x] 要件確認
   - [ ] 次回タスク

   **重要** な内容と [リンク](https://example.com)
   ```
   - 確認項目: 入力直後に保存状態バッジが黄色の **未保存** に変わり、**保存** ボタンおよび **戻す** ボタンが有効化され、フッタステータスバーの行数・文字数・カーソル位置がリアルタイムに更新されること。
   - 確認項目: **戻す** / **やり直す**（または `Ctrl+Z` / `Ctrl+Y`）で編集履歴が正常に巻き戻し・やり直しできること。
3. **整理** ボタンを押してメタデータパネルを開き、フォルダに `work/weekly`、タグに `議事録, 重要` を入力して、**保存** ボタン（または `Ctrl+S`）を押す。
   - 確認項目: 緑色の通知バナー「文書を保存しました。」が表示され、保存状態バッジが緑色の **保存済み** に戻ること。
   - 確認項目: 左一覧（モバイルでは **一覧へ** で戻った一覧画面）にタイトル `議事録テスト`、フォルダ `work/weekly`、タグ `#重要`・`#議事録` が表示されること。
4. モード切替タブで **プレビュー**（PC 幅では **分割** も確認）へ切り替える。
   - 確認項目: 見出し `週次メモ`、チェック済み/未チェックのタスクリスト、太字 `重要`、外部リンクが整形表示されること。
   - 確認項目: **編集** へ戻しても CodeMirror 6 の本文内容・Undo 履歴が失われず維持されていること。
5. **検索・置換** ボタン（または `Ctrl+F`）を押して検索欄に `重要` を入力し、**置換欄** を開いて置換欄に `最優先` を入力する。
   - 確認項目: エディタ本文中の `重要` がハイライトされ、一致カウンタに `1件`（または `1 / 1件`）が表示されること。
   - 確認項目: **次へ** で対象を選択してから **置換** を押す（または **すべて置換** を押す）と本文内の `重要` が `最優先` に置換され、保存状態が **未保存** になること。
6. 未保存のまま **一覧へ**（モバイル）または **新規作成** を押す。
   - 確認項目: 「未保存の変更があります」の確認ダイアログが表示されること。**キャンセル** を押すと編集内容が維持されたままエディタに戻り、`Ctrl+S` で保存すると **保存済み** になること。
7. 一覧画面の検索欄に `最優先` を入力して **検索** を押し、さらにフォルダドロップダウンで `work/weekly`、タグドロップダウンで `#重要` を選択する。
   - 確認項目: いずれの条件でも `議事録テスト` が一覧に表示され、**解除** ボタンを押すとフィルタが初期化されること。
8. エディタの **ゴミ箱へ** ボタンを押す。
   - 確認項目: 「文書をゴミ箱へ移動しました。」が表示され、稼働中一覧が **0件** になること。
9. 一覧上部の **ゴミ箱** タブへ切り替え、該当文書を開く。
   - 確認項目: 上部に「ゴミ箱内の文書は閲覧専用です。編集するには「復元」を押してください。」の警告バナーと **閲覧専用** バッジが表示され、本文・タイトルが編集不可であること。
10. **復元** を押して稼働中へ戻し、再度ゴミ箱へ移動した後、**ゴミ箱** タブから **完全削除** を押す。
    - 確認項目: 「文書を完全に削除しますか？」の確認ダイアログが表示され、**完全に削除する** を押すと「文書を完全に削除しました。」が表示されてゴミ箱が「ゴミ箱は空です。」になること。
11. 検証終了後、一時 DB ファイル (`./tmp-mt4.sqlite3`, `./tmp-mt4.sqlite3-wal`, `./tmp-mt4.sqlite3-shm`) を削除する。

**再実施が必要になる変更**: `TamaTsuzura.Client.{App,View,I18n,EditorBridge,Markdown}`、`src/client/web/{editor-entry.js,styles.css,index.js}` の変更時。

---

### MT-5: IndexedDB 未保存ドラフト自動退避・再読み込み復元および Markdown/テキストのエクスポート・インポート

**確認したいこと**: 編集中の未保存ドラフトが IndexedDB へ自動退避され、ブラウザ再読み込み後に自動で復元・破棄できること（サーバ更新済み時の競合保護および 404 孤立ドラフト救出を含む）、ならびに Markdown / プレーンテキスト形式のエクスポート（ダウンロード）およびインポート（ファイル読込）がスマートフォン幅・PC 幅双方で正常に動作すること。

**手順**:
1. MT-3 の手順 1〜2 と同様に `scripts/build.sh` および一時 DB (`TAMATSUZURA_DB_PATH=./tmp-mt5.sqlite3`) での Warp サーバ起動を行い、ブラウザで `http://127.0.0.1:18080/` を開く。
2. **新規作成** を押し、タイトルに `ドラフト検証`、本文に `保存済みの初期本文` を入力して **保存**（または `Ctrl+S`）を押す。
   - 確認項目: 保存状態が **保存済み** となり、ステータスバーにドラフト退避インジケータが表示されないこと。
3. 本文を `未保存の追記内容（リロード耐性確認）` に変更し、保存ボタンを押さずに約 1 秒待機する。
   - 確認項目: 保存状態が黄色の **未保存** に変わり、一覧の該当文書行に黄色の **ドラフトあり** バッジが表示されること。
   - 確認項目: エディタ下部ステータスバーに **ドラフト退避中...** を経て緑色の **ドラフト退避済み** が表示されること。
4. **保存** を押さずにブラウザを再読み込み (F5) する。
   - 確認項目: 再起動後、対象文書が自動で開き、本文に `未保存の追記内容（リロード耐性確認）` が自動復元されること。
   - 確認項目: エディタ上部に水色のバナー「ローカルに退避されていた未保存ドラフトを復元しました。」と **ドラフトを破棄** ボタンが表示されること。
5. 復元バナーの **ドラフトを破棄** ボタンを押す。
   - 確認項目: 通知バナー「ローカルの未保存ドラフトを破棄しました。」が表示され、本文が `保存済みの初期本文` に戻り、一覧の **ドラフトあり** バッジが消えること。
   - 確認項目: 再度ブラウザを再読み込み (F5) しても、未保存ドラフトが再出現しないこと。
6. （競合保護確認）再度本文を `別更新前のローカルドラフト` へ編集して **ドラフト退避済み** を確認したあと、別ターミナルから対象文書の ID (`<DOC_ID>` は `curl -s http://127.0.0.1:18080/api/documents` で確認) に対してサーバ側を直接更新し、ブラウザを再読み込み (F5) する:
   ```bash
   curl -X PUT http://127.0.0.1:18080/api/documents/<DOC_ID> \
     -H 'Content-Type: application/json' \
     -d '{"title":"ドラフト検証","body":"サーバ側の新しい本文","format":"markdown","folder":null,"tags":[]}'
   ```
   - 確認項目: サーバ側の新しい本文が自動上書きされずに表示され、上部に黄色の警告バナー「この文書はサーバ側で更新されていますが、更新前のローカル未保存ドラフトが残っています。」と **ドラフトを適用** / **ドラフトを破棄** ボタンが表示されること。
   - 確認項目: **ドラフトを適用** を押すと本文が `別更新前のローカルドラフト` へ切り替わり、**ドラフトを破棄** を押すと `サーバ側の新しい本文` へ戻ること。
7. （404 孤立ドラフト救出確認）再度本文を `削除前に編集していた内容` へ変更して **ドラフト退避済み** を確認したあと、別ターミナルから対象文書を論理削除→完全削除し、ブラウザを再読み込み (F5) する:
   ```bash
   curl -X DELETE http://127.0.0.1:18080/api/documents/<DOC_ID>
   curl -X DELETE http://127.0.0.1:18080/api/documents/<DOC_ID>/permanent
   ```
   - 確認項目: 一覧画面へ戻り、上部に黄色の警告バナー「元の文書がサーバ上に見つかりませんでした。ローカルの未保存ドラフトを新規文書として復元するか、破棄できます。」と **新規文書として復元** / **ドラフトを破棄** ボタンが表示されること。
   - 確認項目: **新規文書として復元** を押すと `削除前に編集していた内容` を本文とする新規文書が作成されて開き、警告バナーが消えること。
8. 本文に `# エクスポート確認` と入力した状態で、ツールバーの **エクスポート** ボタンを押す。
   - 確認項目: ブラウザから `ドラフト検証.md` がダウンロードされ、通知バナー「ファイルをエクスポートしました: ドラフト検証.md」が表示されること。ダウンロードしたファイルの内容が `# エクスポート確認` であること。
   - 確認項目: **整理** パネルで形式を **テキスト** に切り替えて再度 **エクスポート** を押すと、`ドラフト検証.txt` がダウンロードされること。
9. ローカルにテスト用ファイル `インポート確認.md`（内容: `# 取り込み見出し\n\nテスト本文`）と `メモ.txt`（内容: `プレーンテキスト内容`）を用意し、先に手順 8 の文書を保存（または未保存のまま **インポート** を押して未保存確認ダイアログが出ることを確認した上で続行）してからヘッダの **インポート** ボタンを押して `インポート確認.md` を選択する。
   - 確認項目: 新規文書としてタイトル `インポート確認`・形式 **Markdown**・本文 `# 取り込み見出し\n\nテスト本文` がサーバへ作成されてエディタに開き、通知バナー「ファイルをインポートしました: インポート確認.md」が表示されること。
   - 確認項目: 続けて `メモ.txt` をインポートすると、タイトル `メモ`・形式 **テキスト**・本文 `プレーンテキスト内容` が作成されること。
10. （任意・異常系）**インポート** を押し、ファイル選択ダイアログの表示対象フィルタを「すべてのファイル (`*.*`)」等へ切り替えてから非 UTF-8 のバイナリファイル（例: PNG 画像など）を選択する。
    - 確認項目: エラーバナー「UTF-8 テキストとして読み込めないファイルのためインポートできません。」が表示され、不正な文書が作成されないこと。
11. 検証終了後、一時 DB ファイル (`./tmp-mt5.sqlite3`, `./tmp-mt5.sqlite3-wal`, `./tmp-mt5.sqlite3-shm`) およびブラウザの開発者ツール → Application → IndexedDB → `tamatsuzura`（必要に応じて削除）を整理する。

**再実施が必要になる変更**: `TamaTsuzura.Client.{DraftStorage,FileTransfer,App,View,I18n}`、`src/client/web/{browser-bridge.js,index.js,styles.css}`、`scripts/build-wasm.sh` の変更時。

---

## 実施記録

| 日付 | 項目番号 | 対象コミット / バージョン | 実施者 | 結果 | 備考 |
| --- | --- | --- | --- | --- | --- |
| — | — | — | — | — | 初期構築 |
