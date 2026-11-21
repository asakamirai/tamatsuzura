#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
USE_DOCKER="${USE_DOCKER:-0}"

cd "${ROOT_DIR}"

echo "Starting Wasm client build..."

run_in_docker() {
  if command -v docker >/dev/null 2>&1 && [ -f "docker/docker-compose.yml" ]; then
    docker compose -f docker/docker-compose.yml run --rm -T dev ghc-wasm-exec "$@"
  else
    echo "Error: Docker or docker/docker-compose.yml is not available." >&2
    exit 1
  fi
}

bundle_wasm_assets() {
  local builddir="$1"
  local dist_dir="dist/web"
  local wasm_bin
  wasm_bin="$(wasm32-wasi-cabal --project-file=cabal.project.wasm --builddir="${builddir}" list-bin exe:tamatsuzura-client | tail -n 1)"
  if [ -z "${wasm_bin}" ] || [ ! -f "${wasm_bin}" ]; then
    echo "Error: Built Wasm binary not found: ${wasm_bin}" >&2
    exit 1
  fi

  local post_link
  post_link="$(wasm32-wasi-ghc --print-libdir)/post-link.mjs"
  if [ ! -f "${post_link}" ]; then
    echo "Error: GHC Wasm post-link.mjs not found at ${post_link}" >&2
    exit 1
  fi

  mkdir -p "${dist_dir}"
  cp "${wasm_bin}" "${dist_dir}/tamatsuzura-client.wasm"
  node "${post_link}" -i "${dist_dir}/tamatsuzura-client.wasm" -o "${dist_dir}/ghc_wasm_jsffi.js"

  if [ -d "src/client/web" ]; then
    cp src/client/web/index.html src/client/web/index.js src/client/web/browser-bridge.js src/client/web/wasi-shim.js src/client/web/styles.css src/client/web/manifest.webmanifest "${dist_dir}/"
    mkdir -p "${dist_dir}/icons"
    cp src/client/web/icons/icon.svg src/client/web/icons/icon-192.png src/client/web/icons/icon-512.png "${dist_dir}/icons/"
    if [ -f "src/client/web/package.json" ]; then
      local need_install=0
      if [ ! -d "src/client/web/node_modules" ] || [ ! -x "src/client/web/node_modules/.bin/esbuild" ]; then
        need_install=1
      elif [ -f "src/client/web/pnpm-lock.yaml" ] && [ ! -f "src/client/web/node_modules/.modules.yaml" ]; then
        need_install=1
      elif [ -f "src/client/web/pnpm-lock.yaml" ] && [ "src/client/web/pnpm-lock.yaml" -nt "src/client/web/node_modules/.modules.yaml" ]; then
        need_install=1
      elif [ "src/client/web/package.json" -nt "src/client/web/node_modules/.modules.yaml" ]; then
        need_install=1
      fi
      if [ "${need_install}" = "1" ]; then
        if [ -f "src/client/web/pnpm-lock.yaml" ]; then
          pnpm --dir src/client/web install --frozen-lockfile --prefer-offline
        else
          pnpm --dir src/client/web install --prefer-offline
        fi
      fi
      pnpm --dir src/client/web run build
    fi
  fi

  if [ ! -s "${dist_dir}/editor-bridge.js" ]; then
    echo "Error: Built CodeMirror 6 bundle not found: ${dist_dir}/editor-bridge.js" >&2
    exit 1
  fi

  # Generate Service Worker with deterministic content hash across all app-shell assets
  node --input-type=module -e '
    import crypto from "node:crypto";
    import fs from "node:fs/promises";
    import path from "node:path";

    const distDir = path.resolve("./dist/web");
    const manifestPath = path.join(distDir, "manifest.webmanifest");
    const manifest = JSON.parse(await fs.readFile(manifestPath, "utf8"));
    if (!manifest.name || !manifest.start_url || !Array.isArray(manifest.icons) || manifest.icons.length === 0) {
      throw new Error("Invalid manifest.webmanifest: missing required fields");
    }
    for (const icon of manifest.icons) {
      const iconPath = path.resolve(distDir, icon.src);
      const relToDist = path.relative(distDir, iconPath);
      if (!relToDist || relToDist.startsWith("..") || path.isAbsolute(relToDist)) {
        throw new Error(`Invalid manifest icon path outside dist/web: ${icon.src}`);
      }
      await fs.access(iconPath);
    }

    const shellFiles = [
      "index.html",
      "index.js",
      "editor-bridge.js",
      "browser-bridge.js",
      "wasi-shim.js",
      "ghc_wasm_jsffi.js",
      "styles.css",
      "tamatsuzura-client.wasm",
      "manifest.webmanifest",
      "icons/icon.svg",
      "icons/icon-192.png",
      "icons/icon-512.png",
    ];
    const hash = crypto.createHash("sha256");
    for (const rel of shellFiles) {
      const fullPath = path.join(distDir, rel);
      const data = await fs.readFile(fullPath);
      hash.update(rel);
      hash.update("\0");
      hash.update(data);
    }
    const version = hash.digest("hex").slice(0, 12);
    const precacheUrls = ["./", ...shellFiles.map((rel) => `./${rel}`)];

    const swTemplate = await fs.readFile("./src/client/web/sw.js", "utf8");
    if (!swTemplate.includes("__TAMATSUZURA_CACHE_VERSION__")) {
      throw new Error("sw.js template is missing __TAMATSUZURA_CACHE_VERSION__ placeholder");
    }
    if (!swTemplate.includes("__TAMATSUZURA_APP_SHELL_ASSETS__")) {
      throw new Error("sw.js template is missing __TAMATSUZURA_APP_SHELL_ASSETS__ placeholder");
    }
    const swOut = swTemplate
      .replace("\"__TAMATSUZURA_CACHE_VERSION__\"", JSON.stringify(version))
      .replace("__TAMATSUZURA_APP_SHELL_ASSETS__", JSON.stringify(precacheUrls, null, 2));
    await fs.writeFile(path.join(distDir, "sw.js"), swOut, "utf8");
    console.log(`Generated dist/web/sw.js (cache version: ${version}, ${precacheUrls.length} precached entries).`);
  '

  node --check "${dist_dir}/editor-bridge.js"
  node --check "${dist_dir}/browser-bridge.js"
  node --check "${dist_dir}/index.js"
  node --check "${dist_dir}/sw.js"

  # Verify that every import required by tamatsuzura-client.wasm is satisfied by wasi-shim.js and ghc_wasm_jsffi.js
  node --input-type=module -e '
    import fs from "node:fs/promises";
    import { BrowserWasiShim } from "./dist/web/wasi-shim.js";
    import ghcWasmJsffi from "./dist/web/ghc_wasm_jsffi.js";

    const bytes = await fs.readFile("./dist/web/tamatsuzura-client.wasm");
    const mod = await WebAssembly.compile(bytes);
    const wasi = new BrowserWasiShim();
    const providers = {
      wasi_snapshot_preview1: wasi.wasiImport,
      ghc_wasm_jsffi: ghcWasmJsffi({}),
    };
    const missing = [];
    for (const imp of WebAssembly.Module.imports(mod)) {
      const modObj = providers[imp.module];
      if (!modObj || typeof modObj[imp.name] !== "function") {
        missing.push(`${imp.module}.${imp.name}`);
      }
    }
    if (missing.length > 0) {
      console.error("Missing Wasm imports:", missing.join(", "));
      process.exit(1);
    }
    console.log(`Verified Wasm imports (${WebAssembly.Module.imports(mod).length} total) and exports (${WebAssembly.Module.exports(mod).length} total).`);
  '
}

if [ ! -f "cabal.project.wasm" ]; then
  echo "Notice: cabal.project.wasm not found. Skipping Wasm build."
  exit 0
fi

# Load host ghc-wasm environment if installed under ~/.ghc-wasm
if [ "${USE_DOCKER}" != "1" ] && ! command -v wasm32-wasi-cabal >/dev/null 2>&1 && [ -n "${HOME:-}" ] && [ -f "${HOME}/.ghc-wasm/env" ]; then
  set +u
  # shellcheck source=/dev/null
  . "${HOME}/.ghc-wasm/env"
  set -u
fi

if [ "${USE_DOCKER}" = "1" ]; then
  run_in_docker sh -c 'set -eu; wasm32-wasi-cabal --project-file=cabal.project.wasm --builddir=dist-newstyle-docker build all'
  run_in_docker bash -euo pipefail -c "$(declare -f bundle_wasm_assets); bundle_wasm_assets dist-newstyle-docker"
elif command -v wasm32-wasi-cabal >/dev/null 2>&1; then
  wasm32-wasi-cabal --project-file=cabal.project.wasm build all
  bundle_wasm_assets dist-newstyle
elif command -v docker >/dev/null 2>&1 && [ -f "docker/docker-compose.yml" ]; then
  run_in_docker sh -c 'set -eu; wasm32-wasi-cabal --project-file=cabal.project.wasm --builddir=dist-newstyle-docker build all'
  run_in_docker bash -euo pipefail -c "$(declare -f bundle_wasm_assets); bundle_wasm_assets dist-newstyle-docker"
else
  echo "Error: Neither wasm32-wasi-cabal nor docker is available." >&2
  exit 1
fi

echo "Wasm client build step completed (bundle ready in dist/web/)."
