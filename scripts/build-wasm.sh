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
    cp src/client/web/index.html src/client/web/index.js src/client/web/browser-bridge.js src/client/web/wasi-shim.js src/client/web/styles.css "${dist_dir}/"
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
  node --check "${dist_dir}/editor-bridge.js"
  node --check "${dist_dir}/browser-bridge.js"

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
