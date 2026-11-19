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
  run_in_docker wasm32-wasi-cabal --project-file=cabal.project.wasm --builddir=dist-newstyle-docker build all
elif command -v wasm32-wasi-cabal >/dev/null 2>&1; then
  wasm32-wasi-cabal --project-file=cabal.project.wasm build all
elif command -v docker >/dev/null 2>&1 && [ -f "docker/docker-compose.yml" ]; then
  run_in_docker wasm32-wasi-cabal --project-file=cabal.project.wasm --builddir=dist-newstyle-docker build all
else
  echo "Error: Neither wasm32-wasi-cabal nor docker is available." >&2
  exit 1
fi

echo "Wasm client build step completed."
