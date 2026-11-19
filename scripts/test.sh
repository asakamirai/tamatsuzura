#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
USE_DOCKER="${USE_DOCKER:-0}"

cd "${ROOT_DIR}"

echo "Starting tests..."

run_in_docker() {
  if command -v docker >/dev/null 2>&1 && [ -f "docker/docker-compose.yml" ]; then
    docker compose -f docker/docker-compose.yml run --rm -T dev "$@"
  else
    echo "Error: Docker or docker/docker-compose.yml is not available." >&2
    exit 1
  fi
}

has_hs_files() {
  [ -d "src" ] && [ -n "$(find src -name '*.hs' -print -quit 2>/dev/null)" ]
}

if [ -f "stack.yaml" ]; then
  if [ "${USE_DOCKER}" = "1" ]; then
    run_in_docker stack test --fast
  elif command -v stack >/dev/null 2>&1; then
    stack test --fast
  elif command -v docker >/dev/null 2>&1 && [ -f "docker/docker-compose.yml" ]; then
    run_in_docker stack test --fast
  else
    echo "Error: Neither stack nor docker is available." >&2
    exit 1
  fi
elif [ -n "$(find . -maxdepth 1 -name '*.cabal' -print -quit 2>/dev/null)" ] || [ -f "cabal.project" ]; then
  if [ "${USE_DOCKER}" = "1" ]; then
    echo "Error: Docker image provides stack only; cabal test is not supported with USE_DOCKER=1." >&2
    exit 1
  elif command -v cabal >/dev/null 2>&1; then
    cabal test all
  else
    echo "Error: cabal is not available on host (Docker image provides stack only)." >&2
    exit 1
  fi
elif has_hs_files; then
  echo "Error: Haskell source files exist in src/, but no stack.yaml or cabal project file was found." >&2
  exit 1
else
  echo "Notice: Test suites not yet initialized (M0 pending). Test check passed as initial state."
fi

echo "Test step completed."
