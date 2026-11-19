#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
USE_DOCKER="${USE_DOCKER:-0}"

cd "${ROOT_DIR}"

echo "Starting lint check..."

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

if has_hs_files; then
  if [ "${USE_DOCKER}" = "1" ]; then
    run_in_docker hlint src
  elif command -v hlint >/dev/null 2>&1; then
    hlint src
  elif command -v docker >/dev/null 2>&1 && [ -f "docker/docker-compose.yml" ]; then
    run_in_docker hlint src
  else
    echo "Error: Neither hlint nor docker is available." >&2
    exit 1
  fi
else
  echo "Notice: Source code not yet populated in src/ (M0 pending). Lint check passed as initial state."
fi

echo "Lint step completed."
