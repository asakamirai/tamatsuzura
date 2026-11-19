#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
USE_DOCKER="${USE_DOCKER:-0}"

cd "${ROOT_DIR}"

PORT="${PORT:-8080}"
echo "Starting TamaTsuzura development server on port ${PORT}..."

run_in_docker() {
  if command -v docker >/dev/null 2>&1 && [ -f "docker/docker-compose.yml" ]; then
    PORT="${PORT}" docker compose -f docker/docker-compose.yml run --rm --service-ports dev "$@"
  else
    echo "Error: Docker or docker/docker-compose.yml is not available." >&2
    exit 1
  fi
}

if [ -f "stack.yaml" ]; then
  if [ "${USE_DOCKER}" = "1" ]; then
    run_in_docker stack --allow-different-user run tamatsuzura-server -- --port "${PORT}"
  elif command -v stack >/dev/null 2>&1; then
    stack run tamatsuzura-server -- --port "${PORT}"
  elif command -v docker >/dev/null 2>&1 && [ -f "docker/docker-compose.yml" ]; then
    run_in_docker stack --allow-different-user run tamatsuzura-server -- --port "${PORT}"
  else
    echo "Error: Neither stack nor docker is available." >&2
    exit 1
  fi
else
  echo "Notice: Server binary not yet built. Please complete M0 and M1 to launch server."
  exit 1
fi
