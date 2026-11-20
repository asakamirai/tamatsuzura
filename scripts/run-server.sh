#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
USE_DOCKER="${USE_DOCKER:-0}"

cd "${ROOT_DIR}"

PORT="${PORT:-8080}"

# Automatically serve dist/web when a built Wasm bundle exists and TAMATSUZURA_STATIC_DIR was not explicitly set.
# Passing TAMATSUZURA_STATIC_DIR="" explicitly disables static serving.
if [ -z "${TAMATSUZURA_STATIC_DIR+x}" ]; then
  if [ -f "dist/web/index.html" ]; then
    export TAMATSUZURA_STATIC_DIR="dist/web"
  fi
elif [ -z "${TAMATSUZURA_STATIC_DIR}" ]; then
  unset TAMATSUZURA_STATIC_DIR
fi

echo "Starting TamaTsuzura development server on port ${PORT}..."
if [ -n "${TAMATSUZURA_STATIC_DIR:-}" ]; then
  echo "Serving static client assets from ${TAMATSUZURA_STATIC_DIR}"
fi

run_in_docker() {
  if command -v docker >/dev/null 2>&1 && [ -f "docker/docker-compose.yml" ]; then
    local docker_env_args=()
    if [ -n "${TAMATSUZURA_DB_PATH+x}" ]; then
      docker_env_args+=(-e "TAMATSUZURA_DB_PATH=${TAMATSUZURA_DB_PATH}")
    fi
    if [ -n "${TAMATSUZURA_DB_BUSY_TIMEOUT_MS+x}" ]; then
      docker_env_args+=(-e "TAMATSUZURA_DB_BUSY_TIMEOUT_MS=${TAMATSUZURA_DB_BUSY_TIMEOUT_MS}")
    fi
    if [ -n "${TAMATSUZURA_STATIC_DIR+x}" ]; then
      docker_env_args+=(-e "TAMATSUZURA_STATIC_DIR=${TAMATSUZURA_STATIC_DIR}")
    fi
    if [ -n "${TAMATSUZURA_ALLOW_REGISTRATION+x}" ]; then
      docker_env_args+=(-e "TAMATSUZURA_ALLOW_REGISTRATION=${TAMATSUZURA_ALLOW_REGISTRATION}")
    fi
    if [ -n "${TAMATSUZURA_SESSION_TTL_SECONDS+x}" ]; then
      docker_env_args+=(-e "TAMATSUZURA_SESSION_TTL_SECONDS=${TAMATSUZURA_SESSION_TTL_SECONDS}")
    fi
    if [ -n "${TAMATSUZURA_COOKIE_SECURE+x}" ]; then
      docker_env_args+=(-e "TAMATSUZURA_COOKIE_SECURE=${TAMATSUZURA_COOKIE_SECURE}")
    fi
    if [ -n "${TAMATSUZURA_TRUST_PROXY_HEADERS+x}" ]; then
      docker_env_args+=(-e "TAMATSUZURA_TRUST_PROXY_HEADERS=${TAMATSUZURA_TRUST_PROXY_HEADERS}")
    fi
    PORT="${PORT}" docker compose -f docker/docker-compose.yml run --rm --service-ports "${docker_env_args[@]}" dev "$@"
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
