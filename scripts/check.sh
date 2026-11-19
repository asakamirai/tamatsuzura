#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
LOG_FILE="${ROOT_DIR}/.check.log"

cd "${ROOT_DIR}"

echo "=== Running check (build -> test -> lint) ==="
rm -f "${LOG_FILE}"
: > "${LOG_FILE}"

step() {
  local name="$1"
  local cmd="$2"
  printf '[check] Running %s... ' "${name}"
  printf '\n===== [check] %s =====\n' "${name}" >> "${LOG_FILE}"
  if "${cmd}" >> "${LOG_FILE}" 2>&1; then
    echo "OK"
  else
    echo "FAILED"
    echo ""
    echo "=== Error details (${name}) from .check.log ==="
    tail -n 30 "${LOG_FILE}"
    echo "=============================================="
    echo "For full logs, see .check.log"
    return 1
  fi
}

step "build" "${SCRIPT_DIR}/build.sh"
step "test" "${SCRIPT_DIR}/test.sh"
step "lint" "${SCRIPT_DIR}/lint.sh"

echo "[check] All checks passed successfully!"
