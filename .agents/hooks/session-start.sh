#!/usr/bin/env bash
set -euo pipefail

# SessionStart hook: inject a concise summary of current project status from docs/PROJECT.md.
# Resolves paths relative to this script so it works from any working directory.
# Must output valid SessionStart hook JSON on stdout.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
PROJECT_DOC="${ROOT_DIR}/docs/PROJECT.md"

# Consume piped stdin payload so upstream pipe never gets SIGPIPE (skip when run interactively on a TTY).
if [ ! -t 0 ]; then
  cat >/dev/null || true
fi

if [ ! -f "${PROJECT_DOC}" ]; then
  printf '{}\n'
  exit 0
fi

if command -v python3 >/dev/null 2>&1; then
  python3 - "${PROJECT_DOC}" <<'PY'
import json
import sys

path = sys.argv[1]
try:
    with open(path, "r", encoding="utf-8") as f:
        lines = [line.rstrip("\n") for line in f]
except OSError:
    print("{}")
    sys.exit(0)

status_line = ""
in_status = False
in_next = False
next_header = ""
pending_tasks = []

for line in lines:
    stripped = line.strip()
    if stripped.startswith("## 現在地"):
        in_status = True
        in_next = False
        continue
    if stripped.startswith("## 次のタスク"):
        in_status = False
        in_next = True
        continue
    if stripped.startswith("## "):
        in_status = False
        in_next = False
        continue

    if in_status and not status_line and stripped and not stripped.startswith("|") and not stripped.startswith("※"):
        status_line = stripped.strip("*")
    if in_next:
        if not next_header and stripped.startswith("**") and stripped.endswith("**"):
            next_header = stripped.strip("*")
        elif stripped.startswith("- [ ] "):
            pending_tasks.append(stripped[6:])

parts = ["[TamaTsuzura Session Context]"]
if status_line:
    parts.append(f"現在地: {status_line}")
if next_header:
    parts.append(f"対象: {next_header}")
if pending_tasks:
    preview = "; ".join(pending_tasks[:4])
    if len(pending_tasks) > 4:
        preview += f" (+{len(pending_tasks) - 4}件)"
    parts.append(f"未完了タスク: {preview}")
parts.append("詳細は docs/PROJECT.md を確認してください。")

payload = {
    "injectSteps": [
        {
            "systemMessage": {
                "systemMessage": "\n".join(parts)
            }
        }
    ]
}
print(json.dumps(payload, ensure_ascii=False))
PY
else
  printf '{"injectSteps":[{"systemMessage":{"systemMessage":"[TamaTsuzura Session Context] セッション開始時は docs/PROJECT.md で現在地と次の Phase を確認してください。"}}]}\n'
fi
