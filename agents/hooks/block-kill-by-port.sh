#!/bin/bash
# Block killing "whatever is on a port" when lsof is not limited to the listener.
# `lsof -ti tcp:8131` lists EVERY process with a socket on the port, the browser
# showing the page included: `kill $(lsof -ti tcp:8131)` killed the user's Firefox
# (needwise, 2026-11-05). `-sTCP:LISTEN` narrows it to the server.
# Exit 2 = blocked, 0 = allowed.

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')
[ -z "$COMMAND" ] && exit 0

# Only lsof that feeds a kill: `kill … $(lsof …)` / backticks, or `lsof … | xargs … kill`.
FEEDS_KILL='kill[^;&|]*(\$\(|`)[[:space:]]*lsof|lsof[^;&]*\|[[:space:]]*xargs[^;&|]*kill'
echo "$COMMAND" | grep -qE "$FEEDS_KILL" || exit 0

# Each lsof invocation up to its closing ) ` | ; &
LSOFS=$(echo "$COMMAND" | grep -oE 'lsof[^)`|;&]*')
while IFS= read -r l; do
  [ -z "$l" ] && continue
  case "$l" in
    *-sTCP:LISTEN*) ;;
    *)
      echo "BLOCKED: '$l' lists every process with a socket on that port, the user's browser included; killing them all closes the browser. Kill the listener only: lsof -ti tcp:PORT -sTCP:LISTEN (or the server's own PID)." >&2
      exit 2 ;;
  esac
done <<< "$LSOFS"
exit 0
