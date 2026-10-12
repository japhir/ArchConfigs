#!/bin/bash
# Run graphify's search guard only for commands that search files
# (grep/rg/ugrep/find/ag as a command, not as a pipe filter).
# Every other Bash call (git, gh, sed, emacsclient, ...) skips it,
# so its "run graphify query first" reminder appears only where it applies.

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

if echo "$COMMAND" | grep -qE '(^|&&|;)[[:space:]]*(grep|rg|ugrep|find|ag)([[:space:]]|$)'; then
  echo "$INPUT" | /Users/iljakocken/.local/bin/graphify hook-guard search
  exit $?
fi
exit 0
