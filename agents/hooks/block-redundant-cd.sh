#!/bin/bash
# Block a leading `cd <path>` when <path> is the directory the shell is already in.
# The Bash tool's cwd persists across calls, so `cd $PWD && ...` is pure noise.
# Scoped to a LEADING cd only; a `cd` into a subdir or another repo passes through.

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command')

# Session cwd is the authoritative "where the Bash tool currently is". The hook
# process's own pwd is the launch dir and drifts from the Bash tool cwd (worktrees,
# subagents, bg jobs) — so compare against .cwd from the hook input, not `pwd`.
SESSION_CWD=$(echo "$INPUT" | jq -r '.cwd // empty')
[ -n "$SESSION_CWD" ] || SESSION_CWD=$(pwd -P)
CWD_RESOLVED=$(cd "$SESSION_CWD" 2>/dev/null && pwd -P)
[ -n "$CWD_RESOLVED" ] || CWD_RESOLVED=$(pwd -P)

if echo "$COMMAND" | grep -qE '^[[:space:]]*cd[[:space:]]'; then
  # target = first arg after `cd`, up to a && ; | separator or end
  TARGET=$(echo "$COMMAND" | sed -E 's/^[[:space:]]*cd[[:space:]]+//; s/[[:space:]]*(&&|;|\|).*$//; s/[[:space:]]+$//')
  TARGET="${TARGET%\"}"; TARGET="${TARGET#\"}"
  TARGET="${TARGET%\'}"; TARGET="${TARGET#\'}"
  if [ -n "$TARGET" ]; then
    RESOLVED=$(cd "$SESSION_CWD" 2>/dev/null && cd "$TARGET" 2>/dev/null && pwd -P)
    if [ -n "$RESOLVED" ] && [ "$RESOLVED" = "$CWD_RESOLVED" ]; then
      echo "BLOCKED: redundant 'cd $TARGET' — shell is already in $CWD_RESOLVED. Drop the leading cd and rerun the rest of the command." >&2
      exit 2
    fi
  fi
fi

exit 0
