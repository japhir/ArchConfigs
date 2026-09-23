#!/bin/bash

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command')

# Push has its own message: it is the maintainer's move, never a pending item for the agent.
if echo "$COMMAND" | grep -qE "git push"; then
  echo "Pushing is the maintainer's move. Local main is the landed state; report the sha and continue." >&2
  exit 2
fi

# Broad staging sweeps in another agent's edits (shared trees) — stage explicit paths.
if echo "$COMMAND" | grep -qE "git add( -[a-zA-Z]+)* +(-A|--all|\.)( |$)"; then
  echo "Stage explicit paths (git add <files you changed>); a parallel agent may have edits in this tree." >&2
  exit 2
fi

DANGEROUS_PATTERNS=(
  "git reset --hard"
  "git clean -fd"
  "git clean -f"
  "git branch -D"
  "git checkout \."
  "git restore \."
  "push --force"
  "reset --hard"
)

for pattern in "${DANGEROUS_PATTERNS[@]}"; do
  if echo "$COMMAND" | grep -qE "$pattern"; then
    echo "BLOCKED: '$COMMAND' matches dangerous pattern '$pattern'. The user has prevented you from doing this." >&2
    exit 2
  fi
done

exit 0
