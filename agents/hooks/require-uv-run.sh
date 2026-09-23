#!/bin/bash
# `uvx ruff` / `uvx ty` run outside the project env and miss its config and pins.
# Only these two tools are redirected; uvx for anything else (whisper, one-off CLIs) passes.

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command')

if echo "$COMMAND" | grep -qE '(^|[;&| ])uvx +(ruff|ty)( |$)'; then
  echo "Use \`uv run ruff\` / \`uv run ty\` (project env, pinned versions), not uvx." >&2
  exit 2
fi
exit 0
