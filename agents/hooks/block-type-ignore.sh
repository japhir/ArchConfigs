#!/bin/bash
# Edits that add `# type: ignore` / `# ty: ignore` route around the type checker.
# Fix the type instead (narrow with assert/isinstance, adjust the annotation, or fix the call).

INPUT=$(cat)
# Prose files may quote the comment; only code files are guarded.
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')
case "$FILE" in *.md|*.org|*.txt|*.rst|*.json|*.yaml|*.yml|*.toml) exit 0 ;; esac
NEW=$(echo "$INPUT" | jq -r '.tool_input.new_string // .tool_input.content // empty')
OLD=$(echo "$INPUT" | jq -r '.tool_input.old_string // empty')

count() { printf '%s' "$1" | grep -cE '#\s*(type|ty):\s*ignore'; }

if [ "$(count "$NEW")" -gt "$(count "$OLD")" ]; then
  jq -n '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",
    permissionDecisionReason:"This edit adds a type-ignore comment. Fix the type instead: narrow with `assert x is not None` / `isinstance`, correct the annotation, or fix the call site. If the checker is genuinely wrong, say so and ask."}}'
fi
exit 0
