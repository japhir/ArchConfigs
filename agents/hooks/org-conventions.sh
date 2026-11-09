#!/bin/bash
# PostToolUse on Read|Edit|Write|MultiEdit|Bash: an .org file was just touched.
# Hands the org conventions back as additionalContext: the full org-mode.md on the
# first touch per session (and per subagent), a one-line pointer on later touches,
# plus the tracker-doc pointer whenever a touched file has a `* Tickets` heading.
# CLAUDE.md carries no org line; a session that never touches org pays nothing.
# PostToolUse rather than PreToolUse: there additionalContext rides on
# permissionDecision "allow", which would auto-approve the call. Fail-open.
# bash 3.2 compatible (macOS).

INPUT=$(cat) || exit 0
command -v jq >/dev/null 2>&1 || exit 0
TOOL=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty' 2>/dev/null) || exit 0
CWD=$(printf '%s' "$INPUT" | jq -r '.cwd // empty' 2>/dev/null)

case "$TOOL" in
  Read|Edit|Write|MultiEdit)
    TOKENS=$(printf '%s' "$INPUT" | jq -r '.tool_input.file_path // empty') ;;
  Bash)
    TOKENS=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty' |
      grep -oE "[^[:space:]\"'\`;|&()<>=]+\.org(_archive)?([^[:alnum:]_./-]|$)" |
      sed -E 's/[^[:alnum:]_./-]$//' | grep -vE '^[a-z]+://' || true) ;;
  *) exit 0 ;;
esac

resolve() {  # token -> absolute path (~ and cwd-relative expanded)
  local t="$1"
  case "$t" in "~"|"~/"*) t="$HOME${t#\~}" ;; esac
  case "$t" in /*) ;; *) t="${CWD:-$PWD}/$t" ;; esac
  printf '%s' "$t"
}

ORG=""; TRACKER=""
for tok in $TOKENS; do
  case "$tok" in *.org|*.org_archive) ;; *) continue ;; esac
  p=$(resolve "$tok")
  if [ -f "$p" ]; then
    ORG="$ORG $tok"
    grep -q '^\* Tickets' "$p" 2>/dev/null && TRACKER="$TRACKER $tok"
  else
    case "$tok" in */*) ORG="$ORG $tok" ;; esac   # a path that does not exist yet
  fi
done
[ -n "$ORG" ] || exit 0

SID=$(printf '%s' "$INPUT" | jq -r '.session_id // empty')
AID=$(printf '%s' "$INPUT" | jq -r '.agent_id // empty')
DIR=$(printf '%s' "$INPUT" | jq -r '.scratchpad_dir // empty'); [ -d "$DIR" ] || DIR="${TMPDIR:-/tmp}"
MARK="$DIR/.org-conventions-${SID:-nosession}-${AID:-main}"

DOC="$HOME/.claude/docs/org-mode.md"
if [ -e "$MARK" ]; then
  CTX="Org file touched:$ORG. Conventions: $DOC."
else
  touch "$MARK" 2>/dev/null
  CTX="First .org file this session:$ORG. Conventions for editing it, from $DOC:"$'\n'"$(cat "$DOC" 2>/dev/null)"
fi
if [ -n "$TRACKER" ]; then
  CTX="$CTX"$'\n'"Ticket tracker (a \`* Tickets\` heading):$TRACKER. Read ~/.claude/docs/issue-tracker-org.md before touching it: ticket shape, claim, finish, edits through emacsclient."
fi
printf '%s' "$CTX" | jq -Rs '{hookSpecificOutput:{hookEventName:"PostToolUse",additionalContext:.}}'
exit 0
