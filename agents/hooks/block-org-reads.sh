#!/bin/bash
# PreToolUse on Read|Grep|Bash: ~/org holds private GTD/journal data.
# Metadata (ls, wc, file, grep -c, Glob) is fine; contents only for the files
# listed in org-read-allowlist.txt (paths relative to ~/org, one per line).
# Read/Grep are checked deterministically; Bash is a best-effort guardrail
# (a determined `python -c open(...)` gets through), not a proof.

INPUT=$(cat)
TOOL=$(echo "$INPUT" | jq -r '.tool_name')
ORG="$HOME/org"
ALLOW="$(dirname "$(readlink -f "$0")")/org-read-allowlist.txt"

deny() {
  jq -n --arg why "$1" '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",
    permissionDecisionReason:($why + " ~/org is private: metadata only (ls, wc, file, grep -c). Contents are allowed only for the files in agents/hooks/org-read-allowlist.txt; ask the user to extend it rather than working around this.")}}'
  exit 0
}

allowed() {  # $1: absolute path
  [ -f "$ALLOW" ] || return 1
  while IFS= read -r rel; do
    [ -n "$rel" ] && [ "$1" = "$ORG/$rel" ] && return 0
  done < "$ALLOW"
  return 1
}

under_org() { case "$1" in "$ORG"|"$ORG"/*) return 0 ;; esac; return 1; }

case "$TOOL" in
  Read)
    F=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')
    F=$(realpath -m "$F")
    under_org "$F" && ! allowed "$F" && deny "Read of $F blocked."
    ;;
  Grep)
    P=$(echo "$INPUT" | jq -r '.tool_input.path // empty')
    [ -z "$P" ] && P=$(echo "$INPUT" | jq -r '.cwd')
    P=$(realpath -m "$P")
    MODE=$(echo "$INPUT" | jq -r '.tool_input.output_mode // "files_with_matches"')
    [ "$MODE" = "content" ] || exit 0
    # A search rooted above ~/org would sweep it too.
    case "$ORG/" in "$P"/*) deny "Content grep over $P would include ~/org." ;; esac
    under_org "$P" && ! allowed "$P" && deny "Content grep in $P blocked."
    ;;
  Bash)
    CMD=$(echo "$INPUT" | jq -r '.tool_input.command // empty')
    CWD=$(echo "$INPUT" | jq -r '.cwd // empty')
    # Strip allowlisted paths, then look for any remaining mention of ~/org.
    REST="$CMD"
    if [ -f "$ALLOW" ]; then
      while IFS= read -r rel; do
        [ -z "$rel" ] && continue
        for pre in "$ORG" '~/org' '$HOME/org'; do REST=${REST//"$pre/$rel"/}; done
      done < "$ALLOW"
    fi
    MENTIONS=0
    printf '%s' "$REST" | grep -qE "(~|\\\$HOME|$HOME)/org(/|\b|$)" && MENTIONS=1
    under_org "$(realpath -m "${CWD:-/}")" && MENTIONS=1
    [ "$MENTIONS" = 1 ] || exit 0
    # Counting greps only print numbers.
    READERS=$(printf '%s' "$REST" | sed -E 's/grep +-[a-zA-Z]*c[a-zA-Z]*//g')
    if printf '%s' "$READERS" | grep -qE '(^|[^a-zA-Z_-])(cat|head|tail|less|more|bat|sed|awk|rg|grep|emacs|emacsclient|python3?|perl|jq|strings|xxd|od|cp|rsync|tar|zip|base64|git|diff|sort|uniq|cut|tr)([^a-zA-Z_-]|$)'; then
      deny "Bash command reads inside ~/org."
    fi
    ;;
esac
exit 0
