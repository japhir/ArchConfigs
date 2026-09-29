#!/bin/bash
# Off a TTY `gh issue/pr view N --comments` prints the comments ONLY, never the body.
# Block a `--comments` view unless the same command also runs the plain view of the
# same ref (or asks for --json). Prescribe the canonical compound form.
# bash 3.2 compatible (macOS): no associative arrays.

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')
[ -z "$COMMAND" ] && exit 0

# Only `gh` in command position (line start or after ; & | ( ), so a quoted
# mention in prose or a commit message does not trip it.
PAT='(^|[;&|(])[[:space:]]*gh (issue|pr) view[^;&|]*'
echo "$COMMAND" | grep -qE "$PAT" || exit 0

# Each `gh <kind> view ...` segment up to the next shell operator.
SEGMENTS=$(echo "$COMMAND" | grep -oE "$PAT" | sed -E 's/^[;&|(]?[[:space:]]*//')

# ref = first token after `view` that is not a flag or a flag's argument.
ref_of() {
  local skip=0 tok
  for tok in $1; do
    if [ "$skip" = 1 ]; then skip=0; continue; fi
    case "$tok" in
      gh|issue|pr|view) ;;
      -R|--repo|-t|--template|-q|--jq|--json) skip=1 ;;
      -*) ;;
      *) echo "$tok"; return ;;
    esac
  done
}

PLAIN=" "
while IFS= read -r seg; do
  [ -z "$seg" ] && continue
  echo "$seg" | grep -qE -- '--comments' && continue
  PLAIN="$PLAIN$(ref_of "$seg") "
done <<< "$SEGMENTS"

while IFS= read -r seg; do
  [ -z "$seg" ] && continue
  echo "$seg" | grep -qE -- '--comments' || continue
  echo "$seg" | grep -qE -- '--json' && continue
  kind=$(echo "$seg" | awk '{print $2}')
  ref=$(ref_of "$seg")
  case "$PLAIN" in *" $ref "*) continue ;; esac
  if [ "$kind" = pr ]; then
    note="PR view shows no comment count, so always run both."
  else
    note="Skip the second only when the first prints \`comments:\t0\`."
  fi
  cat >&2 <<MSG
BLOCKED: off a TTY \`gh $kind view $ref --comments\` prints the comments ONLY, never the body.
Run both in one call, plain view first:
  gh $kind view $ref && gh $kind view $ref --comments
$note
MSG
  exit 2
done <<< "$SEGMENTS"

exit 0
