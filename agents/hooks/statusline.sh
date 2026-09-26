#!/bin/bash
# Claude Code statusline: context %% used, followed by (Xk in, Yk out).
# Reads the stdin JSON Claude Code feeds statusLine commands.
# https://code.claude.com/docs/en/statusline

INPUT=$(cat)

PCT=$(jq -r '.context_window.used_percentage // empty' <<<"$INPUT")
IN_TOK=$(jq -r '.context_window.total_input_tokens // 0' <<<"$INPUT")
OUT_TOK=$(jq -r '.context_window.total_output_tokens // 0' <<<"$INPUT")

fmt_k() {
  awk -v n="$1" 'BEGIN { printf "%.1fk", n/1000 }'
}

if [ -z "$PCT" ]; then
  PCT_STR="–%"
else
  PCT_STR=$(awk -v p="$PCT" 'BEGIN { printf "%.0f%%", p }')
fi

IN_STR=$(fmt_k "$IN_TOK")
OUT_STR=$(fmt_k "$OUT_TOK")

printf '%s (%s in, %s out)' "$PCT_STR" "$IN_STR" "$OUT_STR"
