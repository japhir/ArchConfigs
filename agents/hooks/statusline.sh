#!/bin/bash
# Claude Code statusline: [model] Ctx:NN% (in:Xk out:Yk), coloured by usage
# (green < 50%, yellow < 80%, red above).
# Reads the stdin JSON Claude Code feeds statusLine commands.
# https://code.claude.com/docs/en/statusline

input=$(cat)

# Model indicator (e.g. "Opus 5.5")
model_name=$(printf '%s' "$input" | jq -r '.model.display_name // empty')
model=""
[ -n "$model_name" ] && model=$(printf '\033[35m[%s]\033[0m' "$model_name")

# Context usage percentage (pre-calculated field, null before first API call)
used=$(printf '%s' "$input" | jq -r '.context_window.used_percentage // empty')

# Tokens in the context window (in/out, most recent API response)
tok_in=$(printf '%s' "$input" | jq -r '.context_window.total_input_tokens // 0')
tok_out=$(printf '%s' "$input" | jq -r '.context_window.total_output_tokens // 0')
fmt_tok() {
    if [ "$1" -ge 1000 ]; then
        awk -v t="$1" 'BEGIN{printf "%.1fk", t/1000}'
    else
        printf '%s' "$1"
    fi
}
tok_in_fmt=$(fmt_tok "$tok_in")
tok_out_fmt=$(fmt_tok "$tok_out")

parts=()
[ -n "$model" ] && parts+=("$model")
if [ -n "$used" ]; then
    used_int=$(printf '%.0f' "$used")
    if [ "$used_int" -ge 80 ]; then colour=31
    elif [ "$used_int" -ge 50 ]; then colour=33
    else colour=32
    fi
    parts+=("$(printf '\033[%smCtx:%s%% (in:%s out:%s)\033[0m' "$colour" "$used_int" "$tok_in_fmt" "$tok_out_fmt")")
else
    parts+=("Ctx:–% (in:$tok_in_fmt out:$tok_out_fmt)")
fi

printf '%s' "${parts[*]}"
