#!/bin/bash
# PreToolUse on Agent: every subagent spawn names its model tier explicitly.
# Silent when model is sonnet/opus/fable (or subagent_type is fork, which inherits).
# Denies haiku or a missing model and hands the tier table back as the reason,
# so the rule costs zero context until the moment it applies.

INPUT=$(cat)
MODEL=$(echo "$INPUT" | jq -r '.tool_input.model // empty')
TYPE=$(echo "$INPUT" | jq -r '.tool_input.subagent_type // empty')

[ "$TYPE" = "fork" ] && exit 0
case "$MODEL" in sonnet|opus|fable) exit 0 ;; esac

if [ -z "$MODEL" ]; then WHY="no model: given"; else WHY="model: $MODEL is out of the rotation"; fi

jq -n --arg why "$WHY" '{
  hookSpecificOutput: {
    hookEventName: "PreToolUse",
    permissionDecision: "deny",
    permissionDecisionReason: ($why + ". Pick the tier for this task and retry with model: set.\n- sonnet: mechanical, fully specified — sweeps, lint/format fixes, running a written recipe, lookups.\n- opus: judgement — review, design, debugging, non-trivial code.\n- fable: rarely — only when opus would plausibly get it wrong (deep multi-file architecture, hard diagnosis).")
  }
}'
