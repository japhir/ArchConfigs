#!/bin/bash
# Usage: test-block-kill-by-port.sh <path to block-kill-by-port.sh>
# Feeds each case as {"tool_input":{"command":...}} and checks the exit code: 2 = blocked, 0 = allowed.

HOOK=${1:?usage: test-block-kill-by-port.sh <path to block-kill-by-port.sh>}
pass=0 fail=0
check() {
  local want=$1 label=$2 cmd=$3 got
  jq -n --arg c "$cmd" '{tool_input: {command: $c}}' | /bin/bash "$HOOK" >/dev/null 2>&1
  got=$?
  if [ "$got" = "$want" ]; then pass=$((pass + 1)); else fail=$((fail + 1)); printf 'FAIL want %s got %s: %s\n' "$want" "$got" "$label"; fi
}

check 2 'kill $(lsof) by port'            'kill $(lsof -ti tcp:8131) 2>/dev/null; echo ok'
check 2 'two lsofs, one fed to kill'      'kill $(lsof -ti tcp:8131) $(lsof -ti tcp:8133)'
check 2 'second lsof lacks LISTEN'        'kill $(lsof -ti tcp:8131 -sTCP:LISTEN) $(lsof -ti tcp:8133)'
check 2 'kill -9 with -i :port'           'kill -9 $(lsof -t -i :3000)'
check 2 'backticks'                       'kill `lsof -ti tcp:8131`'
check 2 'piped to xargs kill'             'lsof -ti tcp:8131 | xargs kill'

check 0 'listener only'                   'kill $(lsof -ti tcp:8131 -sTCP:LISTEN)'
check 0 'listener only, piped'            'lsof -ti tcp:8131 -sTCP:LISTEN | xargs kill -9'
check 0 'lsof to look, no kill'           'lsof -i tcp:8131'
check 0 'kill by pid'                     'kill 12345'
check 0 'unrelated'                       'curl -s http://127.0.0.1:8131/'

printf '%s passed, %s failed\n' "$pass" "$fail"
[ "$fail" = 0 ]
