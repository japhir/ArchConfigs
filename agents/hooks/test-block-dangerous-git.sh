#!/bin/bash
# Usage: test-hook.sh <path to block-dangerous-git.sh>
# Feeds each case to the hook as {"tool_input":{"command":...},"cwd":...} and checks its exit code:
# 2 = blocked, 0 = allowed. Prints each mismatch and a tally; exits 1 on any mismatch.

HOOK=${1:?usage: test-hook.sh <path to block-dangerous-git.sh>}
if [ -x "$HOOK" ]; then RUN=("$HOOK"); else RUN=(/bin/bash "$HOOK"); fi
WT=/r/.claude/worktrees/a   # an agent worktree
MAIN=/r                     # the main checkout
pass=0 fail=0

# check <want> <cwd> <label> [command]; with no command argument, the command is read from stdin.
check() {
  local want=$1 cwd=$2 label=$3 cmd got
  if [ $# -ge 4 ]; then cmd=$4; else cmd=$(cat); fi
  jq -n --arg c "$cmd" --arg d "$cwd" '{tool_input: {command: $c}, cwd: $d}' | "${RUN[@]}" >/dev/null 2>&1
  got=$?
  if [ "$got" = "$want" ]; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    printf 'FAIL want %s got %s: %s\n' "$want" "$got" "$label"
  fi
}

# Blocked in an agent worktree: switching its branch, stashing its work.
check 2 "$WT"   'branch checkout in a worktree'            'git checkout main'
check 2 "$WT"   'new-branch checkout in a worktree'        'git checkout -b feature'
check 2 "$WT"   'checkout <rev> -- with no path switches'  'git checkout main --'
check 2 "$WT"   'switch in a worktree'                     'git switch main'
check 2 "$WT"   'stash push in a worktree'                 'git stash push -m wip'
check 2 "$WT"   'bare stash in a worktree'                 'git stash'
check 2 "$WT"   'stash pop in a worktree'                  'git stash pop'
check 2 "$WT"   'stash pop after &&'                       'git status && git stash pop'
check 2 "$WT"   'stash pop in $( ) inside quotes'          'echo "$(git stash pop)"'
check 2 "$MAIN" 'branch checkout via -C into a worktree'   'git -C /r/.claude/worktrees/a checkout main'

# Blocked anywhere.
check 2 "$MAIN" 'push'                                     'git push'
check 2 "$MAIN" 'push to a remote branch'                  'git push origin main'
check 2 "$MAIN" 'force push'                               'git push --force origin main'
check 2 "$MAIN" 'push via -C'                              'git -C /r push'
check 2 "$MAIN" 'push after cd &&'                         'cd /r && git push'
check 2 "$MAIN" 'push after ;'                             'git status; git push'
check 2 "$MAIN" 'push behind VAR=value'                    'GIT_TRACE=1 git push'
check 2 "$MAIN" 'add -A'                                   'git add -A'
check 2 "$MAIN" 'add --all'                                'git add --all'
check 2 "$MAIN" 'add .'                                    'git add .'
check 2 "$MAIN" 'reset --hard'                             'git reset --hard'
check 2 "$MAIN" 'reset --hard to a rev'                    'git reset --hard HEAD~1'
check 2 "$MAIN" 'clean -fd'                                'git clean -fd'
check 2 "$MAIN" 'clean -xfd'                               'git clean -xfd'
check 2 "$MAIN" 'checkout .'                               'git checkout .'
check 2 "$MAIN" 'restore .'                                'git restore .'
check 2 "$WT"   'checkout -- . in a worktree'              'git checkout -- .'
check 2 "$MAIN" 'force branch delete'                      'git branch -D old'
check 2 "$MAIN" 'force branch delete, long flags'          'git branch --delete --force old'
check 2 "$MAIN" 'force branch delete, split flags'         'git branch -d -f old'
check 2 "$MAIN" 'force branch delete via -C'               'git -C /r branch -D old'
check 2 "$MAIN" 'force branch delete in a for loop'        'for b in a b; do git branch -D "$b"; done'
check 2 "$MAIN" 'force branch delete through xargs'        'git branch --merged | xargs git branch -D'
check 2 "$MAIN" 'push on the line after a comment' <<'CASE'
# don't push yet
git push
CASE
check 2 "$MAIN" 'push after a heredoc ends' <<'CASE'
cat > notes.md <<'EOF'
prose
EOF
git push
CASE
check 2 "$MAIN" 'push after a heredoc with a hyphenated delimiter' <<'CASE'
cat > notes.md <<'END-OF-DOC'
prose
END-OF-DOC
git push
CASE
check 2 "$MAIN" 'push in a heredoc fed to bash' <<'CASE'
bash <<'EOF'
git push
EOF
CASE
check 2 "$MAIN" 'force branch delete across a line continuation' <<'CASE'
git branch \
  -D old
CASE

# Allowed in an agent worktree: reading revisions and restoring files.
check 0 "$WT"   'checkout -- <path> in a worktree'         'git checkout -- src/x.rs'
check 0 "$WT"   'checkout <rev> -- <path> in a worktree'   'git checkout main -- src/x.rs'
check 0 "$MAIN" 'checkout -- <path> via -C into a worktree' 'git -C /r/.claude/worktrees/a checkout -- src/x.rs'
check 0 "$WT"   'restore <path> in a worktree'             'git restore src/x.rs'
check 0 "$WT"   'restore ./<path> in a worktree'           'git restore ./src/x.rs'
check 0 "$WT"   'restore --staged <path> in a worktree'    'git restore --staged src/x.rs'
check 0 "$WT"   'stash list in a worktree'                 'git stash list'
check 0 "$WT"   'show HEAD:<path> > <path> in a worktree'  'git show HEAD:src/x.rs > src/x.rs'
check 0 "$WT"   'diff against main in a worktree'          'git diff main'
check 0 "$MAIN" 'log via -C in a worktree'                 'git -C /a/.claude/worktrees/b log'

# Allowed: prose that mentions a blocked command.
check 0 "$WT"   'echo of a stash mention'                  'echo "see git stash"'
check 0 "$WT"   'echo of a stash list mention'             'echo "run git stash list to see them"'
check 0 "$MAIN" 'printf of a reset mention'                "printf '%s\n' 'git reset --hard is blocked' > notes.txt"
check 0 "$MAIN" 'grep for a push mention'                  'grep -n "git push" docs/notes.md'
check 0 "$MAIN" 'commit subject with ; then a push mention' 'git commit -m "docs: never git push; git stash is refused in worktrees"'
check 0 "$WT"   'heredoc whose body names blocked commands' <<'CASE'
cat > notes.md <<'EOF'
Agents never run git push; the maintainer does.
git stash pop and git checkout main are refused in a worktree.
git branch -D old: rename it to dropped/old instead.
git reset --hard and git add -A stay blocked too.
EOF
CASE
check 0 "$WT"   'commit message heredoc naming blocked commands' <<'CASE'
git commit -m "$(cat <<'EOF'
fix(hooks): let worktrees restore files

git checkout -- <path> and git stash list now pass; git push
stays the maintainer's move.
EOF
)"
CASE

# Allowed: everyday git outside a worktree.
check 0 "$MAIN" 'checkout main outside a worktree'         'git checkout main'
check 0 "$MAIN" 'safe branch delete'                       'git branch -d done'
check 0 "$MAIN" 'rename to dropped/'                       'git branch -m old dropped/old'
check 0 "$MAIN" 'add explicit paths'                       'git add src/x.rs docs/y.md'
check 0 "$MAIN" 'add a dotfile'                            'git add .gitignore'
check 0 "$MAIN" 'clean dry run'                            'git clean -n'
check 0 "$MAIN" 'log'                                      'git log --oneline -5'

echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
