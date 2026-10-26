#!/bin/bash
# bash 3.2 compatible (macOS): no associative arrays.

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command')
case "$COMMAND" in *git*) ;; *) exit 0 ;; esac   # most commands never name git

# Every rule below matches git at command position only: the start of the text, or after &&, ||, ;,
# |, &, a newline, (, $( or a backtick, and past do, then, xargs, VAR=value and the like. Text that
# merely mentions a command passes: a quoted string stays one word (its blanks become _), and heredoc
# bodies (unless fed to a shell) and comments drop out. SPLIT prints the commands the text runs, one
# per line. A guard against slips, not a sandbox: bash -c, eval and scripts pass unread.
SPLIT='
function lit(c) { return (c == " " || c == "\t") ? "_" : c }
function emit(   s) {
  s = seg; seg = ""
  gsub(/[ \t]+/, " ", s); sub(/^ /, "", s); sub(/ $/, "", s)
  # Words that leave the next word at command position: keywords, wrappers, VAR=value.
  while (sub(/^(if|then|elif|else|do|while|until|time|!|[{]|env|command|exec|nohup|sudo|xargs( -[^ ]+)*|[A-Za-z_][A-Za-z0-9_]*=[^ ]*) /, "", s)) {}
  if (s != "") print s
}
# A $(...), (...) or backtick runs its own command; the command around it resumes after it.
function enter(closer) { sp++; cl[sp] = closer; oq[sp] = q; os[sp] = seg; seg = ""; q = "" }
function leave() { emit(); seg = os[sp] "$()"; q = oq[sp]; sp-- }
BEGIN { SQ = sprintf("%c", 39) }
hn {                                     # a heredoc body is data, not commands
  t = $0; if (htab[hi]) sub(/^\t+/, "", t)
  if (t == hword[hi] && ++hi > hn) hn = 0
  next
}
{
  n = split($0, ch, ""); cont = 0
  for (i = 1; i <= n; i++) {
    c = ch[i]
    if (q == SQ) { if (c == SQ) q = ""; else seg = seg lit(c); continue }
    if (c == "\\") { if (i == n) cont = 1; else seg = seg lit(ch[++i]); continue }
    if (q == "\"") {
      if (c == "\"") q = ""
      else if (c == "$" && ch[i + 1] == "(") { i++; enter(")") }
      else if (c == "`") enter("`")
      else seg = seg lit(c)
      continue
    }
    if (c == SQ || c == "\"") q = c
    else if (c == "#" && (seg == "" || seg ~ /[ \t]$/)) break            # a comment
    else if (sp && c == cl[sp]) leave()
    else if (c == "$" && ch[i + 1] == "(") { i++; enter(")") }
    else if (c == "(") enter(")")
    else if (c == "`") enter("`")
    else if (c ~ /[;&|)]/) emit()
    else if (c == "<" && ch[i + 1] == "<" && ch[i - 1] != "<" && ch[i + 2] != "<") {  # a heredoc
      i += 2; tab = (ch[i] == "-"); if (tab) i++
      while (ch[i] == " " || ch[i] == "\t") i++
      w = ""; d = ch[i]                  # the delimiter word, quotes dropped
      if (d == SQ || d == "\"") { while (++i <= n && ch[i] != d) w = w ch[i] }
      else {
        for (; i <= n && ch[i] !~ /[ \t;&|<>()]/; i++) if (ch[i] !~ /[\\"]/ && ch[i] != SQ) w = w ch[i]
        i--
      }
      # A body fed to a shell is commands after all; an unquoted <<2 is arithmetic.
      if ((d == SQ || d == "\"" || w ~ /^[A-Za-z_]/) && seg !~ /^[ \t]*([^ \t]*\/)?(ba|z|da|k)?sh([ \t]|$)/) { htab[++hp] = tab; hword[hp] = w }
      seg = seg " "
    }
    else seg = seg c
  }
  if (!cont) { if (q == "") emit(); else seg = seg "_" }
  if (hp && !cont && q == "") { hn = hp; hi = 1; hp = 0 }
}
END { emit(); while (sp) { seg = os[sp--]; emit() } }
'
CMDS=$(printf '%s\n' "$COMMAND" | awk "$SPLIT")

# git and its global options (-C <path>, -c <key=value>, --no-pager, --git-dir=<path>, ...).
GIT='^([^ ]*/)?git( -[Cc] [^ ]+| --[a-z-]+(=[^ ]+)?| -[pP])* '
# The commands that run git <subcommand>; $1 is an extended regex starting at the subcommand.
git_cmds() { printf '%s\n' "$CMDS" | grep -E "$GIT$1"; }
first() { printf '%s\n' "$CMDS" | grep -m1 -E "$GIT$1"; }
printf '%s\n' "$CMDS" | grep -qE "$GIT" || exit 0   # git only in prose

# Push has its own message: it is the maintainer's move, never a pending item for the agent.
# Force push is a push: this rule refuses it too.
if git_cmds 'push( |$)' >/dev/null; then
  echo "Pushing is the maintainer's move. Local main is the landed state; report the sha and continue." >&2
  exit 2
fi

# An agent worktree (.claude/worktrees/) holds one branch: its agent commits there and a reviewer
# reads diffs. Switching or stashing in it has left an implementer's tree on another branch mid-task.
# Restoring a file leaves the branch where it is: checkout <rev> -- <path>, restore <path> and
# stash list/show pass. A checkout with no path after -- switches the branch.
CWD=$(echo "$INPUT" | jq -r '.cwd // empty')
if [[ "$CWD" == */.claude/worktrees/* ]] || printf '%s\n' "$CMDS" | grep -q '\.claude/worktrees/'; then
  if git_cmds 'switch( |$)' >/dev/null ||
     git_cmds 'checkout( |$)' | grep -qvE ' -- [^ ]' ||
     git_cmds 'stash( |$)' | grep -qvE "${GIT}stash (list|show)( |$)"; then
    echo "This is an agent worktree: work on its branch as it is, and commit work in progress instead of stashing it. To read another revision, use git show <rev>:<path> or git diff <rev>; to restore a file, git restore <path> or git checkout -- <path>." >&2
    exit 2
  fi
fi

# Broad staging sweeps in another agent's edits (shared trees): stage explicit paths.
if git_cmds 'add( [^ ]+)* (-A|--all|\.\.?/?|:/)( |$)' >/dev/null; then
  echo "Stage explicit paths (git add <files you changed>); a parallel agent may have edits in this tree." >&2
  exit 2
fi

# Destructive anywhere. Each refusal names the command, the pattern, and what to do instead.
refuse() {
  [ -n "$1" ] || return 0
  echo "BLOCKED: '$1' matches dangerous pattern '$2'. The user has prevented you from doing this. $3" >&2
  exit 2
}

# A force delete drops commits no other branch holds: -D, or a delete flag with a force flag.
refuse "$(git_cmds 'branch( |$)' | grep -E ' (-[a-zA-Z]*[dD][a-zA-Z]*|--delete)( |$)' |
          grep -m1 -E ' (-[a-zA-Z]*[fD][a-zA-Z]*|--force)( |$)')" 'branch -D' \
  'Rename the branch instead: git branch -m <branch> dropped/<branch>.'
refuse "$(first 'reset( [^ ]+)* --hard( |$)')" 'reset --hard' \
  'Restore the files you changed by path: git restore <path>.'
refuse "$(first 'clean( [^ ]+)* (-[a-zA-Z]*f[a-zA-Z]*|--force)( |$)')" 'clean -f' \
  'Delete the untracked files you created by name; git clean -n lists them.'
# . (or ./, .., :/) as a checkout or restore path means the whole tree, a parallel agent's edits included.
refuse "$(first '(checkout|restore)( [^ ]+)* (\.\.?/?|:/)( |$)')" 'checkout . / restore .' \
  'Restore the files you changed by path: git restore <path>.'

exit 0
