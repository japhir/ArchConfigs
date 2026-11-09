# Recipes every repo shares. A repo's justfile pulls them in with
#   import? '~/ArchConfigs/agents/just/land.just'
# (`import?`: a machine without this checkout, such as CI, skips it).

# land a worktree branch onto this one: rebase, check, fast-forward, clean up
land branch:
    #!/usr/bin/env bash
    set -euo pipefail
    here=$(git branch --show-current)
    if [ -z "$here" ] || [ "$here" = "{{branch}}" ]; then
        echo "check out the branch to land onto first"; exit 1
    fi
    # `local/*` names a branch that stays on this machine (real data, a personal setup).
    case "{{branch}}" in local/*) echo "{{branch}} is a local/ branch: it never lands"; exit 1 ;; esac
    wt=$(git worktree list --porcelain | awk -v b="branch refs/heads/{{branch}}" '/^worktree /{w=substr($0, 10)} $0==b{print w}')
    [ -n "$wt" ] || { echo "no worktree holds {{branch}}"; exit 1; }
    [ -z "$(git -C "$wt" status --porcelain)" ] || { echo "$wt has uncommitted changes"; exit 1; }
    git -C "$wt" rebase "$here"
    # The repo's own gate, when it has one, runs on the rebased tree.
    if (cd "$wt" && just --summary 2>/dev/null) | tr ' ' '\n' | grep -qx check; then
        (cd "$wt" && just check)
    fi
    git merge --ff-only "{{branch}}"
    git worktree remove "$wt"
    git branch -d "{{branch}}"
