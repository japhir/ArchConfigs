# Git workflow & orchestration

Local-first, linear history. Commit per coherent step, early — sessions can be cut off mid-work. Push happens only when the maintainer chooses; local `main` may sit ahead of `origin/main` indefinitely.

## Landing a branch
1. **Order by containment.** `git merge-base --is-ancestor A B` true → B subsumes A: land B, delete A.
2. **Rebase onto current main:** `git rebase main <branch>`. Shared hot-file conflicts surface here, with the branch author's context still available.
3. **Fast-forward:** `git checkout main && git merge --ff-only <branch>`. Done when `git log --oneline -1 main` is the branch tip.
4. Repeat 2–3 per remaining branch (main has moved). `--squash` for messy WIP; a real merge commit only for genuinely parallel branches.
5. `git worktree remove <path> && git branch -d <branch>`.

Skip any branch whose worktree shows `locked` in `git worktree list` — another session is mid-work there. Land quiescent branches first.

## Parallel work
- **Hot files** (e.g. a CLI subcommand enum, a crate/module list): slices touching the same hot file run serially; leaf/data slices parallelize. After landing, build and test the merged tree: isolated slices miss cross-slice conflicts.
- **Stage explicit paths** (`git add <files I changed>`). Another agent may be committing in the same tree; a "file modified by user or linter" notice on a file you didn't touch is the tell. Check `git branch --show-current` before each commit.
- **Background jobs**: start it, then do unrelated work until the harness's completion notification arrives. Read output on notification or when genuinely blocked.

## Subagent contract (pin it in every spawn prompt)
> Commit on your worktree branch. The orchestrator merges, closes the issue, and pushes. Your blocker landed in **local** main and your worktree base includes it; verify against local, not origin.

After a subagent returns:
- Verify: `git -C <worktree> status` clean and a commit exists before merging.
- **Harvest side-findings.** Scan the result for "found / flagged / out of scope / pre-existing" and file each real one as an issue now — result text is the only place they live.
- A subagent that died mid-flight on a stale base: restart fresh on current main rather than resume.
