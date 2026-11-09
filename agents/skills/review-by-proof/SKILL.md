---
name: review-by-proof
description: Review a large body of work where every finding arrives as proof (a red test or a green patch) on one branch you read commit by commit.
disable-model-invocation: true
---

Every finding reaches the maintainer as **proof** a machine can check: a **red test** (a test that fails on the reviewed commit, plus the smallest fix that turns it green) or a **green patch** (the smallest fix, with the repo's check passing). A claim without proof is **unproven**: it goes in the report's list, and the branch holds only proof.

## Steps

1. **Pin the scope.** The fixed point the user gave (a sha, branch, `main`), or "whole repo" for everything the repo owns. `HEAD`'s sha is the **reviewed commit**. Collect the sources: the repo's coding standards (`CODING_STANDARDS.md` or what `CLAUDE.md` points to), ADRs, and the spec or tickets behind the work. Done when you hold the reviewed sha, the files in scope, and the source paths.

2. **Slice.** Split the scope into 2–5 slices by module, each a fresh context's worth of code. Done when every file in scope sits in exactly one slice.

3. **Spawn finders**, one per slice, in parallel, `model: opus`, `isolation: worktree`, each given its slice, the reviewed sha, the source paths, and [FINDER.md](FINDER.md) pasted in full. Done when every finder has returned its branch and report.

4. **Re-run every proof yourself**; proof is what you saw run. For each commit on each finder branch:
   - The repo's check (`just check`) passes at that commit.
   - A red-test commit: in a detached worktree at the reviewed sha (`git worktree add --detach <path> <sha>`, then the repo's setup recipe), apply only the commit's test hunks (`git diff <sha> <commit> -- <test paths> | git -C <path> apply`) and run that test file: it goes red.

   A commit that fails either check moves to unproven, with what failed. Done when every commit is verified or moved.

5. **Assemble** in a worktree of its own: `git worktree add <path> -b review/<YYYY-MM-DD> <reviewed sha>`, then `git -C <path> cherry-pick` the verified commits, slice after slice. A cherry-pick that conflicts is an **overlap**: `git -C <path> cherry-pick --skip`, and list it beside the commit it collided with. Run the check once on the tip. Done when the tip is green and every verified commit is on the branch or listed as an overlap.

6. **Clean up** every worktree this review added (finders, verify, assembly) and the finder branches, keeping `review/<date>`. Done when `git worktree list` matches what it showed before step 3.

## Report

`review/<date>` @ `<sha>` on reviewed `<sha>`, then:

- **Proven**, one line per commit in branch order: short sha, red test or green patch, the source it cites, lines changed.
- **Overlaps**: each skipped commit and what it collided with.
- **Unproven**: each claim in one line, with why it has no proof (no test went red, the fix outgrew the size cap, the check failed). Offer to file them as one `needs-triage` issue.
- **How to take it**: read `git log -p --reverse <reviewed sha>..review/<date>` one commit at a time; drop what you don't want (`git rebase -i`), then `just land review/<date>`.
