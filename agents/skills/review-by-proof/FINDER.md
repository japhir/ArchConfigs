# Finder brief

You review one slice of a codebase against its sources (coding standards, ADRs, spec or tickets) at the reviewed commit, and turn each finding into **proof**: one commit the maintainer reads as a diff and a machine re-runs. Your worktree branch holds the commits; the orchestrator verifies and assembles them.

## Setup

`git merge --ff-only <reviewed sha>` if your worktree starts elsewhere, then the repo's setup recipe (`just setup`) if it has one. Read the sources in full, then the code.

## One finding, one commit

For each finding:

1. **Name the claim** in one line, citing its source exactly: the standards file and rule, the ADR, or the spec line or acceptance criterion.
2. **Prove it.**
   - **Red test** (a bug, or a spec requirement the code misses): write the test first, at the public interface a caller uses, and run it: it goes **red** for the reason the claim names. Then the smallest fix that turns it green.
   - **Green patch** (a standards breach or a smell): the smallest change that fixes it.
3. **Check** with the repo's check recipe (`just check`): green.
4. **Commit** that finding's files alone. Subject in the repo's commit style; body: the claim, its source, and for a red test the test's name.

A claim is **unproven**, and gets a line in your report instead of a commit, when no test you write goes red for it, when its fix outgrows about 50 changed lines (put the red test's code in the report: it becomes a ticket), or when its patch can't pass the check (say what broke: often the claim was wrong).

One proven finding is worth three suspected ones: your review is measured by the commits that survive verification.

## Report

Your branch name, then one line per commit (sha, red test or green patch, source cited), then the unproven list (claim, source, why there is no proof).
