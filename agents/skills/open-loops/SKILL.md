---
name: open-loops
description: Close out a stretch of work — landed tickets closed, side-findings filed, questions surfaced.
disable-model-invocation: true
---

Local `main` is the landed state; pushing is the maintainer's move.

1. **Landed commits:** `git log --oneline origin/main..main`. For every `#N` referenced, `gh issue view N --json state`. Close each whose work landed, commenting the sha; comment what remains on any left open. Done when no referenced issue is silently open.
2. **Side-findings:** anything this session flagged, deferred, or called out of scope gets an issue now. Done when each has a number.
3. **Tree:** `git status --short` is clean, or every dirty path is named with a reason.
4. **Questions for the maintainer:** decisions taken on their behalf they may want to reverse, and anything genuinely blocked on them. Nothing else.

Report: `main @ <sha>` (N ahead of origin), then the lists from 1–4.
