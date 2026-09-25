---
name: commit
description: >
  Caveman commit messages. Use when writing a commit, staging changes, or when
  another skill needs a commit message.
---

Write commit messages like a caveman. Conventional Commits. Why over what.

Derived from `caveman-commit` by Julius Brussee (github.com/JuliusBrussee/caveman).

## Subject

`<type>(<scope>): <imperative summary>` — scope optional.

Types: `feat`, `fix`, `refactor`, `perf`, `docs`, `test`, `chore`, `build`, `ci`,
`style`, `revert`.

- Imperative mood: "add", "fix", "remove".
- ≤50 characters where possible, hard cap 72.
- Match the project's capitalisation after the colon.
- No trailing period.
- Scope carries the area; the summary carries the change.

The diff already says what the code does. The subject says which change this is.

## Body

Include one when the *why* is non-obvious — and always for a breaking change,
migration, security fix, or revert, where a future debugger needs the reason the
change existed at all.

- Wrap at 72. Bullets `-`.
- State the constraint that forced this shape, or the alternative it rejected.
- Reference issues at the end: `Closes #42`, `Refs #17`.
- Findings, measurements and investigation notes belong in the tracker, linked
  from here.

When the subject already carries the reason, ship it alone.

## Committing

Stage what the message describes, then commit. Amend freely while the commit is
unpushed.

## Attribution

Every commit ends with `Co-Authored-By: Claude <Model> <noreply@anthropic.com>`
naming the model that did the work — write it from what this session ran, never
copied from an earlier commit. When a subagent ran with a `model` override, or
more than one model shaped the commit, read `ATTRIBUTION.md` for who earns a
line and in what order.

## Guardrails

Two things never appear: emoji, and a "Generated with Claude Code" footer.
Credit belongs in the trailer instead.

## Examples

```
feat(api): add GET /users/:id/profile

Mobile client needs profile data without the full user payload to
reduce LTE bandwidth on cold-launch screens.

Closes #128

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
```

```
feat(api)!: rename /v1/orders to /v1/checkout

BREAKING CHANGE: clients on /v1/orders must migrate to /v1/checkout
before 2026-06-01. Old route returns 410 after that date.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
```
