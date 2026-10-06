<!-- synced from ~/ArchConfigs/agents/docs/domain.md; edit there, then run agents-docs-sync. Repo specifics go in docs/agents/domain.local.md -->
# Domain Docs

How the engineering skills consume this repo's domain documentation when exploring the codebase. Single-context: one `GLOSSARY.md` plus `docs/adr/` at the repo root cover the whole domain.

## Before exploring, read these

- **`GLOSSARY.md`** at the repo root: the glossary, the project's domain language. Read first.
- **`docs/adr/`**: read the ADRs that touch the area you're about to work in. The status line names what amends or supersedes each one; the newest ADR wins.

If either doesn't exist, proceed silently. Don't flag the absence or suggest creating them upfront; `/domain-modeling` (reached via `/grill-with-docs` and `/improve-codebase-architecture`) creates them lazily when terms or decisions actually get resolved.

## Use the glossary's vocabulary

When your output names a domain concept (issue title, refactor proposal, hypothesis, test name), use the term as defined in `GLOSSARY.md`. Don't drift to synonyms the glossary explicitly avoids.

If the concept you need isn't in the glossary yet, that's a signal: either you're inventing language the project doesn't use (reconsider) or there's a real gap (note it for `/domain-modeling`).

## Flag ADR conflicts

If your output contradicts an existing ADR, surface it explicitly rather than silently overriding:

> _Contradicts ADR-0007 (its title), but worth reopening because…_
