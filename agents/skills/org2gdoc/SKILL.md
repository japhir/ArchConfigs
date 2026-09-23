---
name: org2gdoc
description: Turn an .org file into a native, linkable, tidy Google Doc.
disable-model-invocation: true
---

Source: `$ARGUMENTS` (path to `.org`; optional Drive folder id).

1. Run `org2gdoc run <org> [folder-id]` (on PATH; source `~/.agents/skills/org2gdoc/org2gdoc.py`).
   It exports (pandoc + filter), imports as a native Doc, mints `headingId`s, sets heading spacing,
   table borders, the TITLE ` — ` split and verbatim-block styling, then checks. It prints `0 bad`,
   the URL and one `#heading=` link; anything else is a defect to report verbatim. Add `--emacs`
   when the org relies on Emacs-only export features (`:ignore:` tags, babel results, `#+INCLUDE`,
   macros, ID links); the filter is wired in via `org-pandoc-options-for-docx`.
2. **Report** both links. Then ask: does the `.org` stay the source, or does the Doc take over?

## If the Doc takes over

Prepend `BEVROREN <date>: werkversie is <url>; niet meer bijwerken.` to the `.org`
(rules: `~/.claude/docs/org-mode.md`).

## Reference

- The script is the single source for shapes and values. `org2gdoc check <doc-id>` lists an
  existing Doc's defects (rules: `~/.claude/docs/gws.md`).
- Kept: `#+title`/`#+author`/`#+date` (date verbatim, `<2026-09-23 Wed>`), TODO keywords,
  tables, footnotes, links. Dropped: `#+created`, `#+last_modified`, property drawers,
  `CLOSED:` lines. Org `::` lists flatten to paragraphs, so tabular data goes in org tables.
- Running the script yourself, outside an agent, keeps the file's contents out of the agent's
  context; only Google sees them.
