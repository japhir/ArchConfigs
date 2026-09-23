# Org-mode conventions

Rules for editing `.org` files (Emacs org-mode).

## Headers and timestamps

- Leave `#+last_modified:` headers untouched — Emacs manages them.
- Leave heading states as-is; let the user toggle `DONE` themselves, since
  it requires a `CLOSED:` timestamp with the real current time.

## Headings

- Put content immediately after a heading line — no blank line between
  `** Heading` and its first line of body text.
- Leave exactly one blank line after a section's last content line, before
  the next heading.

## Source blocks

- Indent everything inside `#+begin_src` / `#+end_src` blocks by 2 spaces,
  including `<<noweb-ref>>` references.

## Tags

- Build headline tags (`:tag:`) from letters, digits, `_`, and `@` only.
  Use underscores as word separators (`:CP_MAIN_FRESH_CSV:`, not
  `:CP-MAIN-FRESH-CSV:`) — hyphens, spaces, and other punctuation are not
  valid tag characters and break tag recognition.
