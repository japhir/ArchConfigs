# Issue tracker: org file

For efforts whose tickets are private-ish, the tracker is one `.org` file instead of GitHub issues. The project's CLAUDE.md names the file. Also read `org-mode.md` before editing it.

## File layout

Level-1 sections, in this order:

- `* Spec`: what `/to-spec` produces (Problem Statement, Solution, User Stories, Implementation Decisions, Testing Decisions, Further Notes as level-2 headings).
- `* Tickets`: one level-2 heading per ticket, in dependency order (blockers first).
- `* Out of scope`: one line per ruled-out item, with a link to the ticket that was closed as `CANC`.

## A ticket

```org
** TODO <Ticket title>
:PROPERTIES:
:ID:         <org-id uuid>
:CREATED:    [YYYY-MM-DD Day]
:CREATED_BY: Claude
:STATUS:     ready-for-agent
:TYPE:       grilling
:BLOCKER:    ids(<id> <id>)
:END:
What to build: <end-to-end behaviour, user's view>
- [ ] acceptance criterion
*** Answer
*** Comments
```

- `STATUS` is the triage role, set once and never rewritten, so it keeps saying who the work was meant for: `ready-for-agent` (AFK), `ready-for-human` (HITL), `wontfix`. Progress lives elsewhere: the claim property, LOGBOOK notes, the TODO state. It is a property, not a tag, because tags such as `research` already mean something in the user's GTD setup.
- `TYPE` appears only on HITL decision tickets: `grilling`, `prototype`, `task`, `research`.
- `BLOCKER` uses org-edna syntax. Leave it out when nothing blocks the ticket.
- Acceptance criteria are plain checkboxes, never nested TODO headings.
- `CREATED_BY` records who captured the ticket (ADR 0006). An assignee tag, `:ilja:` or `:line:`, goes on the heading when a human claims it or when agent work waits for their check.
- `CLAIMED_BY` appears only while an agent works on the ticket.
- Anything the user must notice goes in the keyword or a tag: those show on the heading line, while properties and drawers stay folded.

## Operations

- **Frontier**: a ticket is on the frontier when its state is `TODO`, it has no `CLAIMED_BY` and no assignee tag, and every ID in its `BLOCKER` points to a ticket in a done state (`DONE-*`, `DONE`). The first frontier ticket in file order goes first.
- **Claim**: before any work, set `CLAIMED_BY: Claude`. A human claiming it adds their assignee tag and changes the state to `NEXT`.
- **Comment**: a LOGBOOK note, the equivalent of a GitHub comment (the user writes them with `C-c C-z`). Agents start theirs with `Claude: `. Long outcomes (research summaries, decisions) go under `*** Answer`, and the note points there.
- **Finish, proven**: when tests cover every acceptance criterion, tick the criteria, add a note `Claude: <what>, <commit>`, remove `CLAIMED_BY`, and close the ticket with the user's done keyword (`DONE-ILJA`, ADR 0005) **via emacsclient**, so `CLOSED:` is the real time.
- **Finish, needs a human check**: add a note `Claude: te controleren: <what to check>`, remove `CLAIMED_BY`, add the user's assignee tag, and leave the state at `TODO`. The user closes it after checking.
- **Human tickets**: never close one yourself unless the user asks, and then via emacsclient.
- **Out of scope**: the user sets the ticket to `CANC`. Add one line under `* Out of scope`.
- **Publish** (for `/to-tickets` and `/to-spec`): append headings to the file. To create an ID, generate a uuid (`uuidgen`), then create every ticket before wiring any `BLOCKER`s.

## Editing through emacsclient

The tracker is usually open in the user's Emacs, and phones sync it, so edit the live buffer, not the file on disk. Check `buffer-modified-p` **once, before your first change**; checking before every change makes your own edits trip it. Then `save-buffer`. Write the script to the scratchpad with the Write tool and run `emacsclient --eval '(load "<script>" nil t)'`, using full file paths: shell text naming an org directory trips the privacy hook. Never call `org-id-update-id-locations` or another global rescan: it walks every agenda file, private ones included. A new ID is found anyway the first time a link to it is followed.

A note, with point on the ticket heading:

```elisp
(save-excursion
  (goto-char (org-log-beginning t))
  (insert (format "- Note taken on %s \\\\\n  Claude: %s\n"
                  (format-time-string (org-time-stamp-format t t)) text)))
```

Close: `(let ((org-enforce-todo-dependencies nil) (org-enforce-todo-checkbox-dependencies nil)) (org-todo "DONE-ILJA"))`.

## Targeted reads (don't load the whole file)

1. Outline: `grep -n '^\*\{1,2\} ' <file>`
2. Spec or a single section: `sed -n '<start>,<end>p'` using the line numbers from the outline.
3. One ticket: `grep -n ':ID: *<id>' <file>`, then read from its heading to the next `^\*\* `.
4. Frontier: `grep -n -e '^\*\* TODO' -e ':STATUS:' -e ':CLAIMED_BY:' -e ':BLOCKER:' <file>`, then resolve the blocker IDs.

## Wayfinding operations

Used only when an effort genuinely needs `/wayfinder`. The map is a file with the same layout, plus `* Destination`, `* Notes`, `* Decisions so far` and `* Not yet specified` before `* Tickets`. Every ticket carries a `TYPE`. On resolution, add one line to `* Decisions so far`: `- [[id:<id>][<title>]]: <gist>`. Research output goes into its own org file, linked from the ticket through a `FILE:` property, and not onto a branch.
