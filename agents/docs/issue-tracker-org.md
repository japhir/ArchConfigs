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

- `STATUS` is the triage role: `ready-for-agent` (AFK), `ready-for-human` (HITL), `claimed`, `resolved`, `wontfix`. It is a property, not a tag, because tags such as `research` already mean something in the user's GTD setup.
- `TYPE` appears only on HITL decision tickets: `grilling`, `prototype`, `task`, `research`.
- `BLOCKER` uses org-edna syntax. Leave it out when nothing blocks the ticket.
- Acceptance criteria are plain checkboxes, never nested TODO headings.
- `CREATED_BY` records who captured the ticket (ADR 0006). An assignee tag, `:ilja:` or `:line:`, goes on the heading only when a human claims it.

## Operations

- **Frontier**: a ticket is on the frontier when its state is `TODO`, every ID in its `BLOCKER` points to a ticket in a done state (`DONE-*`, `DONE`), and `STATUS` is not `claimed`. The first frontier ticket in file order goes first.
- **Claim**: before any work, set `STATUS: claimed`. A human claiming it also adds their assignee tag and changes the state to `NEXT`.
- **Resolve**: append the outcome under `*** Answer` and set `STATUS: resolved`. **Never change the TODO state to done yourself.** The user closes the ticket, so its `CLOSED:` timestamp is the real time.
- **Out of scope**: the user sets the ticket to `CANC`. Add one line under `* Out of scope`.
- **Publish** (for `/to-tickets` and `/to-spec`): append headings to the file. To create an ID, generate a uuid (`uuidgen`), then create every ticket before wiring any `BLOCKER`s.

## Targeted reads (don't load the whole file)

1. Outline: `grep -n '^\*\{1,2\} ' <file>`
2. Spec or a single section: `sed -n '<start>,<end>p'` using the line numbers from the outline.
3. One ticket: `grep -n ':ID: *<id>' <file>`, then read from its heading to the next `^\*\* `.
4. Frontier: `grep -n -e '^\*\* TODO' -e ':STATUS:' -e ':BLOCKER:' <file>`, then resolve the blocker IDs.

## Wayfinding operations

Used only when an effort genuinely needs `/wayfinder`. The map is a file with the same layout, plus `* Destination`, `* Notes`, `* Decisions so far` and `* Not yet specified` before `* Tickets`. Every ticket carries a `TYPE`. On resolution, add one line to `* Decisions so far`: `- [[id:<id>][<title>]]: <gist>`. Research output goes into its own org file, linked from the ticket through a `FILE:` property, and not onto a branch.
