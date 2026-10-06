<!-- synced from ~/ArchConfigs/agents/docs/issue-tracker-github.md (= mattpocock setup template + our additions); edit there, then run agents-docs-sync -->
# Issue tracker: GitHub

Issues and specs for this repo live as GitHub issues. Use the `gh` CLI for all operations.

## Conventions

- **Create an issue**: `gh issue create --title "..." --body "..."`. Use a heredoc for multi-line bodies.
- **Read an issue**: `gh issue view <n> && gh issue view <n> --comments`, in one call, plain view first. Off a TTY `--comments` prints the comments *only*, never the body; a hook blocks it on its own. Drop the second half only when the plain view shows `comments: 0`.
- **`gh` latency**: `gh issue/pr view` can take ~1s before printing, longer piped through `| grep`/`| head`. Wait for it to return before judging the command.
- **List issues**: `gh issue list --state open --json number,title,body,labels,comments --jq '[.[] | {number, title, body, labels: [.labels[].name], comments: [.comments[].body]}]'` with appropriate `--label` and `--state` filters.
- **Comment on an issue**: `gh issue comment <number> --body "..."`
- **Apply / remove labels**: `gh issue edit <number> --add-label "..."` / `--remove-label "..."`
- **Close**: `gh issue close <number> --comment "..."`
- **Deferred refactors**: refactor/simplification findings not fixed in-session (e.g. from code review) become a `needs-triage` issue, so they aren't lost to session output alone.

Infer the repo from `git remote -v`; `gh` does this automatically when run inside a clone.

## Cross-references are invisible to `gh`

`gh issue view --comments` and the `comments` JSON field return comments only, never
timeline events. So a "Refs #26" in another issue's comment or a commit message shows up
in the web UI as a backlink but is **absent** from what an agent reads. When a change
resolves or reshapes an item tracked on another issue, post an explicit comment on *that*
issue saying what landed and which items it retires. Don't rely on the mention.

The backlinks do exist server-side; when you need them, read the timeline directly:
`gh api repos/{owner}/{repo}/issues/<number>/timeline --paginate --jq '.[] | select(.event=="cross-referenced") | .source.issue.number'`.

## Pull requests as a triage surface

**PRs as a request surface: no.** _(Set to `yes` if this repo treats external PRs as feature requests; `/triage` reads this flag.)_

When set to `yes`, PRs run through the same labels and states as issues, using the `gh pr` equivalents:

- **Read a PR**: `gh pr view <n> && gh pr view <n> --comments` (PR view shows no comment count, so always both), and `gh pr diff <n>` for the diff. `--comments` omits review comments; fetch those with `gh api repos/{owner}/{repo}/pulls/<number>/comments`.
- **List external PRs for triage**: `gh pr list --state open --json number,title,body,labels,author,authorAssociation,comments` then keep only `authorAssociation` of `CONTRIBUTOR`, `FIRST_TIME_CONTRIBUTOR`, or `NONE` (drop `OWNER`/`MEMBER`/`COLLABORATOR`).
- **Comment / label / close**: `gh pr comment`, `gh pr edit --add-label`/`--remove-label`, `gh pr close`.

GitHub shares one number space across issues and PRs, so a bare `#42` may be either: resolve with `gh pr view 42` and fall back to `gh issue view 42`.

## When a skill says "publish to the issue tracker"

Create a GitHub issue.

## When a skill says "fetch the relevant ticket"

Read it as in **Read an issue** above: body first, then comments.

## Wayfinding operations

Used by `/wayfinder`. The **map** is a single issue with **child** issues as tickets.

- **Map**: a single issue labelled `wayfinder:map`, holding the Notes / Decisions-so-far / Fog body. `gh issue create --label wayfinder:map`.
- **Child ticket**: a GitHub sub-issue of the map: `gh issue create --parent <map> ...`, or `gh issue edit <n> --parent <map>` for an existing issue (`gh issue edit <map> --add-sub-issue <n>,<n>` from the other side). Labels: `wayfinder:<type>` (`research`/`prototype`/`grilling`/`task`). Once claimed, the ticket is assigned to the driving dev.
- **Blocking**: GitHub's **native issue dependencies**, the canonical, UI-visible representation. `gh issue create --blocked-by <n>,<n>` on creation, or `gh issue edit <child> --add-blocked-by <n>` / `gh issue edit <blocker> --add-blocking <n>` later. Plain issue numbers or URLs; no database ids. A ticket is unblocked when every blocker is closed.
- **Reading structure**: the plain `gh issue view <n>` header already prints `parent:`, `sub-issues:`, `blocked-by:` and `blocking:`. Via `--json` (both `view` and `list`): `parent`, `subIssues`, `subIssuesSummary`, `blockedBy`, `blocking`. **Blockers are listed regardless of state**, in the header and in `blockedBy.totalCount` alike, so a ticket whose blockers are all closed still looks blocked. Check `blockedBy.nodes[].state` before treating it as blocked.
- **Frontier query**, one call: `gh issue list --state open --limit 200 --json number,parent,assignees,blockedBy --jq '[.[] | select(.parent.number==<map>) | select(.assignees|length==0) | select([.blockedBy.nodes[]|select(.state=="OPEN")]|length==0) | .number] | sort | first'`. Open children of the map, no assignee, no *open* blocker; lowest number wins as the proxy for map order.
- **Claim**: `gh issue edit <n> --add-assignee @me`, the session's first write.
- **Resolve**: `gh issue comment <n> --body "<answer>"`, then `gh issue close <n>`, then append a context pointer (gist + link) to the map's Decisions-so-far.
