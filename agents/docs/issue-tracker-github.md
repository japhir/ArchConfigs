<!-- synced from ~/ArchConfigs/agents/docs/issue-tracker-github.md — edit there, then run agents-docs-sync -->
# Issue tracker: GitHub

Issues and PRDs for this repo live as GitHub issues. Use the `gh` CLI for all operations.

## Conventions

- **Create an issue**: `gh issue create --title "..." --body "..."`. Use a heredoc for multi-line bodies.
- **Read an issue**: `gh issue view <number> --comments`, filtering comments by `jq` and also fetching labels.
- **List issues**: `gh issue list --state open --json number,title,body,labels,comments --jq '[.[] | {number, title, body, labels: [.labels[].name], comments: [.comments[].body]}]'` with appropriate `--label` and `--state` filters.
- **Comment on an issue**: `gh issue comment <number> --body "..."`
- **Apply / remove labels**: `gh issue edit <number> --add-label "..."` / `--remove-label "..."`
- **Close**: `gh issue close <number> --comment "..."`

Infer the repo from `git remote -v` — `gh` does this automatically when run inside a clone.

## Cross-references are invisible to `gh`

`gh issue view --comments` and the `comments` JSON field return comments only, never
timeline events. So a "Refs #26" in another issue's comment or a commit message shows up
in the web UI as a backlink but is **absent** from what an agent reads. When a change
resolves or reshapes an item tracked on another issue, post an explicit comment on *that*
issue saying what landed and which items it retires. Don't rely on the mention.

The backlinks do exist server-side; when you need them, read the timeline directly:
`gh api repos/{owner}/{repo}/issues/<number>/timeline --paginate --jq '.[] | select(.event=="cross-referenced") | .source.issue.number'`.

## When a skill says "publish to the issue tracker"

Create a GitHub issue.

## When a skill says "fetch the relevant ticket"

Run `gh issue view <number> --comments`.
