# Attribution — multi-model commits

One `Co-Authored-By:` trailer per model that materially shaped the commit,
ordered by contribution, largest first.

Display names come from the session, not from a list: your own name is in the
system prompt's trailer instruction; a subagent's comes from what the harness
reports about it. When only an alias is known (`model: "sonnet"`), write the
family name without a version (`Claude Sonnet`) rather than guessing one.
Include the variant qualifier when the session runs one — `Claude Opus 5 (1M
context)`.

## Which models earn a line

The counterfactual test: **would this commit be different if that model had not
run?**

- Wrote content that landed in the diff — yes.
- Findings or a design that changed the result, with no bytes of its own in the
  diff — yes. A research subagent whose probe picked the schema co-authored the
  decision.
- Located a file, answered a lookup, confirmed something already known — no.

## Naming the model

- A subagent spawned **without** an explicit `model` override runs on the
  parent's model — one trailer, not two.
- A subagent spawned **with** an override (`model: "sonnet"`) earns its own
  trailer alongside the parent's.
- A subagent committing its own work names itself, not its parent.
