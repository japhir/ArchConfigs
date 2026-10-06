<!-- synced from ~/ArchConfigs/agents/docs/triage-labels.md — edit there, then run agents-docs-sync -->
# Triage Labels

The skills speak in terms of five canonical triage roles. This file maps those roles to the actual label strings used in this repo's issue tracker. Same vocabulary in every repo.

| Label in mattpocock/skills | Label in our tracker | Meaning                                  |
| -------------------------- | -------------------- | ---------------------------------------- |
| `needs-triage`             | `needs-triage`       | Maintainer needs to evaluate this issue  |
| `needs-info`               | `needs-info`         | Waiting on reporter for more information |
| `ready-for-agent`          | `ready-for-agent`    | Fully specified, ready for an AFK agent  |
| `ready-for-human`          | `ready-for-human`    | Requires human implementation            |
| `wontfix`                  | `wontfix`            | Will not be actioned                     |

When a skill mentions a role (e.g. "apply the AFK-ready triage label"), use the corresponding label string from this table.

## Additional labels

| Label                 | Meaning                                                                                  |
| --------------------- | ---------------------------------------------------------------------------------------- |
| `parked`              | Trigger-gated idea: not wontfix, not ready. The issue body names the trigger that revives it. |
| `wayfinder:map`       | A wayfinder map issue; its tickets are native sub-issues.                                |
| `wayfinder:research`  | Wayfinder ticket, AFK: surface a fact a decision waits on.                               |
| `wayfinder:prototype` | Wayfinder ticket, HITL: build a rough artifact to react to.                              |
| `wayfinder:grilling`  | Wayfinder ticket, HITL: a decision made in conversation.                                 |
| `wayfinder:task`      | Wayfinder ticket: manual work that unblocks a decision.                                  |

A `parked` issue keeps `needs-triage` off; re-triage when its trigger fires.
