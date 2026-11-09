---
name: cold-read
description: Cold read a page (artifact link or file) by a fresh no-tools subagent playing a named reader; report what that reader can't follow.
disable-model-invocation: true
---

Arguments: `$ARGUMENTS` = `<artifact URL or file path> [reader]`. Default reader: a capable newcomer with none of the author's context (e.g. a project partner or new colleague).

A **cold read** sees only the page. A reviewer with repo or web access silently fills the gaps a real reader hits, so the subagent gets the page text in its prompt and works from that alone.

1. **Page text.** claude.ai artifact link → Artifact `action: read` (read-only; leave the artifact as is). File → read it. Reduce HTML to readable text in reading order, keeping headings, list structure, table cells, link texts, diagram source (mermaid, graphviz, etc.), and text the page's script renders (nav labels, checklists); drop CSS, code, and markup. Write it to a scratchpad file. Done when every visible sentence and diagram on the page is in that file.
2. **Dispatch.** Spawn one fresh subagent (Agent tool, `general-purpose`, model `opus`: judging comprehension is judgement; not a fork). Its prompt holds only the reader persona, the instructions below, and the full page text pasted inline. Add nothing about the author, project, or why the page exists.

   > You are {reader}. Below is a page someone wrote for you. Read it cold: answer from the page text alone, using no tools (no file reads, search, or web). Your reading is the test, so a gap you can't fill from the page is a finding.
   > Report, each item with a short quote or section location:
   > 1. Terms and acronyms used before or without definition.
   > 2. References to things the page doesn't contain: files, repos, tickets, people, "the note", internal tools.
   > 3. Claims with no stated basis.
   > 4. Jumps you can't follow without outside context.
   > 5. One paragraph: what you take the page's purpose to be and what it asks of you.
   > List every instance, not samples.

   Done when the subagent's report covers all five parts.
3. **Relay.** Regroup the findings by severity: **blocks understanding** / **slows** / **nit**, keeping each quote. Put the reader's purpose paragraph first, so the author can check intent landed, and note where it diverges from the page's evident aim. Offer concrete fixes per item; edit the page only when asked.
