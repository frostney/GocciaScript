# PR titles and descriptions

Describe the final aggregate change against the PR base and its effect on the
reader. Omit intermediate commits, abandoned approaches and line-count
reduction during development unless they explain a material decision in the
final change.

## Body structure

Unless an explicit repository template requires other headings, write the body
in this order:

```markdown
## What changed

Problem and resulting behavior, at most 500 characters.

Closes #N

<details>
<summary>Details</summary>

Rationale, decisions, risks and limits, at most 1200 characters.

</details>

## Visual walkthrough

Before/after media, walkthrough video, benchmark table, diagram or example.

## How to test

- Exact step a reviewer performs. Expected: observable result.
```

The 500 and 1200 character limits are hard maximums on the Markdown source of
each section's content. They exclude the heading, the `<details>` and
`<summary>` tags and `Closes` lines. Count both before every PR body write.
When content does not fit, keep what a reviewer needs to judge the change and
leave longer rationale to commit messages, code comments or the linked issue.
Do not move overflow into another section.

- **What changed** starts with the concrete problem and resulting behavior. Use
  bullets when they make distinct changes easier to scan. Put each closing
  keyword on its own line after the summary.
- **Details** holds why this approach, material decisions and tradeoffs, risks,
  compatibility and limits. Keep it short when the change is simple.
- **Visual walkthrough** is always present. For directly or indirectly visible
  changes, show comparable before/after images or videos in a table with
  uploaded, reviewer-accessible assets, and label the states. A narrated
  walkthrough can supplement that comparison. For benchmark claims, show the
  before/after table here: target-branch baseline, PR candidate, units, the
  comparison statistic, and the revisions and measurement conditions that make
  the comparison meaningful. For a CLI, backend or structural change, or
  alongside media, show a Mermaid diagram, CLI session, usage example or short
  code sample that makes the changed behavior or structure clear. If a
  baseline capture or attachment is unavailable, state the gap here. Do not
  invent a comparison or link a local file as if reviewers can access it.
- **How to test** lists bullets that a human or an agent reviewer can follow
  from the PR branch without the author's environment. Give exact commands,
  URLs, inputs and setup, and the expected observable result for each. Name
  any prerequisite, such as credentials or a preview deployment, that a
  reviewer may lack. Repository-relative paths are fine; do not use the
  author's absolute paths, private fixtures or results from the author's runs.

Leave the author's verification out of these sections. Do not include
sections, collapsed blocks or log lines reporting the author's checks, review
results, project gates, CI status, revert probes, replays or command output.
Expected behavior, such as a command's exit status, still belongs in the
walkthrough and test steps. Run every required check anyway; the final user
handoff reports observed validation. Review and status results go only in the
review note at the end of the body, as
[Review and status results](#review-and-status-results) describes.

## Templates

Preserve explicit repository-template requirements. Map this structure into
the template's required headings where they match, and keep any content the
template demands, including validation results it explicitly requires.

## Linked issues

Keep linked issues accurate for the whole change; a closing keyword belongs
only on the PR or stack layer that completes the issue.

## Review and status results

Record review and status results where the PR's author reads them. Compare the
PR's author with the authenticated GitHub user first.

- On a pull request the authenticated user opened, keep them in one review
  note at the very end of the body, after every other section and a `---`
  rule with a blank line before and after it. Open it with a plain line, not a heading, naming the head it covers,
  such as ``Edit: `1a2b3c4` reviewed``, and follow it with the review
  details. After a later review or status change, update that note in place
  for the new head.
  Do not post review rounds, checkpoints or status updates as comments.
- On a pull request someone else opened, post them as a comment from the
  authenticated user. Post a new comment when a head brings findings that
  change what the author must do, because edits do not notify. When a re-check
  only confirms fixes, edit the latest such comment instead.

Record what the author acts on: each review's open findings or that none are
open, and the checks' result on that head. Routine test narration and command
logs stay out. Replies to a review thread stay in that thread.

```markdown
---

Edit: `1a2b3c4` reviewed

- Code review: two findings fixed, none open.
- CI: every workflow run passed on `1a2b3c4`.
```
