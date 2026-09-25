# PR titles and descriptions

Describe the final aggregate change against the PR base and its effect on the
reader. Start with the concrete problem and resulting behavior. Keep ordinary
PR bodies concise; use bullets when they make distinct changes easier to scan.
Omit intermediate commits, abandoned approaches and line-count reduction during
development unless they explain a material decision in the final change.

Preserve explicit repository-template requirements. Otherwise omit routine
"ran tests" narration, testing sections and command logs. Keep material risks,
unverified behavior and accepted limitations visible. Detailed supporting logs
can go in a collapsed `<details>` block when useful; the final user handoff
still reports observed validation.

Use a Mermaid diagram, short code sample, usage example or precise code
reference when it explains the change more clearly than prose. Do not add one
just to fill a section. Difficult or unusually broad or risky changes can need
more context, rationale and examples; length should follow reviewer needs.

For directly or indirectly visible changes, show comparable before/after
images or videos in a table with uploaded, reviewer-accessible assets. Label
the states and reuse relevant implementation evidence. A narrated walkthrough
can supplement that comparison. If a baseline capture or attachment is
unavailable, identify the gap; do not invent a comparison or link a local file
as if reviewers can access it.

For benchmark claims, show a visible before/after table: target-branch baseline,
PR candidate, units and the comparison statistic. Identify the revisions and
measurement conditions that make the comparison meaningful. Do not hide this
table inside collapsed logs or claim an improvement from incomparable runs.

Keep linked issues accurate for the whole change; a closing keyword belongs
only on the PR or stack layer that completes the issue.
