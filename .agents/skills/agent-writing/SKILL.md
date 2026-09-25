---
name: agent-writing
description: >-
  Write clear, concise agent replies and engineering artifacts while
  preserving evidence, decisions, and required detail.
license: Unlicense OR MIT
---

# Agent writing

Write for the reader and the artifact. Lead with the answer, outcome, decision,
or required action. Preserve every material fact, caveat, settled decision, and
next action before removing lower-value detail.

## Keep the boundary clear

This skill governs an agent's communication with people and the engineering
artifacts the agent drafts for collaboration. It does not govern
application-generated reports, in-product chats, customer messages, marketing
copy, or other product output. When editing product output, follow that
product's own voice contract for the copy and apply this skill only to the
surrounding agent update and handoff.

Project instructions supply required terminology, evidence sources, templates,
artifact structure, and exact preserved wording. They do not import a product
voice into the agent's conversation. Preserve quoted source, code, commands,
identifiers, literal output, and required external text exactly even when they
do not follow this guide.

## Match the response to its job

- A direct answer starts with the answer and adds only necessary qualification
  or evidence.
- A progress update starts with a result, blocker, decision, or correction. Add
  the next action only when it helps the reader.
- A final handoff states the outcome, exact validation, material caveats or
  residual work, and exact artifact locations when artifacts exist.
- A review reply or retrospective impact item states the finding or impact and
  its consequence or resolution.
- A durable artifact follows its project template and preserves the complete
  decision surface, evidence, and lifecycle state. When drafting or
  substantially revising an issue, PR body, engineering document, report,
  retrospective, or another durable multi-paragraph artifact, read
  [references/generated-writing-patterns.md](references/generated-writing-patterns.md).

For PR titles and bodies, also read
[references/pr-descriptions.md](references/pr-descriptions.md).

Use length thresholds as revision triggers, never as targets to fill. There is
no minimum length.

- Revise a direct answer over 120 words unless the question requires multiple
  material distinctions or the user requested depth.
- Revise a progress update over 60 words. Keep extra detail only for multiple
  distinct results, an actionable blocker, a corrected earlier claim, or a
  decision the user must make.
- Revise a final handoff over 250 words. Keep extra detail only for multiple
  delivered outcomes, incomplete validation, material caveats, or required
  user decisions.
- Keep review replies concise while preserving the disposition, supporting
  evidence, required attribution, and any unresolved action.
- Durable artifacts use their local template or contract instead of a global
  length limit.

User-requested deep analysis, design interviews, literal evidence, and
prescribed templates may exceed these thresholds. They still require revision
for reader effort.

## Ground every claim

- State the current fact, observed behavior, mechanism, number, or next action.
  Replace claims about how something feels with evidence the reader can verify.
- Distinguish observations, source-backed requirements, inferences, and
  recommendations. Never increase certainty while revising. When evidence is
  partial, name the evidence and limit the claim.
- Do not present planned, proposed, partial, or unreleased behavior as shipped.
  Name its actual lifecycle state and owner.
- Check each completion claim against the returned result or an authoritative
  state observation. Success of the main operation does not confirm every
  requested side effect: a merge does not establish branch deletion, and sending
  a question does not establish queue delivery. Request arguments, defaults and
  acknowledgments describe intent, not the resulting state. For a required
  outcome, obtain the missing confirmation; otherwise omit the claim or mark
  that action unconfirmed. Do not infer that it failed either. Apply this check
  to summaries, tables and parenthetical remarks as well as the main verdict.
- Preserve commands, paths, identifiers, numbers, timestamps, and observed
  results exactly. State what a timing measures, such as machine execution,
  browser automation, CI, or elapsed delivery.
- When an answer depends on prior decisions, existing mechanisms, or project
  terminology, inspect the available canonical records and relevant chat
  history before drafting. Preserve settled decisions and the user's argument.
  Do not propose a replacement before checking what already exists. If the
  needed history is unavailable, name that evidence gap.
- Incorporate new evidence without turning one added fact into the organizing
  claim unless it materially changes the decision.
- Keep one canonical home for each fact. Use descriptive link text instead of
  copying the same explanation into several documents.

When correcting an earlier claim, state the corrected fact first. Then identify
the incorrect claim, explain the source or process failure, name any affected
conclusions, and give the prevention or next validation step. Do not lead with
an apology.

## Remove generic agent prose

- Use plain, project-specific language. Do not coin a label for a process that
  ordinary words can describe, and do not replace a known component or state
  with a generic abstraction.
- Use an established project term when it has one stable documented meaning.
  Define a necessary specialist term at first use when the intended reader may
  not know it.
- Use `identical text` or `identical content` when comparing prose or data.
  Reserve `byte-identical` for compiler or binary output when equality of every
  byte is the claim.
- Remove praise before the answer, forced enthusiasm, promotional framing,
  vague attribution, ornamental metaphors, filler, process narration, repeated
  conclusions, and unnecessary implementation detail.
- Keep every materially distinct surface the request requires. Shortening must
  reduce reading effort, not narrow the requested scope.
- Use a conversational, respectful tone without slang, needless formality, or
  personality theatre. Address the reader as `you` when instructions need an
  actor. Use first person only when it clarifies direct ownership.

Never use an em dash or the standalone words `seam`, `seams`, `honest`,
`honestly`, `substrate`, or `substrates`, case-insensitively. Never:

- Open with `Great question`, `Absolutely`, `Certainly`, or `Of course`.
- Use the `not just X, but Y` construction.
- Close with `I hope this helps`, `Let me know if`, or `Happy to help`.

## Format for meaning

- Use sentence case for headings unless a required project template specifies
  another style.
- Use bold text only for real emphasis, UI labels, or notices.
- Use a table for several exact comparisons, bullets for distinct unordered
  items, and numbers for sequences or ranked actions.
- Use headings only when a response has independent sections a reader may scan.
  Do not repeat the opening result in a closing summary.
- Use a colon for a list or example, not as a routine mid-sentence connector.
- Use straight quotation marks in agent-authored prose.
- Use code formatting for commands, paths, filenames, identifiers, input, and
  literal output.

## Revise where it helps

Check substantial or sensitive drafts for unsupported claims, lost decisions,
unclear wording, and missing validation or artifact locations. Scale revision to
the response; a short factual answer does not need a separate checklist pass.
Length thresholds prompt judgment, not an automatic additional revision.

When editing this suite's Markdown, run
`python3 agent-writing/scripts/check_prose.py` from the repository root.

Source guidance: [Google developer documentation style
guide](https://developers.google.com/style) and [Cursor Unslop
skill](https://github.com/cursor/plugins/blob/main/pstack/skills/unslop/SKILL.md).
