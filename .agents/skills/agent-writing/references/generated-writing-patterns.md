# Generated-writing revision catalogue

Read this reference when drafting or substantially revising an issue, PR body,
documentation, report, retrospective, or another durable multi-paragraph
artifact. Apply it after the artifact contains the required facts and structure.

Cut these patterns when they do not carry project meaning:

- Puffery, promotional adjectives, name-dropping without a relevant claim,
  formulaic challenge-and-triumph framing, and generic conclusions.
- Superficial `-ing` tails such as `ensuring`, `highlighting`, or `showcasing`
  when they add no mechanism, evidence, or consequence.
- Stock agent vocabulary such as `additionally`, `crucial`, `delve`, `enhance`,
  `intricate`, `landscape` used abstractly, `pivotal`, `showcase`, `tapestry`,
  `testament`, `underscore`, and `vibrant`. Prefer the plain, project-specific
  word.
- Fancy substitutes for `is`, `has`, or `use`; forced groups of three; false
  `from X to Y` ranges; and repeated restatements with new synonyms.
- Unexplained technical or business metaphors such as `substrate`, `wedge`,
  `vector`, `locus`, `vantage`, `nexus`, `primitive` used as a noun, `harness`
  used metaphorically, `surface` used vaguely, `bedrock`, `scaffolding`,
  `modality`, `paradigm`, `gold-plating`, `ratchet`, `evacuate`, `endgame`,
  `north star`, and `flywheel`. Keep a metaphor when the project gives it a
  stable, explicit meaning; otherwise name the actual component, operation,
  boundary, or tradeoff.
- Dense sentences that make the reader backtrack, weak verbs propped up by
  adverbs, passive constructions that hide a relevant actor, and clauses that
  do not change what the reader should know or do.
- Coined labels that compress a multi-step process, and generic service terms
  when the actual system or evidence is known. Name the action, result, service,
  or state the reader needs. Prefer `requirements for marking the PR ready` to
  `readiness criteria`, `time from making an edit to reliable evidence that the
  change works` to `edit-to-trustworthy-answer`, and `GitHub`, `Git hosting
  service`, `pull-request state`, or `CI results` to `forge`, as the context
  requires. Preserve quoted headings, fixture keys, and code identifiers.

After cutting a pattern, restore any mechanism, evidence, caveat, decision, or
next action the reader still needs. Shorter prose is not better when it removes
meaning.

Adapted from the [Cursor Unslop
skill](https://github.com/cursor/plugins/blob/main/pstack/skills/unslop/SKILL.md).
