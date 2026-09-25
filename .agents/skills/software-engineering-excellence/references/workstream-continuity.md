# Workstream continuity contracts

Use these contracts only when work spans turns, compaction, external waits, or
multiple workers. Keep one canonical record instead of copying the same context
into several files.

## Workstream record

```markdown
## Active workstream

- Objective: <original owned outcome>
- State: active | blocked | awaiting-authority | complete
- Settled decisions:
  - <stable ID>: <decision, rationale, provenance, active or superseded>
- Open obligations:
  - <required result still missing>
- Authority:
  - Allowed: <authorized actions>
  - Gates: <transitions requiring the user>
  - Excluded: <out-of-scope actions>
- Completion evidence:
  - <proof required before complete>
- Active lanes:
  - <lane, owner, exact state or revision, dependency>
- Next transition: <next safe executable action>
```

Record an intentional update to the objective or authority with its user turn
or durable source. Do not silently promote an agent inference into a settled
decision.

## Worker packet

Give a worker only what its lane needs:

```markdown
- Parent objective: <why this result is needed>
- Lane outcome: <one bounded result>
- Fresh state: <repository, branch, exact head, external identities>
- Applicable decisions: <stable IDs and selected contracts>
- In scope: <owned files, questions, or mutations>
- Out of scope: <explicit exclusions>
- Dependencies: <inputs and blocked successors>
- Authority and gates: <what the worker may do and where it must stop>
- Required evidence: <tests, source, rendered result, or external proof>
- Return contract: <result envelope fields below>
```

Default to isolated context. Include a small recent-turn slice only when it is
directly relevant. Full conversation inheritance requires a recorded reason.

## Result envelope

Every worker returns:

```markdown
- Lane status: complete | blocked | checkpoint
- Delivered: <bounded result>
- Evidence: <exact observations and validation>
- Mutations: <files, commits, issues, PRs, or external state changed>
- Remaining obligations: <new or unresolved work>
- Blocker or gate: <exact dependency, or none>
- Successor transition: <what the parent can safely do next>
```

`complete` applies only to the lane. The parent reconciles the envelope against
the parent objective, integrates it, and continues until the parent terminal
check passes.

Before starting a worker, identify its actual model from host metadata. Deliver
the applicable role, scope, authority, completion condition, and required skill
contents or reachable reference paths; naming a skill is not proof of delivery.
Record the resources the worker actually loaded and any missing capability in
its result. An isolated worker must not assume parent conversation or loaded
skills are inherited. Keep model-specific settings at the host boundary and
apply them only to the actual worker when supported by evidence.
