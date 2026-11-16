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
- Lane outcome: <one scoped result>
- Fresh state: <repository, branch, exact head, external identities>
- Applicable decisions: <stable IDs and selected contracts>
- In scope: <owned files, questions, or mutations>
- Out of scope: <explicit exclusions>
- Fallout: <what this lane's change breaks outside its scope is the lane's to fix with the smallest edit, called out in its result; when that edit would collide with another lane's open work, the lane returns the exact edit instead>
- Dependencies: <inputs and blocked successors>
- Authority and gates: <what the worker may do and where it must stop>
- Capability tier: <mechanical | bounded | open>
- Required evidence: <tests, source, rendered result, or external proof>
- Return contract: <result envelope fields below>
```

The [capability tiers](../../milestone-rush/references/orchestration.md#capability-tiers)
define what each tier covers and how a lane escalates. Full conversation
inheritance, instead of an isolated context with only the directly relevant
recent turns, requires a recorded reason.

## Result envelope

Every worker returns:

```markdown
- Lane status: complete | blocked | checkpoint
- Delivered: <scoped result>
- Evidence: <exact observations and validation>
- Mutations: <files, commits, issues, PRs, or external state changed>
- Remaining obligations: <each item left undone or found outside scope: what it is, whether this lane's change caused it, and the proposed disposition or exact edit>
- Blocker or gate: <exact dependency, or none>
- Successor transition: <what the parent can safely do next>
```

`complete` applies only to the lane. When the envelope arrives, the parent reads
the lane's change or evidence, settles each remaining obligation as
[Coordinating deliverables](../SKILL.md#coordinating-deliverables) describes,
integrates the result, and continues until the parent terminal check passes.

Before starting a worker, identify its actual model from host metadata. Deliver
the applicable role, scope, authority, completion condition, and required skill
contents or reachable reference paths; naming a skill is not proof of delivery.
Record the resources the worker actually loaded and any missing capability in
its result. An isolated worker must not assume parent conversation or loaded
skills are inherited. Keep model-specific settings at the host boundary and
apply them only to the actual worker when supported by evidence.
