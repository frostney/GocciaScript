# Repository-owned orchestration

Inspect repository-root `ORCHESTRATION.md` before building the execution plan or
spawning any worker.

## Policy gate

A valid file is authoritative for repository-owned capability classes, routing
rules, context envelopes, token checkpoints, monitoring requirements, and
escalation policy. It must use host-neutral capabilities rather than requiring a
particular provider or model. Higher-authority repository instructions still
win.

- If absent, use a conservative provider-neutral fallback: bounded capable
  workers, compact isolated context, host-default usage limits, explicit
  escalation for material decisions, and only supported non-LLM waits. Report
  the fallback.
- If malformed, contradictory, incompatible with higher-authority instructions,
  or dependent on unsupported host capabilities, stop before spawning and
  request resolution. Do not silently weaken it.
- Repository-owned soft usage limits trigger the declared `continue`, `split`,
  `replace`, or `escalate` intervention after a durable checkpoint. Never
  silently downgrade capability or stop solely because a threshold was crossed.

## Lane-admission preflights

Before a worktree is adopted or created, execute any preflight the repository
declares for path budgets, filesystem placement, toolchain availability,
resource ceilings, credentials, or other environmental constraints. A generic
skill consumes named constraints or commands from repository policy; it never
hardcodes one language, compiler, project layout, or host path.

Preflights are admission checks, not diagnostic hints. An unsafe candidate is
relocated or rejected before implementation and before a complete local gate.
Record the candidate, observed value, declared limit, and disposition in the
event ledger.

## Stable decisions and worker packets

Maintain a canonical `Decisions` section in ignored `.agent/HANDOFF.md`. Each
entry has a stable ID, selected option or contract, rationale and provenance,
scope, and `active`, `superseded`, or `blocked` status. Before asking a material
question, search this registry, issue and PR evidence, and available conversation
evidence. Report contradictory current evidence against the decision ID rather
than silently overriding it or asking the same question again.

Default independent workers to the host equivalent of no inherited conversation
history. A packet contains only applicable decision IDs and selected contracts,
issue or PR identity, branch and exact head, owned scope, dependencies,
requirements, applicable gates, capability class, context envelope, and
the required structured transition output. Include a small recent-turn slice
only when immediately relevant. Full-history inheritance requires a recorded,
scoped exception.

Before starting a worker, identify its actual model from host metadata. Deliver
the applicable role, scope, authority, completion condition, and required skill
contents or reachable reference paths; naming a skill is not proof of delivery.
Record the resources the worker actually loaded and any missing capability in
its result. An isolated worker must not assume parent conversation or loaded
skills are inherited. Keep model-specific settings at the host boundary and
apply them only to the actual worker when supported by evidence.

## Delivery integration recommendation

During reconciliation inspect current workflows, required checks, review
automation, delivery states, native stack metadata, fork policy, and available
controller or watchdog mechanisms. Recommend only the provider-neutral contract
the selected graph needs:

- ordinary versus managed admission and ordered CI-ready, review-ready, and
  merge-ready transitions when delivery is deferred;
- exact-head checks and invalidation after a new head or topology change;
- singleton versus cumulative native-stack-prefix full-CI evidence;
- terminal review, zero unresolved threads, and inline-reply evidence;
- stale-event refusal, cancellation, fork security, and orphan recovery; and
- an observable fallback plus its expected cost when a capability is absent.

The consuming repository owns concrete workflow files, labels, check identities,
rulesets, apps or controllers, provider adapters, credentials, rollout, and
rollback. Never create or mutate that infrastructure merely because this plan
recommends it. If execution requires an absent capability, record a
repository-owned issue or confirmed mini-spec as a prerequisite and do not
pretend the capability exists. When it is optional, execute through the current
safe CI contract and report the limitation and extra cost.

## Event-driven waits

Use the installed deterministic review and delivery transition commands for
GitHub state and the host's passive wait primitive for worker completion. These
are transition sources for the inner and outer workflow loops, not separate
orchestrators. Wake the coordinator only on changed, terminal, or exceptional
state; use an exact known timestamp for time-based wakes. If the host cannot
passively await the foreground command, report the capability as unsupported.
Never substitute repeated model inferences for waiting.
