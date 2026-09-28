# Repository Orchestration Policy

## Authority and fallback

This file is GocciaScript's repository policy for multi-agent work (Milestone
Rush and any coordinator that runs parallel lanes). It is subordinate to
[`AGENTS.md`](./AGENTS.md), [`DEFINITION_OF_DONE.md`](./DEFINITION_OF_DONE.md),
and the safety gates of the invoked workflow. It names capability classes,
never products, models, or harnesses.

A consumer classifies this policy before it plans or spawns work: **valid**
(apply it), **invalid** or **contradictory** (block spawning and name the
rule), or **unsupported** (block and report the missing capability). Only a
missing file permits a generic fallback.

## Capability classes

- **Efficient:** monitoring, status collection, deterministic checks, and
  mechanical evidence extraction.
- **Frontier (high reasoning):** design, implementation, diagnosis, and
  independent review.
- When classification is ambiguous, use frontier and record why.

## Concurrency

- At most **4** implementation lanes run at once in one run. A lane's
  review work counts against that lane: it runs in the lane's foreground,
  and the lane never ends its turn waiting on background children.
- Usage limits are shared across every workstream on the same account.
  When the maintainer reports other concurrent workstreams, lower the cap
  before dispatching.
- Lanes that change the same files are sequenced, or delivered as a native
  stack, rather than run in parallel.

## Durable checkpoints

- Every lane works in its own worktree and pushes its branch at each durable
  transition (settled decision, completed step, new exact head), so a killed
  lane loses at most its current step.
- On a usage-limit or rate-limit failure, do not retry before the
  host-reported reset. Resume each lane from its checkpoint and report the
  lost window.

## Context limits

- Record a warning when a lane's context passes 100k tokens. Past 150k
  tokens, checkpoint and split or replace the lane before its next
  inference. This is an absolute limit, independent of the model's context
  window: a large window lets a lane grow, and every later inference pays
  for re-reading the whole context.
- A milestone-sized run starts from a fresh coordinator session seeded by
  the handoff, not from a long-running conversation.

## Context packets

Lanes start with no inherited conversation. A packet carries the applicable
decision IDs and text, the issue and exact head, the owned scope,
dependencies, acceptance criteria, and the required gates.

## Usage ledger

Record per-lane and per-class usage (inferences; input, cached, output, and
reasoning tokens; tool calls; wall time) from the host's own usage records,
such as completion metadata or transcripts. Mark a field unavailable only
when the host exposes no record of it.

## Waiting

External state (CI, merges, releases) is awaited with non-model watchers.
A model is invoked only on changed, terminal, or exceptional state.

A lane never waits more than five minutes inside its own context. Before a
longer wait (CI, a queued build or test run, a release workflow, or a
usage-limit reset), the lane pushes its checkpoint, hands the wait to the
coordinator or a non-model watcher, and ends its turn. It resumes when the
result arrives. Lane context caches can expire within minutes, so each
longer wait rewrites the whole context; in a measured GocciaScript session,
those rewrites were 89% of all cache writes.
