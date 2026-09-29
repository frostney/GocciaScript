# Repository Orchestration Policy

## Authority and fallback

This file is GocciaScript's repository policy for multi-agent work (Milestone
Rush and any coordinator that runs parallel lanes). It is subordinate to
[`AGENTS.md`](./AGENTS.md), [`DEFINITION_OF_DONE.md`](./DEFINITION_OF_DONE.md),
and the safety gates of the invoked workflow. The Milestone Rush
[policy gate](.agents/skills/milestone-rush/references/orchestration.md#policy-gate) defines how a
consumer validates this file and what it does when the file is absent.

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
- Lanes that would conflict are sequenced through the Milestone Rush
  [dependency and conflict graph](.agents/skills/milestone-rush/SKILL.md#reconcile-and-plan).

## Durable checkpoints

- Every lane pushes its branch at each durable transition (settled decision,
  completed step, new exact head), so a killed lane loses at most its current
  step. After a usage-limit reset, resume each lane from that checkpoint and
  report the lost window.

## Context limits

- Record a warning when a lane's context passes 100k tokens. Past 150k
  tokens, checkpoint and split or replace the lane before its next
  inference. This is an absolute limit, independent of the model's context
  window: a large window lets a lane grow, and every later inference pays
  for re-reading the whole context.
- A milestone-sized run starts from a fresh coordinator session seeded by
  the handoff, not from a long-running conversation.

## Context packets

Lane packets follow the Milestone Rush
[worker packet](.agents/skills/milestone-rush/references/orchestration.md#stable-decisions-and-worker-packets)
contract.

## Usage ledger

Record usage through the Milestone Rush
[event ledger](.agents/skills/milestone-rush/references/event-ledger.md). When usage cannot be
attributed to a lane, the per-lane context threshold above uses the lane's own
most recent inference size, which every host reports.

## Waiting

External state is awaited through the Milestone Rush
[event-driven waits](.agents/skills/milestone-rush/references/orchestration.md#event-driven-waits).

A lane never waits more than five minutes inside its own context. Before a
longer wait (CI, a queued build or test run, a release workflow, or a
usage-limit reset), the lane pushes its checkpoint, hands the wait to the
coordinator or a non-model watcher, and ends its turn. It resumes when the
result arrives. Hosts may expire a lane's context cache within minutes; the
next turn after an expiry rewrites the whole context. In one measured
GocciaScript session, rewrites after waits longer than about five minutes
were 89% of all cache writes.
