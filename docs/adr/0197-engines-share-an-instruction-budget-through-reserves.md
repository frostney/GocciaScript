# 0197 - Engines share an instruction budget through reserves

**Date:** 2026-11-27
**Area:** `engine`, `embedding`
**Related:** [#2240](https://github.com/frostney/GocciaScript/issues/2240), [#2051](https://github.com/frostney/GocciaScript/issues/2051)

## Context

A host can split one job across engines, for example a script whose tasks run on worker threads, each in an engine of
its own. The job's instruction limit belongs to the job, but each engine counted against its own `MaxInstructions`,
so a host had to hand each engine a guess at the remainder and add the counts up afterwards. A shared counter that the
dispatch loop writes on every instruction would make every counted run pay for an atomic or a lock, and capped runs
are the common case for a host that sets limits at all.

## Decision

**Engines that share a job attach one `IGocciaInstructionBudget`; each takes units from it in reserves and spends
them on its own counter.**

- The dispatch loop's fast path stays the per-engine counter; only an exhausted reserve reaches the budget.
- An engine takes a reserve only when its own units run out, so it holds at most one at a time. A reserve is the
  units the pending charge needs (one for an instruction, N for `PollInstructionUnits(N)`) or more: up to 1,024 and,
  under a cap, up to an eighth of what is left, but never less than what the charge needs. Each engine's reserve is
  bounded when it is taken, but the reserves of engines that pause while holding them add up: the job can be refused
  well below the cap when many engines hold reserves at once.
- An uncapped total that overflows `High(QWord)` stays saturated: refunds no longer apply to it.
- An engine returns its unspent reserve when its limits disarm. A refusal fixes the count at `Limit + 1`, as an
  engine's own cap does, and later refunds do not change it.
- The budget uses a lock, not 64-bit atomics, which 32-bit targets lack. The lock is taken once per reserve.
- `PollInstructionUnits(N)` lets native work charge host-chosen units against the same counter; a zero charge also
  checks the shared budget.

## Consequences

- An attached budget replaces the engine's own instruction settings; the smaller of a per-engine and a shared limit
  is #2051's work.
- A job's count is exact once every engine has ended, unless the job was refused.
- Setup work an engine runs under its limits (a globals module, the host environment) is charged to the job.
