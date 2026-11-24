# 0185 - On G2 the memory limit refuses with a catchable RangeError at the next back edge or call

**Date:** 2026-11-21
**Area:** `gc`, `g2`, `sandbox`
**Related:** [ADR 0130](0130-vm-stacks-are-charged-to-the-memory-budget.md), [ADR 0132](0132-call-stack-records-and-stack-copies-are-charged.md), [ADR 0110](0110-growth-gate-collects-before-refusing.md)

## Context

GocciaScript's `--max-memory` refuses a charged allocation that would cross
the ceiling with a catchable `RangeError` at that allocation, after a forced
collection ([Memory Ceiling](../garbage-collector.md#memory-ceiling)). A script
can catch it, drop what it holds, call `Goccia.gc()` and go on, and a test
runner's `afterEach` still runs after a refusal.

G2's allocator cannot fail: every native and VM operation that allocates
assumes it succeeds. Until now G2 ended the run when a full collection left the
heap over the ceiling, at the next back edge or call, past every handler
(`Fatal error: Live heap of N bytes exceeds the memory limit of M bytes`). A
script that catches out-of-memory, and every test that relies on that, failed
on G2.

Refusing at the exact allocation, as the old engine does, would give every
allocating native and VM operation a failure path, and the checks would slow
G2's hot paths.

## Decision

The refusal moves to the next safe point and stays catchable:

- A full collection that leaves the heap and external bytes over the ceiling
  marks a refusal pending (`MemoryLimitHit`). The run goes on to its next back
  edge or call, where the interrupt poll that serves timeouts throws an
  ordinary `RangeError: Allocation failed: GC heap size exceeds the configured
  memory limit` with the old engine's suggestion. The script can catch it.
- A refusal met at a call, with at least 1,024 frames on the stack, is the call
  stack's: it is `RangeError: Maximum call stack size exceeded`, located at the
  call, with the stack-memory suggestion.
- A refusal that would follow a deadline already passed yields to it: the
  timeout aborts the run.
- The limit still ends a run that does not free anything. Each refusal records
  what its collection left. Eight refusals in a row, each leaving no less than
  the one before, end the run as before (`AbortKind` 3, the `Live heap`
  message). A collection that ends within the limit resets the count.
- Making the refusal's error does not refuse again, and `LimitsDisarm` drops a
  refusal the run never reached a safe point for.

## Consequences

- The user-visible contract holds: catch, free and go on, and an uncaught
  refusal reports the old engine's message and suggestion.
- The error lands at the next safe point, not at the allocation, so its
  location can differ from the old engine's (a loop's head instead of the
  allocating expression). An allocation-heavy native that runs to completion
  past the ceiling finishes before the refusal.
- Tests of the old collector's internals (the pressure reserve, its back-off,
  the growth gate's capacity) measure heuristics G2 does not have. They go with
  the old runtime and are replaced by engine-neutral tests of this contract.
