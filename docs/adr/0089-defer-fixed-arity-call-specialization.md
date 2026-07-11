# Defer fixed-arity call specialization without end-to-end transfer

**Date:** 2026-07-10
**Area:** `bytecode`, `runtime`, `performance`
**Related:** [PR #948](https://github.com/frostney/GocciaScript/pull/948), [ADR 0076](0076-same-runner-benchmark-comparison.md), [ADR 0087](0087-awfy-cross-engine-benchmark-methodology.md), [call argument overhead investigation](../spikes/call-argument-overhead.md)

GocciaScript will keep the current pooled `TGocciaArgumentsCollection` and
generic native callback interface. The July 2026 fixed-argument collection,
native wrapper, and bytecode method-call experiment is rejected because it
added interface and call-path complexity without a repeatable end-to-end gain.

The rejected native design stored zero to three arguments in a lazy collection
subclass, but created and freed one subclass instance per qualifying call. The
existing VM path retains generic argument collections in a pool, so the
candidate did not eliminate the hot-path collection allocation it claimed to
replace. It also kept the generic native callback interface and did not cover
common host-to-JavaScript callback paths such as sort comparators.

The isolated bytecode method-call change was small and semantically sound, but
its performance result did not repeat. A 15-repetition run measured `1.019x` by
independent medians and `1.013x` by paired median; a 30-repetition rerun measured
`0.983x` and `0.998x`, respectively. The full measurements and source-path
analysis live in the linked investigation snapshot.

Consequences:

- New fixed-argument paths must preserve the existing argument-pool advantage or
  replace it with a measured lower-cost common representation.
- Targeted probes must execute the changed path inside their timed loop. An
  ordinary JavaScript call probe is not evidence for a native callback ABI.
- Method-call staging, native ingress, host callbacks, and `call`/`apply`/bound
  calls must be isolated before their results are combined.
- Probe improvements must transfer to the profiled real rows before the project
  accepts additional callable interfaces or argument representations.
- This decision does not prohibit pooled inline argument storage, a true
  arity-specific native ABI with a generic fallback, or fixed-arity host callback
  entry points. Those remain future candidates after profiling identifies the
  common call seam and interleaved measurements justify the complexity.
