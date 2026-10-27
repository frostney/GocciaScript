# 0165 - The dispatch loop keeps its hot arms first and aligned

**Date:** 2026-10-27
**Area:** `bytecode VM`, `benchmarking`
**Issue:** [#1917](https://github.com/frostney/GocciaScript/issues/1917)
**Related:** [ADR 0003](0003-opcode-layout-by-hotness.md), [ADR 0076](0076-same-runner-benchmark-comparison.md), [ADR 0087](0087-awfy-cross-engine-benchmark-methodology.md), [ADR 0154](0154-the-vm-dispatch-loop-uses-no-goto.md), [Bytecode VM § Dispatch layout](../bytecode-vm.md#dispatch-layout)

## Context

Per-opcode work on `ExecuteClosureRegistersInternal` could not be judged by
its benchmark times: #1909, a change to the numeric arms, measured Bounce,
List and Havlak several percent slower, and was closed for it. Measured on a
Zen 5 core (Ryzen AI MAX+ 395) with FPC 3.2.4-rc2 production builds, in user
cycles and with Linux `perf_event` and IBS fetch sampling, three things move a
benchmark without changing the instructions it executes:

- **Page placement.** Byte-identical copies of one binary run apart, because
  the kernel places each file's pages anew. A comparison of one binary per
  side measures one placement of each. #1909's change, rebuilt on today's
  `main` and measured with four fresh copies per side over eight rounds, is
  Mandelbrot 6.6%, NBody 3.3% and Sieve 4.3% faster (7.4%, 1.6% and 1.2%
  fewer instructions), and within 0.9% elsewhere except DeltaBlue, the
  noisiest benchmark here (-1.6%).
- **Moving code that never runs.** 80 bytes added to the cold
  `OP_LOAD_REGEXP` arm, which came before most hot arms, changed the op-cache
  misses of Mandelbrot by -44%, Richards -27%, List -20%, Towers -18% and
  Permute -17%, while the instruction counts stayed the same to the
  instruction. Here that cost at most 1.3% of time, but how much it costs
  depends on where the code lands.
- **Unlucky layouts.** Most layouts land within about 1% of each other, and
  some are far off. Putting the hot arms first, with nothing else changed,
  made Towers, Queens, Permute and Richards 10-13% slower and List 7.6%: their
  instruction-cache fills rose up to six-fold and their op-cache misses by up
  to half. In Towers, 78% of the op-cache misses fell in four of the 64 sets,
  where lines of `VMFillProtoReadCacheCore`, the first hot arm, `PushFrame` and
  a collector list method sit at the same offset in their pages. Moving all of
  that code by any of eleven distances from 64 to 3072 bytes brought those
  benchmarks back to within about 1% of each other, except Richards at 64
  bytes (+3.6%).

## Decision

**The hot arms come first.** FPC lays out a case statement's arms in source
order. The 49 arms the AWFY workloads execute most, by their mean share of
executed opcodes, open the dispatch case, highest first; every other arm
follows them, and a new opcode's arm goes there. An edit to a cold arm then
cannot move a hot one.

**Every arm starts on a 64-byte boundary.** The dispatch function is compiled
with `{$CODEALIGN JUMP=64}` between `{$PUSH}` and `{$POP}`, so no arm shares a
cache line or an op-cache window with the end of the one before it. FPC also
aligns the labels where a case statement ends and where an `if` without an
`else` rejoins, in the function, the helpers inlined into it and the routines
nested in it, and nothing outside them.

`scripts/check-vm-dispatch-layout.ts`, run by CI and the pre-commit hook, holds
the hot arms and fails when they are no longer first and in order, or when the
alignment is gone. `./build.pas --prod --layout-probe` builds the same binary
with a few hundred bytes of code that never runs ahead of the dispatch
function, as a placement control.

**A benchmark A/B can tell work from placement.** The AWFY driver records
instructions retired, cycles and branch, L1i and iTLB misses for every Goccia
sample with `--counters`, where Linux `perf_event` allows, and the PR comment
shows the instruction and L1i-miss deltas next to the time delta.
`--fresh-copies`, `--cpu`, `--alternate-order` and `--warmup` sample placements
and keep the comparison balanced. They extend the driver and the report of
ADR 0087 rather than adding another path.

## Measurements

Same machine and builds, four copies per binary, each copied afresh or with
its page cache dropped before every round, rounds in alternating order on one
core whose SMT sibling stayed idle, the first round discarded. Medians of user
cycles over twelve AWFY benchmarks. These runs used a measurement script
outside the repository that does what the driver's `--fresh-copies
--alternate-order --cpu --warmup` do, and also counted the AMD op-cache event
`op_cache_hit_miss.op_cache_miss`, which `--counters` does not record.

| Pair | Largest benchmark difference | Geomean |
| --- | ---: | ---: |
| `main` and `main` with the cold-arm edit | 0.70% (Bounce) | -0.07% |
| `main` and `main` with the layout probe | 1.35% (Permute) | -0.01% |
| this layout and it with the cold-arm edit | 0.72% (Json) | -0.04% |
| this layout and its layout probe | 1.39% (Json) | +0.02% |
| this layout against `main` | 1.76% (DeltaBlue), 1.65% (Storage, faster), 1.15% (Bounce) | +0.35% |

Two earlier comparisons of the same binary against `main` gave a geomean of
+0.32% and +0.06%, with Bounce 1.4-1.8% slower and Storage 1.2-1.6% faster each
time. With the cold-arm edit, this layout's op-cache misses stay within one
point of their unedited values in ten of twelve benchmarks, against changes of
up to 44% on `main`; against `main` they fall by 22-39% in List, Queens,
Permute, Towers, Sieve and Richards and rise by 7-16% in Bounce, DeltaBlue and
NBody. The layout moved by each of eleven distances from 64 to 3072 bytes kept
every benchmark within 1.9% of its other layouts.

The driver itself (`--counters --fresh-copies --alternate-order --cpu
--warmup 1`, twelve repetitions, eleven benchmarks, wall time), on this layout
merged with `main` at `2fcffb67`:

| Pair | Largest benchmark difference | Geomean |
| --- | ---: | ---: |
| `main` and its layout probe | 1.31% (DeltaBlue) | +0.12% |
| this layout and its layout probe | 1.79% (List) | +0.16% |
| this layout against `main` | 1.96% (Storage, faster), 1.39% (Bounce) | +0.10% |

Each probe pair executes the same instructions to within 0.001%; this layout
executes 0.0-0.7% more than `main`.

## Consequences

- This is not a speed-up. Against `main` the layout is 0.06-0.35% slower on
  geometric mean in four comparisons, Bounce 1.2-1.8% slower and Storage
  1.2-2.0% faster each time, within the spread of the layouts measured.
- An edit to a cold arm leaves the hot arms where they are, and a time delta
  it shows is noise. An edit to a hot arm still moves the hot arms after it,
  by whole lines.
- The dispatch function grows from 237 KB to 310 KB, mostly through the
  aligned `if` joins, and the padding before aligned labels that code falls
  through to costs 0.0-0.7% more instructions (List 0.7%, the others below
  0.3%).
- A different hot set, or another order, is a measured change: it is laid out
  differently everywhere after its first arm. `check-vm-dispatch-layout.ts
  --rank` derives the list from opcode profiles, with the command that made
  it.
- The alignment and the order were measured on one x86-64 core; other
  processors cache code differently. The layout probe and `--counters` measure
  them where they run.
- A placement can still be unlucky for a given benchmark. The instruction count
  says whether a change did work, and the layout probe how far placement moves
  the times on that machine.
