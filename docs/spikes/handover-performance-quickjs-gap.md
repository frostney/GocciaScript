# Handover: Performance (QuickJS-scale gap)

Pick-up brief for the next performance workstream. Background audit:
[performance-simplification-audit.md](performance-simplification-audit.md).

## Executive Summary

- Goal of this lane: multi-× movement on AWFY/JetStream vs QuickJS, not local
  Goccia-vs-Goccia 10–20% patches.
- Current barometer (main CI run `29849904157`, 2026-07-21): ~**16.6×** AWFY
  geomean, ~**27.3×** JetStream frozen subset behind QuickJS.
- Primary tax: dual value model — unboxed `TGocciaRegister` inside the VM,
  boxed `TGocciaValue` at property/native/host/string boundaries
  (`RegisterToValue` in `Goccia.VM.Registers.pas`).
- Do not reopen rejected *tactics* (value caches, string interning, that read
  PIC, that fixed-arity collection). Do reopen *representation* questions those
  ADRs never measured.
- Vision constraint ([VISION.md](../../VISION.md)): not competing with
  V8/SpiderMonkey; QuickJS remains a barometer reference (ADR 0091), not a
  product ranking.

## Mission

Decide and spike the smallest representation change that can move object- and
call-heavy AWFY rows (Richards, Bounce, Storage, Json) and JetStream
hash-map/raytrace by a clearly multi-× or large-fraction amount — or prove that
the shared `TGocciaValue` substrate is a hard ceiling under current product
constraints.

This lane owns **runtime/VM performance**. ADR assumption write-ups and new
decision records belong to the sibling ADR handover.

## Start here (read order)

1. This handover + audit spike § “Measured gap” and § “Highest-leverage path”
2. [bytecode-vm.md](../bytecode-vm.md) § Performance Direction / Inline Caches
3. [profiling.md](../profiling.md) — `RegisterToValue` / `SysGetMem` as sample tops
4. ADRs **0081**, **0088**, **0089**, **0101** (tactic bans + open doors)
5. Spike [call-argument-overhead.md](call-argument-overhead.md)
6. Source: `Goccia.VM.Registers.pas`, `Goccia.VM.pas` dispatch,
   `Goccia.Values.Shape.pas`, `Goccia.Values.ObjectValue.pas`

## What is already known

| Fact | Evidence |
| --- | --- |
| Gap is structural (~16–27×), not noise | CI AWFY/JetStream artifacts |
| Caching boxed values does not buy runtime | ADR 0081 |
| Broader read PIC does not transfer to AWFY | ADR 0088 |
| Write-IC looked promising (+3–8% AWFY) but was deferred | ADR 0088 tables |
| Fixed-arity *collection subclass* lost to the argument pool | ADR 0089 / call-argument spike |
| Proof-backed scalar self-call frames work on fib (~70%); Sieve ≈ noise | ADR 0101 |
| Ordinary 0–3-arg bytecode calls already skip staging; method/native/host do not | call-argument spike |
| No `grkString` — strings are always heap objects | `Goccia.VM.Registers.pas` |

## Recommended work order

### Gate 0 — vision check (before code)

Confirm with a human whether bytecode may grow a denser value/object layout
that the interpreter does not share. If “both modes must stay representation-
identical forever,” Track B collapses to write-IC + proof-backed call ABIs only.
That decision is owned by the ADR handover; do not invent it in a perf PR.

### Track A — cheap wins that do not close QuickJS (optional, parallel)

Only if they stay small and do not steal focus from Track B:

- Unify `ValueToRegister` / `VMValueToRegisterFast` (`-0` semantics disagree today)
- Shared `GetIterator` sync core; shared `ToIndex`
- Document VM→evaluator edges (`eval`, `InstantiateClass`, decorator helpers)

These are simplification/correctness hygiene, not the QuickJS plan.

### Track B — performance (this handover’s main job)

1. **Spike: monomorphic inline / fixed-slot property storage** (or unboxed
   scalar slots) on shape-stable objects — bytecode-first prototype.
   - Measure: interleaved AWFY Richards/Bounce/Storage + JetStream hash-map
   - Preserve: accessors, delete→dict, Proxy, descriptor changes
   - Success bar: clear transfer on those rows, not probe-only
2. **Write-IC** as its own interleaved patch (ADR 0088 candidate), after or
   beside (1) if (1) is blocked on vision — expect single-digit % alone.
3. **Extend 0101-style proof** only where end-to-end transfer shows:
   ordinary/method scalar calls that beat the argument pool (ADR 0089 open
   doors). No per-call allocator shortcuts.
4. **String representation** only after object/call spikes: short-string /
   `grkString` / arena — **not** dictionary interning (ADR 0013).

## Acceptance gate (non-negotiable)

- Interleaved `--goccia-baseline` / `--goccia-candidate` (ADR 0076 / 0087)
- Probe wins are diagnostics only; AWFY/JetStream transfer required to merge
- Do not use allocation count as a success metric (ADR 0081)
- Full JS suite + bytecode mode green for any semantic-adjacent change

## Hard non-goals / false leads

- Value caches / small-int box pools (0081)
- Dictionary string interning (0013)
- Broader read PIC without new AWFY transfer evidence (0088)
- Fixed-arity collections that bypass the VM argument pool (0089)
- Merging interpreter + bytecode control flow “for speed”
- Optimizing to beat V8 or to “win” the public barometer ranking (VISION / 0091)
- Web Tooling lane as a QuickJS-gap closer (ADR 0090 — Goccia-only)

## Commands

```bash
# Same-runner candidate vs main (project pattern)
# AWFY / JetStream: see docs/benchmarks.md and perf/ manifests
./build.pas --prod scriptloader   # or project’s production bytecode loader build
./build.pas testrunner && ./build/GocciaTestRunner tests --mode=bytecode
```

Pull latest `awfy-report` / `jetstream-report` artifacts from main CI rather
than trusting numbers committed in-repo (there are none under `perf/`).

## Done looks like

- Either: a measured spike + draft ADR proposing a representation change that
  moved the barometer in a multi-× or large-fraction way on named workloads
- Or: a written negative result that the shared substrate is a ceiling under
  current vision, with the ADR handover owning the product decision

## Handoff to ADR lane when

- You need a new ADR to supersede or carve an exception from 0020/0014/0081/0088
- Vision disagrees with a bytecode-only denser layout
- A rejected tactic needs a *new* experiment design recorded (not a quiet reopen)
