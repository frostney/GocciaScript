# Performance and Simplification Audit

Point-in-time deep audit of duplication, QuickJS gap levers, and ADR validity
(2026-07-21, commit family around `c477011e` / main CI AWFY+JetStream).

## Executive Summary

- The useful ADR question is not “does source still match the decision text?”
  It is whether the **assumption behind the decision** still names the right
  counterfactual for closing a multi-× QuickJS gap.
- Three performance ADRs need assumption re-framing (not automatic reversal):
  **0081** (caches ≠ boxing diagnosis), **0088** (read-IC expansion ≠ remaining
  object cliff), and the **implicit 0020/0014 substrate** (shared heap
  `TGocciaValue` can still compete with tagged-value engines).
- Rejection ADRs that measured a *specific tactic* still hold as tactic bans:
  string *interning* (0013), value *caches* (0081), that *read-PIC design*
  (0088), that *fixed-arity collection design* (0089). What must not be
  generalized from them is “strings / properties / calls / boxing are solved.”
- Methodology ADRs (0076, 0087, 0091) and recent numeric ADRs (0100, 0101) do
  **not** need assumption re-validation for the QuickJS goal.
- On current main CI, Goccia is ~16.6× AWFY / ~27.3× JetStream behind QuickJS;
  that gap is the evidence that several older “local win / local reject”
  assumptions were looking at the wrong baseline.
- Pick-up briefs: [handover-performance-quickjs-gap.md](handover-performance-quickjs-gap.md),
  [handover-adr-assumption-revalidation.md](handover-adr-assumption-revalidation.md).

## Assumption validity (the actual ADR question)

ADRs are append-only history. “Re-validate” here means: decide whether the
*premise* still justifies treating the decision as a ceiling on future work.

### Must re-frame — premise aimed at the wrong counterfactual

| ADR | Stated / implied assumption | Why it is the wrong lens for QuickJS | What still stands |
| --- | --- | --- | --- |
| **0081** reject value caches | “Short-lived boxed `TGocciaValue`s are cheap under FPC+GC; cutting alloc *count* via caches does not move runtime.” | Measured **Goccia-with-cache vs Goccia-without-cache**. Correct for that. The QuickJS counterfactual is **no box at the boundary**. 0081 falsified a remedy, not the diagnosis that `RegisterToValue` / boxed slots are the structural tax. Reading 0081 as “boxing is not the bottleneck” is the wrong assumption. | Keep rejecting content/range caches. Re-open *representation* work (unboxed slots, arenas without per-box lookup) — 0081 already points there once, but the project culture treated the ADR as a broader “stop worrying about boxes.” |
| **0088** reject broader read PIC | “Property probes are far behind QuickJS ⇒ expanding read-side IC is the lever for object-heavy AWFY.” | After shape-lite, **lookup miss/hash was not what AWFY still paid**. Read-PIC transferred to a polymorphic *probe* (+9.9%) and not to Richards/Bounce/Storage. The wrong assumption was the *class* of fix (more IC state), not the transfer gate. | Keep the transfer gate. Do **not** treat “property access is done.” The open premise is storage/layout (inline slots, write path, boxing on define/set), which 0088 never tested as the primary hypothesis. |
| **0020 + 0014** (implicit) | Unified heap hierarchy + bytecode/interpreter feature parity are compatible with a competitive bytecode runtime. | These ADRs decided *correctness/architecture consolidation*, not speed. The unspoken performance assumption — “one `TGocciaValue` substrate can approach QuickJS” — is exactly what the barometer now challenges. Parity of *semantics* does not require parity of *representation* forever. | Keep semantic parity. Re-validate whether bytecode may grow a denser value/object layout that the interpreter does not share, or whether both modes must move together. That is vision-level; the ADRs never measured it. |

### Re-validate the *experiment design*, not the rejection

| ADR | Flawed assumption in what was tried | Decision still OK? |
| --- | --- | --- |
| **0089** defer fixed-arity | Candidate “eliminated” collection cost by allocating a *new* fixed-arity collection per call — worse than the pool it claimed to beat. | **Yes.** Defer that design. The ADR already leaves open pooled inline storage and a true arity-specific ABI; those assumptions were never tested. 0101 re-validated a *different* call assumption (scalar self-frame) successfully on a probe. |
| **0065 / 0066** shape-lite | Shapes as **IC identity** (key sequence → entry index) are enough to close object-heavy gaps. | **Design still right for what it is.** Assumption to re-validate: whether shapes should become **layouts** (fixed slots, unboxed fields). 0088 already showed more IC identity does not move AWFY. |

### Do not re-validate — assumption still matches the goal

| ADR | Why the premise is still right |
| --- | --- |
| **0013** reject string *interning* | Premise is “hash+lookup beats FPC COW alloc for dictionary intern.” Still true for that tactic. Does **not** forbid short-string tags / arenas / `grkString` — and must not be read as “strings are fine” (Json is ~24× behind QuickJS). |
| **0005** register VM | Fewer instructions / less shuffle vs stack — still the right VM shape; QuickJS is not an argument to go stack. |
| **0074** deferred call-stack frames | Premise: Error.stack needs ordering; pointer push is enough. Still holds. |
| **0087 / 0076 / 0091** measurement | Interleaved same-runner transfer is how you avoid false ADR premises. Keep. |
| **0100 / 0101** | Native binary64 + proof-backed scalar self-calls; premises narrow and freshly measured. 0101 already admits AWFY Sieve non-transfer — no hidden wrong assumption. |
| **0002** singletons | Tiny fixed set is free; widening failed under 0081. Holds. |

### One-line answer

**Yes — re-validate assumptions on 0081, 0088, and the implicit 0020/0014 substrate.**  
Not because those ADR *decisions* were careless, but because they answered “does this local tactic beat current Goccia?” while the QuickJS goal needs “does our value/object *model* pay a tax no IC or cache can remove?” Most other performance ADRs do not need that re-validation.

## Scope and method

Evidence sources:

- Architecture and VM docs: [architecture.md](../architecture.md),
  [bytecode-vm.md](../bytecode-vm.md), [profiling.md](../profiling.md),
  [CONTEXT.md](../../CONTEXT.md)
- Performance ADRs 0001–0101 (critical set listed below), plus spikes under
  `docs/spikes/`
- Current source under `source/units` (VM, registers, values, evaluator,
  compiler)
- Main CI artifacts `awfy-report` and `jetstream-report` (workflow run
  `29849904157`, generated 2026-07-21; QuickJS `2026-06-04`, Node `v26.5.0`)

This spike records findings. It does **not** decide a new architecture; any
representation change that conflicts with the shared interpreter/bytecode value
model is a vision-level decision.

## Measured gap to QuickJS

No numeric baselines are committed under `perf/` (manifests only). Live ratios
come from CI:

### AWFY (geomean `goccia-baseline_over_qjs` ≈ 16.6×)

| Bench | Goccia median (ms) | QuickJS (ms) | Ratio |
| --- | ---: | ---: | ---: |
| Json | 336.6 | 14.1 | 23.9× |
| Permute | 42.1 | 1.8 | 23.0× |
| Sieve | 12.7 | 0.58 | 22.0× |
| Bounce | 22.7 | 1.1 | 20.8× |
| CD | 201.5 | 9.8 | 20.6× |
| Towers | 61.2 | 3.1 | 19.8× |
| Havlak | 80682 | 4180 | 19.3× |
| Richards | 911.8 | 52.1 | 17.5× |
| Queens | 22.1 | 1.3 | 17.1× |
| DeltaBlue | 4.7 | 0.29 | 16.1× |
| List | 15.4 | 1.0 | 15.1× |
| NBody | 0.41 | 0.032 | 12.6× |
| Storage | 34.7 | 3.0 | 11.4× |
| Mandelbrot | 0.033 | 0.006 | 5.9× |

### JetStream frozen subset (geomean ≈ 27.3×)

Score-ratio suite (`qjs_score / goccia_score`). All six workloads sit roughly
24–33× behind QuickJS (hash-map, lazy-collections, raytrace, sync-fs, ai-astar,
gaussian-blur).

Interpretation (project policy): the barometer is a **north-star aid**, not a
product ranking ([CONTEXT.md](../../CONTEXT.md),
[ADR 0091](../adr/0091-unified-performance-barometer.md)). QuickJS is a
reference engine, not a twin architecture.

## Why Goccia is much slower (architectural)

Ordered by how much they explain the multi-× gap:

1. **Dual value representation.** Registers are a tagged variant
   (`grkUndefined|Null|Hole|Boolean|Int|Float|Object`) in
   `Goccia.VM.Registers.pas`. There is no `grkString`; strings are always
   `grkObject`. Crossing into property maps, descriptors, native callbacks, or
   host APIs goes through `RegisterToValue`, which allocates
   `TGocciaNumberLiteralValue` for ordinary integers (only `0`/`1` plus
   boolean/null/undefined/hole singletons are reused). QuickJS keeps primitives
   in one compact tagged word end-to-end.

2. **Shared heap object model with the tree-walk interpreter.**
   `architecture.md` / ADR 0020 / 0021: one `TGocciaValue` hierarchy for both
   executors. That consolidates semantics and blocks a QuickJS-like compact
   object layout without rewriting both modes.

3. **Property storage is boxed.** Own properties live in shaped maps of
   descriptors/`TGocciaValue`. Shape-lite monomorphic read + shallow proto ICs
   exist (ADR 0065/0066); they help local probes but do not remove heap boxes.

4. **Call seams outside proven numeric self-recursion.** Ordinary 0–3-arg
   bytecode calls already skip argument staging; method/native/host paths still
   use pooled `TGocciaArgumentsCollection` and a generic native ABI
   ([call-argument-overhead.md](call-argument-overhead.md), ADR 0089).
   ADR 0101’s `OP_CALL_SELF_NUM` proves a narrow scalar frame (~70% on the fib
   probe) without corpus-wide transfer (AWFY Sieve ≈ noise).

5. **Large Pascal dispatch loop.** `ExecuteClosureRegistersInternal` in
   `Goccia.VM.pas` pays opcode decode, a giant `case`, and frequent boxing
   helpers. External `sample` profiles commonly surface `RegisterToValue`,
   `SysGetMem`, and exception-frame helpers ([profiling.md](../profiling.md)).

6. **Mark-and-sweep over TObject-shaped values**, with GC-visible register /
   local / argument windows kept deliberately full for safety
   ([bytecode-vm.md](../bytecode-vm.md) § Performance Direction).

7. **Host language and product constraints.** FreePascal, sandbox-first design,
   dual-executor feature parity, and an optimization gate that demands
   interleaved AWFY/JetStream transfer rather than probe-only wins.

8. **Residual bytecode→evaluator coupling.** Docs claim near-independence; the
   VM still uses `Goccia.Evaluator` for direct `eval`, decorator helpers, and at
   least one `InstantiateClass` fallback (`Goccia.VM.pas`). That is weight and
   drift risk more than the AWFY cliff, but it undercuts the “bytecode-only”
   story.

## Duplication audit (ranked)

### Keep deliberately (not a bug)

| Surface | Why it exists | Action |
| --- | --- | --- |
| Full Evaluator vs Compiler+VM | ADR 0005/0014/0021 — tree-walk default + register bytecode | Keep. Extract shared **policy leaves** only |
| `TGocciaFunctionValue` / class vs bytecode counterparts | Different callable representations | Keep; push shared behavior into bases |
| Generators (continuation vs bytecode generator object) | Different suspend models | Keep; share IteratorClose helpers |
| Triple arithmetic in VM (`OP_*_INT` / scalar / `Goccia.Arithmetic`) | Hot typed ops + shared semantic leaves (ADR 0100) | Keep |
| Array vs TypedArray iteration methods | Spec-different exotics | Share callback helpers only |

### High-value simplification (maintenance)

1. **Unify `GetIterator` cores** —
   `Goccia.Values.IteratorSupport.GetIteratorFromValue` vs
   `TGocciaVM.GetIteratorValue`. Sync rules should be one leaf; VM wraps async.
2. **Centralize `ToIndex` / `ToIntegerOrInfinity` variants** scattered across
   ArrayBuffer, Atomics, DataView, TypedArray, BigInt — different error text is
   fine as parameters; duplicate conversion is not.
3. **Unify `ValueToRegister` and `VMValueToRegisterFast`** —
   `Goccia.VM.Registers.pas` vs local fast path in `Goccia.VM.pas`. They already
   disagree on `-0` handling (`RegisterFloat` vs `RegisterObject` for negative
   zero). One helper, one semantic.
4. **Document VM→evaluator edges accurately** in architecture/bytecode-vm
   (eval, `InstantiateClass`, decorator unit naming).
5. **Move `Goccia.Evaluator.Decorators` helpers** to a neutral unit name so
   bytecode does not appear to “depend on the evaluator” for shared policy.
6. **Collapse duplicate `InferredExpressionType` locals** in
   `Goccia.Compiler.Expressions.pas` and `Goccia.Compiler.Statements.pas`.

### Do not “simplify” by merging

Merging interpreter and bytecode control flow, or introducing a second silent
value representation without an explicit dual-mode plan, would fight the
architecture and ADR 0014 parity policy.

## ADR validity (critical performance set)

| ADR | Verdict | Notes |
| --- | --- | --- |
| 0001 dual number enum | **SUPERSEDED** | Replaced by 0016 / 0100; no `TGocciaNumberSpecialValue` in source |
| 0005 register bytecode | **YES** | Still the right baseline vs stack |
| 0013 reject string interning | **YES** | No content intern pool; shape-key interning is unrelated |
| 0014 feature parity | **YES** | Perf-only opcodes OK; no language forks |
| 0016 IEEE Double | **PARTIAL** | Representation holds; lifetime FPU mask **superseded by 0100** scoped enter/leave |
| 0020 unified heap | **YES** (name stale) | `TGCManagedObject`, not historical `TGocciaCompiledHeapObject` |
| 0021 Goccia VM unification | **YES** | Souffle bridge gone |
| 0040 constant folding | **YES** | Still format-neutral compiler work |
| 0065 shape-lite IC | **PARTIAL** | Core holds; eager shapes / class-method table refined by **0066** |
| 0066 lazy shapes | **YES** | Matches `EnsureShape` + proto cache docs |
| 0074 deferred call-stack frames | **YES** | `PushTemplate` still on hot path |
| 0081 reject value caches | **YES** | Alloc count ≠ speed; singleton set only |
| 0088 reject broader read PIC | **YES** | Write-IC deferred as separate candidate |
| 0089 defer fixed-arity ABI | **YES**, refined by **0101** | 0101 is proof-backed scalar self-call, not the rejected general ABI |
| 0091 performance barometer | **YES** (pin SHA stale) | JetStream pin in ADR text lags `perf/jetstream/manifest.json` |
| 0100 native binary64 | **YES** | Scoped FP + shared semantic leaves |
| 0101 closed numeric self-call | **YES** | Narrow lever; do not generalize without AWFY transfer |

### Other still-valid performance ADRs (skim)

0002 singletons, 0003 opcode hotness tiers, 0004 compiler desugaring, 0007 VMT
dispatch, 0017 AST VMT eval, 0018 `TStringBuffer`, 0019 custom maps, 0035–0037
lexer/parser hot paths, 0070 / 0098 numeric text, 0087 AWFY methodology, 0090
Web Tooling Goccia-only lane.

### Explicit supersessions (healthy history)

- 0001 → 0016 → 0100 (number representation + FP control scope)
- 0065 → 0066 (eager shapes → lazy; class-method table → proto-holder cache)
- 0080 FormatDouble Str scan → 0098 Ryū `NumberToString`

### Doc drift found during validation

- `docs/embedding.md` § FPU Exception Mask still described constructor/destructor
  lifetime `SetExceptionMask` on engine/VM. Source uses
  `EnterGocciaFloatingPointScope` / `LeaveGocciaFloatingPointScope`
  (`Goccia.FloatingPoint.pas`) around execute paths (ADR 0100). **Corrected in
  the same change as this spike; do not rewrite ADR 0016** (immutable history).

## Highest-leverage path toward QuickJS (multi-×, not 10–20%)

Gate every candidate with interleaved AWFY + JetStream transfer (ADR 0087 /
0076). Probe wins alone are diagnostics.

| Priority | Lever | Mechanism | Risk / conflict |
| --- | --- | --- | --- |
| 1 | **Fewer register↔heap crossings on hot edges** | Keep scalars in property slots and call args without `RegisterToValue` | Vision-level: challenges shared boxed `TGocciaValue` substrate |
| 2 | **Inline / fixed-slot property storage for monomorphic shapes** | Field get/set become offset loads | High; must preserve accessors, delete→dict, Proxy |
| 3 | **Proof-backed scalar call ABIs beyond 0101** | Ordinary + method + hot native paths for 0–3 args without pool tax | Must beat the pool (ADR 0089); no per-call allocator shortcuts |
| 4 | **Write-side IC** (from ADR 0088 split) | +3–8% on object AWFY historically — enabler, not multi-× alone | Separate patch; still needs transfer |
| 5 | **String register kind / short-string / arena** | Json/string cliffs; ADR 0013 only rejects *interning* | High Unicode/GC cost |
| 6 | **More typed/superinstruction lowering** | Cut dispatch where scalar hit rate is ~100% | Aligns with current VM direction |
| 7 | **Shrink Pascal `try/except` on hot call/dispatch if attributed** | Exception frames show up in `sample` | Preserve Error.stack / ADR 0074 |
| 8 | **Arity-specific native/host callbacks for profiled builtins** | Sort/map/forEach stay in registers | Open under ADR 0089; keep the argument pool |

ADR 0100/0101 show the *winning style*: closed-world proof, no deopt fiction,
measure transfer. They do not by themselves close the corpus gap.

## Already tried — rejection still holds

| Experiment | Result | Still hold? |
| --- | --- | --- |
| Broader read PIC (0088) | Probe +9.9% polymorphic; AWFY flat/negative | **Yes** |
| Write-IC-only (0088) | Positive AWFY medians; not merged | **Deferred**, still best incremental IC lane |
| Fixed-arity call specialization (0089 / #948) | Native path worse than pool; method-only noise | **Yes for that design** |
| Small-int / value caches (0081) | −25% allocs, +2.2% runtime | **Yes** |
| String interning (0013) | −4% over 172 benches | **Yes** |
| `TScopeMap` linear scopes (0010) | 2.7× regression in real scopes | **Yes** |
| Removing call-stack push entirely (0074) | Rejected for Error.stack | **Yes** |

## False leads (do not chase)

- Treating **allocation count** as a success metric
- Dictionary string interning or range-keyed boxed-number caches
- Broader read PIC from synthetic property probes without AWFY transfer
- Fixed-argument collections that **bypass the VM argument pool** while keeping
  the generic native callback
- Sequential baseline-then-candidate timing
- Web Tooling lane as a QuickJS-gap closer (Goccia-only viability, ADR 0090)
- Optimizing for a leaderboard vs QuickJS as a product goal (ADR 0091 /
  CONTEXT)
- Merging interpreter and bytecode into one execution path “for simplicity”

## Recommended next workstreams

Split into two tracks so simplification does not wait on representation research:

### Track A — simplification (low vision risk)

1. Shared `GetIterator` sync core + shared `ToIndex` helper
2. Single register conversion helper (resolve `-0` once)
3. Docs: list VM→evaluator edges; FPU masking narrative already aligned to
   ADR 0100 in `embedding.md` (same change as this spike)
4. Neutral home for decorator helpers; dedupe compiler `InferredExpressionType`

### Track B — multi-× performance (vision-gated)

1. Spike compact property storage / unboxed slot values on monomorphic shapes
   with a **bytecode-only** prototype first, measuring AWFY Richards/Bounce/
   Storage and JetStream hash-map/raytrace
2. Revisit write-IC as its own interleaved patch (ADR 0088 candidate)
3. Extend 0101-style proof to ordinary self/mutual numeric calls and method
   calls only where end-to-end transfer is shown
4. Profile-attribute Pascal exception and allocator cost on AWFY Json/Richards;
   only then consider hot-path error-register changes

Any Track B item that changes the shared interpreter/bytecode value contract
needs an explicit ADR before merge.

## Evidence anchors

- CI AWFY/JetStream artifacts (run `29849904157`, 2026-07-21)
- `source/units/Goccia.VM.Registers.pas` — `ValueToRegister` / `RegisterToValue`
- `source/units/Goccia.VM.pas` — dispatch, `VMValueToRegisterFast`, evaluator
  imports, `OP_CALL_SELF_NUM` / `PushClosedNumericFrame`
- `source/units/Goccia.FloatingPoint.pas` — scoped FP control
- ADRs 0081, 0087–0091, 0100–0101; spikes `call-argument-overhead.md`,
  `fpc-hashmap-performance.md`
- Issues historically closed on this frontier: `#822`, `#860`, `#861`, `#862`
  (as cited in prior ADR/spike text)
