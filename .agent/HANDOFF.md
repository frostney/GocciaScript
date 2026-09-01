# Handoff

Updated: 2026-08-26 (wave 2 lanes launching from stacked head)

## Experiment

- **Goal:** Goccia bytecode at 0.6×–0.8× of QuickJS *speed* (AWFY `goccia_over_qjs` ≈ 1.25–1.67).
- **Current main CI** (`f4d403f0`, linux/x64 Azure): AWFY geomean `goccia_over_qjs` = **16.256** (speed **0.062×**). Need ~10–13×.
- **Delivery branch:** `perf/bytecode-quickjs-gap` @ `9e0dfde8` (skill adoption on `origin/main` `f4d403f0`). Wave-2 lanes branch from this head. After they return: merge one accepted lane at a time, combined A/B vs `/tmp/goccia-combined-dc9e958e` (then the new combined head), full interpreter+bytecode JS gate, then submit accepted work as a **native `gh stack`** (wave-1 combined vs `main` as the bottom layer; each newly accepted wave-2 commit as its own layer). Do not reconstruct wave-1 as five force-pushed layers unless `gh stack` can do it from existing commits without a raw rebase.
- **Accepted code:** `ad0023da` `OP_ADD_NUM_IMM` (opcode **230**, format **v78**); `315bfd28` numeric `i = i + 1` → `OP_INC_NUMERIC`; `15e8eca7` own writable write IC; `e6f3e177` counted-for `i = i + 1` → `OP_ADD_INT`; `db9567bd` `OP_GET_LOCAL_PROP_CONST` (opcode **231**, format **v79**). Next free opcode **232**.
- **Wave-2 A/B baseline binary:** `/tmp/goccia-combined-dc9e958e` (engine equivalent to `9e0dfde8`). Lanes must not overwrite it or `/tmp/goccia-baseline-f4d403f0`.
- **Baseline binary:** `/tmp/goccia-baseline-f4d403f0` (`--prod` loader, darwin/aarch64, FPC 3.2.2).
- **QuickJS:** 2026-06-04 at `/tmp/quickjs/bin/qjs`.
- **Lock:** `/tmp/gocciascript-perf-gate.lock`.
- **AWFY sources:** `/tmp/are-we-fast-yet` @ `74306fe`.
- **Invariant:** interpreter/bytecode observationally identical; no sandbox weakening.
- **Non-goals:** V8/SpiderMonkey parity; rejected read-PIC (ADR 0088); value caches (ADR 0081).

## Local probe baseline (darwin/aarch64, 7 interleaved reps)

| Probe | Goccia us | QJS us | g/qjs | speed |
| --- | ---: | ---: | ---: | ---: |
| loop-dispatch-floor | 129288 | 7130 | 18.13 | 0.055× |
| generic-plus-scalars | 63495 | 4874 | 13.03 | 0.077× |
| nbody-minimal | 66948 | 2581 | 25.94 | 0.039× |
| fib-recursive | 46752 | 7351 | 6.36 | 0.157× |
| propaccess-monomorphic | 17006 | 985 | 17.26 | 0.058× |
| fixed-arg-call | 36265 | 2488 | 14.58 | 0.069× |
| geomean |  |  | 14.63 | 0.068× |

Fib is the best relative row because it already uses `OP_CALL_SELF_NUM` / `OP_SUB_NUM_IMM` / `OP_JUMP_IF_NUM_NOT_LTE_IMM`. Dispatch-floor is 18×: the interpreter tax.

## Profile facts (function-wrapped equivalents)

- `loop-dispatch-floor`: original baseline was 38% `OP_GET_LOCAL`, 15% `OP_LOAD_INT`, 11% `OP_SET_LOCAL`, 8% `OP_ADD_FLOAT`. Counted-for now matches `i = i + 1` and emits `OP_ADD_INT` (`e6f3e177`); standalone assignment still uses `OP_INC_NUMERIC`. Compare stays generic `OP_LT` when the limit is an untyped parameter. Number literals type as `sltFloat`.
- `nbody-minimal`: original baseline was 31% `OP_GET_LOCAL`, 12% `OP_GET_PROP_CONST`, 10% `OP_LOAD_HOLE`, 8% `OP_MOVE`. Hot pair `GET_LOCAL → GET_PROP_CONST` (11%) now fuses as `OP_GET_LOCAL_PROP_CONST` (`db9567bd`). Generic `OP_MUL`/`OP_ADD` still 100% scalar hit rate.
- Script-level `let` in a non-function profiled as `OP_GET_GLOBAL` (29% of opcodes) — not the AWFY/probe shape.

## CI AWFY worst rows (linux/x64, time ratio)

Json 24.96, Permute 21.86, Sieve 21.63, CD 20.41, Bounce 19.94, Havlak 19.09, Towers 18.91, Richards 16.21. Mandelbrot 5.58 (best).

## Accepted this wave

- **`optimize/add-num-imm`** `ad0023da`, merged at `d099bdf9`. `OP_ADD_NUM_IMM` fuses proven `Number + Int16` (including `1 + i`) the same way `OP_SUB_NUM_IMM` does. Checksums matched. Isolated report: `/tmp/lane-add-num-imm-ab.json`. Combined re-measure vs `/tmp/goccia-baseline-f4d403f0` (7 interleaved reps, `/tmp/combined-add-num-imm-ab.json`):

  | Probe | Base µs | Combined µs | Ratio |
  | --- | ---: | ---: | ---: |
  | loop-dispatch-floor | 126029 | 124766 | 0.990 |
  | generic-plus-scalars | 63284 | 59013 | 0.933 |
  | nbody-minimal | 67202 | 65455 | 0.974 |
  | fib-recursive | 45487 | 44952 | 0.988 |

  Geomean combined/base **0.971**. Isolated lane had loop-dispatch-floor 0.967 / generic-plus-scalars 0.970. AWFY `i++` loops are not expected to hit this opcode; transfer is the `i + K` / `1 + i` probes. Treat `d099bdf9` as the next combined baseline for later merges — do not add isolated percentages.

  Overlap with inc-assign is resolved: assignment matching runs first, so `i = i + 1` emits `OP_INC_NUMERIC` rather than add-immediate plus store. `OP_ADD_NUM_IMM` still covers `i + K` for K ≠ 1 and non-assignment uses.

- **`optimize/inc-assign`** `315bfd28`, merged at `1c2e0412`. Proven-numeric `id = id + 1` / `id = 1 + id` emits `OP_INC_NUMERIC`. Isolated vs original baseline: loop-dispatch-floor 0.930. Combined re-measure vs `/tmp/goccia-combined-d099bdf9` (7 interleaved reps, `/tmp/combined-inc-assign-ab.json`):

  | Probe | Prev µs | Combined µs | Ratio |
  | --- | ---: | ---: | ---: |
  | loop-dispatch-floor | 117529 | 113682 | 0.967 |
  | generic-plus-scalars | 57182 | 56515 | 0.988 |
  | nbody-minimal | 65105 | 65914 | 1.012 |
  | fib-recursive | 44025 | 45288 | 1.029 |

  Geomean combined/prev **0.999** (fib/nbody overlap noise). Target still faster after ADD_NUM_IMM; checksums matched. Treat `1c2e0412` as the next combined baseline.

- **`optimize/write-ic`** `15e8eca7`, merged at `0293229e`. Shape + entry-index write IC for own writable data on `OP_SET_PROP_CONST`; semantic misses still use `AssignProperty`. Isolated vs original baseline: Richards +21.14% speed, Bounce +8.42%, Storage +4.96%. Combined re-measure vs `/tmp/goccia-combined-1c2e0412` (7 interleaved reps, `/tmp/combined-write-ic-ab.json`):

  | Target | Prev | Combined | Time ratio | Speed |
  | --- | ---: | ---: | ---: | ---: |
  | Richards | 396.332 ms | 307.419 ms | 0.776 | +28.9% |
  | Bounce | 8.892 ms | 7.698 ms | 0.866 | +15.5% |
  | Storage | 14.663 ms | 13.882 ms | 0.947 | +5.6% |
  | loop-dispatch-floor | 114.154 ms | 113.167 ms | 0.991 | flat |
  | propaccess-monomorphic | 16.467 ms | 14.238 ms | 0.865 | +15.7% |

  Geomean combined/prev **0.886**. Checksums matched. Do not bundle with read-PIC (ADR 0088). Treat `0293229e` as the next combined baseline.

- **`optimize/counted-for-assign`** `e6f3e177`, merged at `7a31bc96`. `TryCompileCountedFor` now matches `i = i + 1` / `i = i - 1` and `i += 1` / `i -= 1`; literal Number limits can use `OP_GTE_INT`/`OP_LTE_INT`. Isolated vs original baseline: loop-dispatch-floor 0.913. Combined remaining win is smaller because inc-assign already covers standalone `i = i + 1`. Combined re-measure vs `/tmp/goccia-combined-0293229e` (7 interleaved reps, `/tmp/combined-counted-for-ab.json`; repeat `/tmp/combined-counted-for-ab-repeat.json`):

  | Probe | Prev µs | Combined µs | Ratio |
  | --- | ---: | ---: | ---: |
  | loop-dispatch-floor | 113492 | 110868 | 0.977 |
  | generic-plus-scalars | 55023 | 56309 | 1.023 |
  | nbody-minimal | 60997 | 60213 | 0.987 |
  | fib-recursive | 46475 | 46898 | 1.009 |

  Repeat loop-dispatch-floor 111446 → 108555 (**0.974**). Geomean combined/prev **0.999**. Checksums matched. Treat `7a31bc96` as the next combined baseline.

- **`optimize/get-local-prop`** `db9567bd`, merged at `dc9e958e`. `OP_GET_LOCAL_PROP_CONST` (opcode **231**, format **v79**) fuses `local.ident` and jumps into the existing `OP_GET_PROP_CONST` IC. Isolated lane used format v78 with 230 reserved; integrator kept `OP_ADD_NUM_IMM` as 230 and bumped format to v79. Isolated vs original baseline: nbody-minimal +1.75% (BA +2.28%), propaccess-monomorphic +4.2–5.5%. Combined re-measure vs `/tmp/goccia-combined-7a31bc96` (7 interleaved reps, `/tmp/combined-get-local-prop-ab.json`):

  | Probe | Prev µs | Combined µs | Ratio | Speed |
  | --- | ---: | ---: | ---: | ---: |
  | nbody-minimal | 58515 | 57705 | 0.986 | +1.4% |
  | loop-dispatch-floor | 106733 | 107265 | 1.005 | flat |
  | propaccess-monomorphic | 13500 | 12731 | 0.943 | +6.0% |
  | generic-plus-scalars | 54287 | 53303 | 0.982 | +1.9% |

  Geomean combined/prev **0.979**. Checksums matched. Treat `dc9e958e` as the next combined baseline.

  Rejected along the way: extracting the property-read IC into a nested helper (nbody +15%, propaccess +35%). Do not retry a helper call on the IC hit path.

## Wave 2 lanes launching

Branch from delivery head `9e0dfde8`. A/B vs `/tmp/goccia-combined-dc9e958e`. Serialize perf on `/tmp/gocciascript-perf-gate.lock`. Do not edit this file. Next opcode **232** / format **v80** is reserved for `optimize/w2-jump-if-not-lt` if it adds an opcode.

1. `optimize/w2-int-literals` — integer-valued number literals as `sltInteger` / TypeHints; target `OP_LT_INT` and non-counted-for integer arith. No new opcode. Retry of stalled `optimize/int-literals`.
2. `optimize/w2-prod-dispatch` — prod vs instrumented **dual loop** in `ExecuteRegisters`; keep **one** `case`; strip coverage/profiler/`AStopAtIP` (and hoist `PollInstructionLimit` when inactive) on the measured path. Do **not** split hot/cold opcodes.
3. `optimize/w2-jump-if-not-lt` — fuse loop/if `OP_LT` + `OP_JUMP_IF_FALSE` (numeric imm form mirroring `OP_JUMP_IF_NUM_NOT_LTE_IMM` where proven). Opcode **232**, format **v80**.
4. `optimize/w2-bit-eq-jump` — fuse `(x & K) === 0` style tests in `loop-dispatch-floor` / Richards (`OP_BAND` + `OP_EQ` + jump). No collision with 232: use **233** / format **v81** only if a new opcode is required; prefer existing ops if a compiler peephole suffices.

## Lanes launching (wave 1, closed)

1. `optimize/inc-assign` — **accepted** (see above).
2. `optimize/int-literals` — **stalled** (see below). No commit, no A/B.
3. `optimize/add-num-imm` — **accepted** (see above).
4. `optimize/hot-dispatch-extract` — **rejected** (see below).
5. `optimize/get-local-prop` — **accepted** (see above).
6. `optimize/write-ic` — **accepted** (see above).
7. `optimize/counted-for-assign` — **accepted** (see above).

## Investigation conclusions (do not contradict)

- **NaN-box / tagged-pointer rewrite:** out of this wave (`TGocciaRegister` is a 16-byte fat union by design).
- **Numeric loops already unboxed** on the generic scalar arm; remaining tax is dispatch + property-boundary boxing on store.
- **Broader read-PIC:** still rejected (ADR 0088). Own+proto read ICs already ship.
- **Write-IC:** landed this wave (`15e8eca7`). Own writable-data stores on `OP_SET_PROP_CONST` hit a shape-keyed IC; misses still go through `AssignProperty`. Broader read-PIC remains rejected (ADR 0088).
- **`OP_SET_PROP_CONST`** uses the write IC for ordinary own writable data; `VMTrySetOwnWritableDataProperty` remains available for non-IC paths.
- **Counted-for** now matches `i = i + 1` / `i += 1` and minus (`e6f3e177`), emitting `OP_ADD_INT` in the loop template. Untyped parameter limits still use generic `OP_LT`. Standalone assignment `i = i + 1` remains `OP_INC_NUMERIC`. Integer-valued number literals still type as `sltFloat` (`ExpressionType`); that remaining lane stalled before a patch.
- **CALL:** bytecode→bytecode already trampolines; `ExecuteClosureRegisters0–3` are native ingress only. Revisit `OP_CALL_METHOD` staging only with AWFY transfer (ADR 0089 previously noise).
- **Dispatch preamble:** ~10–15 predictable cold branches per opcode. A remaining DISPATCH idea is a **prod vs instrumented dual loop** that keeps one `case` and only strips coverage/profiler/`AStopAtIP` on the measured path — not a hot/cold case split.
- **ALLOC:** do not revive value caches. Property-store `RegisterToValue` boxing is the live allocation tax.

## Stalled this wave

- **`optimize/int-literals`**. Subagent stopped after resume loops with no patch, no commit, and no A/B. Branch is still at baseline `56eb8849`. Hypothesis remains open: `ExpressionType` types every number literal as `sltFloat`, which can keep generic `OP_LT` / `OP_ADD_FLOAT` on integer-valued literals. Counted-for already emits `OP_ADD_INT` for `i = i + 1` updates, so a retry should target integer TypeHints and `OP_LT_INT` (especially literal limits and non-counted-for arithmetic), not re-measure the already-landed increment path. Not a reject — never measured.

## Rejected this wave

- **`optimize/hot-dispatch-extract`** (first-level hot `case` + `ExecuteColdOpcode` nested helper). Checksums matched; fully reverted; no commit. Medians vs `/tmp/goccia-baseline-f4d403f0`, 7 interleaved reps (`/tmp/lane-hot-dispatch-ab.json`):

  | Probe | Base µs | Cand µs | Ratio |
  | --- | ---: | ---: | ---: |
  | loop-dispatch-floor | 121819 | 123053 | 1.010 |
  | generic-plus-scalars | 59864 | 455680 | 7.612 |
  | nbody-minimal | 64089 | 233806 | 3.648 |
  | fib-recursive | 44227 | 192440 | 4.351 |
  | fixed-arg-call | 34331 | 35860 | 1.045 |

  Do not retry a sparse hot `case` plus nested cold helper. High-numbered opcodes (`OP_ADD`, `OP_MUL`, `OP_SUB_NUM_IMM`, `OP_LOAD_HOLE`) became much more expensive while the all-hot floor stayed flat.

## Rejected (do not retry)

- Broader read-side PIC (ADR 0088)
- Value caches (ADR 0081)
- String interning on `RuntimeCopy`
- July 2026 pooled-collection call bypass (ADR 0089)
