# Handoff

Updated: 2026-08-25 (write-IC accepted on top of inc-assign)

## Experiment

- **Goal:** Goccia bytecode at 0.6×–0.8× of QuickJS *speed* (AWFY `goccia_over_qjs` ≈ 1.25–1.67).
- **Current main CI** (`f4d403f0`, linux/x64 Azure): AWFY geomean `goccia_over_qjs` = **16.256** (speed **0.062×**). Need ~10–13×.
- **Delivery branch:** `perf/bytecode-quickjs-gap` @ `0293229e` (skill adoption on `origin/main` `f4d403f0`).
- **Accepted code:** `ad0023da` `OP_ADD_NUM_IMM` (opcode **230**, format **v78**); `315bfd28` numeric `i = i + 1` → `OP_INC_NUMERIC`; `15e8eca7` own writable write IC. Next free opcode **231**; `optimize/get-local-prop` must take 231 and bump format to v79 if it lands.
- **Combined candidate binary:** `/tmp/goccia-combined-0293229e`. Previous combined heads: `/tmp/goccia-combined-1c2e0412` (inc-assign), `/tmp/goccia-combined-d099bdf9` (ADD_NUM_IMM).
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

- `loop-dispatch-floor`: 38% `OP_GET_LOCAL`, 15% `OP_LOAD_INT`, 11% `OP_SET_LOCAL`, 8% `OP_ADD_FLOAT` on the original baseline. Increment `i = i + 1` now compiles as `OP_INC_NUMERIC` (`315bfd28`); compare is still generic `OP_LT`. Number literals type as `sltFloat` (`ExpressionType` in `Goccia.Compiler.Statements.pas`).
- `nbody-minimal`: 31% `OP_GET_LOCAL`, 12% `OP_GET_PROP_CONST`, 10% `OP_LOAD_HOLE`, 8% `OP_MOVE`. Hot pair `GET_LOCAL → GET_PROP_CONST` (11%). Generic `OP_MUL`/`OP_ADD` with 100% scalar hit rate.
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

## Lanes launching

1. `optimize/inc-assign` — **accepted** (see above).
2. `optimize/int-literals` — integer-valued number literals as `sltInteger`.
3. `optimize/add-num-imm` — **accepted** (see above).
4. `optimize/hot-dispatch-extract` — **rejected** (see below).
5. `optimize/get-local-prop` — fuse `GET_LOCAL` + `GET_PROP_CONST` (must use opcode **231** / format **v79** if it lands after this merge).
6. `optimize/write-ic` — **accepted** (see above).
7. `optimize/counted-for-assign` — widen `TryCompileCountedFor` to `i = i + 1`.

## Investigation conclusions (do not contradict)

- **NaN-box / tagged-pointer rewrite:** out of this wave (`TGocciaRegister` is a 16-byte fat union by design).
- **Numeric loops already unboxed** on the generic scalar arm; remaining tax is dispatch + property-boundary boxing on store.
- **Broader read-PIC:** still rejected (ADR 0088). Own+proto read ICs already ship.
- **Write-IC:** landed this wave (`15e8eca7`). Own writable-data stores on `OP_SET_PROP_CONST` hit a shape-keyed IC; misses still go through `AssignProperty`. Broader read-PIC remains rejected (ADR 0088).
- **`OP_SET_PROP_CONST`** uses the write IC for ordinary own writable data; `VMTrySetOwnWritableDataProperty` remains available for non-IC paths.
- **Counted-for** only matches `i++`, so AWFY/probe `i = i + 1` still misses `OP_ADD_INT` loop microcode. Assignment `i = i + 1` now emits `OP_INC_NUMERIC` outside that counted-for path.
- **CALL:** bytecode→bytecode already trampolines; `ExecuteClosureRegisters0–3` are native ingress only. Revisit `OP_CALL_METHOD` staging only with AWFY transfer (ADR 0089 previously noise).
- **Dispatch preamble:** ~10–15 predictable cold branches per opcode. A remaining DISPATCH idea is a **prod vs instrumented dual loop** that keeps one `case` and only strips coverage/profiler/`AStopAtIP` on the measured path — not a hot/cold case split.
- **ALLOC:** do not revive value caches. Property-store `RegisterToValue` boxing is the live allocation tax.

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
