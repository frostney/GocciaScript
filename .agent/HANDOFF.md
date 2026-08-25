# Handoff

Updated: 2026-08-25 (investigation folded; write-IC + counted-for lanes added)

## Experiment

- **Goal:** Goccia bytecode at 0.6×–0.8× of QuickJS *speed* (AWFY `goccia_over_qjs` ≈ 1.25–1.67).
- **Current main CI** (`f4d403f0`, linux/x64 Azure): AWFY geomean `goccia_over_qjs` = **16.256** (speed **0.062×**). Need ~10–13×.
- **Delivery branch:** `perf/bytecode-quickjs-gap` @ `3a2e2949` (skill adoption on `origin/main` `f4d403f0`).
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

- `loop-dispatch-floor`: 38% `OP_GET_LOCAL`, 15% `OP_LOAD_INT`, 11% `OP_SET_LOCAL`, 8% `OP_ADD_FLOAT`. Loop compare is generic `OP_LT`. Increment is `i = i + 1` (not `++`), so existing `OP_INC` is unused. Number literals type as `sltFloat` (`ExpressionType` in `Goccia.Compiler.Statements.pas`).
- `nbody-minimal`: 31% `OP_GET_LOCAL`, 12% `OP_GET_PROP_CONST`, 10% `OP_LOAD_HOLE`, 8% `OP_MOVE`. Hot pair `GET_LOCAL → GET_PROP_CONST` (11%). Generic `OP_MUL`/`OP_ADD` with 100% scalar hit rate.
- Script-level `let` in a non-function profiled as `OP_GET_GLOBAL` (29% of opcodes) — not the AWFY/probe shape.

## CI AWFY worst rows (linux/x64, time ratio)

Json 24.96, Permute 21.86, Sieve 21.63, CD 20.41, Bounce 19.94, Havlak 19.09, Towers 18.91, Richards 16.21. Mandelbrot 5.58 (best).

## Lanes launching

1. `optimize/inc-assign` — compile `id = id + 1` as existing `OP_INC`.
2. `optimize/int-literals` — integer-valued number literals as `sltInteger`.
3. `optimize/add-num-imm` — `OP_ADD_NUM_IMM` mirroring `OP_SUB_NUM_IMM`.
4. `optimize/hot-dispatch-extract` — shrink register pressure / strip cold preamble in the VM loop.
5. `optimize/get-local-prop` — fuse `GET_LOCAL` + `GET_PROP_CONST`.
6. `optimize/write-ic` — own writable-data write IC (ADR 0088 leftover; requires AWFY transfer).
7. `optimize/counted-for-assign` — widen `TryCompileCountedFor` to `i = i + 1`.

## Investigation conclusions (do not contradict)

- **NaN-box / tagged-pointer rewrite:** out of this wave (`TGocciaRegister` is a 16-byte fat union by design).
- **Numeric loops already unboxed** on the generic scalar arm; remaining tax is dispatch + property-boundary boxing on store.
- **Broader read-PIC:** still rejected (ADR 0088). Own+proto read ICs already ship.
- **Write-IC:** unimplemented; prior isolated 30× AWFY was Richards +8%, Bounce +5%, Storage +3%. Re-measure interleaved; do not bundle with read-PIC.
- **`OP_SET_PROP_CONST`** still calls full `AssignProperty` except literal-object fast path; `VMTrySetOwnWritableDataProperty` exists but is unused there.
- **Counted-for** only matches `i++`, so AWFY/probe `i = i + 1` misses `OP_ADD_INT` loop microcode.
- **CALL:** bytecode→bytecode already trampolines; `ExecuteClosureRegisters0–3` are native ingress only. Revisit `OP_CALL_METHOD` staging only with AWFY transfer (ADR 0089 previously noise).
- **Dispatch preamble:** ~10–15 predictable cold branches per opcode; dual prod/instrumented loop is the DISPATCH bet, not jumptable surgery.
- **ALLOC:** do not revive value caches. Property-store `RegisterToValue` boxing is the live allocation tax.

## Rejected (do not retry)

- Broader read-side PIC (ADR 0088)
- Value caches (ADR 0081)
- String interning on `RuntimeCopy`
- July 2026 pooled-collection call bypass (ADR 0089)
