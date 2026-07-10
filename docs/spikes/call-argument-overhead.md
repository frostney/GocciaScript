# Call Argument Overhead Investigation

Point-in-time investigation of fixed-argument bytecode, method, and native calls

## Executive Summary

- The July 2026 combined call-overhead patch was rejected after its apparent
  gains did not survive source-path attribution and isolated measurement.
- Ordinary bytecode calls with up to three arguments already bypass argument
  staging; ordinary bytecode method calls do not.
- The proposed native wrappers bypassed the VM's argument pool and allocated a
  fresh fixed-argument collection object for each qualifying call.
- The isolated method-call change was neutral: a 30-repetition rerun measured
  `0.983x` by independent medians and `0.998x` by paired median.
- Future work should optimize a common call seam only after profiling identifies
  argument staging as material and a path-valid probe transfers to real rows.

## Question

The investigation started from large cross-engine gaps in
`fixed-arg-call` and `method-call-fixed-arg`, plus call and object pressure in
`sort-comparator`, `Richards`, and `Havlak`. It asked whether GocciaScript could
reduce call overhead by:

1. removing the temporary register array used by small bytecode method calls;
2. avoiding argument collection and backing-list work for small native calls;
3. reusing fixed-argument entry points in `call`, `apply`, and bound calls where
   necessary.

The semantic constraints were unchanged `this` binding, strict versus
non-strict `this` coercion, spread calls, bound functions, constructors,
async/generator functions, direct eval, and exception behavior. Dispatch-loop,
arithmetic, property-cache, and string/RegExp work were outside this lane.

## Baseline Call Paths

Current `main` does not have one uniform cost for calls:

| Surface | Current behavior | Performance implication |
| --- | --- | --- |
| Ordinary bytecode call, 0-3 arguments | Captures arguments in `FixedArg0..2` and passes them directly to `SetupNewFrame` | Already avoids `RegisterArgs` staging |
| Ordinary bytecode method call | Copies arguments into a dynamic `RegisterArgs` array before `SetupNewFrame` | Has a local staging opportunity |
| Generic VM call | Uses `AcquireArguments` and `ReleaseArguments` | Collections are retained in a pool of up to 32 objects |
| Native callback | Receives `TGocciaArgumentsCollection` | The callback interface is generic even when the caller knows the arity |
| Host-to-JavaScript callback | Usually enters through `InvokeCallable` | Builds temporary GC-root storage and calls the generic function interface |
| Array sort comparator | Reuses a two-element argument collection across comparisons | Comparator pressure is not per-comparison collection allocation |

Source anchors for the next pass:

- `source/units/Goccia.VM.pas` owns the argument pool and the `OP_CALL` /
  `OP_CALL_METHOD` bytecode paths.
- `source/units/Goccia.Arguments.Collection.pas` owns the mutable collection and
  backing list.
- `source/units/Goccia.Values.FunctionBase.pas` owns generic and fixed-arity
  function entry points plus `call`, `apply`, and bound-function routing.
- `source/units/Goccia.Values.NativeFunctionCallback.pas` defines the native
  callback interface.
- `source/units/Goccia.Utils.pas` owns generic host-to-JavaScript invocation and
  argument rooting.
- `source/units/Goccia.Values.ArrayValue.pas` demonstrates reused comparator
  arguments in a real callback-heavy path.

The pool is the key constraint. After warm-up, a generic VM call can clear and
reuse an existing `TGocciaArgumentsCollection` and its backing list. A competing
fixed-argument path must preserve or beat that reuse; merely replacing the list
with a freshly allocated adapter is not an allocation elimination.

## Combined Experiment

The first candidate, commit `0c33d6e9` on closed
[PR #948](https://github.com/frostney/GocciaScript/pull/948), changed seven files
with 335 insertions and 32 deletions. It combined two independent ideas:

- `OP_CALL_METHOD` used the ordinary-call fixed-register frame setup for
  non-spread bytecode methods with up to three arguments.
- `TGocciaFixedArgumentsCollection` stored zero to three values in fields and
  lazily materialized `TGocciaValueList`; `TGocciaNativeFunctionValue` created
  one of these collections in new fixed-arity call overrides.

The VM also routed small non-spread native calls and native method calls through
those overrides. Compatibility behavior remained behind the existing generic
collection interface, and focused tests covered method receivers and native
`call`/`apply` behavior.

The native half was structurally weak. `TGocciaFixedArgumentsCollection` still
required one object allocation and free per call, while the displaced VM path
used the argument pool. The callback interface also remained
`TGocciaNativeFunctionCallback(AArgs, AThisValue)`, so the candidate did not
establish a true fixed-arity native ABI.

## Initial Measurements

The first run used production bytecode loaders, interleaved baseline/candidate
samples, FPC 3.2.2, and Darwin arm64. The baseline was `0a3e748f`; probes used
five repetitions and selected AWFY rows used three.

Speedup is baseline median divided by candidate median, so values above `1.0x`
favor the candidate.

### Probes

| Probe | Baseline median | Candidate median | Speedup | Baseline CV | Candidate CV |
| --- | ---: | ---: | ---: | ---: | ---: |
| `fixed-arg-call` | 264.270 ms | 226.534 ms | `1.167x` | 12.61% | 2.97% |
| `method-call-fixed-arg` | 260.480 ms | 252.820 ms | `1.030x` | 5.57% | 4.25% |
| `bound-spread-apply` | 535.425 ms | 571.002 ms | `0.938x` | 3.37% | 6.28% |
| `sort-comparator` | 2094.662 ms | 2033.506 ms | `1.030x` | 12.70% | 8.39% |
| `typed-array-call-return` | 53.183 ms | 53.623 ms | `0.992x` | 5.73% | 12.41% |

The probe geomean was `1.029x`.

### Selected AWFY Rows

| Row | Baseline median | Candidate median | Speedup | Baseline CV | Candidate CV |
| --- | ---: | ---: | ---: | ---: | ---: |
| `Richards` | 1316.027 ms | 1336.679 ms | `0.985x` | 23.08% | 7.17% |
| `Havlak` | 105334.720 ms | 91397.249 ms | `1.152x` | 7.83% | 5.82% |
| `Bounce` | 22.372 ms | 22.146 ms | `1.010x` | 6.31% | 12.96% |
| `Towers` | 64.601 ms | 64.853 ms | `0.996x` | 7.26% | 3.74% |
| `Json` | 333.982 ms | 315.484 ms | `1.059x` | 1.30% | 1.75% |

The selected-row geomean was `1.039x`. This looked encouraging, but the result
could not be assigned to either half of the patch and several rows had high
variance.

## Attribution Failure

Source inspection showed that the strongest reported result did not exercise a
changed hot path:

- `fixed-arg-call` repeatedly invokes a JavaScript `add3` function. The ordinary
  bytecode-call fixed-register path already existed in the baseline, so its
  `1.167x` result was not direct evidence for this candidate.
- `typed-array-call-return` constructs a typed array once, then repeatedly calls
  two JavaScript functions. It was not a native fixed-call probe.
- `sort-comparator` calls the native `sort` method once per outer iteration, but
  its dominant comparator loop reuses one argument collection and enters through
  `InvokeCallable`. The candidate did not specialize that hot callback path.
- `bound-spread-apply` mixed three call forms and regressed, so it could not
  identify which form changed the result.

The combined run therefore established neither a native ABI win nor a reliable
ordinary-call win. It justified an isolated method-call experiment, not the
combined implementation.

## Isolated Method-Call Experiment

The branch was updated through `c3a8cdc5`, which included the separate dispatch
loop optimization from PR #949. The native collection and wrapper changes were
removed. The candidate retained only the `OP_CALL_METHOD` fixed-register change
and a receiver/arity test; the baseline was current `main`.

Both production loaders passed checksum verification. The focused method test
passed in interpreted and bytecode modes. Measurements again interleaved
baseline and candidate samples.

The reproducible command shape was:

```bash
node scripts/awfy-driver.js \
  --goccia-baseline <baseline-loader> \
  --goccia-candidate <candidate-loader> \
  --engines goccia-baseline,goccia-candidate \
  --probe fixed-arg-call:300000 \
  --probe method-call-fixed-arg:300000 \
  --repetitions 15 \
  --timeout-ms 120000 \
  --output <results.json>
```

The target-only repeat used the same command with
`--probe method-call-fixed-arg:300000 --repetitions 30`.

| Run | Target | Repetitions | Baseline median | Candidate median | Speedup | Paired median | Baseline CV | Candidate CV |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Control | `fixed-arg-call` | 15 | 285.195 ms | 286.715 ms | `0.995x` | `1.002x` | 6.86% | 5.37% |
| First target | `method-call-fixed-arg` | 15 | 296.713 ms | 291.055 ms | `1.019x` | `1.013x` | 28.52% | 3.67% |
| Target repeat | `method-call-fixed-arg` | 30 | 332.356 ms | 338.241 ms | `0.983x` | `0.998x` | 8.16% | 6.35% |

The first target run contained a large baseline outlier. The larger repeat did
not reproduce its independent-median gain, while the paired median was neutral.
The local method-call change did not meet the acceptance bar and was reverted in
`b6feb267`.

## Conclusions

This experiment rejects a design, not the call-overhead problem:

- Do not add a second argument-collection implementation that bypasses the VM
  argument pool while preserving the same generic callback interface.
- Do not infer a native-call improvement from probes whose timed loop only calls
  JavaScript functions.
- Do not merge the isolated `OP_CALL_METHOD` change on the July 2026 evidence;
  its measured effect was within noise after repetition.
- Do not use a combined geomean as acceptance evidence when independent changes
  cannot be attributed to path-valid probes.

The current pooled collection interface remains the baseline. That is a
deferral, not a claim that it is optimal.

## Future Design Options

Future performance passes should begin with retained function, opcode, and
allocation profiles from the affected real rows. If argument movement remains a
material cost, the viable designs are:

1. **Inline small storage in the pooled collection.** Store zero to three values
   inside `TGocciaArgumentsCollection`, materializing a list only for larger or
   list-style use. This keeps one implementation and preserves pool reuse.
2. **A true fixed-arity native ABI.** Add arity-specific callback types with a
   generic fallback, then migrate only profiled hot builtins. This is broader
   interface work and must prove enough transfer to pay for that surface.
3. **Fixed-arity host callback entry points.** Specialize the common
   host-to-JavaScript seam, including GC rooting, for known small arities. This is
   the path relevant to sort and array callback pressure; opcode-only native
   ingress does not cover it.
4. **A local method-call frame change.** Revisit the small `OP_CALL_METHOD` hunk
   independently if a stable target probe and method-heavy real rows improve on
   the same current-main baseline.

## Revisit Checklist

Before reopening this lane:

- profile the current production bytecode build and name the exact dominant
  allocation, copy, root, or dispatch cost;
- verify from source that every target probe executes the proposed hot path in
  its timed loop;
- include an unaffected negative control to expose machine drift or code-layout
  effects;
- measure method staging, native ingress, host callbacks, and `call`/`apply`/
  bound behavior as separate candidates;
- retain raw interleaved samples with baseline/candidate commits and environment
  metadata, following [ADR 0087](../adr/0087-awfy-cross-engine-benchmark-methodology.md);
- repeat noisy probes until the paired result is stable, then require transfer
  to the profiled AWFY rows before accepting additional interface complexity;
- preserve tests for `this`, strictness, spread, bound functions, constructors,
  async/generator calls, direct eval, and exceptions whenever their path changes.

The architectural decision derived from this snapshot is recorded in
[ADR 0089](../adr/0089-defer-fixed-arity-call-specialization.md).
