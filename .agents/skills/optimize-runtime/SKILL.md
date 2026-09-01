---
name: optimize-runtime
description: >-
  Runs benchmark-gated GocciaScript runtime optimization waves from a verified
  baseline through profiling, isolated implementation lanes, serialized A/B
  measurement, combined re-measurement, and interpreter/bytecode correctness
  gates. Use when asked to optimize the bytecode VM, interpreter, startup,
  calls, numeric, property access, allocation, or GC; close a QuickJS gap;
  run another optimization wave; or retain only changes with a measured
  positive impact.
---

# Optimize runtime

Improve execution speed without trading away conformance. Treat every change
as a candidate until repeatable measurement and mode-identical correctness
accept it.

Adapted from Wasmlight's `optimize-runtime` playbook. GocciaScript has no
JIT/AOT tiers: the two execution modes are the tree-walk interpreter and the
bytecode VM, sharing the same runtime objects and GC.

## Establish the experiment

1. Read `.agent/HANDOFF.md` when it exists, `VISION.md`, `CONTEXT.md`, the
   hot-path policy in `docs/contributing/code-style.md` and
   `docs/core-patterns.md`, `docs/testing.md`, `docs/benchmarks.md`,
   `docs/profiling.md`, `docs/bytecode-vm.md`, and the ADRs governing the
   affected seam (especially 0005, 0014, 0065, 0076, 0081, 0087, 0088, 0091).
2. Apply `git-workflow`. Require a clean tree, fetch the remote default, and
   start from its exact tip on a focused branch. Never benchmark an unexplained
   dirty or mixed-revision tree.
3. State the target workload, affected execution mode (bytecode unless the
   user names the interpreter), expected invariant, guard workloads, platforms,
   and non-goals before editing.
4. Use `--prod` builds for performance measurement. Use development builds as
   an additional checked-build correctness gate, never as performance evidence.
5. Make every workload verify its result independently. Do not use the
   interpreter as the bytecode VM's only oracle: run both modes against the
   JavaScript suite, and require AWFY/probe workloads to check their own
   computed result.

Default wave target unless the user overrides it: close the bytecode-vs-QuickJS
gap until GocciaScript reaches **0.6×–0.8× of QuickJS speed** on the chosen
barometer (throughput or score). On AWFY that is `Goccia time / QuickJS time`
between about 1.25× and 1.67×; on JetStream invert the score ratio the same
way. Record the exact convention used in the wave handoff. Do not treat
beating V8/SpiderMonkey as in-scope (`VISION.md`).

## Capture the baseline

1. Build and retain a baseline binary from the exact starting commit with the
   same compiler, dependencies, flags, and host used for candidates:

   ```sh
   ./build.pas --prod loader benchmarkrunner
   cp build/GocciaScriptLoader /tmp/goccia-baseline-$(git rev-parse --short HEAD)
   ```

2. Stop competing benchmark processes. Serialize measurements through one
   shared exclusive lock, conventionally `/tmp/gocciascript-perf-gate.lock`.
3. Discard at least one warm-up. Default to seven measured samples and report
   the median plus the sample spread. Use fewer only for an expensive workload
   and record why. AWFY/JetStream drivers default to fewer repetitions; raise
   `--repetitions` for accept/reject decisions.
4. Measure the target and representative guards. Prefer:
   - focused `perf/probes/` diagnostics for the suspected mechanism;
   - object/call/numeric AWFY rows (`Richards`, `DeltaBlue`, `Bounce`,
     `Storage`, `Mandelbrot`, `NBody`, `Sieve`, `Json`) as transfer guards;
   - a JetStream workload when the change is likely to show there;
   - `GocciaBenchmarkRunner` files only as supporting signal, never as the
     sole merge criterion for a VM change.
   Keep iteration counts large enough to escape timer quantization.
5. Record the exact commit, command, OS, architecture, execution mode,
   workload size, warm-up count, sample count, order, median, spread, QuickJS
   version, and verified result.
6. When comparing QuickJS, run an identical portable bundle and entry point
   through `scripts/awfy-driver.js` / `scripts/jetstream-driver.js`, exclude
   compilation from both sides where the driver already does so, verify
   observable results, and run on the same host. Pin QuickJS to the version in
   `.github/scripts/install-quickjs.sh`. Label emulated or virtualized Linux
   results explicitly rather than presenting them as native hardware.

Use interleaved `--goccia-baseline` / `--goccia-candidate` for Goccia-vs-Goccia
A/B. Do not compare sequential batches on a noisy laptop as accept evidence.

## Find the bottleneck

1. Profile a long-running version of the target workload. Combine:
   - language-level VM profiles (`--profile=opcodes|functions|all`, see
     `docs/profiling.md`);
   - host samples of the `--prod` binary (`sample` on macOS, `perf` on Linux)
     of the interpreter dispatch path.
2. Trace the dominant samples to source and state the suspected cost in
   mechanism terms: dispatch, register-file traffic, helper crossings, frame
   publication, boxing, property resolution, call/return, allocation, GC, or
   another observed cause.
3. Form bounded candidate lanes only after the baseline and profile exist.
   Prefer independent lanes with disjoint ownership and one primary hypothesis
   each.
4. Preserve architectural invariants in every lane: evaluation stays pure;
   `TGocciaScope` is created only through `CreateChild`; bytecode and
   interpreter remain observationally identical; tagged `TGocciaRegister`
   scalars stay unboxed until a runtime boundary; GC roots stay complete;
   capability/sandbox defaults stay closed; ECMAScript semantics for proxies,
   accessors, deletion, and prototype mutation stay correct.

Rejected complexity that must not be revived without new transfer evidence:

- broader read-side property inline caches (ADR 0088);
- value caches whose only win is allocation reduction on probes (ADR 0081);
- string interning on the universal `RuntimeCopy` path (`docs/core-patterns.md`).

## Run isolated candidate lanes

Use a bounded subagent fan-out when independent lanes can run concurrently.
Give every lane an isolated worktree and branch at the same exact baseline.
When subagents are unavailable, run the same lanes sequentially in isolated
worktrees.

Require every lane to:

- own a concrete bottleneck and a bounded set of files;
- capture its own serialized baseline before changing code;
- keep register, GC-root, call-frame, sandbox, and mode-parity invariants
  explicit;
- measure the target immediately before and after the candidate under the
  shared lock;
- run guard workloads and focused correctness tests;
- reject and fully revert experiments that regress, overlap noise, fail result
  verification, or weaken an invariant;
- commit only an accepted candidate and return its exact hash, measurements,
  guard results, correctness evidence, and rejected experiments;
- avoid editing `.agent/HANDOFF.md`; the integration owner records the wave.

Do not let multiple lanes benchmark concurrently. Parallelize investigation,
implementation, builds, and correctness tests; serialize performance runs.

Probe-only wins are diagnostics, not merge criteria. A candidate must transfer
to at least one representative AWFY (or JetStream) guard before integration.

## Accept or reject a candidate

1. Run an immediate same-load A/B comparison using retained baseline and
   candidate binaries. Confirm in reverse order or an ABBA sequence.
2. Accept only a repeatable positive target delta that exceeds observed noise
   and timer resolution. A single favorable sample or a one-millisecond shift
   at one-millisecond resolution is not evidence.
3. Reject a target win if a representative guard materially regresses unless
   the user explicitly accepts that trade-off after seeing both measurements.
4. Require identical verified results and relevant focused tests before
   integration. Never turn benchmark numbers into test assertions.
5. Keep rejected work out of the accepted commit. Record why it lost so a later
   wave does not unknowingly repeat it.

## Re-measure combined integration

1. Begin from the current accepted integration head, not the original baseline.
2. Merge one accepted lane at a time into a disposable integration branch or
   worktree. Never rebase or force-push.
3. Rebuild and compare the combined candidate against the immediately previous
   accepted head under the same serialized protocol.
4. Advance the delivery branch only when the combined state remains positive
   and its guards remain flat. Leave a lane unintegrated when interaction with
   earlier work erases its benefit or creates a regression.
5. After each accepted merge, treat that result as the next baseline. Do not
   add isolated percentages to predict the combined outcome.

## Prove correctness and report

Run the smallest focused checks first, then the repository gates on the final
combined diff:

```sh
./format.pas --check
./build.pas testrunner
./build/GocciaTestRunner tests
./build/GocciaTestRunner tests --mode=bytecode
./build.pas --prod loader
```

For VM, compiler, register, or GC changes also run the relevant native Pascal
tests as described in `docs/testing.md`. Use a clean build
(`./build.pas --clean <target>`) after a merge or unexplained FPC error.

Interpreter and bytecode suite results must match on the public JavaScript
tests: same pass/fail set, no new crashes. Do not weaken sandbox defaults or
capability policy to buy speed.

Update `.agent/HANDOFF.md` with:

- the exact before/after medians and method;
- accepted commits and their invariants;
- rejected experiments and measured reason;
- guard workloads and correctness gates;
- cross-architecture results and virtualization caveats;
- the remaining QuickJS gap and next profiled bottlenecks.

Use `create-pr` when delivery is requested. Keep its PR draft until the
Definition of Ready is satisfied and exact-head CI is green, then mark it ready.

## Stop conditions

Stop and report rather than integrate when the baseline is unstable, the target
does not verify its result, the candidate's improvement is not repeatable, a
guard regresses materially, interpreter and bytecode diverge, a
cross-architecture gate fails, or the change depends on an unresolved register,
GC-root, call-frame, sandbox, or mode-parity assumption.
