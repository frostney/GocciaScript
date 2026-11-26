# Engine

This is GocciaScript's engine. It keeps Goccia's lexer, parser and RegExp compiler (`source/units`) and
provides everything after the AST: compiler, register bytecode, VM, object model, GC and built-ins
(`source/engine`). It began as a spike to find out whether an FPC engine built for speed could match or beat
QuickJS while passing test262; it replaced the old engine, whose units are gone, and every application in
`source/app` runs on it.

## Status

test262 runs on every pull request through `.github/workflows/engine.yml` against the pinned suite, and every
test passes (with the corrections in `scripts/test262-patches`, [test262](../test262.md)). The JavaScript suite
under `tests/` passes in full on every platform. A failure in either fails the job.

Largest remaining test262-adjacent gaps outside the pinned run: Intl (Temporal's non-ISO calendars and
locale data), realms (`$262.createRealm`, ShadowRealm), import defer, Atomics on BigInt arrays, immutable
ArrayBuffers. Performance is measured against QuickJS and Node with `scripts/engine/wallclock.sh` on JetStream and
the Web Tooling Benchmark.

## Layout

| Path | Contents |
|---|---|
| `source/engine/` | the engine: `Goccia.Core` (values, cells, shapes, GC), `Goccia.Compiler`, `Goccia.Instructions`, `Goccia.VM`, `Goccia.ObjModel` and the built-in units; `goccia.dpr` is the command-line host |
| `source/engine/prelude/` | built-ins written in JavaScript (RegExp's spec layer, Set methods, iterator helpers, disposal, base64); compiled at start-up, snapshotted into the binary by the build |
| `scripts/engine/` | build, gate, gcstress and wall-clock scripts; `run262.py` is the contract-4 test262 runner (its host object, `$262.js`, is `source/engine/prelude/test262host.js`) |
| `docs/engine/` | this file and [design.md](design.md) |

## Build

The engine builds with FPC 3.3.1 from fpc-toolchain (native TLS threadvars), build 3.3.1+5 (release `3.3.1-b5`) or
later; CI and `scripts/engine/build.sh` use 3.3.1+9 (release `3.3.1-b9`), selected by `fpc-toolchain env 3.3.1+9` or
any command given in `GOCCIA_FPC_ENV`. Builds before 3.3.1+4 hang in the DFA pass on some units and drop the `@tpoff`
relocation of a threadvar field at -O1; 3.3.1+4 drops the `%fs` prefix of a threadvar the peephole optimizer folds
into an instruction at -O1 (patch 0123 in 3.3.1+5); 3.3.1+5 can fail an incremental compile after a used unit was
recompiled, which `build.pas` retries from a clean unit directory (patches 0124 to 0128 in 3.3.1+6); 3.3.1+6 at
-O4 hoists a load of an array or record field out of a loop into a copy it then indexes (patch 0129 in 3.3.1+7):

```sh
./build.pas --prod engine         # build/engine/goccia, with the 3.3.1 toolchain selected
sh scripts/engine/build.sh        # the same, selecting the toolchain itself
build/engine/goccia file.js           # run a script (always a Script; modules run through --test and the test262 host)
```

The `goccia` target turns each JS prelude into a Pascal include under `build/engine/gen`, then builds in two stages:
the first binary compiles the preludes and writes their bytecode, the second embeds it. It always compiles
optimized (`-O4`); `--prod` strips the binary and a dev build keeps line information, in its own unit cache.
On Linux x86-64 it fails when `scripts/engine/tlsscan.py` finds a native-TLS miscompile (a threadvar address with
an absolute relocation), the bugs 3.3.1+4 and 3.3.1+5 fix; it stays as a guard against a toolchain that regresses, and is
skipped when python3 or binutils is missing. The full build (`./build.pas` without targets) includes `goccia` and
every application, each built the same way.

The engine builds for every target GocciaScript ships: Linux and macOS on x86-64 and aarch64, Windows on x86-64 and
i386. Only Linux x86-64 has the JIT (`GOCCIA_JIT` in `Goccia.Checks.inc`): it emits code for the System V calling convention
and reads threadvars `%fs`-relative. The other targets run the interpreter. With `FPC_TARGET_CPU` and
`FPC_TARGET_OS` naming another platform, `build.pas` builds a program for the host first (`build/engine/host`) to
make its prelude snapshot, which holds the same bytes on every platform, then builds the target once with it;
`GOCCIA_FPC_FLAGS` adds the target's library options. aarch64 needs 3.3.1+7: earlier builds form a `tbz` or `tbnz`
past its 32 KiB reach (patches 0131 to 0134). Without `fork` (Windows),
GocciaTest262Runner runs each case on a thread of its own, bounded by the engine's deadline instead of a kill.

### Proper tail calls

A call in tail position (ES2026 §15.10.1 IsInTailPosition: strict code, not a generator or async function, no try
block, `using` declaration or `for-of` loop still to act after it) gets bit 41 of its `opCall` or `opCallV`, set
by `Goccia.Compiler` for a `return`'s value and a concise arrow body, through `?:`, `&&`, `||`, `??` and a comma
expression's last operand (§15.10.2). Once the frames are 1,024 deep, or half a smaller `--max-stack` bound,
such a call of a bytecode function replaces the calling frame (`XTailCall` in `Goccia.VM`): FastLoop hands it to
Execute, and compiled code exits to the interpreter at it. A shallower one stays an ordinary call, so stack traces
keep its frames and compiled code its own calls. A construction's frame, a native, bound or proxy callee, a
generator or async callee, and another realm's function keep their frames
([ADR 0187](../adr/0187-g2-reuses-a-frame-for-a-tail-call-only-past-a-depth-floor.md)).

### Execution limits

The host takes GocciaRunner's limits: `--timeout=` (500ms, 5s, 2m or milliseconds), `--max-instructions=N`,
`--max-stack=N` (nested calls below the entry's frame; 0 or none is the VM's bound of 200,000) and
`--max-memory=`. A spent timeout or
instruction budget aborts the run: every taken backward jump and every call checks it, no `catch` or `finally`
runs and no further job runs, and the host prints `Fatal error: file timed out after Nms` or `Fatal error:
Execution exceeded instruction limit of N` and exits 1. A timer thread interrupts the engine thread by setting
its `Interrupt` and dropping its frame bound to 0, which the existing frame check of each call sees, so calls pay
nothing extra. Under an instruction budget `Execute` counts every instruction itself and skips `FastLoop`; the
count is exact and the same on every run. The JIT stays off while a timeout, an instruction budget or a frame bound
is set: compiled calls check the fixed bound and do not count instructions.

The memory limit is always in force: `--max-memory`, else half of physical memory up to 8 GB (700 MB on a 32-bit
build). Under a limit the nursery is no larger than the room the old space leaves below it, so the collection that
tests the limit runs before the heap can pass it. A full collection that leaves more live than the limit refuses at
the next back edge or call with a catchable `RangeError: Allocation failed: GC heap size exceeds the configured
memory limit` ([ADR 0185](../adr/0185-g2-refuses-memory-at-the-next-safe-point.md)). At a call with at least 1,024
frames on the stack it is `RangeError: Maximum call stack size exceeded` with the stack-memory suggestion. A script
can catch it, drop what it holds and go on. Eight refusals in a row that free nothing end the run as a spent limit
does, with `Fatal error: Live heap of N bytes exceeds the memory limit of M bytes` (exit 1; GocciaRunner's JSON report
names it `MemoryLimitError`). An explicit reservation that does not fit, such as an `ArrayBuffer`'s bytes, is a
`RangeError` at once. Compiled loops poll for both at every back edge (one compare of the thread's `Interrupt`): the
baseline code exits to the interpreter there, and an optimized region leaves at the jump with its frame state
written back. `scripts/engine/limits-test.sh` checks all of this.

A built-in whose loop runs as often as the script says (an array-like's `length` up to 2^53 - 1, a `split` or a
spread) polls the same `Interrupt` each step (`PollInterrupt`), since it takes no back edge or call and may allocate
nothing the memory limit counts: `Array.prototype.join.call({ length: 2 ** 53 - 1 })` ends at the deadline instead
of running on. Strings that `join`, `JSON.stringify`, `padStart`, `padEnd` and `repeat` build stop at V8's
length bound, 536,870,888 code units, with `RangeError: Invalid string length`. The timer thread is joined by
waiting for it to finish before `TThread.WaitFor`, which on the main thread polls in 100 ms steps: without that
every bounded run took at least 100 ms.

BigInt multiplication, division, exponentiation, radix conversion and parsing take time that grows faster than
their operands, so `TBigInteger` asks the engine through `BigIntegerAbortCheck` as it goes: a spent limit stops the
operation, and its caller polls and aborts before the unfinished value is used. A BigInt has at most 2^30 bits
(V8's bound); a product, power or left shift past it is `RangeError: Maximum BigInt size exceeded`, decided from the
operands' sizes before any work. A shift count of any size is exact: past the bound, a right shift leaves 0 or -1
and a left shift of a nonzero value is that RangeError. A power of two raised to a power is a shift.

Whatever the frame bound, a call that native code makes (a getter, a `toString`, a sort comparator, a generator's
`next()`) nests the interpreter on the native stack, so it is refused with a `RangeError` once that stack runs low.

### Script entries with imports

An explicit `--source-type=script` entry keeps GocciaScript's acceptance of import and export declarations
([language.md](../language.md#modules)). The engine takes the entry's import declarations and re-exports out of the
Script and runs them, each at its own line and column, as a module of their own keyed by the entry's path plus
`#script-imports`, so its requests resolve as the entry's would. That module's graph links as one, so a missing
export fails before any module evaluates. It then evaluates, through the event loop when a top-level await
waits on more. Each imported name then becomes a const global lexical binding that shares the cell of the binding
it imports: it reads live, throws in that binding's temporal dead zone, and refuses assignment. An export
declaration declares its binding as it would without `export`. The entry's own file, imported by a module, loads
as a separate module.

### Host environment

GocciaRunner takes `--host-environment=<module>` and the config key `host-environment`, as
[Host Environment](../host-environment.md) describes. The module is evaluated before the entry. Its clocks reach
`Date`, `Temporal.Now` and `performance`, and its `random(streamId)` reaches `Math.random` (stream `0n` for the
first realm, a derived stream for each other realm). The time zone is read once, when the module loads, where
GocciaScript reads it on each use. A named zone gives `Date` its local time from the zone's TZif data
(`Goccia.Temporal.TimeZone`), as Temporal reads it, rather than from libc's `TZ`. `performance.now()` counts from
the time origin, taken as the monotonic time, the epoch time, then the monotonic time again. An error thrown in
the module is located there, without its source.

### Reports, workers and the audit log

An uncaught value is reported from its string data properties only (`name`, `message`, `Symbol.toStringTag`): a
getter or Proxy trap does not run, and an object without them reads as `[object Tag]`, as GocciaRunner reports it.

A JSON report's `memory.gc` comes from the engine's heap: `liveBytes` is the old space and the nursery's used part,
garbage included until a collection frees it; `peakLiveBytes` is the most the heap held at a collection or a
direct old-space allocation; `allocatedDuringRunBytes` counts every byte allocated after the engine was made.
The engine does not count objects, so `startObjectCount`, `endObjectCount` and `collectedObjects` stay 0. One file reports
its memory at the top level. Files run at once report each file's memory and their sum, and files run one after
another report only the sum.

A directory's files run at once when more than one worker is allowed (`--jobs`, else the processors). Each file
prints its console output and its failure, but no banner and no `--print` value, as GocciaRunner's workers do;
GocciaRunner also leaves the failure out.

`--audit-log` opens each engine's events with `capabilities.effective`. Then come the decisions of the Function
constructor, a ShadowRealm, `eval` and a `node_modules` grant, each at the script's call site. An event that cannot
be written ends the run past every handler with `Fatal error: capability audit delivery failed: ...`.

### FFI

With the `ffi` capability (`--allow-ffi`, or a trusted config's `allow-ffi`), the engine installs the `FFI` global that
[FFI Built-ins](../built-ins-ffi.md) describes, through `Goccia.FFI`. The call plans, layouts, callback trampolines and
library guard are GocciaScript's own units; `Goccia.FFI` is their JS face. FFI objects are ordinary objects with
internal slots, so nothing is finalized by the collector: libraries, signatures and callback slots are released
when the engine is torn down, and a closed library is unloaded no earlier than that. An aggregate's fields are its
own accessor properties.

The engine's collector moves young objects, so a buffer's address is taken only after every argument conversion has run.
A call that can enter script through a callback runs a minor collection first, which moves the buffers it passes
into the old space, where nothing moves. `goccia --test` grants the permissions of the test file's `goccia.json`, as
`GocciaTestRunner -P` accepts them.

### fetch

`fetch` is `prelude/fetch.js`'s, over `Goccia.Fetch`: the URL's host is judged by the net capability before the call
returns, as [Fetch Runtime APIs](../built-ins-fetch.md) describes, and each request runs on a worker thread through
the shared HTTP client, which checks the capability again on every hop and against every resolved address. Those
hop decisions are audited when the request settles, at its `fetch()` call. The event loop settles finished requests
before anything else, and while a request is in flight it waits for it before firing timers: the engine's timers do not
keep real time, so an `AbortSignal.timeout` timer would otherwise abort the request as soon as the loop went idle.
The wait wakes at a signal's deadline and at the run's timeout. A timer that is due while a request is in flight
fires after the request settles.

### Coverage

`--coverage` (and `--coverage-format`, `--coverage-output`) report through GocciaScript's tracker and writers
(`Goccia.Coverage`, `Goccia.Coverage.Report`). Under coverage the compiler gives every source line the code runs an
instruction, an `opCovLine` where it would have none (an operand a register already holds, a declaration whose
initializer starts on a later line), and the VM runs every instruction through a hook, in the interpreter only (no
fast loop, no compiled code). The hook counts a line each time its frame enters it, a function when a call starts
it (and registers each closure it creates, so one never called reports 0), and the outcome of each conditional
jump: branch 0 when it jumps, 1 when it falls through. A function is located where its source starts. Each file's
thread keeps its own tracker, merged into the run's when the file ends.

### Read capability

The read capability judges the run's module graph as GocciaScript's loader does ([ADR 0122](../adr/0122-unified-capability-model.md)): a deny wins; a literal import inside the project (the nearest goccia config's directory, or the entry's, outside its
`.goccia` cache) or inside the package a literal bare import resolved in needs no grant; anything else, a computed
`import()` specifier included, needs a read grant. `Goccia.HostAPI` sets the resolver's probe guard, so every candidate
path is judged before the host is asked whether it exists, then judges the file the request resolved to. A refusal
throws `PermissionDenied` naming the specifier as written, with the host path and the grant in the host's
suggestion, and both kinds of decision are audited. The host's own loads before the run (the command line's
manifests, globals and host environment) are not judged. What a config names is read as the script's: judged
against the directory of the config that names it, its JavaScript manifests and globals modules evaluated in a realm
of their own, and a globals module may export data only.

### Provider imports and install mode

A `github:` import-map entry resolves through the shared resolver's package store, as on the old engine
([ADR 0122](../adr/0122-unified-capability-model.md)). The runner grants it when an import scope covers the
repository (`github`, `github:<owner>` or `github:<owner>/<repo>`); a refusal throws `PermissionDenied` naming the
package, with the grants that would cover it or the deny that refused it. A package file reached through its
import-map entry, or imported by another file of the same package, is part of the module graph despite living in
`.goccia`. Each one is checked against its pin as it loads (`Goccia.Modules` `SetModuleContentCheck`), so the bytes
compiled are the bytes hashed. Sandbox mode refuses every provider package. Install mode (`--add`, `--remove`,
`--install`, `--update`) runs the old runner's installer (`Goccia.Packages.Install`) unchanged.

### Profiling

`--profile` reports through GocciaScript's profilers and writers (`Goccia.Profiler`, `Goccia.Profiler.Compile`,
`Goccia.Profiler.Report`). Profiled files run one at a time; each file's thread records into the main thread's
profilers (`Adopt`). Under the runtime profile (`Goccia.Profile`) every instruction runs through the interpreter's
counted loop, as under coverage: it counts each opcode (the engine's own names), each pair of consecutive ones, and for a
binary arithmetic or comparison instruction whether both operands are Numbers (the scalar fast path). A function
is timed from its frame's push to the push that reuses its slot or the next instruction in a frame below it, so an
unwinding throw needs no hook. A resumed generator or async function counts as a call; a call that only creates a
generator does not, and a module resumed after an await continues its one evaluation. Allocations are the objects
made, charged to the innermost profiled function. The host's code (the preludes) is not profiled: its time and
allocations are its caller's. An opcode or compile profile compiles the entry as if its completion value were
reported, so `--output` and `--print` do not change the code profiled.

The compile profile's parse side is the shared parser's. `Goccia.Compiler` reports each function's compile at the
position the parser recorded it at, and counts its own work under the old compiler's counter names:

| Counter | What the engine counts |
|---------|------------------------|
| `nodesCompiled` (and nodes by kind) | each statement and expression compiled |
| `constantEmitAttempts` | each expression's literal check |
| `constantEvaluationVisits` | each binary operator's right operand checked for an immediate |
| `expressionTypeVisits` | each expression's kind classification |
| `numberProofVisits` | each number immediate checked for a small integer |
| `localLookups`, `localLookupProbes` | scope lookups, and the bindings (or hash buckets) each examined |
| `upvalueResolutions`, `upvalueListProbes` | each function an upvalue lookup climbs, and the names it scans (one bucket once hashed) |
| `localListScans` | the bindings each closed scope walks |
| `parameterNameProbes` | each parameter name's duplicate test and its record |
| `directEvalRecordProbes` | each entry of a direct eval's scope description |
| `parameterInitializerVisits` | each parameter the closure check visits |
| `parameterInitializerSourceChecks` | each initializer or pattern checked for a closure |

The binding-name and type-argument counters are the shared parser's. A `Function` constructor's source is a unit
of its own, and its parameters and body are parsed on their own as units too, as GocciaScript validates them;
under the compile profile the engine parses them for that, ignoring the results.

### Sandbox mode

GocciaRunner's sandbox mode ([ADR 0068](../adr/0068-goccia-sandbox-runner.md),
[ADR 0122](../adr/0122-unified-capability-model.md)) runs through `Goccia.CLI.SandboxHost`, with the same
options, config section, copied inputs, diff and write-back as the old engine's (the shared `Goccia.Sandbox.*` and
`Sandbox*` units). A program runs on a thread of its own with a fresh
engine whose thread is given the sandbox context (`Goccia.Sandbox`): its modules and data files are read from the
virtual filesystem (`Goccia.Modules` `SetFileReadHook`), and `prelude/sandbox.js` installs the `fs`, `goccia` and
`goccia:search` modules over `Goccia.Sandbox`'s natives.

A nested run (`runScript`, or the shell's `goccia`) runs on a new thread while its caller's thread waits in the
native: an engine's state is its thread's. The child inherits its caller's capabilities, at most the time its
caller has left, and under `--deterministic` a random stream forked from its caller's. Its completion value comes
back encoded as text (primitives, arrays and own data properties, cycles cut) and is decoded in the caller's realm.
A child's error reaches its caller with the location only: no code frame and none of the host's suggestions.

The child has its own heap, so a collection in it frees nothing of its caller's. When its event loop runs out of
work while a `FinalizationRegistry` cell still waits on its target, the engine collects once, so a caller's cleanup that
came due during a nested run still runs.

### Module reload

A module record read from a file remembers when the file was last modified (`Goccia.Modules`, the host's file
system, or a sandbox's virtual one through `SetFileTimeHook`). A request that finds the record settled and the file
newer loads it again ([ADR 0139](../adr/0139-a-reload-links-into-the-cached-module-record.md)): a changed module
gives a later `import()` its new version, and an import that failed is retried. The old record is superseded rather
than refilled, so what imported it keeps its bindings. Under `--deterministic` a sandbox stamps every file with the
epoch, so nothing reloads.

### GocciaTestRunner

`./build.pas testrunner` builds `build/GocciaTestRunner` from `source/app/GocciaTestRunner.dpr`: the old
runner's options, file discovery (a directory's `node_modules` is skipped), stdin and `--multifile`, the results
summary, the JSON envelope, exit codes, `--exit-on-first-failure` and coverage. Each file runs on a thread of its
own with an engine in test mode (`TestMode`, `prelude/testing.js`), its suites reported by `RunTestSuites`; at most
`--jobs` run at once, and their console output is printed only in a sequential run. What it shares with
GocciaRunner for one file's engine (the console capture, settings, the import grant) is in
`Goccia.CLI.FileEngine`.

Snapshots format values in `prelude/testing.js` as GocciaScript's Vitest pretty-format port does
(`Goccia.Builtins.Testing.SnapshotFormatting`), and match them through `Goccia.HostAPI`'s snapshot natives over the
engine-independent `Goccia.Builtins.Testing.Snapshots` state and the runner's `Goccia.TestRunner.SnapshotHost`
(the `.snap` file, inline edits written back once every file has run, `-u`, CI detection). The file's `--timeout`
covers its suites (`TGocciaEngine.RunTests`); `--test-timeout` runs each test body, and the work it waits for, inside
a native (`callWithin`) under a deadline of its own, so a test that runs out fails with `TIMEOUT after Nms` and the
next one runs. Host work is attributed as the old library does: a rejection or a throwing job a describe body, hook
or test left fails that unit (`Goccia.Promise` rejection marks, recorded job errors), a failed hook gives up the timers
and jobs it left, and work left after the last test still runs. `--describe-timeout` bounds each describe callback
through the same native. The console report's per-test lines (`❌`, `⏸️`, `📝`) and each failure's wording follow
the old library; a test function's frames count from its own frame for `--max-stack` (`Goccia.VM` `CallRebased`), and a
test's `import()` resolves through its file's resolver. Globals and virtual modules apply as in GocciaRunner
(`Goccia.CLI.FileEngine`). Not yet supported: `.gbc` test files.

### Heap snapshots

`Goccia.HeapSnapshot` writes what a value reaches so that a fresh engine of the same build, set up the same way,
restores it ([Heap Snapshots](../heap-snapshots.md), [ADR 0148](../adr/0148-heap-snapshots-at-idle-points.md)).
The writer copies cells as they are: strings, BigInts, symbols (a registered one by its key), value arrays, closed
bindings, accessors, and objects with their kind, flags, prototype, keys, elements and `Aux` values. What an object
holds outside the heap is named instead: a closure's code by the entry or module record it was compiled from and
its path of child templates there (the restoring engine compiles that source and the lazy functions on the path),
a built-in by its path from `globalThis`. Built-ins are named before the entry's code runs; each is written with a
fingerprint of its state (kind, extensibility, prototype, keys, values, the elements present), and a restore
refuses one whose fingerprint differs: the run changed it. An object kind whose state lives elsewhere (Map and Set
tables, buffers, promises, proxies, generators, module namespaces, iterators) and a private name refuse the
snapshot, named after GocciaScript's classes (`no snapshot codec for TGocciaMapValue`). Module records are not
written: the closures carry the module bindings they use, so a restored engine that imports a module evaluates it
again. A module root carries a hash of its source, and a restore refuses a module whose file now holds other code.
The reader checks what it reads, so a damaged or hostile file is refused rather than restored: a raw value must be
one the engine makes (an Int32, one of the five special constants, a Number with only the canonical NaN), and the
prototype chains among the restored objects must end, as `OrdinarySetPrototypeOf` keeps them.

`GocciaTestRunner --heap-snapshots` round-trips each distinct object `expect()` was given. After the file's tests,
`TGocciaEngine.HeapSnapshotObserved` writes each one and describes it through the object model (own keys,
descriptors, prototypes, extensibility; built-ins by name). Once the file's engine is gone, each written object is
restored in a fresh engine on the same thread, set up as the file's was (`TGocciaEngine.HeapSnapshotVerify`), written
again and described again: different bytes or a different description fail the file. The report on stderr is GocciaTestRunner's
(`Heap snapshots: observations=N objects=N passed=N refused=N failed=N`, then each refusal reason with its count).
Over `tests/` no round trip fails. Not yet supported: module records restored whole
([ADR 0150](../adr/0150-heap-snapshots-restore-module-records-whole.md)), the codecs that refuse today, the
embedder's session and restore API and the pause checkpoint.

### GocciaREPL

`./build.pas repl` builds `build/GocciaREPL` from `source/app/GocciaREPL.dpr`: one engine for the session
on a thread of its own, each input run on it as a Script (`TGocciaEngine.Run` again), so declarations and imports stay
for the next input. Values print by the old REPL's rules (`host.js` `replFormat`, the `grfREPL` result form); each
run's Script imports are a module of their own, and a rejection an input leaves goes with it.

### GocciaBenchmarkRunner

`./build.pas benchmarkrunner` builds `build/GocciaBenchmarkRunner` from `source/app/GocciaBenchmarkRunner.dpr`:
the old runner's options, file discovery (a `helpers/` folder is skipped), stdin and `--multifile`, `--jobs`, the
console, text, CSV and JSON reports (`Goccia.Benchmark.Reporter`, shared with the old runner), exit codes and the
profiles (`--profile`, `--profile-deterministic`). Each file runs on a thread of its own with an engine in benchmark
mode (`BenchMode`): the file's limits stay armed while `TGocciaEngine.RunBenchmarks` measures what it registered.
`goccia:microbench` (`bench`, `group`, `run`, `summary`, `boxplot`) is `prelude/microbench.js`, which measures as
`Goccia.Builtins.Benchmark` does: the same warm-up, calibration, rounds, interquartile outlier filter and
percentiles, the same `GOCCIA_BENCH_*` settings, a body's promise or thenable awaited, a generator function's code
before its first `yield` as setup and after it as teardown, and the body's error kept when the teardown throws too.
Its clock is a native in nanoseconds; the old runner turns its collector off while it measures, where the engine's nursery
collects as the body allocates. A benchmark's error reads as an uncaught error does in GocciaRunner, with its
location and suggestion. Not yet supported: `.gbc` benchmark files.

### Bytecode files

The engine has its own `.gbc` format (`Goccia.Artifact`), read as untrusted input as the old engine's is
([Bytecode VM, Binary Format](../bytecode-vm.md#binary-format)). The header carries format version 200, so
each engine refuses the other's files, the runtime tag `goccia-engine`, a fingerprint of the instruction set
(a build with other opcodes refuses the file), the source path and the compile profile (the old wire fields
and the engine's unrestricted flag). The body holds the module record or a Script's imports, the source text once,
and every function compiled eagerly. Constants carry explicit kinds, so a file cannot forge a pointer. The
reader bounds every length and count (64 MiB, 256 nested functions). The verifier (`Goccia.ArtifactVerify.inc`)
checks every operand of every instruction, the jump targets, a terminator at the end, the exception handler
depth at each instruction, the prologue and direct eval descriptions. It refuses instructions the compiler never
emits, and makes every suspension keep all its registers. What the code cannot show, a register holding the
array, object, closure, private name or module cell table an instruction trusts it to hold, is checked at run
time in code read from a file (`TFuncTemplate.Loaded`): `Execute` checks it, FastLoop and the baseline JIT hand
those instructions to `Execute`, and the optimizing tier leaves such templates alone. A class field's definition
(`opDefField`) is checked in every tier instead: FastLoop and the baseline JIT test that the receiver is an object
inline (the JIT's slow path in `JH_DefField`), and the optimizing tier's receiver guard deoptimizes, so field
initializers keep the fast tiers and the inliner. The compiler's own code keeps its fast paths.
`GOCCIA_ARTIFACT_SCRAMBLE=<seed>` (tests only) rewrites register operands after reading, so code that passes the
verifier puts the wrong kinds in registers. `scripts/engine/artifact-tamper-tests.py` points a field definition's
receiver at a value that turns from an object into a number once the code is hot in each tier, and expects the
TypeError.

Code for a file names no global lexical binding by its index in the compiling engine's table: another engine
numbers them differently, so such a binding is found by name at run time. `Goccia.Modules` loads a `.gbc` file as
the module record of its logical address: the file's name with the recorded source extension. An import of
the source and an import of the file then share one record. A file with another profile, or with a goal
other than an explicit `--source-type`, is refused before anything in it runs, never replaced by the source.
A source phase import takes no companion.

`GOCCIA_ARTIFACT_ROUNDTRIP=1` writes, reads and verifies every compiled Script and module before it runs;
`scripts/engine/artifact-roundtrip-tests.py` runs `tests/` with and without it and reports any file whose totals
differ or whose round trip is refused. A test that a dropped value is reclaimed while its frame is suspended
(`*-released.js`, `releases-dead-registers`) may pass fewer of its tests after the round trip: compiled source
keeps only the registers live across a `yield` or `await`, and a file's suspensions keep them all.

### GocciaBundler

`./build.pas bundler` builds `build/GocciaBundler` from `source/app/GocciaBundler.dpr`: the old Bundler's
options, configs, output plan and collision checks, stdin, `--multifile`, `--jobs` and `--source-map` (the JSX
transform's map). Each file compiles on a thread of its own with a fresh engine (`TGocciaEngine.CompileArtifact`),
under its file settings. GocciaRunner runs a `.gbc` entry (`TGocciaEngine.RunArtifact`) and, with
`--prefer-bytecode` or the config key, takes a `.gbc` companion for an import in host and sandbox mode, held to
the source's package policy (a CommonJS file's companion is refused). `scripts/test-cli-aot.ts` and the Bundler
sections of `scripts/test-cli-apps.ts` pass against the runner and bundler.

### GocciaTest262Runner

`./build.pas test262runner` builds `build/GocciaTest262Runner` from `source/app/GocciaTest262Runner.dpr`:
the old runner's options, discovery, sharding, run modes, JSON report and per-test profiles (`--profile-dir`), on
the engine's test262 host (`Goccia.Test262`, which `goccia --test262` also runs). `$262.js`, the host object, is compiled in
(`source/engine/prelude/test262host.js`), so the runner works from any directory. Each run is a forked process with a fresh engine, killed at `--timeout` (the engine's
test262 host has no cooperative timeout) and graded as `scripts/engine/run262.py` grades it; `--max-memory` is the
child's heap limit. Its statuses match `run262.py`'s for every run of the suite. `--eval-host` runs stdin after
`$262.js`, so a probe reaches the hooks as `$262`; the old runner's probes read its own `Goccia.test262` object,
which the engine does not have (the `--eval-host` sections of `scripts/test-cli-apps.ts`).

### GocciaScriptLoaderBare

`./build.pas loaderbare` builds `build/GocciaScriptLoaderBare` from `source/app/GocciaScriptLoaderBare.dpr`:
the old loader's command line (compatibility flags, `--source-type`, `--source-name`, `--print`, the limits and
`--profile`) over an engine with `TGocciaEngine.Bare` set. Its global object is the old loader's: the core built-ins,
`DOMException`, `atob`, `btoa`, `PermissionDenied`, `print` and `Goccia` (without `semver`), and no `console`,
`performance`, timers, web APIs or `goccia:` modules. It has no module content provider: an import resolves as
GocciaScript's default resolver resolves it and, once it finds its file, fails with `Error: Cannot load module: no
module content provider is configured` (`code` `ERR_MODULE_LOADING_UNSUPPORTED`). Without `--max-stack` the frame
bound is the engine's own, not the old loader's 10,000, since a bound keeps the JIT off. Against it, the Bare Loader sections
of `scripts/test-cli-apps.ts`, `test-cli.ts`, `test-cli-permissions.ts` and `test-cli-intl-taint.ts` pass except
two. The two memory-budget sections test the old collector's allocation gate: the engine refuses at the next safe point
([ADR 0185](../adr/0185-g2-refuses-memory-at-the-next-safe-point.md)), and `new Array(100000000)` allocates
nothing.

### GocciaFuzzHarness

`./build.pas fuzzharness` builds `build/GocciaFuzzHarness` from `source/app/GocciaFuzzHarness.dpr`, the old
harness's contract: exit 0 for every outcome the engine models (completed, script-throw, parse-error,
timeout, instruction-limit, memory-limit, module-denied, input-rejected, snapshot-refused) and exit 1 with a
backtrace for anything else. Each input runs twice, on a fresh engine with the fuzz profile (the compatibility
flags on). `[bytecode]` runs under a 1 s timeout, 5,000,000 instructions, 256 frames and a 256 MB heap, which keeps
the JIT off. `[jit]` runs with only the heap limit, so the JIT compiles hot code (`GOCCIA_JIT_HOT` lowers its
threshold), and a watchdog thread ends the process with exit 0 after 1 s, since compiled code does not poll a
deadline. Module loading is denied by a resolver that refuses every specifier and a file-read hook that refuses
every read, so neither a static nor a dynamic import reaches the host filesystem. Inputs over 1 MB are rejected
before they are read, from a file or from stdin (`-`).

`--heap-snapshot FILE` restores FILE as a heap snapshot and exercises what it holds: calls each function three
ways, walks descriptors and prototypes, and stringifies the root. `--write-heap-snapshot-seeds DIR` writes the
eight seeds (primitives, sparse arrays, shared and cyclic graphs, symbols and built-ins, descriptors and
integrity levels, closures, accessors and bound functions, errors and wrappers). Seeds belong to the build that
wrote them: a built-in's fingerprint holds its native code's address, so another build refuses them. The CLI
sections of `scripts/test-cli-apps.ts` for the fuzz harness pass against it.

`--grammar FILE` reads FILE's bytes as the choices of a program generator (`source/app/Goccia.Fuzz.Generator.pas`):
each byte picks the next statement, expression or binding, so every input is a program that parses, and a mutated
input is a program mutated at the level of its syntax. When the bytes run out every further choice is the first,
which is always the smallest (a block ends, an expression is a literal), so a zero byte removes or simplifies what
it chose. The programs read no clock or random numbers, every loop runs a constant number of times, and each
function is called hot (up to 2,500 times, past the optimizing tier's threshold) with arguments whose types can
change late in the loop. Bindings carry the type their value was made with, so expressions use them as numbers,
strings, arrays, objects, BigInts, functions or class instances; a wrong guess is a TypeError the program catches.
Each program prints its results through a renderer of its own rather than the console's formatting, and the harness
compares the `[bytecode]` run's lines with the `[jit]` run's: when both end as the program does (completed, or an
uncaught throw) and neither reached the frame limit, which differs between the runs by design, a difference in a
line or in how they ended exits 1. `--print-program` prints the program instead of running it.
`scripts/engine/grammar-fuzz.py HARNESS CASES` runs a campaign of random inputs without AFL, reports how the runs ended,
and keeps each finding's input, program and report after reducing the input by truncating it and zeroing runs of
bytes.

## Gate

Every change is gated on test262 with no newly failing (id, mode) pair, the 18 Web Tooling digests, and,
for anything near allocation or rooting, a GC stress run:

```sh
GOCCIA_TEST262=/path/to/test262 GOCCIA_WT_DIR=/path/to/wt-verify sh scripts/engine/gate.sh NEW BASE
GOCCIA_WT_DIR=... GOCCIA_JS_DIR=/path/to/jetstream scripts/engine/gcstress.sh
GOCCIA_QJS=/path/to/qjs GOCCIA_WT_DIR=... GOCCIA_JS_DIR=... scripts/engine/wallclock.sh build/engine/goccia [other-goccia]
```

`gate.sh` writes `build/engine/gate/NEW.json` and compares it with `BASE.json`; `gcstress.sh` runs every bundle
with a 256 KB nursery and `--gc-verify`, the scripts in `scripts/engine/regress` (a first line
`// goccia-flags: ...` adds flags), and `scripts/engine/gcstress-tests.py`. That script runs every file under `tests/`
through `goccia --test` twice: plain, and with a 16 KB nursery (`GOCCIA_NURSERY_KB`) and `--gc-verify`. It reports
a file whose totals differ, or whose stressed run crashes, as `BAD`. A stressed run past 600 s is reported as
`TIMEOUT`, not a failure: the deep-chain tests under `tests/built-ins/Goccia/gc` verify a large heap at
every collection. `scripts/engine/artifact-roundtrip-tests.py` and `scripts/engine/artifact-tamper-tests.py` (see
[Bytecode files](#bytecode-files)) run with the gate. The digest bundles print `DIGEST n:hash` of each workload's result; `scripts/engine/digests.txt`
holds the expected lines.

`.github/workflows/engine.yml` runs part of this in CI on every pull request and on pushes to `main` and `feat/g2-**`,
with FPC `3.3.1-b9`, as the rest of CI. On Linux x86-64 three jobs build what they run: GocciaTestRunner over
`tests/` with `--heap-snapshots`, GocciaTest262Runner over the pinned test262, and the limits test, both
bytecode-file tests and a 1,000-case grammar fuzz campaign. `platform-build` builds GocciaTestRunner for
macOS and Linux on aarch64, x86-64 macOS and the two Windows targets, and `platform-test` runs `tests/` on each target's own runner. Any
failing test or test262 run fails its job. The digests, GC stress
and the benchmarks stay with the local gate, which needs the bundles under `GOCCIA_WT_DIR` and `GOCCIA_JS_DIR`.

## Changes to Goccia's front end

The engine parses with `source/units`. Building the engine changed it in three ways, each checked against Goccia's tests and
its Pascal unit tests:

- Speed. Parsing and compiling the typescript bundle took about half the instructions afterwards (6.5 G to
  3.1 G, measured in the spike), with identical bytecode on all 18 Web Tooling bundles:
  - a source span holds a plain reference to the source's coordinates instead of a reference-counted
    interface, so copying a span into a token or node no longer initializes, finalizes and counts a
    reference; the AST arena keeps the coordinates alive for as long as the tree
    (`TGocciaASTArena.KeepCoordinates`);
  - line and column are computed from offsets when read, not stored per node;
  - a function's source text is a range of the parse's source, copied only when read;
  - tokens are records in chunks owned by a `TGocciaTokenList`, and punctuators share one lexeme string per
    token type;
  - source lines are split only when an error is raised, and the lexer reads the clock only when asked to;
  - error messages and suggestions are constants rather than `resourcestring`, which FPC converts and
    allocates at every call;
  - `Goccia.ClassMask` replaces chains of `is` tests with one cached bit mask per class.
- `Goccia.Lexer`: `#` must be followed directly by an identifier name (`# x` is a SyntaxError).

Goccia's RegExp compiler rejects a repetition count too large to copy; the engine treats that as a limit of the
implementation rather than an early error, so such a literal throws when it is evaluated instead of when the
script is parsed.

## Working rules

- A `TValue` held in a Pascal local is stale after any allocation: root it (`PushRoot` / `RootAt`) or re-read
  it from a rooted place. `gcstress.sh` finds the misses.
- Keep strings, dynamic arrays and other managed temporaries out of hot functions and out of `Execute`:
  FPC gives such a function an exception frame and initialize / finalize calls on every call. Put the cold
  path in a `noinline` helper, and check hot functions' disassembly for `fpc_pushexceptaddr`.
- The engine's Pascal is outside `./format.pas` (which covers `source/app`, `source/units` and `source/shared`), and
  the pre-commit format hook skips `source/engine`: the formatter's parameter renaming does not follow the engine's nested
  routines and `with` blocks.
- Write a built-in in a prelude when its spec steps are what matter and its speed does not; keep a native fast
  path, guarded on the objects involved being unmodified, where it does.
