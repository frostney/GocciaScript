# G2: an experimental VM and GC for GocciaScript

G2 is a spike: a second engine behind Goccia's front end. It keeps Goccia's lexer, parser and RegExp
compiler (`source/units`) and replaces everything after the AST: compiler, register bytecode, VM, object
model, GC and built-ins (`source/g2`). It answers whether an FPC engine built for speed can match or beat
QuickJS while passing test262.

It is not wired into `build.pas`, the CI or the release, and nothing in Goccia depends on it.

## Status

| Measure | Value |
|---|---|
| test262 (pin 91c70759, contract 4, all six categories) | 92,493 / 103,264 runs (89.6%) |
| JetStream, 6 bundles, wall clock | QuickJS time / G2 time 0.92 (below 1: QuickJS faster) |
| Web Tooling, 18 bundles, wall clock | 1.23, or 1.03 without coffeescript (QuickJS runs it 25x slower) |

Wall clock is best of 3, one engine at a time, from `scripts/g2/wallclock.sh`; the figures above were taken on a
shared machine at a load average of 5 to 10, so single bundles can be off by several percent. The front-end
changes below made Web Tooling 1.21x faster end to end than on this repository's front end before them
(typescript 1.55x, prettier 1.68x).

Largest remaining test262 gaps: Intl (about 6,300 runs, including Temporal's non-ISO calendars), realms
(`$262.createRealm`, ShadowRealm: about 700), Date, import defer, Atomics on BigInt arrays, immutable ArrayBuffers.
Largest performance gaps: generators (lazy-collections 0.67), float arithmetic on typed arrays
(gaussian-blur 0.78), string-heavy bundles (source-map 0.85, buble 0.85), hash-map and sync-fs (0.88).

## Layout

| Path | Contents |
|---|---|
| `source/g2/` | the engine: `G2.Core` (values, cells, shapes, GC), `G2.Compiler`, `G2.Bytecode`, `G2.VM`, `G2.ObjModel` and the built-in units; `g2.dpr` is the command-line host |
| `source/g2/prelude/` | built-ins written in JavaScript (RegExp's spec layer, Set methods, iterator helpers, disposal, base64); compiled at start-up, snapshotted into the binary by the build |
| `scripts/g2/` | build, gate, gcstress and wall-clock scripts; `run262.py` is the contract-4 test262 runner, `harness/$262.js` its host object |
| `docs/g2/` | this file and [design.md](design.md) |

## Build

G2 builds with FPC 3.3.1 from fpc-toolchain (native TLS threadvars), build 3.3.1+5 (release `3.3.1-b5`) or
later, selected by `fpc-toolchain env 3.3.1+5` or any command given in `G2_FPC_ENV`. Builds before 3.3.1+4 hang in
the DFA pass on some units and drop the `@tpoff` relocation of a threadvar field at -O1; 3.3.1+4 drops the `%fs`
prefix of a threadvar the peephole optimizer folds into an instruction at -O1 (patch 0123 in 3.3.1+5):

```sh
./build.pas --prod g2         # build/g2/g2, with the 3.3.1 toolchain selected
sh scripts/g2/build.sh        # the same, selecting the toolchain itself
build/g2/g2 file.js           # run a script (always a Script; modules run through --test and the test262 host)
```

The `g2` target turns each JS prelude into a Pascal include under `build/g2/gen`, then builds in two stages:
the first binary compiles the preludes and writes their bytecode, the second embeds it. It always compiles
optimized (`-O4`); `--prod` strips the binary and a dev build keeps line information, in its own unit cache.
On Linux x86-64 it fails when `scripts/g2/tlsscan.py` finds a native-TLS miscompile (a threadvar address with
an absolute relocation), the bugs 3.3.1+4 and 3.3.1+5 fix; it stays as a guard against a toolchain that regresses, and is
skipped when python3 or binutils is missing. The full build (`./build.pas` without targets) leaves `g2` out
until CI's toolchain is FPC 3.3.1.

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
build). A full collection runs once the old space would pass it, and one that leaves more live than the limit
aborts the run the same way, with `Fatal error: Live heap of N bytes exceeds the memory limit of M bytes` (exit 1;
GocciaRunner's JSON report names it `MemoryLimitError`). An explicit reservation that does not fit, such as an
`ArrayBuffer`'s bytes, is still a catchable `RangeError`. Compiled loops poll for the abort at every back edge (one
compare of the thread's `Interrupt`): the baseline code exits to the interpreter there, and an optimized region
leaves at the jump with its frame state written back. `scripts/g2/limits-test.sh` checks all of this.

Whatever the frame bound, a call that native code makes (a getter, a `toString`, a sort comparator, a generator's
`next()`) nests the interpreter on the native stack, so it is refused with a `RangeError` once that stack runs low.

### Script entries with imports

An explicit `--source-type=script` entry keeps GocciaScript's acceptance of import and export declarations
([language.md](../language.md#modules)). G2 takes the entry's import declarations and re-exports out of the
Script and runs them, each at its own line and column, as a module of their own keyed by the entry's path plus
`#script-imports`, so its requests resolve as the entry's would. That module's graph links as one, so a missing
export fails before any module evaluates. It then evaluates, through the event loop when a top-level await
waits on more. Each imported name then becomes a const global lexical binding that shares the cell of the binding
it imports: it reads live, throws in that binding's temporal dead zone, and refuses assignment. An export
declaration declares its binding as it would without `export`. The entry's own file, imported by a module, loads
as a separate module.

### Host environment

GocciaRunner on G2 takes `--host-environment=<module>` and the config key `host-environment`, as
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

A JSON report's `memory.gc` comes from G2's heap: `liveBytes` is the old space and the nursery's used part,
garbage included until a collection frees it; `peakLiveBytes` is the most the heap held at a collection or a
direct old-space allocation; `allocatedDuringRunBytes` counts every byte allocated after the engine was made.
G2 does not count objects, so `startObjectCount`, `endObjectCount` and `collectedObjects` stay 0. One file reports
its memory at the top level. Files run at once report each file's memory and their sum, and files run one after
another report only the sum.

A directory's files run at once when more than one worker is allowed (`--jobs`, else the processors). Each file
prints its console output and its failure, but no banner and no `--print` value, as GocciaRunner's workers do;
GocciaRunner also leaves the failure out.

`--audit-log` opens each engine's events with `capabilities.effective`. Then come the decisions of the Function
constructor, a ShadowRealm, `eval` and a `node_modules` grant, each at the script's call site. An event that cannot
be written ends the run past every handler with `Fatal error: capability audit delivery failed: ...`.

### FFI

With the `ffi` capability (`--allow-ffi`, or a trusted config's `allow-ffi`), G2 installs the `FFI` global that
[FFI Built-ins](../built-ins-ffi.md) describes, through `G2.FFI`. The call plans, layouts, callback trampolines and
library guard are GocciaScript's own units; `G2.FFI` is their JS face. FFI objects are ordinary objects with
internal slots, so nothing is finalized by the collector: libraries, signatures and callback slots are released
when the engine is torn down, and a closed library is unloaded no earlier than that. An aggregate's fields are its
own accessor properties.

G2's collector moves young objects, so a buffer's address is taken only after every argument conversion has run.
A call that can enter script through a callback runs a minor collection first, which moves the buffers it passes
into the old space, where nothing moves. `g2 --test` grants the permissions of the test file's `goccia.json`, as
`GocciaTestRunner -P` accepts them.

### fetch

`fetch` is `prelude/fetch.js`'s, over `G2.Fetch`: the URL's host is judged by the net capability before the call
returns, as [Fetch Runtime APIs](../built-ins-fetch.md) describes, and each request runs on a worker thread through
the shared HTTP client, which checks the capability again on every hop and against every resolved address. Those
hop decisions are audited when the request settles, at its `fetch()` call. The event loop settles finished requests
before anything else, and while a request is in flight it waits for it before firing timers: G2's timers do not
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
`import()` specifier included, needs a read grant. `G2.HostAPI` sets the resolver's probe guard, so every candidate
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
`.goccia`. Each one is checked against its pin as it loads (`G2.Modules` `SetModuleContentCheck`), so the bytes
compiled are the bytes hashed. Sandbox mode refuses every provider package. Install mode (`--add`, `--remove`,
`--install`, `--update`) runs the old runner's installer (`Goccia.Packages.Install`) unchanged.

### Profiling

`--profile` reports through GocciaScript's profilers and writers (`Goccia.Profiler`, `Goccia.Profiler.Compile`,
`Goccia.Profiler.Report`). Profiled files run one at a time; each file's thread records into the main thread's
profilers (`Adopt`). Under the runtime profile (`G2.Profile`) every instruction runs through the interpreter's
counted loop, as under coverage: it counts each opcode (G2's own names), each pair of consecutive ones, and for a
binary arithmetic or comparison instruction whether both operands are Numbers (the scalar fast path). A function
is timed from its frame's push to the push that reuses its slot or the next instruction in a frame below it, so an
unwinding throw needs no hook. A resumed generator or async function counts as a call; a call that only creates a
generator does not, and a module resumed after an await continues its one evaluation. Allocations are the objects
made, charged to the innermost profiled function. The host's code (the preludes) is not profiled: its time and
allocations are its caller's. An opcode or compile profile compiles the entry as if its completion value were
reported, so `--output` and `--print` do not change the code profiled.

The compile profile's parse side is the shared parser's. `G2.Compiler` reports each function's compile at the
position the parser recorded it at, and counts its own work under the old compiler's counter names:

| Counter | What G2 counts |
|---------|----------------|
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
under the compile profile G2 parses them for that, ignoring the results.

### Sandbox mode

GocciaRunner's sandbox mode ([ADR 0068](../adr/0068-goccia-sandbox-runner.md),
[ADR 0122](../adr/0122-unified-capability-model.md)) runs on G2 through `Goccia.CLI.G2SandboxHost`, with the same
options, config section, copied inputs, diff and write-back as the old engine's (the shared `Goccia.Sandbox.*` and
`Sandbox*` units). A program runs on a thread of its own with a fresh
engine whose thread is given the sandbox context (`G2.Sandbox`): its modules and data files are read from the
virtual filesystem (`G2.Modules` `SetFileReadHook`), and `prelude/sandbox.js` installs the `fs`, `goccia` and
`goccia:search` modules over `G2.Sandbox`'s natives.

A nested run (`runScript`, or the shell's `goccia`) runs on a new thread while its caller's thread waits in the
native: a G2 engine's state is its thread's. The child inherits its caller's capabilities, at most the time its
caller has left, and under `--deterministic` a random stream forked from its caller's. Its completion value comes
back encoded as text (primitives, arrays and own data properties, cycles cut) and is decoded in the caller's realm.
A child's error reaches its caller with the location only: no code frame and none of the host's suggestions.

The child has its own heap, so a collection in it frees nothing of its caller's. When its event loop runs out of
work while a `FinalizationRegistry` cell still waits on its target, G2 collects once, so a caller's cleanup that
came due during a nested run still runs.

### GocciaTestRunner

`./build.pas g2testrunner` builds `build/g2/GocciaTestRunner` from `source/app/GocciaTestRunnerG2.dpr`: the old
runner's options, file discovery (a directory's `node_modules` is skipped), stdin and `--multifile`, the results
summary, the JSON envelope, exit codes, `--exit-on-first-failure` and coverage. Each file runs on a thread of its
own with an engine in test mode (`TestMode`, `prelude/testing.js`), its suites reported by `RunTestSuites`; at most
`--jobs` run at once, and their console output is printed only in a sequential run. What it shares with
GocciaRunner for one file's engine (the console capture, settings, the import grant) is in
`Goccia.CLI.G2FileEngine`. Not yet on G2: snapshots, heap snapshots, `--globals` and `--global`, the per-test and
per-describe timeouts, and suite errors counted apart from failed tests.

## Gate

Every change is gated on test262 with no newly failing (id, mode) pair, the 18 Web Tooling digests, and,
for anything near allocation or rooting, a GC stress run:

```sh
G2_TEST262=/path/to/test262 G2_WT_DIR=/path/to/wt-verify sh scripts/g2/gate.sh NEW BASE
G2_WT_DIR=... G2_JS_DIR=/path/to/jetstream scripts/g2/gcstress.sh
G2_QJS=/path/to/qjs G2_WT_DIR=... G2_JS_DIR=... scripts/g2/wallclock.sh build/g2/g2 [other-g2]
```

`gate.sh` writes `build/g2/gate/NEW.json` and compares it with `BASE.json`; `gcstress.sh` runs every bundle
with a 256 KB nursery and `--gc-verify`. The digest bundles print `DIGEST n:hash` of each workload's result;
`scripts/g2/digests.txt` holds the expected lines.

## Changes to Goccia's front end

G2 parses with `source/units`. This branch changes it in three ways, each checked against Goccia's tests and
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

Goccia's RegExp compiler rejects a repetition count too large to copy; G2 treats that as a limit of the
implementation rather than an early error, so such a literal throws when it is evaluated instead of when the
script is parsed.

## Working rules

- A `TValue` held in a Pascal local is stale after any allocation: root it (`PushRoot` / `RootAt`) or re-read
  it from a rooted place. `gcstress.sh` finds the misses.
- Keep strings, dynamic arrays and other managed temporaries out of hot functions and out of `Execute`:
  FPC gives such a function an exception frame and initialize / finalize calls on every call. Put the cold
  path in a `noinline` helper, and check hot functions' disassembly for `fpc_pushexceptaddr`.
- G2's Pascal is outside `./format.pas` (which covers `source/app`, `source/units` and `source/shared`), and
  the pre-commit format hook skips `source/g2`: the formatter's parameter renaming does not follow G2's nested
  routines and `with` blocks.
- Write a built-in in a prelude when its spec steps are what matter and its speed does not; keep a native fast
  path, guarded on the objects involved being unmodified, where it does.
