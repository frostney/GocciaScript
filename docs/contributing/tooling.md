# Tooling

*Auto-formatting, editor configuration, and platform-specific pitfalls for FreePascal development.*

## Executive Summary

- **Auto-formatter** — `./format.pas` auto-fixes uses clauses, PascalCase naming, parameter prefixes, and stray spaces; runs via Lefthook pre-commit hook
- **Editor config** — `.editorconfig` + VSCode/Cursor extensions for zero-config formatting on save
- **Platform pitfalls** — stale FPC artifacts after branch changes, FPC 3.2.2 `Int64`→`Double` conversion bugs (all platforms + AArch64-specific) and FPC 3.2.4 integer arguments resolving to `Single` overloads, endian-dependent byte indexing, `Int64` loop counters and lengths on 32-bit targets
- **Fuzzing and probes** — `GocciaFuzzHarness` fuzzes single inputs and the scheduled run checks memory safety; `scripts/depth-probe.ts` is a manual, memory-capped probe of the runner's depth limits

## Auto-Formatting

The project includes `./format.pas`, an instantfpc script that formats Pascal source files. It runs automatically as a pre-commit hook via [Lefthook](https://github.com/evilmartians/lefthook) and can also be invoked manually — no build step needed.

### Setup

Install Lefthook as described in [Workflow — Local setup](workflow.md#local-setup), then register hooks with `lefthook install`.

### Manual Usage

```bash
# Format all project Pascal files
./format.pas

# Format specific files
./format.pas source/units/Goccia.Engine.pas

# Check only (exit 1 if changes needed)
./format.pas --check
```

### What It Enforces

All of the following are **auto-fixed** (not just warned about):

- **Uses clauses**: one unit per line, grouped (System > Third-party > Project > Relative), alphabetically sorted within each group, blank line between groups. Units with an `in` path are always in the Relative group; `Goccia.*` units are Project; known FPC standard library units are System; everything else is Third-party.
- **Function naming**: capitalizes the first letter of function, procedure, constructor, and destructor names to enforce PascalCase. Renames all references within the same file. External C bindings are excluded.
- **Parameter naming**: adds the `A` prefix to multi-letter parameters (e.g., `Value` -> `AValue`) and renames all references within the function scope (declaration, local variables, and body). Single-letter parameters and Pascal keyword conflicts are skipped.
- **Stray spaces**: removes spurious spaces before `;`, `)`, and `,` (e.g., `string ;` -> `string;`). String literals and comments are left untouched.

## Editor Configuration

### `.editorconfig`

The project uses `.editorconfig` for consistent formatting:

- **Indent:** 2 spaces (no tabs)
- **Line endings:** LF
- **Trailing whitespace:** trimmed
- **Final newline:** inserted
- **Charset:** UTF-8

### VSCode / Cursor Setup

The repository includes `.vscode/settings.json` and `.vscode/extensions.json` for a zero-config experience in VSCode and Cursor.

#### Recommended Extensions

Open the project and accept the "Install Recommended Extensions" prompt, or install them manually:

| Extension | ID | Purpose |
|-----------|----|---------|
| Pascal | `alefragnani.pascal` | Syntax highlighting, code navigation, and symbol search for Pascal/Delphi |
| Run on Save | `emeraldwalk.RunOnSave` | Triggers `./format.pas` automatically when a `.pas` or `.dpr` file is saved |
| EditorConfig | `editorconfig.editorconfig` | Applies `.editorconfig` rules (indent size, line endings, etc.) |

These are declared in `.vscode/extensions.json` so VSCode/Cursor will prompt to install them on first open.

#### Format on Save

`.vscode/settings.json` configures the [Run on Save](https://marketplace.visualstudio.com/items?itemName=emeraldwalk.RunOnSave) extension to run `./format.pas` on every `.pas` and `.dpr` file when saved. This keeps code style consistent without manual intervention — the formatter fixes uses clause ordering, PascalCase naming, parameter prefixes, and stray spaces in the background.

The `runOnSave` command runs silently (`"runIn": "backend"`), so it will not open a terminal or interrupt your workflow. The file is re-read by the editor after formatting, so changes appear immediately.

> **Note:** This requires `instantfpc` (ships with FreePascal) to be on your `PATH`. If you installed FreePascal via the methods in [Build from source](../../README.md#build-from-source), this is already the case.

#### How the Layers Work Together

| Layer | When it runs | What it does |
|-------|-------------|--------------|
| `.editorconfig` | While typing | Sets indent size, line endings, trailing whitespace, charset |
| `runOnSave` | On file save | Runs `./format.pas` to auto-fix code conventions |
| Lefthook pre-commit | On `git commit` | Runs `./format.pas` on staged files as a safety net |
| CI `--check` | On push / PR | Fails the build if any file needs formatting |

All four layers enforce the same rules, providing defence in depth. The typical developer experience is: EditorConfig handles whitespace while you type, format-on-save fixes everything else when you save, and the pre-commit hook and CI catch anything that slips through.

## Platform-Specific Pitfalls

### Stale FPC Build Artifacts

FPC 3.2.2 can report stale compiled state as misleading internal compiler
errors or resource-list errors, for example after a branch switch, merge, PR
sync, or generated resource update.

`build.pas` retries this itself. When an FPC invocation fails and its output
contains a stale-state symptom (`Internal error`, `Compilation raised exception
internally`, `Error while compiling resources`, or a missing `.reslst`), it
prints one line, `FPC reported a stale-state error for <target>; retrying once
from a clean unit directory`, deletes the compiled units of that target's own
unit directory (the same files `--clean` removes), and compiles that target
once more. The retry happens at most once per compile, and an ordinary compile
error (a real `Error:` in source without those symptoms) is never retried.

If the retry also fails, `build.pas` reports the retry's output and exits
non-zero. Only then diagnose the reported Pascal source line. A manual
`--clean` is needed only for a failure the retry did not fix, or to clean
every target at once:

```bash
./build.pas --clean loaderbare
./build.pas --clean testrunner
./build.pas --clean
```

### Website Checks Are Inert Without Its Dependencies

The website has its own dependency tree, and its checks fail to load rather
than fail loudly when that tree is missing. In a fresh clone or worktree,
`bun run test` inside `website/` reports some modules failing to import — and
reports exactly that before and after any change you make, because those
failures are modules that cannot load, not assertions that disagree with your
edit.

Install first; then every file loads, the suite reports `0 fail`, and a genuine
regression fails immediately and by name:

```bash
cd website
bun install
bun run test
bun run lint
```

Two regressions shipped in `website/src/lib/positioning.ts` during 0.11.0
because the unprovisioned numbers were compared before and after a change and
read as "unchanged, therefore safe". They were identical because the file
guarding that text never loaded. A check that cannot run is not a passing
check — confirm the checker actually executed, not merely that it reported.

### Shared `-FU` Directories Across Programs — Internal Error 200611011

FPC 3.2.2 aborts with `Fatal: Internal error 200611011` when a second program
is compiled against the `.ppu` files another program left in a shared `-FU`
unit-output directory (the inliner trips while recompiling a unit it loaded
from the first program's build). This is why `build.pas` compiles every target,
including the TOML compliance runner, into its own
`build/compiled/targets/<target>` directory. Any script that compiles more than
one program with `fpc @config.cfg` must use a separate `-FU` directory per
program.

### `Int64` to `Double` Conversion on FPC 3.2.2

FPC 3.2.2 has **two bugs** affecting `Int64` -> `Double` conversion. Bug A is a Delphi-mode front-end issue that affects **all platforms**. Bug B is an AArch64-specific codegen issue.

#### Bug A: `Double(Int64Var)` bit reinterpretation — [FPC #35886](https://gitlab.com/freepascal.org/fpc/source/-/issues/35886)

In `{$mode delphi}`, an explicit `Double(Int64Var)` cast performs a **Turbo Pascal-style bit reinterpretation** instead of a value conversion. This produces garbage floating-point values (e.g., `Double(Int64(1000))` yields `~4.94e-315` instead of `1000.0`). This is a compiler front-end bug in `defcmp.pas` that affects all platforms in Delphi mode. Fixed in FPC trunk (3.3.1, [commit `1da43f67`](https://gitlab.com/freepascal.org/fpc/source/-/commit/1da43f67d40dd92ea2fb1cfa327d6088fa838aa7)) but **not backported** to 3.2.x.

#### Bug B: `Int64 * 1.0` wrong results near +/-2^31 (AArch64 only)

Mixed `Int64 * Double` arithmetic produces **wrong results** for `Int64` values near the `LongInt` boundary (+/-2,147,483,648). This affects all arithmetic operators (`*`, `+`, `-`, `/`) where one operand is `Int64` and the other is `Double`. FPC appears to use a 32-bit `SCVTF` instruction instead of 64-bit when promoting `Int64` through arithmetic expressions. This is AArch64-specific and has **not yet been reported upstream**.

```text
// Observed on FPC 3.2.2 AArch64, all optimization levels:
Int64(-2147483647) * 1.0  ->  -2147483648   // WRONG (should be -2147483647)
Int64(-2147483649) * 1.0  ->  -2147483648   // WRONG (should be -2147483649)
Int64( 2147483649) * 1.0  ->   2147483648   // WRONG (should be  2147483649)
Int64(-3000000000) * 1.0  ->  -3000000000   // correct (far from boundary)
```

#### Safe conversion

Use **implicit assignment** or **function parameter passing** — both use the correct 64-bit conversion path and are unaffected by either bug:

```pascal
// WRONG — bit reinterpretation in Delphi mode (Bug A)
Result := Double(FEpochMilliseconds) * 1000000.0;

// WRONG — wrong results near +/-2^31 on AArch64 (Bug B)
Result := FEpochMilliseconds * 1.0;

// CORRECT — implicit assignment
var D: Double;
D := FEpochMilliseconds;
Result := D * 1000000.0;

// CORRECT — implicit promotion at function call boundary
// When passing Int64 to a function/constructor that takes Double,
// FPC generates the correct 64-bit SCVTF instruction.
Result := TGocciaNumberLiteralValue.Create(SomeInt64Value);
```

Parameter passing is safe only when the parameter is declared `Double`. When the
callee is overloaded over several floating-point types, an integer argument can
resolve to the `Single` overload: FPC 3.2.4's `Math` unit overloads `Ldexp` for
`Single` and `Double` (and `Extended` on targets that have it), and
`Ldexp(Int64Var, N)` picks `Single`, which rounds the value to 24 significant
bits (FPC 3.2.2 declares one `Ldexp`, for its `Float` type, and is unaffected).
Assign the integer to a `Double` variable first and pass that, as
`TBigInteger.ToDouble` does.

This affects any code that converts `Int64` fields to `Double` for floating-point arithmetic. Note that `Int64 / Int64` is safe — FPC's `/` operator already returns `Extended` for integer operands, so no explicit promotion is needed for division.

### Endian-Dependent Byte Indexing

Do **not** inspect raw byte arrays of `Double` values to check the sign bit (e.g., `Bytes[7] and $80`). This assumes little-endian byte layout and breaks on big-endian platforms.

Instead, use the helpers in `NumberBits` (`source/shared/NumberBits.pas`), which compare the value as a whole 64-bit integer:

```pascal
// WRONG — assumes little-endian byte order
var V: Double; Bytes: array[0..7] of Byte absolute V;
begin
  Result := (V = 0.0) and ((Bytes[7] and $80) <> 0);
end;

// CORRECT — endian-neutral: NumberBits.IsNegativeZero compares
// DoubleToBits(V) with the sign bit alone (1 shl 63)
Result := NumberBits.IsNegativeZero(V);
```

This works because `DoubleToBits` copies the whole `Double` into a `UInt64`, so the sign is bit 63 of the integer regardless of byte ordering. Use `DoubleToBits` / `BitsToDouble` for any other bit-level test.

### `Int64` Loop Counters and Lengths on 32-bit Targets

`i386-win32` is a 32-bit target, and only full CI builds it (see
[Workflow — Full CI](workflow.md#full-ci)). FPC rejects a `for` loop whose
counter is wider than the target's word, so an `Int64` counter compiles on
every 64-bit target and fails on `i386` with `Error: Ordinal expression
expected`. Keep `for` counters `Integer` (or `SizeInt`) and do any 64-bit
arithmetic on the bounds outside the loop header.

Sizes need the same care: `SetLength`, `Move` and array indexes take
`SizeInt`, which is 32 bits there, and FPC works out an array's or string's
byte size in `SizeInt` too, so an `Int64` length, or a count times its
element size, past `High(SizeInt)` can wrap to a small allocation in a build
without range checks. Size a native allocation whose count comes from input
through `CheckedNativeByteSize` (`Goccia.NativeLimits`), which refuses
anything past `MAX_NATIVE_ALLOCATION_BYTES`, the most one allocation may ask
for on the target; the heap snapshot reader refuses such a count, and an
input that large, before it reserves anything.

### `WideChar in [set]` Allocates

Under `{$mode delphiunicode}` a set test on a `WideChar` (`Ch in ['0'..'9']`)
converts the character through the widestring manager (`fpc_uchar_to_char`),
which allocates and frees a temporary string on every test. A scanner that does
this per source character makes several heap allocations per character, which
is slow natively and very slow under heaptrc and Valgrind (the JSX transformer
did 226,196 allocations for a 36,028-character source). Use `CharInSet(Ch,
['0'..'9'])`, or compare code points (`(Ch >= '0') and (Ch <= '9')`), in any
loop over source text. Likewise, append a scanned character to a string or
builder directly, or copy a range of the source, instead of building a
one-character string per character.

## Fuzzing and Memory Safety

`GocciaFuzzHarness` drives a single input through lex, parse, bytecode
compilation, and the VM under tight instruction, timeout, memory, and stack bounds. It exists
because the engine is a from-scratch, manually memory-managed implementation
whose stated purpose is running adversarial input — see
[VISION.md](../../VISION.md).

The harness classifies every outcome the engine models — parse error, runtime
error, script throw, instruction limit, timeout, memory limit, denied module —
as **normal**, and exits `0`. Only an outcome the engine does *not* model
(an unexpected Pascal exception, an access violation, a heap abort) exits
nonzero. That is what makes a nonzero exit a finding rather than noise.

### Building and running

```bash
./build.pas fuzzharness
./build/GocciaFuzzHarness --verbose path/to/input.js
```

Read from stdin with `-`. `--verbose` prints the classification;
without it the harness is silent and communicates only through its exit code,
which is what `afl-fuzz` needs.

### Seed corpus

```bash
bun run scripts/build-fuzz-corpus.ts --verbose
```

Seeds are derived from `fixtures/`, `tests/`, and `examples/`, deduplicated by
content hash and capped at 8 KiB, plus a small set of synthetic seeds for
shapes the repo's own tests avoid by construction (deep nesting, unterminated
literals, mixed dialect features). Set `TEST262_PATH` to include a strided
sample of a local test262 checkout:

```bash
TEST262_PATH=../test262 bun run scripts/build-fuzz-corpus.ts
```

### Fuzzing locally

AFL++ needs a coverage-instrumented harness, and FPC has no AFL compiler, so
the instrumentation comes from the assembler. On x86-64 Linux with AFL++
installed (`afl++` on Debian and Ubuntu):

```bash
mkdir -p build/compiled/targets/fuzzharness-afl
fpc @config.cfg -O- -gl -Ct -Cr -Sa -B -Aas -FDscripts/afl \
  -FUbuild/compiled/targets/fuzzharness-afl \
  -obuild/GocciaFuzzHarnessAfl source/app/GocciaFuzzHarness.dpr
afl-fuzz -i build/fuzz/corpus -o build/fuzz/out -- ./build/GocciaFuzzHarnessAfl @@
```

`-Aas` makes FPC write GNU assembly and `-FD` makes it run
[`scripts/afl/as`](../../scripts/afl/as) as its assembler. That script reshapes
FPC's section directives and jump labels into the form AFL++'s `afl-as`
recognises, and `afl-as` adds a coverage trampoline at every function entry,
branch target and conditional branch, plus the fork server. Set `AFL_PATH`
when `afl-as` is not in `/usr/lib/afl`. FPC's runtime library stays
uninstrumented. AFL keeps every mutant that reaches new coverage, so the queue
grows past the seeds; `afl-showmap` on two different inputs shows different
maps. Both builds come from the same source with the same checks, so either
replays a finding.

### Reproducing a finding

A reproducer from CI or from `build/fuzz/out/default/crashes/` replays directly:

```bash
./build/GocciaFuzzHarness --verbose build/fuzz/out/default/crashes/id:000000,...
```

The run is deterministic — same input, same classification, same exit code.
The harness prints a backtrace on the finding path. On Linux the frames carry
file and line. On macOS FPC emits DWARF into a separate `.dSYM` it does not
read back, so frames print as bare addresses; resolve them with:

```bash
atos -o build/GocciaFuzzHarness 0x102d69c6c
```

To check the fault path itself is intact — useful when a fuzz run comes back
suspiciously clean — inject a fault:

```bash
./build/GocciaFuzzHarness --self-test-fault
```

That must print a backtrace and exit `1`.

### Modules are denied, deliberately

The harness installs a content provider that refuses every module load. A fuzz
input must not be able to reach the host filesystem — without this,
`import "/etc/passwd"` would be a file read driven by attacker-shaped input.
Inputs containing `import` are therefore classified `module-denied`, and the
harness does not exercise the module loader.

### The heap snapshot reader

The reader of [heap snapshots](../heap-snapshots.md) takes hostile input
too ([ADR 0148](../adr/0148-heap-snapshots-at-idle-points.md)), and the
harness fuzzes it in a mode of its own. A snapshot names the engine build
that wrote it and a reader refuses another build's, so the seeds come from
the harness itself:

```bash
./build/GocciaFuzzHarness --write-heap-snapshot-seeds build/fuzz/heap-snapshot-corpus
afl-fuzz -i build/fuzz/heap-snapshot-corpus -o build/fuzz/heap-snapshot-out \
  -- ./build/GocciaFuzzHarnessAfl --heap-snapshot @@
```

Four of the seeds hold closures and suspended generators, and one holds
built-ins the run changed (their differences). Functions are named
by the bytecode of the module that compiled them, so the harness runs the
seeds' program before it restores an input, and then uses what restored: it
walks up to 600 distinct objects, reading properties, calling functions and
resuming, returning or throwing into generators (the input's size picks
which). A refusal (`snapshot-refused`) is a normal outcome, as are the run limits and
a script error (`script-throw`); anything else is a finding. The scheduled
workflow runs this as its `fuzz-heap-snapshot` job. It replays the seeds, a
sample of what AFL kept and an input the reader refuses under a heaptrc build
of the harness, and each must end with no unfreed block: like the application
runners, the harness shuts its runtime down before it exits, which destroys
what the collector still manages.

Its `heap-snapshot-hardening` job runs
`Goccia.HeapSnapshot.Hardening.Test` with
`GOCCIA_HEAP_SNAPSHOT_HARDENING=full`, which sweeps every payload value and
every reference target and makes its chains 1,000,000 links long (pull
requests run a rotating share and 200,000;
[Heap Snapshots — Testing](../heap-snapshots.md#testing)), and every
`Goccia.HeapSnapshot*.Test` program under heaptrc. Freed blocks are not kept
there (`keepreleased` grows past a runner's memory within a minute), so
heaptrc catches double frees and writes past a block. The job does not judge
heaptrc's exit leak report, which counts what the test programs leave to
process exit; the hardening program's leak test checks the heap between
passes instead.

`TGarbageCollector.CollectBeforeEveryReservation` is a test switch: every
external reservation (`TryReserveExternalBytes`) runs the last-resort
collection first, as one over the memory ceiling would, so a value the
reader holds only in a local is freed at once. The hardening program sets it
only while the reader runs; engine creation and some lazy built-in
initialisers are not safe under it yet
([#1816](https://github.com/frostney/GocciaScript/issues/1816)).

### Memory safety

The scheduled [fuzz workflow](../../.github/workflows/fuzz.yml) runs the full
JavaScript suite under two tools that answer different questions:

| Tool | Catches | Invocation |
|------|---------|-----------|
| **heaptrc** (`-gh`) | FPC-level leaks, double frees, unfreed blocks with allocation sites | `mkdir -p DIR && fpc @config.cfg -gh -gl -FUDIR -oBIN source/app/GocciaTestRunner.dpr` |
| **Valgrind memcheck** | Invalid reads/writes the allocator never sees, uninitialised values | `valgrind --tool=memcheck --error-exitcode=42 --suppressions=scripts/valgrind/fpc-rtl.supp ./build/GocciaTestRunner -P --jobs=1 --timeout=55m tests` |

`DIR` is a unit-output directory of your own (CI uses
`build/compiled/targets/testrunner-heaptrc`). Give the heaptrc build its own
unit-output directory per the
[per-program `-FU` rule](#shared--fu-directories-across-programs--internal-error-200611011) —
and create the directory first, because `fpc` writes into a `-FU` directory but
will not create one; against a missing directory it fails with
`Can't create object file: … (error code: 2)` (on external-assembler targets
such as macOS the message is `Can't create assembler file: …` instead).

`scripts/valgrind/fpc-rtl.supp` suppresses memcheck's report on the Free
Pascal runtime's `IndexByte` and `IndexWord`, whose aligned 16-byte loads can
read past the end of a buffer and ignore what they find there. The file's
header explains the over-read and what the suppression gives up, including the
undefined values it hides inside those two routines.

Valgrind runs one thread of a process at a time. Under `--jobs` the pool's
workers wait for each other, and its watchdog abandons workers that are only
waiting, so run memcheck with `--jobs=1`. One process for the whole suite took
58.6 minutes on a fast workstation, so CI splits the suite into one shard per
CPU, balanced by each file's native time
(`bun scripts/valgrind/shard-suite.ts`), and runs one single-worker Valgrind
process per shard side by side. Memcheck slowed the files that take longest
natively by 68 to 109 times (measured with one worker on 2026-10-19), so the
step gives each file the default 30-second budget times 110: `--timeout=55m`.

heaptrc costs about 0.6 µs per allocation and free, so a file that allocates
per character or per digit is slow under it; see
[`WideChar in [set]` Allocates](#widechar-in-set-allocates).

Both run on Linux only, and neither runs per-PR. Findings upload as artifacts
with a 30-day retention.

### GC-stress build

A missing GC root is a value that only a Pascal local holds while guest code
runs. It is freed only if a collection falls inside that window, and an
ordinary build collects only under memory pressure, so it usually is not.
`./build.pas --gc-stress` makes the window certain:

| Defined by `-dGOCCIA_GC_STRESS` | Effect |
|---|---|
| `Goccia.VM.pas` | The memory-pressure checkpoint in the dispatch loop runs every 8 instructions instead of every 1,025 and forces a full collection each time. |
| `Goccia.GarbageCollector.pas` | heaptrc keeps every freed block out of reuse (`keepreleased`). heaptrc fills a freed block with `$F0`, so a stale reference faults on its next use. heaptrc still halts on a double free, but the exit leak report is skipped: leaks are the memory-safety job's concern. |

`--gc-stress` adds `-gh` and the define to the development flags. It compiles
into `build/compiled/gc-stress/` and writes binaries to `build/gc-stress/`, so
it never shares FPC units with an ordinary build. It cannot be combined with
`--prod`. Without the define, both units compile to the same machine code as
before.

```bash
./build.pas --gc-stress testrunner test262runner
./build/gc-stress/GocciaTestRunner -P tests/built-ins/Promise --timeout=0
```

Pass `--timeout=0`: under stress, `tests/language/functions/function-parameters.js`
alone takes about 50 seconds.

The scheduled [GC-stress workflow](../../.github/workflows/gc-stress.yml) runs
every Sunday at 04:41 UTC and on demand. `scripts/gc-stress.ts` runs the
GC-sensitive test262 directories (Promise, Iterator, the async-function and
await directories, eval code, arguments objects and generators) and `tests/`
paths under the stress build and under an ordinary development build of the
same commit. It reports only tests that pass in the ordinary build and fail
under stress. Each `tests/` file runs in its own process, because an
engine-integrity fault stops a whole test-runner run. To reproduce the whole
comparison locally, build both variants and point the script at a test262
checkout:

```bash
./build.pas testrunner test262runner
./build.pas --gc-stress testrunner test262runner
bun scripts/gc-stress.ts --suite-dir=../test262 --jobs=8
```

The report goes to `build/gc-stress-report/report.md`. A stress-only failure
fails the job. On a scheduled or `main` run, it also opens one tracking issue
or comments on the open one (`.github/scripts/report-gc-stress.sh`). Runs on
other branches only print what they would file.

### GC stress at every reservation

The GC-stress build collects at instruction boundaries. Native code also
collects at a reservation: making a string, a buffer or an error, or growing
element or property storage past the gate, collects first when the request
would cross the `--max-memory` ceiling. Two switches on the collector make
that certain in a test, in any build:

| Switch | Effect |
|---|---|
| `CollectBeforeEveryReservation` | Every `TryReserveExternalBytes` and every gated growth (`RequireNativeBytes`) runs the full collection a request over the ceiling runs. |
| `QuarantineSwept` | A sweep keeps what it would free, intact. A later collection that reaches a kept object counts a violation instead of reading freed memory: `QuarantineViolations`, with the first one's class (`FirstQuarantineViolation`) and the collection that swept it (`FirstQuarantineViolationSweptIn`, a `TotalCollections` value). Turning it off frees what it kept. |

`Goccia.GCStress.Test` sets both and runs engine creation, every lazy global
and its members, and the native construction paths issue #1816 found
unrooted. It takes a few seconds, so it runs per PR in the Pascal unit-test
loop, and the scheduled memory-safety job (`fuzz.yml`) runs it again under
heaptrc and under Valgrind memcheck. A swept value that is stored again
reports a violation; one that native code only reads afterwards (a key list,
an operand) does not, so the memcheck run sets `GOCCIA_GC_STRESS_FREE=1`,
which frees what each sweep takes, and memcheck reports the read. That build
loads `cmem` first (`fpc -Facmem`): FPC's own heap keeps a freed block as
valid memory, so memcheck sees a read of it only when the C allocator owns
it.
`GOCCIA_GC_STRESS_ONLY=<text>` runs only the cases whose name contains the
text.

To sweep a wider workload, set both switches in `TGarbageCollector.Create`
in a local build of `GocciaTestRunner` or `GocciaTest262Runner` and run each
file in its own process with one worker. A violation names the class that was
swept while something still held it; running the file again and stopping in
the collection that swept it (when `TotalCollections` is one less) shows the
native frame that was holding it.

The quarantine misses a value native code only reads after the sweep. For
those, set only `CollectBeforeEveryReservation`, build the runner with
`-Facmem`, and run each file (or each test262 directory with one worker)
under memcheck with `--trace-children=yes`. Each report names the read, the
allocation and the collection that freed the value. Sweeping `tests/` and
parts of test262 this way found the windows that #1816's quarantine sweeps
missed.

A window opens only when guest code allocates while a builtin holds a fresh
value, and test262's getters, `valueOf` methods and traps rarely allocate. A
probe that does finds more: call every method and getter of each builtin with
receivers (a number held in a register, a primitive through `.call`, a
Proxy) and arguments whose coercions, getters, callbacks and traps make a
fresh string first (`({ valueOf() { (s + "x").slice(0, -1); return 2; } })`),
under the same `-Facmem` build and memcheck. Giving the
primitives' prototypes an allocating getter or a Proxy in their chain
reaches the wrappers builtins make with `ToObject`. Issue #1834's probes
found the unrooted number receiver of every native call (and of a callable
Proxy's `apply` trap), the receiver of a Proxy `get` trap, `fill`'s wrapper
and coerced value, Intl options given as a primitive, and the key lists of
`Object.assign`, `Object.getOwnPropertySymbols` and `Promise.allKeyed` this
way.

## Depth and Fuzz Probe

`scripts/depth-probe.ts` runs depth and nesting shapes against a `GocciaRunner`
binary you choose: Proxy nests forwarding each internal method, Proxy handler
chains, native-trap nests, deep prototype chains, deep recursion, deeply nested
values through `JSON`, `structuredClone`, `join` and `flat`, deep syntactic
nesting, and recovery after a caught `RangeError`. Where the fuzz harness asks
whether one input breaks the engine, the probe asks where its depth limits are
and what happens past them.

It is **manual-only**. Nothing in CI, Lefthook, or the nightly runs it. Run it
when you change recursion, stack limits, Proxy forwarding, the parser's
nesting, or the collector, and when you investigate a depth crash.

```bash
./build.pas --prod runner
bun scripts/depth-probe.ts build/GocciaRunner            # the whole catalog
npx tsx scripts/depth-probe.ts build/GocciaRunner --list  # probes, growth class, default depths
bun scripts/depth-probe.ts build/GocciaRunner --probe=proxy-getOwnPropertyDescriptor --depth=30000
bun scripts/depth-probe.ts build/GocciaRunner --fuzz --count=200 --keep
bun scripts/depth-probe.ts build/GocciaRunner --fuzz --seed=1234 --index=17
```

Probe production and development builds separately: the production build
compiles without FPC's stack checking (`-Ct`), so a native stack overflow that
the development build often reports as a `RangeError` or as
`Fatal error: Stack overflow` is a segmentation fault there.

**Every probe process is memory-capped and timed out by default** (2 GiB,
60 s; `--memory` and `--timeout` change them). Exponential shapes and
uncollected garbage have taken a runner past 8 GB and 27 GB, and the kernel OOM
killer then picks processes across the whole host. The cap is a systemd user
scope (`MemoryMax`, `MemorySwapMax=0`) when `systemd-run --user` can apply it,
else an address-space rlimit (`ulimit -v`). Prefer the systemd cap where both
work. An address-space limit counts reserved as well as used memory, and a
runner refused an allocation does not always fail cleanly, so under the rlimit
an `Out of memory`, an access violation or a silent exit 217 is reported as
`memory-cap`. Confirm such an ending under the systemd cap or a larger
`--memory`. Where neither cap works the tool refuses to run unless you pass
`--no-memory-cap`; that flag never turns off a cap that works.
Probe processes also run with core dumps minimised, so a crash records a few
kilobytes instead of the runner's heap.

Each run ends in one of these outcomes:

| Outcome | Meaning | Fails the run |
|---------|---------|---------------|
| `completed` | The probe returned a value | No |
| `error` | An error the engine models (a caught `RangeError`, an uncaught parse-time `SyntaxError`, ...), with its constructor and message | No |
| `timeout` | No result within the timeout | No |
| `memory-cap` | Killed by, or refused memory under, the cap | No |
| `fatal` | The runner printed `Fatal error: ...` | Yes |
| `crash` | Killed by a signal such as `SIGSEGV`, or an FPC runtime error | Yes |
| `unexpected` | Any other ending, including a wrong answer after a caught error | Yes |

Probes are labelled by how their cost grows with depth. Linear probes default
to depths 1,000 and 30,000 (`--depths`); quadratic and exponential ones run at
small depths (`--quadratic-depths`, `--exponential-depths`). A few probes fix
their own depths, and for `recover-recursion` the depth is a repeat count;
`--list` shows them, and `--depth` overrides them too. Each fuzz program's
exponential layers share a depth budget, `--max-exponential-depth`, so stacking
layers cannot multiply past it. An exponential probe at a large depth is
expected to end in `timeout` or `memory-cap`; the cap is what keeps that from
reaching the host.

`--fuzz` composes the catalog's shapes and depths pseudo-randomly. It prints
the seed and each program's identity (`fuzz:<seed>:<index>`) and shape, and
every finding comes with the exact command that replays it. `--keep` writes
each crash, fatal error, unexpected ending, timeout and memory-cap program to
`tmp/depth-probe/` (or `--out`) with that command in its header. Replay a
kept file with that command, or run it under `GocciaRunner` inside the same
`systemd-run` memory cap: it was kept because it crashed, hung or ran out of
memory. Pass extra runner options, such as
`--runner-arg=--max-stack=0`, to probe a configuration other than the default.
