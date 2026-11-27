# Tooling

*Auto-formatting, editor configuration, and platform-specific pitfalls for FreePascal development.*

## Executive Summary

- **Auto-formatter** — `./format.pas` auto-fixes uses clauses, PascalCase naming, parameter prefixes, and stray spaces; runs via Lefthook pre-commit hook
- **Editor config** — `.editorconfig` + VSCode/Cursor extensions for zero-config formatting on save
- **Platform pitfalls** — the engine builds with FPC 3.3.1 while the code outside `source/engine` also builds on FPC 3.2.2 and 3.2.4, stale FPC artifacts after branch changes, FPC 3.2.2 `Int64`→`Double` conversion bugs (all platforms + AArch64-specific) and FPC 3.2.4 integer arguments resolving to `Single` overloads, endian-dependent byte indexing, `Int64` loop counters and lengths on 32-bit targets
- **Fuzzing and probes** — `GocciaFuzzHarness` fuzzes single inputs and the scheduled run checks memory safety; `scripts/depth-probe.ts` is a manual, memory-capped probe of the runner's depth limits
- **GC stress** — The engine's binaries take `--gc-stress` and `--gc-verify` at run time and `GOCCIA_NURSERY_KB` shrinks the nursery; `scripts/engine/gcstress.sh` runs the stress sweep

## Auto-Formatting

The project includes `./format.pas`, an instantfpc script that formats Pascal source files. It runs automatically as a pre-commit hook via [Lefthook](https://github.com/evilmartians/lefthook) and can also be invoked manually — no build step needed.

### Setup

Install Lefthook as described in [Workflow — Local setup](workflow.md#local-setup), then register hooks with `lefthook install`.

### Manual Usage

```bash
# Format all project Pascal files
./format.pas

# Format specific files
./format.pas source/units/Goccia.Parser.pas

# Check only (exit 1 if changes needed)
./format.pas --check
```

### What It Enforces

All of the following are **auto-fixed** (not just warned about):

- **Uses clauses**: one unit per line, grouped (System > Third-party > Project > Relative), alphabetically sorted within each group, blank line between groups. Units with an `in` path are always in the Relative group; `Goccia.*` units are Project; known FPC standard library units are System; everything else is Third-party. A uses clause that holds a compiler directive or a comment is left as written, since sorting could separate a comment or `{$IFDEF}` from the unit it belongs to.
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

### Focused Builds and Tests

Use the [Build System's focused workflow](../build-system.md#build-specific-components)
for explicit targets, test selection, parallelism and scoped cleaning.

### FPC 3.2.2 and 3.2.4

CI builds with FPC 3.3.1 from fpc-toolchain (`3.3.1+9`), which the engine in `source/engine` needs (see [Build System — How `build.pas` Works](../build-system.md#how-buildpas-works)). The source outside `source/engine` must keep building on FPC 3.2.2 and 3.2.4 as well. Three RTL interfaces differ between those two, and `Shared.inc` defines a symbol for each on 3.2.2 only:

- `GOCCIA_FPC_CONSTREF_COMPARERS` — `Generics.Defaults` comparers take `constref` arguments on 3.2.2 and `const` on 3.2.4 (and Delphi). Declare a comparison function as `{$IFDEF GOCCIA_FPC_CONSTREF_COMPARERS}constref{$ELSE}const{$ENDIF} A, B: T`.
- `GOCCIA_FPC_CONSTREF_COLLECTION_NOTIFY` — a `Generics.Collections` list's virtual `Notify` takes its value as `constref` on 3.2.2 and `const` on 3.2.4 (and Delphi); an override declares it the same way (`TUnicodeStringList`).
- `GOCCIA_FPC_OPENSSL_DLL_NAMES` — the `openssl` unit's `DLLSSLName` and `DLLUtilName` are `BaseSSLName` and `BaseCryptoName` on 3.2.4.

Workarounds for 3.2.2 compiler bugs stay while 3.2.2 is supported, including those 3.2.4 fixes, such as the inlined threadvar accessor ([#1699](https://github.com/frostney/GocciaScript/issues/1699)).

### Stale FPC Build Artifacts

FPC can report stale compiled state as misleading internal compiler
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
non-zero. The retry already rebuilt that target from an empty unit directory,
so diagnose the reported Pascal source line. Use `--clean` to rebuild selected
targets from empty unit directories on purpose:

```bash
./build.pas --clean loaderbare
./build.pas --clean testrunner
./build.pas --clean tests --test=TimingUtils
```

Switching compilers needs no `--clean` either. FPC gives every build of one
release the same unit version, so it would reuse the units another build of
that release compiled (3.3.1+6's units under 3.3.1+7, miscompiles included).
Each unit directory holds `compiler-identity.txt`, the `fpc -iWD` output of
the compiler that filled it (such as `3.3.1+7 2026/11/24`). When the selected
compiler's differs, or the file is missing, `build.pas` empties that directory
first and prints `Units in <directory> were made by FPC <old>: compiling them
again with <new>`.

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
bun run typecheck
```

Pull-request CI runs the same three checks in its `website` job
([Build System](../build-system.md#pryml--pull-requests)).

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
Result := NumberToString(SomeInt64Value);
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

### AnsiString RTL Calls on Windows

GocciaScript strings are UTF-16, but FPC 3.2's RTL is not Unicode: `TFileStream`
and `FindFirst` take and return AnsiString file names, `TStringList` holds
AnsiStrings, and `WriteLn` converts text to the output's code page. Each of
these converts through the system code page. That is the locale's code page on
Linux and macOS, usually UTF-8, and the ANSI code page (1252 on most systems)
on Windows, where a character outside it comes back as `?`. On Windows the
`ProcessEncoding` unit makes UTF-8 the code page of those conversions and of
the names the RTL returns, and, in a console application, of `Output`,
`StdOut`, `ErrOutput` and `StdErr`, so output redirected to a file or a pipe
is UTF-8 bytes; a GUI application's standard files keep the RTL's settings. When one of those
handles is a console, the `ConsoleOutput` unit replaces the text file's write
function: each flush converts the UTF-8 in the buffer to UTF-16 and writes it
with `WriteConsoleW`, so every `Write` and `WriteLn` to the console shows
characters outside the console's code page, and the code page itself is never
changed. The RTL opens the standard text files again for each new thread, so
this covers the main thread, which writes the output the thread pool's workers
capture. A unit that converts strings through the RTL before `FileUtils` or
`ConsoleOutput` is initialized lists it in its uses. Because the setting is
process-wide, it also applies to an application that embeds the engine. On
Linux and macOS the unit installs `cwstring`, which converts through iconv, and
in front of it a direct conversion for ASCII text to and from UTF-8 or
US-ASCII, which produces the same bytes without an iconv call.

### Starting a Child Process on Unix

FPC's `TProcess` forks and execs without closing the descriptors the child
inherits, and the RTL opens files and `TProcess` creates pipes without
close-on-exec (`FD_CLOEXEC`). Every such descriptor the parent has open at the
fork, in any thread, stays open in the child until it exits. A `TFileStream`
holds an `flock` on its descriptor, so while the child runs, the `flock` of
the parent's next exclusive open of that file (`fmCreate`,
`TStrings.SaveToFile`) is refused with `EAGAIN`, and the open fails with
`EFCreateError`. Its message gives `EAGAIN`'s text, or `EBADF`'s when the RTL
has already closed the refused descriptor; both texts differ by platform. The
parent's end of a child's input pipe is inherited as well, so a child reading
its standard input sees end of file only once every child started while that
end was open has exited.

Start processes with `TChildProcess` from `source/shared/ChildProcess.pas`.
It closes every descriptor above standard error in the child, just before
exec. On Linux 5.9 and later on x86-64, i386 and AArch64 it does so with one
`close_range(2)` call. Elsewhere it closes them one by one, up to the higher
of the descriptor limit (at least 65,536 when unlimited) and the parent's
highest open descriptor, which it lists from `/proc/self/fd` or `/dev/fd`
before the fork. Linux's and macOS's lists are complete; FreeBSD's `/dev/fd`
names only descriptors 0 to 2 unless `fdescfs` is mounted, so there a
descriptor opened above the limit before the limit was lowered can stay open.
Listing the unit in a program's uses also makes `RunCommand` and its
variants create a `TChildProcess`. The child still holds the parent's
descriptors between the fork and that close, so a parent that reopens a file
exclusively right after starting a process can still be refused for that
moment, but no longer for the child's lifetime. Windows needs nothing:
`TProcess` makes only the child's ends of its pipes inheritable there, and the
RTL opens files non-inheritable.

## Fuzzing and Memory Safety

`GocciaFuzzHarness` runs a single input twice, each time on an engine of its
own: once under tight instruction, timeout, memory and frame bounds, which
keep the JIT off and so exercise the parser, the compiler and the interpreter,
and once without the engine's limits, so the baseline JIT and the optimizing
tier compile what gets hot (Linux x86-64 only; a watchdog thread ends that run
with exit `0` at the deadline, since compiled code does not poll it). It exists
because the engine is a from-scratch, manually memory-managed implementation
whose hosts, sandbox mode among them, run untrusted input — see
[VISION.md](../../VISION.md).

The harness classifies every outcome the engine models — parse error, script
throw (a frame-limit `RangeError` included), instruction limit, timeout, memory
limit, host abort, denied module, rejected input (over 1 MB) — as **normal**,
and exits `0`. Only an outcome the engine does *not* model
(an unexpected Pascal exception, an access violation, a heap abort) exits
nonzero. That is what makes a nonzero exit a finding rather than noise.

### Building and running

```bash
./build.pas fuzzharness
./build/GocciaFuzzHarness --verbose path/to/input.js
```

Read from stdin with `-`. `--verbose` prints each run's classification;
without it the harness is silent and communicates only through its exit code,
which is what `afl-fuzz` needs. `--grammar` reads the input's bytes as the
choices of a program generator (`Goccia.Fuzz.Generator`) and compares the two
runs' output; see [Engine — GocciaFuzzHarness](../engine/README.md#gocciafuzzharness) for that mode and
`scripts/engine/grammar-fuzz.py`.

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
the instrumentation comes from the assembler. On x86-64 Linux with `curl`
and access to GitHub, a C compiler and make:

```bash
scripts/afl/build-afl.sh
export PATH="$PWD/build/afl:$PATH"
unset AFL_MAP_SIZE AFL_MAPSIZE
AFL_PATH= GOCCIA_VARIANT=Afl GOCCIA_FPC_FLAGS="-gl -Ct -Cr -Sa -B -Aas -FDscripts/afl" \
  ./build.pas fuzzharness
afl-fuzz -i build/fuzz/corpus -o build/fuzz/out -- ./build/GocciaFuzzHarnessAfl @@
```

`-Aas` makes FPC write GNU assembly and `-FD` makes it run
[`scripts/afl/as`](../../scripts/afl/as) as its assembler. That script reshapes
FPC's section directives and jump labels into the form AFL++'s `afl-as`
recognises, and `afl-as` adds a coverage trampoline at every function entry,
branch target and conditional branch, plus the fork server. FPC's runtime
library stays uninstrumented. AFL keeps every mutant that reaches new
coverage, so the queue grows past the seeds; `afl-showmap` on two different
inputs shows different maps. Both builds come from the same source with the
same checks, so either replays a finding.

AFL++ removed `afl-as` after v4.21c, so current packages (Ubuntu 26.04's
4.33c, for one) have none. `scripts/afl/build-afl.sh` builds it from the
v4.21c release, and `afl-fuzz` and `afl-showmap` from a pinned current one,
each checked against its SHA-256, into `build/afl/`. Current AFL++ still
accepts the fork server `afl-as` adds ("Old fork server model is used by the
target, this still works though").

All three are built with a 1 MiB coverage map in place of AFL++'s 64 KiB,
which the harness's edges fill within minutes of fuzzing, so that far fewer
edges collide and hide new coverage. `afl-as` draws each trampoline's random
edge ID from below its build's `MAP_SIZE`. A target with the original fork
server cannot report its map size, so `afl-fuzz` and `afl-showmap` take one
from their own build: Ubuntu 26.04's packaged `afl-showmap` sizes the map at
its 64 KiB `MAP_SIZE` even when `AFL_MAP_SIZE` asks for more, and v5.03c
starts from its 8 MiB `DEFAULT_SHMEM_SIZE`. `scripts/afl/build-afl.sh` sets
both to 1 MiB, so by default every map is exactly the harness's;
`AFL_MAP_SIZE` or `AFL_MAPSIZE` would still override it, which the recipe
above and `scripts/afl/fuzz.sh` clear. Use the `afl-fuzz` and
`afl-showmap` from `build/afl/`: under a packaged one, the harness can die of
SIGSEGV writing past a 64 KiB map.

`scripts/afl/as` uses `AFL_PATH/afl-as` when `AFL_PATH` is set and not empty,
else `build/afl/afl-as`, else `/usr/lib/afl/afl-as`; the build above empties
`AFL_PATH` so that it takes the `afl-as` just built. The scheduled workflow
runs each campaign through `scripts/afl/fuzz.sh`, which starts `afl-fuzz` in
a fresh output directory, stops it after the given seconds of fuzzing (1 to
3600) and fails a run that ends any other way, unless it stopped on a crash
it saved.

The build is the scheduled workflow's: `build.pas` builds the variant
(`GOCCIA_VARIANT`) in a unit cache of its own, as `build/GocciaFuzzHarnessAfl`,
with `GOCCIA_FPC_FLAGS` after its own options, so it keeps their `-O4`. The
engine calls each procedural threadvar through a local copy: FPC 3.3.1's GNU
assembler output writes a direct call of the variable otherwise, which ld
refuses or which jumps into the thread's data, and the build's threadvar check
(`scripts/engine/tlsscan.py`) names such a call, also when the link fails.

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

The harness installs a module resolver that refuses every specifier before it
probes a path, and a file-read hook (`SetFileReadHook` in `Goccia.Modules`) that
refuses every read. A fuzz input must not be able to reach the host
filesystem — without this, `import "/etc/passwd"` would be a file read driven
by attacker-shaped input. An input that imports is therefore classified
`module-denied`, and the harness does not exercise the module loader.

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

The eight seeds hold primitives, sparse arrays, shared and cyclic graphs,
symbols and built-ins, descriptors and integrity levels, closures with
accessors and bound functions, and errors with wrapper objects. The engine's writer
has no codec for a generator's frame, a Map's table or a buffer's bytes
(`Goccia.HeapSnapshot`), so no seed holds one. A snapshot names the code its
closures were compiled from, so the seeds and the restore run one program:
after restoring an input it walks up to 4,096 distinct objects, at most 12
deep, reading descriptors and prototypes and calling each function in one of
three ways (a plain call, a call with a receiver, or a construction; the
input's size picks which), then stringifies the root. A refusal
(`snapshot-refused`) is a normal outcome, as are the run limits and a script
error (`script-throw`); anything else is a finding. The scheduled workflow runs
this as its `fuzz-heap-snapshot` job. The Pascal hardening programs the
`heap-snapshot-hardening` job names belonged to the previous engine and are
gone.

### Memory safety

The scheduled [fuzz workflow](../../.github/workflows/fuzz.yml) runs the full
JavaScript suite under two tools that answer different questions:

| Tool | Catches | Invocation |
|------|---------|-----------|
| **heaptrc** (`-gh`) | FPC-level leaks, double frees, unfreed blocks with allocation sites | `GOCCIA_VARIANT=Heaptrc GOCCIA_FPC_FLAGS="-gh -gl" ./build.pas testrunner`, which writes `build/GocciaTestRunnerHeaptrc`, then `HEAPTRC=log=build/heaptrc.log ./build/GocciaTestRunnerHeaptrc -P tests` |
| **Valgrind memcheck** | Invalid reads/writes the allocator never sees, uninitialised values | `bash scripts/valgrind/strip-debug-symbols.sh build/GocciaTestRunner`, then `valgrind --tool=memcheck --error-exitcode=42 --suppressions=scripts/valgrind/fpc-rtl.supp ./build/GocciaTestRunner -P --jobs=1 --timeout=55m tests` |

`GOCCIA_VARIANT` gives the build a unit-output directory of its own
(`build/compiled/targets/testrunner-dev-heaptrc`, or `testrunner-heaptrc` with `--prod`), since FPC does not
recompile a unit for new options, and `GOCCIA_FPC_FLAGS` comes after
`build.pas`'s own options, so its flags win. It applies to the engine programs (every application under
`source/app`) and to `tomlcompliancerunner`; CI's fault build uses it with `-dGOCCIA_FAULT_INJECTION`
([Testing](../testing.md#cli-behavior-tests-ci-integration)). `HEAPTRC=log=FILE` matters: FPC
3.3.1's heaptrc leaves its report unflushed when stderr is a regular file, so
`2> FILE` can capture nothing.

`scripts/valgrind/fpc-rtl.supp` suppresses memcheck's report on the Free
Pascal runtime's `IndexByte` and `IndexWord`, and on FPC 3.3.1 also
`CompareByte` and `CompareWord`, whose 16-byte loads can read past the end of
a buffer and ignore what they find there. The file explains the over-read and
what the suppression gives up, including the undefined values it hides inside
those routines. Run `bash scripts/valgrind/strip-debug-symbols.sh <binary>`
first: FPC 3.3.1's debugging symbols otherwise give frames inside these
routines names no suppression can follow.

Valgrind runs one thread of a process at a time. Under `--jobs` the pool's
workers wait for each other, and its watchdog abandons workers that are only
waiting, so run memcheck with `--jobs=1`. One process for the whole suite took
58.6 minutes on a fast workstation on the previous engine, so CI splits the suite into one shard per
CPU, balanced by each file's native time
(`bun scripts/valgrind/shard-suite.ts`), and runs one single-worker Valgrind
process per shard side by side. Memcheck slowed the files that take longest
natively by 68 to 109 times (measured with one worker on 2026-10-19, on the
previous engine), so the
step gives each file the default 30-second budget times 110: `--timeout=55m`.

heaptrc costs about 0.6 µs per allocation and free, so a file that allocates
per character or per digit is slow under it; see
[`WideChar in [set]` Allocates](#widechar-in-set-allocates).

Both run on Linux only, and neither runs per-PR. Findings upload as artifacts
with a 30-day retention. Neither looks inside the engine's own heap: the engine allocates its
cells in its nursery and old-space pages (`Goccia.Core`), so a read of a moved or
freed cell reads memory those pages still own, and a missing root is a
[GC stress](#gc-stress) finding instead.

### GC stress

A missing GC root is a `TValue` that only a Pascal local holds across an
allocation. The engine's collector moves young cells, so the local then points at a
moved or freed cell, but only if a collection falls inside that window, and
with the default 8 MB nursery it usually does not. The engine has no separate stress
build: its binaries make the window certain at run time.

| Switch | Effect |
|---|---|
| `--gc-stress` (`goccia` command line) | Every allocation runs a minor collection, and every eighth minor collection a major one (`GCStress` in `Goccia.Core`). The JIT's and the optimizing tier's inline allocations take the helper path, which allocates through the collector. |
| `--gc-verify` (`goccia` command line) | After each collection, `VerifyHeap` checks that no live old cell points into the nursery or at a freed cell, and the VM checks that a nursery pointer it is about to use (a closure's upvalues, a captured register) still leads to the start of a live cell (`NurseryCellOK`). A minor collection fills the emptied nursery with `$DB`, so a pointer a collection did not update meets a header no cell has. A failure prints the cell and field, then the `Engine fault:` diagnostic, and exits 70. |
| `GOCCIA_NURSERY_KB=<n>` (environment) | Sizes the nursery in KB, so collections run every few kilobytes. `goccia` reads it for its own engine and, with `--test`, for a test file's engine (`EngineNurseryBytes` in `Goccia.Engine`); the test262 host reads it with `GOCCIA_GC_VERIFY` (`Goccia.Test262`). |
| `GOCCIA_ROOTCHECK=1` (environment, `goccia`) | Each native call must leave the temporary root stack as it found it; an imbalance prints the native, then the `Engine fault:` diagnostic, and exits 70. |
| `GOCCIA_HOST=1` (environment, `goccia`) | Installs the hidden `GocciaHost` object, whose `gc()` collects at once. |

```bash
./build.pas engine
GOCCIA_NURSERY_KB=16 build/engine/goccia --gc-verify script.js
GOCCIA_NURSERY_KB=16 build/engine/goccia --test --gc-verify tests/built-ins/Promise/prototype/then.js
```

`scripts/engine/gcstress.sh` runs the whole sweep and writes
`build/engine/gcstress.txt`:

- every Web Tooling digest bundle and JetStream bundle (`GOCCIA_WT_DIR`,
  `GOCCIA_JS_DIR`; the AWFY bundles in `GOCCIA_AWFY_DIR` too, when given, without
  `--gc-verify`) with a 256 KB nursery (the script's argument changes it) and
  `--gc-verify`, each digest compared with `scripts/engine/digests.txt`;
- each regression script in `scripts/engine/regress` with 16, 64 and 128 KB
  nurseries, `--gc-verify` and `GOCCIA_HOST=1`; a first line `// goccia-flags: ...`
  adds options such as `--gc-stress`, and each script prints `ok` last;
- every test file under `tests/` through `scripts/engine/gcstress-tests.py`, which
  runs it with `goccia --test` twice, as is and with a 16 KB nursery and
  `--gc-verify`, and reports a file whose totals differ or whose stressed run
  crashes.

It prints the number of `OK` lines and every `BAD` and `TIMEOUT` line. A
`TIMEOUT` is not a failure: verifying a large heap at every collection is slow,
and the deep-chain tests under `tests/built-ins/Goccia/gc` take longest. Run it
for any change near allocation or rooting, as part of the
[engine gate](../engine/README.md#gate); CI does not run it.

Each collection under stress costs time proportional to the live heap, so a
test that builds N objects costs time proportional to N times the heap. A test
with a large N may read `Goccia.build.gcStress` to shrink N, or to skip a case
whose point is a size past an engine bound
([ADR 0155](../adr/0155-goccia-build-reports-a-gc-stress-build.md)). The engine sets
it from the build define `GOCCIA_GC_STRESS`, which no build defines, so it is
`false` in every build, including a run with `--gc-stress`:

```javascript
// 100,000 nested Proxies: seconds normally, hours under stress.
const deepNestRuns = typeof Goccia !== "undefined" && !Goccia.build.gcStress;
test.runIf(deepNestRuns)("a read through 100,000 nested Proxies throws RangeError", () => {
  // ...
});
```

`./build.pas --gc-stress` built the previous engine's stress variant and now
stops with an error that names these run-time switches. The scheduled
[GC-stress workflow](../../.github/workflows/gc-stress.yml) runs
`scripts/engine/gc-stress-ci.py`: `tests/` through `gcstress-tests.py`, the
regression scripts at 16, 64 and 128 KB nurseries, and the GC-sensitive
test262 directories through `GocciaTest262Runner` plainly and with
`GOCCIA_NURSERY_KB=16 GOCCIA_GC_VERIFY=1`. A test that passes plainly and fails under
stress is a finding.

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

The engine's production and development builds differ only in debug information
(`GocciaArgs` in `build.pas` gives both `-dPRODUCTION`), and the engine's units
compile without FPC's stack checking in both. The engine guards the native
stack itself: a call that native code makes once that stack runs low
(`NativeStackLow`) is refused with `RangeError: Maximum call stack size
exceeded`, so a crash past it is a finding.

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
