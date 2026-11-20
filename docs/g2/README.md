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
`--max-stack=N` (frames; 0 or none is the VM's bound of 200,000) and `--max-memory=`. A spent timeout or
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
