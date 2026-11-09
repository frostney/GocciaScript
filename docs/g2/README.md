# G2: an experimental VM and GC for GocciaScript

G2 is a spike: a second engine behind Goccia's front end. It keeps Goccia's lexer, parser and RegExp
compiler (`source/units`) and replaces everything after the AST: compiler, register bytecode, VM, object
model, GC and built-ins (`source/g2`). It answers whether an FPC engine built for speed can match or beat
QuickJS while passing test262.

It is not wired into `build.pas`, the CI or the release, and nothing in Goccia depends on it.

## Status

| Measure | Value |
|---|---|
| test262 (pin 91c70759, contract 4, all six categories) | 82,569 / 103,264 runs (80.0%) |
| JetStream, 6 bundles, wall clock | QuickJS time / G2 time 0.84 (below 1: QuickJS faster) |
| Web Tooling, 18 bundles, wall clock | 1.00, or 0.84 without coffeescript (QuickJS runs it 22x slower) |

Wall clock is best of 3, one engine at a time, on a quiet machine, from `scripts/g2/wallclock.sh`. On the
front-end copy G2 was developed against, the same engine ran JetStream at 0.90 and Web Tooling at 1.22
(1.02 without coffeescript): that copy had parser changes this repository's front end lacks (no line and
column computation per node, spans without reference-counted fields, lazily copied function source text).
Typescript and prettier spend about 1.7x as long here, most of the difference in parsing.

Largest remaining test262 gaps: Temporal (about 13,300 runs including intl402), Intl (about 6,700), RegExp
property escapes and `v` flag, Date, realms (`$262.createRealm`, ShadowRealm), import defer, Atomics.waitAsync.
Largest performance gaps: generators (lazy-collections 0.63), float arithmetic on typed arrays
(gaussian-blur 0.74), string-heavy bundles (source-map 0.81).

## Layout

| Path | Contents |
|---|---|
| `source/g2/` | the engine: `G2.Core` (values, cells, shapes, GC), `G2.Compiler`, `G2.Bytecode`, `G2.VM`, `G2.ObjModel` and the built-in units; `g2.dpr` is the command-line host |
| `source/g2/prelude/` | built-ins written in JavaScript (RegExp's spec layer, Set methods, iterator helpers, disposal, base64); compiled at start-up, snapshotted into the binary by the build |
| `scripts/g2/` | build, gate, gcstress and wall-clock scripts; `run262.py` is the contract-4 test262 runner, `harness/$262.js` its host object |
| `docs/g2/` | this file and [design.md](design.md) |

## Build

G2 builds with FPC 3.3.1 (the toolchain with native TLS threadvars), selected by `fpc-toolchain env 3.3.1` or
any command given in `G2_FPC_ENV`:

```sh
sh scripts/g2/build.sh        # build/g2/g2
build/g2/g2 file.js           # run a script
build/g2/g2 --module file.mjs # run a module
```

The build runs in two stages: the first binary compiles the JS preludes and writes their bytecode, the second
embeds it. It fails when `scripts/g2/tlsscan.py` finds an FPC 3.3.1 native-TLS miscompile (a threadvar
address with an absolute relocation); read a threadvar through a `noinline` accessor or a heap record then.

## Gate

Every change is gated on test262 with no newly failing (id, mode) pair, the 18 Web Tooling digests, and,
for anything near allocation or rooting, a GC stress run:

```sh
G2_TEST262=/path/to/test262 G2_WT_DIR=/path/to/wt-verify sh scripts/g2/gate.sh NEW BASE
G2_WT_DIR=... G2_JS_DIR=/path/to/jetstream sh scripts/g2/gcstress.sh
G2_QJS=/path/to/qjs G2_WT_DIR=... G2_JS_DIR=... scripts/g2/wallclock.sh build/g2/g2 [other-g2]
```

`gate.sh` writes `build/g2/gate/NEW.json` and compares it with `BASE.json`; `gcstress.sh` runs every bundle
with a 256 KB nursery and `--gc-verify`. The digest bundles print `DIGEST n:hash` of each workload's result;
`scripts/g2/digests.txt` holds the expected lines.

## Changes to Goccia's front end

G2 uses `source/units` as it is, with three additions on this branch:

- `Goccia.Values.FunctionBase`: `{$OPTIMIZATION NODFA}` under FPC 3.3.1 and later, whose DFA pass hangs on
  `ConstructValue` at -O3 and above.
- `Goccia.Lexer`: `#` must be followed directly by an identifier name (`# x` is a SyntaxError).
- `Goccia.RegExp.Compiler`: an always-consuming atom repeated 2^30 times or more compiles to a failing match
  instead of an error (no string that long fits in memory).

## Working rules

- A `TValue` held in a Pascal local is stale after any allocation: root it (`PushRoot` / `RootAt`) or re-read
  it from a rooted place. `gcstress.sh` finds the misses.
- Keep strings, dynamic arrays and other managed temporaries out of hot functions and out of `Execute`:
  FPC gives such a function an exception frame and initialize / finalize calls on every call. Put the cold
  path in a `noinline` helper, and check hot functions' disassembly for `fpc_pushexceptaddr`.
- Write a built-in in a prelude when its spec steps are what matter and its speed does not; keep a native fast
  path, guarded on the objects involved being unmodified, where it does.
