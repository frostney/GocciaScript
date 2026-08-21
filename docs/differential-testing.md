# Differential Testing

*Four-runtime differential suite comparison: goccia interpreted, goccia bytecode, vitest as the testing-API oracle, and bun as the ECMAScript oracle.*

## Executive Summary

- **Four runtimes per file** — every differential suite under `scripts/differential/` runs under `GocciaTestRunner`, `GocciaTestRunner --mode=bytecode`, and whichever external runtimes its classification names
- **Two oracles, different jobs** — vitest decides testing-API semantics (matchers, hooks, accounting) because being an exact Vitest drop-in *is* the product; bun decides ECMAScript semantics, and is advisory everywhere else
- **Names *and* counts** — two runtimes can fail the same number of different tests, and can agree on which tests fail while running different numbers of them, so every comparison checks both
- **A timeout is a divergence** — a hang is a finding, never an infrastructure error
- **Run with**: `bun run scripts/test-cli-differential.ts` (after `cd scripts/differential && bun install`)

## Why two oracles

Bun was the lane's only oracle at first, on the assumption that it stands in for
Vitest semantics. A three-way audit of 223 probes measured that assumption and
refuted it: across 178 matcher probes, bun and vitest disagreed on 30 — in both
directions, and on exactly the questions a matcher differential suite exists to
settle (deep equality of errors, `.toThrow()` argument forms, trailing
`undefined` in arrays, prototype handling under `.toStrictEqual()`, `Set` and
`Map` subset matching). A runtime that disagrees with the target one time in six cannot
decide what the target requires.

So the oracles are split by what they are actually authoritative about:

- **Vitest is normative for the testing API.** Goccia's `describe`/`test`/
  `expect` surface targets Vitest as an exact drop-in, which makes Vitest the
  definition of correct for matcher results, hook control flow, and how results
  are counted. It is pinned to an exact version, because an oracle that drifts
  silently redefines what the engine must do.
- **Bun is normative for ECMAScript.** For syntax, module semantics, and
  built-ins, bun is a sound, fast oracle and the testing API is incidental to
  what the differential suite asserts.
- **Bun is advisory for matcher and lifecycle differential suites.** Its
  verdicts are still printed there, marked `~~~ ADVISORY(non-gating)`, because
  drift against the fast proxy is worth seeing. It never changes the exit code.
- **Bun is skipped entirely for mock suites.** `e-mocks.test.js` is classified
  `bun: "skip"`, so no bun column is produced for it at all — an absent verdict,
  not a non-gating one. Its mock surface is goccia-and-vitest-specific, so a bun
  run would report divergence that says nothing about the product target.

Cost is not the reason for the split: vitest runs all of the eligible
differential suites in about a second, in a single process.

## Differential suite classification

Every differential suite is registered in the `CLASSIFICATION` table in
`scripts/test-cli-differential.ts`, which names its kind and the role each
external runtime plays for it. A suite that is not registered is reported as
`UNCLASSIFIED` and fails the lane — adding one forces a deliberate choice of
oracle instead of inheriting a default.

| Differential suite | Kind | Vitest | Bun |
|---|---|---|---|
| `a-typesyntax.test.ts` | language | skip | gate |
| `b-modules.test.js` | language | skip | gate |
| `c-builtins.test.js` | language | skip | gate |
| `d-matchers.test.js` | matcher | gate | advisory |
| `e-mocks.test.js` | mocks | gate | skip |
| `f-lifecycle.test.js` | lifecycle | gate | advisory |
| `g-filehook.test.js` | lifecycle | gate | advisory |
| `h-modulemock.test.js` | mocks | gate | skip |
| `i-modulemock-isolation.test.js` | mocks | gate | skip |
| `j-tsspecifier.test.ts` | language | skip | gate |
| `k-callgenerics.test.ts` | language | skip | gate |
| `l-modulefndecl.test.js` | language | skip | gate |
| `m-nodemods.test.js` | language | skip | gate |
| `n-nodemods.goccia.test.js` | language | skip | skip |
| `o-asynccontext.test.js` | language | skip | gate |
| `p-callintrinsics.test.js` | language | skip | gate |
| `q-reflectconstruct.test.js` | language | skip | gate |

`o-asynccontext.test.js` covers `node:async_hooks` propagation only, and stops
there on purpose. Bun 1.3.14 does not honour the `defaultValue` or `name`
constructor options, returns `undefined` rather than the resource from
`emitDestroy`, models `disable()` as an instance-wide flag instead of Node's
edit of the current context frame, and substitutes `undefined` where Node
forwards `bind`'s call-site receiver — and because bun 1.4.0 fixes the last
two, any assertion in that territory makes the verdict depend on which bun the
harness happens to run under. Gating on bun for any of it would report the
oracle's own gaps or version as GocciaScript divergences, so all of it is
covered against Node's behaviour in `tests/built-ins/AsyncHooks` instead,
where bun is not the oracle. Everything the suite does check — stores
surviving `await`, interleaved chains, several instances at once, `.then` /
`.catch` / `.finally` continuations, `exit`, `enterWith`, bind-time callable
validation, and async-generator resumptions — every bun since 1.3.14 and Node
agree on.

`p-callintrinsics.test.js` covers members named `call`, `apply` and `bind` that
are not the `Function.prototype` intrinsics — static class methods, own and
inherited function properties, and `Reflect.apply` installed as a function's own
`apply` — alongside ordinary intrinsic use. That pairing is the point: the
bytecode VM routes `.call`/`.apply`/`.bind` on a function receiver through call
fast paths, and while those matched on the callee's *name* a user-defined member
was silently redirected into the intrinsic in bytecode mode only. The suite fails
in whichever mode stops distinguishing the two.

`q-reflectconstruct.test.js` covers construction that does not go through the
`new` operator — `Reflect.construct`, a proxy without a construct trap, a bound
class — paired with the `new` spelling of the same class. Instance elements are
what a second construction path drops: fields, private fields, and method
initializers live outside the constructor body, so a path that only runs the
body produces an instance that looks plausible and is missing every declared
field. Interpreted mode did exactly that until the shared Construct operation
was routed into the same instantiation `new` uses, and the suite pins the
newTarget and override-return shapes alongside it.

Two shapes are deliberately absent. A class whose superclass chain reaches a
built-in (`extends Array`, `extends Promise`) is a known interpreted-mode gap —
the shared Construct operation declines it and builds it without instance
elements, while `new` initializes them — so gating on bun would report a gap
already tracked elsewhere. It is pinned in
`tests/built-ins/Reflect/construct/native-chain-instance-elements.js`, which
asserts that all the Construct routes agree with each other so that a partial
fix cannot land unnoticed. `new.target` inside a field initializer is absent for
the opposite reason: node, bun and goccia all agree it is `undefined` as a
script, but under `bun test` 1.4.0 the same class body reports it defined, so
gating would report bun's transpile as a goccia divergence. It is covered
against node in `tests/built-ins/Reflect/construct/instance-elements.js`.

`h-modulemock.test.js` and `i-modulemock-isolation.test.js` are a pair: the
first mocks `./mods/mockable.js` with a `vi.mock` factory, the second mocks
nothing and must still see the real module. Under Vitest both files run in one
`vitest run`, so the pair is a genuine cross-file check of Vitest's per-file
mock registry. Under goccia the harness spawns one process per differential
suite, so goccia's half is trivially isolated — the load-bearing goccia
isolation test is the file pair under `tests/language/modules/`, where the whole
directory runs in a single runner process with parallel worker threads and a
leaking registry would actually surface. The differential suite asserts only the
subset both runtimes agree on; automock, `vi.doMock`, and spread-based partial
mocks are left out because
goccia throws on them by design, and a spread partial mock would pass under
Vitest and fail here — a documented gap, not a divergence worth rediscovering on
every run. See [Testing API](testing-api.md) for the supported surface.

`e-mocks.test.js` imports `vi` from a bare `vitest` specifier, which Vitest
resolves to itself and goccia resolves to its bundled compatibility shim, so the
differential suite gates against Vitest across both goccia modes. Bun stays
skipped: it injects its own `vi` under `bun:test`, but importing the real `vitest` package
from a file run by `bun test` drops bun's injected globals and the file fails on
`describe is not defined`, so there is no bun-runnable spelling of it.

## The three invariants

Each differential suite produces a verdict per runtime. A file is reported as
divergent when a gating invariant breaks.

**1. Mode parity.** The interpreter and the bytecode VM must agree on the set of
*failed test names* for the same file, and on the pass and fail counts. A break
here means the two execution paths disagree with each other, independent of what
the correct answer is. Names come first because equal counts prove nothing: the
two modes can fail the same number of different tests. The counts are still
compared, because a failed-name set cannot reveal a test that ran in one mode and
not the other. The two directions are reported separately as
`MODE-PARITY BROKEN: interp-only fails:` and `bytecode-only fails:`. An oracle is
only consulted once goccia agrees with goccia.

**2. Vitest as the testing-API oracle.** For a differential suite vitest gates,
the set of failed test names under goccia must equal the set under vitest; the pass, fail
and skip counts must match both goccia modes; and the two must agree on whether
the *file* failed. Skip counts matter here in a way they do not elsewhere: the
difference between a test that failed and a test that was never entered is
exactly what hook semantics are about, and a runner that failed two tests where
vitest skipped them has the same pass count and the wrong behaviour.

**3. Bun as the ECMAScript oracle.** For a differential suite bun gates, the
failed-name sets and the pass/fail counts must match, held against the interpreter *and* the
bytecode mode. Bun's human summary reports neither skips nor a file-level
verdict, so those two dimensions are compared for vitest only. Where bun is
advisory the same comparison runs and the same messages are printed, without
affecting the exit code.

## Comparing suite-level errors

A failed `beforeAll`, a failed `afterAll`, or a describe callback that throws is
not a test failure in either runner: vitest keeps such an error out of the test
counts and fails the file, and goccia matches that, tracking it in a
goccia-specific `suiteErrors` field and clearing the file's `ok` flag. Neither
runtime gives the error a test name the other could match, so the harness does
not compare `suiteErrors` at all. It compares the dimension both express: whether
the file failed. A suite error that goccia recorded and vitest did not — or the
reverse — shows up as `vitest file verdict differs`.

## The lifecycle differential suites

`f-lifecycle.test.js` covers hook and describe accounting: the skip cascade under
a failed `beforeAll` (including into nested suites and from an `async` hook that
rejects), a failed `afterAll` leaving already-passed tests alone, and
`beforeEach`/`afterEach` failures failing the test they wrap.

Counts cannot tell a skipped test from one whose body ran and passed, so each
case records what actually executed into a module-level marker array, and a
following suite asserts on the array. A runtime that runs a test body it should
have skipped fails that observation test, which the name-set comparison catches.
`g-filehook.test.js` exists separately because a failing *file-level* `beforeAll`
skips every test in its file, which would leave no room for observation suites;
there the test bodies assert something false on purpose, so a body that ran shows
up as a failed test rather than a skipped one.

One case is deliberately excluded: a describe body that throws. Vitest aborts
collection for the whole file, while goccia registers and runs the sibling
describes. It cannot be written to pass under both current behaviours, so it
stays out until the queued describe-body collection-abort layer lands, at which
point it belongs in `f-lifecycle.test.js`.

## Timeout as divergence

Each file gets a per-runtime timeout of `DIFFRUN_TIMEOUT` seconds (default 60);
the single vitest invocation gets that budget multiplied by the number of files
it runs. A runtime that exceeds it reports `TIMEOUT` and counts as a divergence
rather than as a harness error. This is deliberate: a parser or executor that
never terminates on input the other runtimes complete is exactly the class of
defect this lane exists to surface, and treating it as infrastructure noise would
hide it. The counterpart is that the timeout must stay generous enough that a
slow but healthy run is never mistaken for a hang.

## Differential suite layout

Differential suites live in `scripts/differential/`, alongside the harness that
drives them, with shared import fixtures in `scripts/differential/mods/`. They
are deliberately outside `tests/`: `GocciaTestRunner` scans only the paths it is
given on the command line, so suites that assert not-yet-fixed behavior — or
that hang the engine — never join the main `tests/` run. The same placement
keeps them out of `scripts/check-test-structure.ts`.

A differential suite that is handed to an external runtime uses only the
`describe`/`test`/`expect` and hook globals that every runtime injects; a
suite named `*.goccia.test.js` is the exception, because it deliberately
reaches for goccia-only globals — or asserts behavior that deliberately
diverges from both external runtimes — and it is classified `skip` for both. A suite that needs the mocking API instead imports `vi` from
`vitest`, which each runtime that can run it resolves its own way — Vitest to
itself, goccia to its bundled compatibility shim. A `.test.ts`
suite works under bun because bun transpiles TypeScript natively while goccia
parses annotations as types-as-comments.

### Per-suite goccia flags

The harness passes every suite the same goccia flags (`--source-type=module
--compat-function --no-progress`). A suite that needs a capability the default
profile seals names it in its classification's `gocciaFlags`, and only that
suite gets it — enabling a capability for one file cannot quietly change what
the others are testing. `m-nodemods.test.js` and `n-nodemods.goccia.test.js`
use it for `--allow-node-modules`.

A suite belongs in the bun-gated column only when the flag makes goccia do what
the oracle already does natively. Bun resolves `node_modules` on its own, so
`m-nodemods.test.js` compares two runtimes reading the same committed fixture
under `scripts/differential/mods/nodemods/node_modules/` — a hand-written tree
un-ignored in `.gitignore`, kept deliberately apart from the npm-managed
`scripts/differential/node_modules` so `bun install` cannot prune it. The two
behaviors bun does not share — the `module`-field preference and the named
CommonJS refusal, both in [Module Resolution](module-resolution.md) — are in
the `.goccia.test.js` half instead.

The pinned oracle lives in the same directory: `scripts/differential/package.json`
pins vitest to an exact version, `bun.lock` pins its dependencies, and
`vitest.config.mjs` injects the globals so the suite files need no imports.

## Running the lane

```bash
./build.pas testrunner
cd scripts/differential && bun install && cd ../..
bun run scripts/test-cli-differential.ts
```

Divergent files print a `<<<` marker naming the disagreement, advisory drift
prints a `~~~` marker, the run ends with a divergence count and an advisory
count, and the process exits 1 when any file diverged. A missing vitest install
exits 2 with the install command rather than reporting a false divergence.

| Control | Effect |
|---|---|
| `DIFFRUN_TIMEOUT=<seconds>` | Per-file, per-runtime timeout (default `60`) |
| `GOCCIA_BIN=<path>` | Goccia binary to test (default `./build/GocciaTestRunner`) |
| `DIFFRUN_NODE=<path>` | Node binary that hosts vitest (default `node`) |
| trailing file paths | Restrict the run to the named differential suites instead of the whole directory |

```bash
DIFFRUN_TIMEOUT=25 bun run scripts/test-cli-differential.ts scripts/differential/b-modules.test.js
```

The `cli` job in both workflows installs the pinned oracle and runs the lane with
its defaults on every pull request and on pushes to `main`.
