# Differential Testing

*Four-runtime battery comparison: goccia interpreted, goccia bytecode, vitest as the testing-API oracle, and bun as the ECMAScript oracle.*

## Executive Summary

- **Four runtimes per file** — every battery under `scripts/differential/` runs under `GocciaTestRunner`, `GocciaTestRunner --mode=bytecode`, and whichever external runtimes its classification names
- **Two oracles, different jobs** — vitest decides testing-API semantics (matchers, hooks, accounting) because being an exact Vitest drop-in *is* the product; bun decides ECMAScript semantics, and is advisory everywhere else
- **Names *and* counts** — two runtimes can fail the same number of different tests, and can agree on which tests fail while running different numbers of them, so every comparison checks both
- **A timeout is a divergence** — a hang is a finding, never an infrastructure error
- **Run with**: `bun run scripts/test-cli-differential.ts` (after `cd scripts/differential && bun install`)

## Why two oracles

Bun was the lane's only oracle at first, on the assumption that it stands in for
Vitest semantics. A three-way audit of 223 probes measured that assumption and
refuted it: across 178 matcher probes, bun and vitest disagreed on 30 — in both
directions, and on exactly the questions a matcher battery exists to settle
(deep equality of errors, `.toThrow()` argument forms, trailing `undefined` in
arrays, prototype handling under `.toStrictEqual()`, `Set` and `Map` subset
matching). A runtime that disagrees with the target one time in six cannot
decide what the target requires.

So the oracles are split by what they are actually authoritative about:

- **Vitest is normative for the testing API.** Goccia's `describe`/`test`/
  `expect` surface targets Vitest as an exact drop-in, which makes Vitest the
  definition of correct for matcher results, hook control flow, and how results
  are counted. It is pinned to an exact version, because an oracle that drifts
  silently redefines what the engine must do.
- **Bun is normative for ECMAScript.** For syntax, module semantics, and
  built-ins, bun is a sound, fast oracle and the testing API is incidental to
  what the battery asserts.
- **Bun is advisory for testing-API batteries.** Its verdicts are still printed
  there, marked `~~~ ADVISORY(non-gating)`, because drift against the fast proxy
  is worth seeing. It never changes the exit code.

Cost is not the reason for the split: vitest runs all of the eligible batteries
in about a second, in a single process.

## Battery classification

Every battery is registered in the `CLASSIFICATION` table in
`scripts/test-cli-differential.ts`, which names its kind and the role each
external runtime plays for it. A battery that is not registered is reported as
`UNCLASSIFIED` and fails the lane — adding one forces a deliberate choice of
oracle instead of inheriting a default.

| Battery | Kind | Vitest | Bun |
|---|---|---|---|
| `a-typesyntax.test.ts` | language | skip | gate |
| `b-modules.test.js` | language | skip | gate |
| `c-builtins.test.js` | language | skip | gate |
| `d-matchers.test.js` | matcher | gate | advisory |
| `e-mocks.goccia.test.js` | mocks | skip | skip |
| `f-lifecycle.test.js` | lifecycle | gate | advisory |
| `g-filehook.test.js` | lifecycle | gate | advisory |

`e-mocks.goccia.test.js` is compared between the two goccia modes only: it
reaches for the `mock` and `spyOn` globals, which neither external runtime
injects. Its upgrade to a three-way battery waits on the planned `goccia:test`
module and shipped vitest-compat shim, which will let it import `vi` from a bare
`vitest` specifier under every runtime.

## The three invariants

Each battery file produces a verdict per runtime. A file is reported as
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

**2. Vitest as the testing-API oracle.** For a battery vitest gates, the set of
failed test names under goccia must equal the set under vitest; the pass, fail
and skip counts must match both goccia modes; and the two must agree on whether
the *file* failed. Skip counts matter here in a way they do not elsewhere: the
difference between a test that failed and a test that was never entered is
exactly what hook semantics are about, and a runner that failed two tests where
vitest skipped them has the same pass count and the wrong behaviour.

**3. Bun as the ECMAScript oracle.** For a battery bun gates, the failed-name
sets and the pass/fail counts must match, held against the interpreter *and* the
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

## The lifecycle batteries

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

## Battery layout

Batteries live in `scripts/differential/`, alongside the harness that drives
them, with shared import fixtures in `scripts/differential/mods/`. They are
deliberately outside `tests/`: `GocciaTestRunner` scans only the paths it is
given on the command line, so batteries that assert not-yet-fixed behavior — or
that hang the engine — never join the main suite run. The same placement keeps
them out of `scripts/check-test-structure.ts`.

A battery that is handed to an external runtime uses only the
`describe`/`test`/`expect` and hook globals that every runtime injects; a
battery named `*.goccia.test.js` is the exception, because it deliberately
reaches for goccia-only globals, and it is classified `skip` for both external
runtimes. A `.test.ts` battery works under bun because bun transpiles TypeScript
natively while goccia parses annotations as types-as-comments.

The pinned oracle lives in the same directory: `scripts/differential/package.json`
pins vitest to an exact version, `bun.lock` pins its dependencies, and
`vitest.config.mjs` injects the globals so battery files need no imports.

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
| trailing file paths | Restrict the run to the named batteries instead of the whole directory |

```bash
DIFFRUN_TIMEOUT=25 bun run scripts/test-cli-differential.ts scripts/differential/b-modules.test.js
```

The `cli` job in both workflows installs the pinned oracle and runs the lane with
its defaults on every pull request and on pushes to `main`.
