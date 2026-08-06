# Differential Testing

*Three-runtime battery comparison: goccia interpreted, goccia bytecode, and bun as the ECMAScript oracle.*

## Executive Summary

- **Three runtimes per file** — every battery under `scripts/differential/` runs under `GocciaTestRunner`, `GocciaTestRunner --mode=bytecode`, and `bun test`
- **Two invariants** — mode parity between the two goccia modes, and agreement with bun as the oracle
- **Names *and* counts** — two runtimes can fail the same number of different tests, and can agree on which tests fail while running different numbers of them, so every comparison checks both
- **A timeout is a divergence** — a hang is a finding, never an infrastructure error
- **Run with**: `bun run scripts/test-cli-differential.ts`

## The two invariants

Each battery file produces three verdicts. A file is reported as divergent when
either invariant breaks.

**1. Mode parity.** The interpreter and the bytecode VM must agree on the set of
*failed test names* for the same file, and on the pass and fail counts. A break
here means the two execution paths disagree with each other, independent of what
the correct answer is. Names come first because equal counts prove nothing: the
two modes can fail the same number of different tests. The counts are still
compared, because a failed-name set cannot reveal a test that ran in one mode and
not the other. The two directions are reported separately as
`MODE-PARITY BROKEN: interp-only fails:` and `bytecode-only fails:`.

**2. Bun as oracle.** The set of *failed test names* under goccia must equal the
set under `bun test`, and bun's pass and fail counts must match both goccia
modes. Bun stands in for ECMAScript/Vitest semantics: whatever it fails, goccia
should fail, and nothing more. Comparing counts alone would let a goccia-only
failure and a bun-only failure cancel out, so the harness diffs the names and
reports the two directions separately as `goccia-only fails:` and
`bun-only fails:`. Comparing names alone is not enough either — bun can agree on
which tests fail while running a different number of them — so the counts are
held against the interpreter *and* the bytecode mode.

A battery named `*.goccia.test.js` is never executed under bun at all, so both
halves of invariant 2 — the failed-name comparison and the count comparison —
are skipped for it and only mode parity is enforced. Those files use goccia-only
globals such as `mock` and `spyOn` that bun's runner does not provide.

## Timeout as divergence

Each file gets a per-runtime timeout of `DIFFRUN_TIMEOUT` seconds (default 60).
A runtime that exceeds it reports `TIMEOUT` and counts as a divergence rather
than as a harness error. This is deliberate: a parser or executor that never
terminates on input the other runtimes complete is exactly the class of defect
this lane exists to surface, and treating it as infrastructure noise would hide
it. The counterpart is that the timeout must stay generous enough that a slow
but healthy run is never mistaken for a hang.

## Battery layout

Batteries live in `scripts/differential/`, alongside the harness that drives
them, with shared import fixtures in `scripts/differential/mods/`. They are
deliberately outside `tests/`: `GocciaTestRunner` scans only the paths it is
given on the command line, so batteries that assert not-yet-fixed behavior — or
that hang the engine — never join the main suite run. The same placement keeps
them out of `scripts/check-test-structure.ts` and the vitest configuration,
both of which are scoped to `tests/`.

A battery that is compared against bun uses only the `describe`/`test`/`expect`
globals that all three runtimes inject; the `*.goccia.test.js` batteries are
exempt, because they are the ones that deliberately reach for goccia-only
globals and are never handed to bun. A `.test.ts` battery works in both because
bun transpiles TypeScript natively while goccia parses annotations as
types-as-comments.

## Running the lane

```bash
./build.pas testrunner
bun run scripts/test-cli-differential.ts
```

Divergent files print a `<<<` marker naming the disagreement, the run ends with
a divergence count, and the process exits 1 when any file diverged.

| Control | Effect |
|---|---|
| `DIFFRUN_TIMEOUT=<seconds>` | Per-file, per-runtime timeout (default `60`) |
| `GOCCIA_BIN=<path>` | Goccia binary to test (default `./build/GocciaTestRunner`) |
| trailing file paths | Restrict the run to the named batteries instead of the whole directory |

```bash
DIFFRUN_TIMEOUT=25 bun run scripts/test-cli-differential.ts scripts/differential/b-modules.test.js
```

The `cli` job in both workflows runs the lane with its defaults on every pull
request and on pushes to `main`.
