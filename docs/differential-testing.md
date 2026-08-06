# Differential Testing

*Three-runtime battery comparison: goccia interpreted, goccia bytecode, and bun as the ECMAScript oracle.*

## Executive Summary

- **Three runtimes per file** — every battery under `scripts/differential/` runs under `GocciaTestRunner`, `GocciaTestRunner --mode=bytecode`, and `bun test`
- **Two invariants** — mode parity on pass/fail counts, and equality of the *failed-test name sets* between goccia and bun
- **Names, not counts** — two runtimes can fail the same number of different tests, so the oracle comparison diffs names
- **A timeout is a divergence** — a hang is a finding, never an infrastructure error
- **Run with**: `bun run scripts/test-cli-differential.ts`

## The two invariants

Each battery file produces three verdicts. A file is reported as divergent when
either invariant breaks.

**1. Mode parity.** The interpreter and the bytecode VM must report identical
pass and fail counts for the same file. A break here means the two execution
paths disagree with each other, independent of what the correct answer is.

**2. Bun as oracle.** The set of *failed test names* under goccia must equal the
set under `bun test`. Bun stands in for ECMAScript/Vitest semantics: whatever it
fails, goccia should fail, and nothing more. Comparing counts alone would let a
goccia-only failure and a bun-only failure cancel out, so the harness diffs the
names and reports the two directions separately as `goccia-only fails:` and
`bun-only fails:`.

The oracle comparison is skipped for a battery named `*.goccia.test.js`. Those
files use goccia-only globals such as `mock` and `spyOn` that bun's runner does
not provide, so only mode parity is enforced for them.

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

A battery uses only the `describe`/`test`/`expect` globals that all three
runtimes inject. A `.test.ts` battery works in both because bun transpiles
TypeScript natively while goccia parses annotations as types-as-comments.

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
