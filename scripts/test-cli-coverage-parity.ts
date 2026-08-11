#!/usr/bin/env bun
/**
 * test-cli-coverage-parity.ts
 *
 * Coverage must not depend on the execution mode. The interpreter and the
 * bytecode VM run the same program, so `--coverage --coverage-format=json` over
 * the same files has to produce the same file keys and the same hit counts —
 * not merely the same percentages, which can agree while the underlying counts
 * do not.
 *
 * This is a real regression gate, not a formality: a mode-parity check on the
 * summed statement counts is what surfaced an interpreter miscount that the
 * percentage-level checks in CI had reported as green.
 *
 * The comparison covers every istanbul counter the reports carry — `s`
 * (statements), `f` (functions) and `b` (branches) — keyed by counter id, plus
 * the shape of the maps that give those ids meaning. A file whose functions or
 * branches are empty in both modes is also a finding: it means the fixtures
 * stopped exercising that dimension and the check would pass vacuously.
 *
 * Run: bun run scripts/test-cli-coverage-parity.ts
 */

import { readFileSync, rmSync } from "fs";
import { join } from "path";

import { TESTRUNNER } from "./test-cli/binaries";
import { clean, mkdtemp } from "./test-cli/tmpdir";

/**
 * Fixtures chosen to populate every counter kind: statements everywhere,
 * functions from the arrow-heavy suites, branches from the conditionals.
 */
const FIXTURES = [
  "tests/built-ins/Array/array-creation.js",
  "tests/language/statements/if/if-else-statements.js",
  "tests/language/functions/arrow-functions.js",
  "tests/language/statements/switch/switch-statements.js",
] as const;

type CounterMap = Record<string, number>;
type FileCoverage = {
  path: string;
  s?: CounterMap;
  f?: CounterMap;
  b?: Record<string, number[]>;
  statementMap?: Record<string, unknown>;
  fnMap?: Record<string, unknown>;
  branchMap?: Record<string, unknown>;
};

const scratch = mkdtemp("goccia-coverage-parity-");

function collect(bytecode: boolean): Record<string, FileCoverage> {
  const outPath = join(scratch, bytecode ? "bytecode.json" : "interpreted.json");
  const proc = Bun.spawnSync(
    [
      TESTRUNNER,
      ...FIXTURES,
      "--coverage",
      "--coverage-format=json",
      `--coverage-output=${outPath}`,
      "--no-progress",
      ...(bytecode ? ["--mode=bytecode"] : []),
    ],
    { stdout: "pipe", stderr: "pipe" },
  );
  if (proc.exitCode !== 0)
    throw new Error(
      `Coverage run failed (${bytecode ? "bytecode" : "interpreted"}): ${proc.stderr.toString()}`,
    );

  const report = JSON.parse(readFileSync(outPath, "utf8")) as Record<string, FileCoverage>;
  rmSync(outPath, { force: true });
  return report;
}

/** Renders a counter map so a mismatch report names the ids that differ. */
function diffCounters(
  label: string,
  file: string,
  left: CounterMap,
  right: CounterMap,
): string[] {
  const ids = [...new Set([...Object.keys(left), ...Object.keys(right)])].sort();
  return ids
    .filter((id) => left[id] !== right[id])
    .map((id) => `  ${file} ${label}[${id}]: interpreted=${left[id]} bytecode=${right[id]}`);
}

function flattenBranches(branches: Record<string, number[]> | undefined): CounterMap {
  const flat: CounterMap = {};
  for (const [id, counts] of Object.entries(branches ?? {}))
    counts.forEach((count, index) => {
      flat[`${id}.${index}`] = count;
    });
  return flat;
}

try {
  console.log("Coverage mode parity...");

  const interpreted = collect(false);
  const bytecode = collect(true);

  const interpretedKeys = Object.keys(interpreted).sort();
  const bytecodeKeys = Object.keys(bytecode).sort();
  if (interpretedKeys.join("\n") !== bytecodeKeys.join("\n"))
    throw new Error(
      `Coverage file keys differ between modes.\n  interpreted: ${interpretedKeys.join(", ")}\n  bytecode:    ${bytecodeKeys.join(", ")}`,
    );
  if (interpretedKeys.length < FIXTURES.length)
    throw new Error(
      `Expected coverage for at least ${FIXTURES.length} files, got ${interpretedKeys.length}`,
    );

  const mismatches: string[] = [];
  let statementCounters = 0;
  let functionCounters = 0;
  let branchCounters = 0;

  for (const key of interpretedKeys) {
    const left = interpreted[key];
    const right = bytecode[key];

    mismatches.push(...diffCounters("s", key, left.s ?? {}, right.s ?? {}));
    mismatches.push(...diffCounters("f", key, left.f ?? {}, right.f ?? {}));
    mismatches.push(
      ...diffCounters("b", key, flattenBranches(left.b), flattenBranches(right.b)),
    );

    for (const map of ["statementMap", "fnMap", "branchMap"] as const) {
      const leftIds = Object.keys(left[map] ?? {}).sort().join(",");
      const rightIds = Object.keys(right[map] ?? {}).sort().join(",");
      if (leftIds !== rightIds)
        mismatches.push(`  ${key} ${map} ids differ between modes`);
    }

    statementCounters += Object.keys(left.s ?? {}).length;
    functionCounters += Object.keys(left.f ?? {}).length;
    branchCounters += Object.keys(left.b ?? {}).length;
  }

  if (mismatches.length > 0)
    throw new Error(`Coverage differs between modes:\n${mismatches.join("\n")}`);

  // Without these the check could pass on reports carrying no counters at all:
  // diffCounters over two empty maps agrees.
  if (statementCounters === 0)
    throw new Error("No statement counters in the coverage report — fixtures no longer cover statements");
  if (functionCounters === 0)
    throw new Error("No function counters in the coverage report — fixtures no longer cover functions");
  if (branchCounters === 0)
    throw new Error("No branch counters in the coverage report — fixtures no longer cover branches");

  console.log(
    `  ${interpretedKeys.length} files, ${functionCounters} function counters, ${branchCounters} branch counters — identical in both modes.`,
  );
  console.log("\nAll test-cli-coverage-parity.ts tests passed.");
} finally {
  clean(scratch);
}
