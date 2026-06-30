#!/usr/bin/env bun
/**
 * test-benchmark-compare.ts
 *
 * Unit tests for scripts/benchmark-compare.js — the PR "Benchmark Results"
 * comment builder. Locks the range-overlap classifier and proves the issue
 * #815 failure mode: when the PR and `main` ranges overlap (the realistic
 * same-runner case), a benchmark untouched by the diff reads as `unchanged`,
 * not improved/regressed — even when its point-estimate delta is positive.
 *
 * Run: bun run scripts/test-benchmark-compare.ts
 */

import { createRequire } from "node:module";

const require = createRequire(import.meta.url);
const {
  classify,
  medianVariance,
  buildBenchmarkComparison,
} = require("./benchmark-compare.js");

let passed = 0;

function assert(condition: unknown, message: string): void {
  if (!condition) throw new Error(`Assertion failed: ${message}`);
  passed++;
}

function assertEqual<T>(actual: T, expected: T, message: string): void {
  if (actual !== expected) {
    throw new Error(`${message}\n  expected: ${JSON.stringify(expected)}\n  actual:   ${JSON.stringify(actual)}`);
  }
  passed++;
}

// A benchmark entry: opsPerSec point estimate plus the min/max range across rounds.
const bench = (
  suite: string,
  name: string,
  opsPerSec: number,
  min: number,
  max: number,
  variancePercentage = 0,
) => ({ suite, name, opsPerSec, minOpsPerSec: min, maxOpsPerSec: max, variancePercentage });

const report = (fileName: string, benchmarks: object[]) => ({
  files: [{ fileName, benchmarks }],
  totalBenchmarks: benchmarks.length,
});

// -- classify(): pure range overlap -----------------------------------------------

console.log("classify() range overlap...");
{
  // PR range entirely above main range → improved.
  assertEqual(
    classify({ minOpsPerSec: 100, maxOpsPerSec: 110 }, { minOpsPerSec: 120, maxOpsPerSec: 130 }),
    "improved",
    "PR min above main max is improved",
  );
  // PR range entirely below main range → regressed.
  assertEqual(
    classify({ minOpsPerSec: 120, maxOpsPerSec: 130 }, { minOpsPerSec: 100, maxOpsPerSec: 110 }),
    "regressed",
    "PR max below main min is regressed",
  );
  // Overlapping ranges → unchanged noise.
  assertEqual(
    classify({ minOpsPerSec: 100, maxOpsPerSec: 130 }, { minOpsPerSec: 110, maxOpsPerSec: 140 }),
    "unchanged",
    "overlapping ranges are unchanged",
  );
  // No main entry → new.
  assertEqual(
    classify(undefined, { minOpsPerSec: 110, maxOpsPerSec: 140 }),
    "new",
    "missing main entry is new",
  );
}

// -- Issue #815: same-runner overlap must NOT misclassify an untouched benchmark ---

console.log("issue #815: untouched benchmark reads as unchanged...");
{
  // Mirrors the real PR #805 evidence: a regex-only change reported arraybuffer
  // (which uses no regex) as "improved" purely from cross-runner offset. With a
  // same-runner `main` build the ranges overlap, so it reads as unchanged even
  // though the point estimate ticked up +2.5%.
  const main = { opsPerSec: 120000, minOpsPerSec: 111000, maxOpsPerSec: 127000 };
  const pr = { opsPerSec: 123000, minOpsPerSec: 110000, maxOpsPerSec: 127052 };
  assertEqual(classify(main, pr), "unchanged", "same-runner overlap is unchanged, not improved");
}

// -- medianVariance(): honest noise floor from measured data -----------------------

console.log("medianVariance()...");
{
  const data = report("benchmarks/a.js", [
    bench("a", "x", 100, 90, 110, 2),
    bench("a", "y", 100, 90, 110, 6),
    bench("a", "z", 100, 90, 110, 4),
    bench("a", "err-skipped", 0, 0, 0, 0), // zero variance is ignored
  ]);
  assertEqual(medianVariance(data), 4, "median of {2,4,6} is 4");
  assertEqual(medianVariance(null), null, "null data yields null noise");
}

// -- buildBenchmarkComparison(): with a same-runner main build ---------------------

console.log("buildBenchmarkComparison() with main build...");
{
  const interpData = report("benchmarks/arraybuffer.js", [
    bench("arraybuffer", "create", 123000, 110000, 127052, 5),
    bench("arraybuffer", "brand new", 5000, 4900, 5100, 3),
  ]);
  const interpMain = report("benchmarks/arraybuffer.js", [
    bench("arraybuffer", "create", 120000, 111000, 127000, 5),
    // "brand new" intentionally absent from main → should render as 🆕.
  ]);
  const bytecodeData = report("benchmarks/arraybuffer.js", [
    bench("arraybuffer", "create", 162000, 140000, 190000, 8),
  ]);
  const bytecodeMain = report("benchmarks/arraybuffer.js", [
    bench("arraybuffer", "create", 160000, 152000, 164000, 8),
  ]);

  const { body } = buildBenchmarkComparison({
    interpData,
    interpMain,
    bytecodeData,
    bytecodeMain,
    runnerLabel: "ubuntu-latest x64",
  });

  assert(body.includes("PR vs same-runner `main` build"), "header names the same-runner main build");
  assert(body.includes("main → PR"), "table columns show main → PR direction");
  assert(body.includes("Typical per-run noise"), "noise floor line is present");
  assert(body.includes("docs/adr/0076"), "footnote references the ADR");
  assert(body.includes("🆕"), "benchmark missing from main renders as new");
  // The untouched 'create' benchmark overlaps → must read as unchanged overlap,
  // never a green improvement.
  assert(body.includes("~ overlap"), "overlapping benchmark renders as unchanged overlap");
  assert(!body.includes("🟢 1 improved"), "no spurious improvement from same-runner overlap");
}

// -- buildBenchmarkComparison(): no main build to compare --------------------------

console.log("buildBenchmarkComparison() without main build...");
{
  const interpData = report("benchmarks/arraybuffer.js", [bench("arraybuffer", "create", 123000, 110000, 127052)]);
  const bytecodeData = report("benchmarks/arraybuffer.js", [bench("arraybuffer", "create", 162000, 140000, 190000)]);

  const { body } = buildBenchmarkComparison({
    interpData,
    interpMain: null,
    bytecodeData,
    bytecodeMain: null,
  });

  assert(body.includes("no same-runner `main` build to compare"), "states that no main build was available");
  assert(!body.includes("Δ"), "no delta column without a main build");
  // The footnote must not claim a comparison that did not happen (issue #815 review).
  assert(!body.includes("back-to-back"), "no-main footnote does not claim a back-to-back comparison");
}

console.log(`\nbenchmark-compare: ${passed} assertions passed.`);
