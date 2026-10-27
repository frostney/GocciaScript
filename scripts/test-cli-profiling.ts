#!/usr/bin/env bun
/** Allocation profiling through the real loader and host-run benchmarks.
 * Also called by test-cli-apps.ts, so the CLI CI gate runs these regressions.
 */
import { readFileSync, writeFileSync } from "fs";
import { join, resolve } from "path";
import { BENCHRUNNER, RUNNER } from "./test-cli/binaries";
import { clean, mkdtemp } from "./test-cli/tmpdir";

type FunctionProfile = { name: string; calls: number; allocations: number };

export function verifyAllocationProfiles(): void {
  const tmp = mkdtemp("goccia-profiling-");
  try {
    for (const host of ["loader", "benchmark"] as const) {
      let previousAllocations = 0;
      for (const count of [1, 100]) {
        const source = join(tmp, `${host}-${count}.mjs`);
        const output = join(tmp, `${host}-${count}.json`);
        writeFileSync(source, [
          ...(host === "benchmark" ? ['import { bench } from "goccia:microbench";'] : []),
          "const allocateObjects = () => {",
          "  const objects = [];",
          ...Array.from({ length: count }, (_, index) => `  objects.push({ value: ${index} });`),
          "  return objects;",
          "};",
          "const nestedAllocation = () => ({ nested: true });",
          "const nestedThrow = () => { throw { failed: true }; };",
          "const workload = () => {",
          "  [0].map(nestedAllocation);",
          "  try { [0].map(nestedThrow); } catch {}",
          "  return allocateObjects();",
          "};",
          ...(host === "benchmark" ? [
            'bench("allocations", workload);',
            "const afterAwait = () => ({ resumed: true });",
            "const setupAllocation = () => ({ setup: true });",
            "const teardownAllocation = () => ({ teardown: true });",
            'bench("generator and async", {',
            "  *run() {",
            "    setupAllocation();",
            "    try {",
            "      yield async () => { await Promise.resolve(); return afterAwait(); };",
            "    } finally { teardownAllocation(); }",
            "  },",
            "}.run);",
          ] : ["workload();"]),
        ].join("\n"));
        const proc = Bun.spawnSync([
          resolve(host === "benchmark" ? BENCHRUNNER : RUNNER), source,
          "--profile=all", `--profile-output=${output}`,
          ...(host === "benchmark"
            ? ["--profile-deterministic", "--no-progress", "--format=compact-json"]
            : ["--output=json"]),
        ], { stdout: "pipe", stderr: "pipe", timeout: 120_000 });
        if (proc.exitCode !== 0)
          throw new Error(`${host} profiling exited ${proc.exitCode}: ${proc.stderr.toString()}\n${proc.stdout.toString()}`);
        if (host === "benchmark") {
          const report = JSON.parse(proc.stdout.toString());
          const benchmarks = report.files?.[0]?.benchmarks;
          if (benchmarks?.length !== 2 || benchmarks.some((bench: { iterations: number }) => bench.iterations !== 1))
            throw new Error("Deterministic benchmark must run exactly once");
        }
        const profile = JSON.parse(readFileSync(output, "utf-8"));
        const names = ["allocateObjects", "nestedAllocation", "nestedThrow"];
        if (host === "benchmark") names.push("afterAwait", "setupAllocation", "teardownAllocation");
        for (const name of names) {
          const fn = profile.functions?.find((entry: FunctionProfile) => entry.name === name);
          if (fn?.calls !== 1 || !Number.isSafeInteger(fn.allocations) || fn.allocations <= 0)
            throw new Error(`${host}: ${name} must record one call and positive allocations: ${JSON.stringify(fn)}`);
          if (name === "allocateObjects") {
            if (fn.allocations < count || fn.allocations <= previousAllocations)
              throw new Error(`${host}: allocations must grow with objects created: ${fn.allocations} after ${previousAllocations}`);
            previousAllocations = fn.allocations;
          }
        }
        if (host === "benchmark") {
          // The JSON report omits functions with no calls after ResetCounts.
          if (profile.functions.some((entry: FunctionProfile) => entry.name === "<module>"))
            throw new Error("Deterministic profiling must exclude module registration allocations");
        }
      }
    }
  } finally {
    clean(tmp);
  }
}

type TimedProfile = { name: string; calls: number; selfTimeNs: number; totalTimeNs: number };

/** Recursive workloads whose recursion runs in nested frames: direct and
 * mutual recursion, recursion through a native callback, `yield*` and
 * `await`. None is a proper tail call, which would end the caller's frame
 * first. Each recursive function dominates its script, so a total counted
 * once per recursive frame exceeds `<module>`'s total several times over. */
/** `calls` names each recursive function with its expected call count, or
 * `undefined` where the count is not pinned. */
const RECURSIVE_SCRIPTS: Record<string, { source: string[]; calls: Record<string, number | undefined> }> = {
  direct: {
    source: [
      "const fib = (n) => n < 2 ? n : fib(n - 1) + fib(n - 2);",
      "fib(18);",
    ],
    calls: { fib: 8361 },
  },
  mutual: {
    source: [
      "const down = (n) => n === 0 ? 0 : 1 + up(n - 1);",
      "const up = (n) => n === 0 ? 0 : 1 + down(n - 1);",
      "let total = 0;",
      "for (const i of Array.from({ length: 40 })) total += down(100);",
    ],
    calls: { down: 2040, up: 2000 },
  },
  callback: {
    source: [
      "const build = (depth) => ({ value: depth, children: depth === 0 ? [] : [build(depth - 1), build(depth - 1)] });",
      "const tree = build(9);",
      "const sumTree = (node) => node.value + node.children.map(sumTree).reduce((a, b) => a + b, 0);",
      "sumTree(tree);",
    ],
    calls: { sumTree: 1023 },
  },
  generator: {
    source: [
      "const build = (depth) => ({ value: depth, children: depth === 0 ? [] : [build(depth - 1), build(depth - 1)] });",
      "const tree = { *walk(node) { yield node.value; for (const child of node.children) yield* tree.walk(child); } };",
      "let total = 0;",
      "for (const value of tree.walk(build(9))) total += value;",
    ],
    calls: { walk: undefined },
  },
  async: {
    source: [
      "const spin = () => { let s = 0; for (const i of Array.from({ length: 50 })) s += 1; return s; };",
      "const work = async (n) => { spin(); return n === 0 ? 0 : 1 + await work(n - 1); };",
      "let total = 0;",
      "for (const i of Array.from({ length: 10 })) total += await work(40);",
    ],
    // 410 calls, plus 400 resumptions after an await, which count as calls.
    calls: { work: 810 },
  },
};

/** Total time counts each function once per outermost activation (#1941):
 * no function's total exceeds `<module>`'s, which contains every call, and
 * the self times, which partition the profiled time, add up to it. */
export function verifyRecursiveTotals(): void {
  const tmp = mkdtemp("goccia-profiling-totals-");
  try {
    for (const [label, { source, calls }] of Object.entries(RECURSIVE_SCRIPTS)) {
      const script = join(tmp, `${label}.js`);
      const output = join(tmp, `${label}.json`);
      writeFileSync(script, source.join("\n") + "\n");
      const proc = Bun.spawnSync([
        resolve(RUNNER), script, "--profile=functions", `--profile-output=${output}`, "--output=json",
      ], { stdout: "pipe", stderr: "pipe", timeout: 120_000 });
      if (proc.exitCode !== 0)
        throw new Error(`${label}: profiling exited ${proc.exitCode}: ${proc.stderr.toString()}\n${proc.stdout.toString()}`);
      const functions: TimedProfile[] = JSON.parse(readFileSync(output, "utf-8")).functions ?? [];
      const modules = functions.filter((entry) => entry.name === "<module>");
      if (modules.length !== 1 || modules[0].calls !== 1)
        throw new Error(`${label}: expected one <module> call: ${JSON.stringify(modules)}`);
      const moduleTotal = modules[0].totalTimeNs;
      const selfSum = functions.reduce((sum, entry) => sum + entry.selfTimeNs, 0);
      // Every frame here runs inside <module>'s, so its self times partition
      // <module>'s total exactly; the slack only absorbs timer granularity.
      const slack = 1_000;
      if (Math.abs(selfSum - moduleTotal) > slack)
        throw new Error(`${label}: self times sum to ${selfSum} ns, <module> totals ${moduleTotal} ns`);
      for (const entry of functions) {
        if (entry.totalTimeNs > moduleTotal + slack)
          throw new Error(`${label}: ${entry.name} totals ${entry.totalTimeNs} ns, more than <module>'s ${moduleTotal} ns`);
        if (entry.totalTimeNs < entry.selfTimeNs)
          throw new Error(`${label}: ${entry.name} totals ${entry.totalTimeNs} ns, less than its self time ${entry.selfTimeNs} ns`);
      }
      for (const [name, expectedCalls] of Object.entries(calls)) {
        const entry = functions.find((candidate) => candidate.name === name);
        if (!entry || (expectedCalls !== undefined && entry.calls !== expectedCalls))
          throw new Error(`${label}: ${name} must record ${expectedCalls ?? "some"} calls: ${JSON.stringify(entry)}`);
        if (entry.calls < 2 || entry.totalTimeNs <= 0)
          throw new Error(`${label}: ${name} must recurse and record time: ${JSON.stringify(entry)}`);
      }
    }
  } finally {
    clean(tmp);
  }
}

if (import.meta.main) {
  verifyAllocationProfiles();
  console.log("Allocation profiling checks passed (loader, benchmark, nested calls, exceptions, async, generators).");
  verifyRecursiveTotals();
  console.log("Recursive total-time checks passed (direct, mutual, callback, generator, async).");
}
