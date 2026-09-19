#!/usr/bin/env bun
/** Allocation profiling through the real loader and host-run benchmarks.
 * Also called by test-cli-apps.ts, so the CLI CI gate runs these regressions.
 */
import { readFileSync, writeFileSync } from "fs";
import { join, resolve } from "path";
import { BENCHRUNNER, LOADER } from "./test-cli/binaries";
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
          resolve(host === "benchmark" ? BENCHRUNNER : LOADER), source,
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

if (import.meta.main) {
  verifyAllocationProfiles();
  console.log("Allocation profiling checks passed (loader, benchmark, nested calls, exceptions, async, generators).");
}
