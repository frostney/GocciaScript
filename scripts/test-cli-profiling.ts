#!/usr/bin/env bun
/** Allocation profiling through the real loader and host-run benchmarks.
 * Also called by test-cli-apps.ts, so the CLI CI gate runs these regressions.
 */
import { mkdirSync, readFileSync, writeFileSync } from "fs";
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

type CompileFunction = {
  name: string; sourceFile: string; line: number; column: number;
  lexTimeNs: number; parseTimeNs: number; compileTimeNs: number;
  tokens: number; instructions: number; counters: Record<string, number>;
};

const COMPILE_COUNTERS = [
  "nodesCompiled", "constantEmitAttempts", "constantEvaluationVisits",
  "expressionTypeVisits", "numberProofVisits", "localLookups",
  "localLookupProbes", "upvalueResolutions", "upvalueListProbes",
  "depthResetScans", "parameterNameProbes", "bindingNameLookups",
  "bindingNameProbes",
];

/** --profile=compile: one record per source function and file, parse and
 * compile meeting in it, imported modules included, engine shims left out,
 * and `all` keeping its meaning. */
export function verifyCompileProfiles(): void {
  const tmp = mkdtemp("goccia-compile-profile-");
  try {
    const nodes: number[] = [];
    for (const count of [50, 100]) {
      const dir = join(tmp, `n${count}`);
      mkdirSync(dir);
      writeFileSync(join(dir, "dep.mjs"), "export const twice = (x) => x * 2;\n");
      writeFileSync(join(dir, "main.mjs"), [
        'import { twice } from "./dep.mjs";',
        "const big = (p) => {",
        ...Array.from({ length: count }, (_, i) => `  const v${i} = p + ${i};`),
        `  const { ${Array.from({ length: 8 }, (_, i) => `k${i}`).join(", ")} } = { k0: v0 };`,
        "  return twice(v0) + (k0 ?? 0);",
        "};",
        "console.log(big(1));",
      ].join("\n"));
      const output = join(dir, "profile.json");
      const proc = Bun.spawnSync([resolve(RUNNER), join(dir, "main.mjs"),
        "--profile=compile", `--profile-output=${output}`],
        { stdout: "pipe", stderr: "pipe", timeout: 120_000 });
      if (proc.exitCode !== 0)
        throw new Error(`compile profiling exited ${proc.exitCode}: ${proc.stderr.toString()}`);
      const stdout = proc.stdout.toString();
      for (const heading of ["Compile Profile:", "Compile Profile Functions", "Compile Profile Work", "Compile Profile Nodes by Kind"])
        if (!stdout.includes(heading)) throw new Error(`compile profile text lacks ${heading}:\n${stdout}`);
      const profile = JSON.parse(readFileSync(output, "utf-8"));
      for (const key of ["opcodes", "opcodePairs", "scalarFastPath", "shapeSaturation", "functions", "compile"])
        if (!(key in profile)) throw new Error(`profile JSON lacks ${key}`);
      const compile = profile.compile;
      const files: string[] = compile.files.map((file: { sourceFile: string }) => file.sourceFile);
      if (files.length !== 2 || !files.some((f) => f.endsWith("main.mjs")) || !files.some((f) => f.endsWith("dep.mjs")))
        throw new Error(`compile profile must report main.mjs and dep.mjs only, not engine shims: ${JSON.stringify(files)}`);
      for (const counter of COMPILE_COUNTERS)
        if (!Number.isSafeInteger(compile.counters[counter]))
          throw new Error(`compile counters lack ${counter}`);
      const big = compile.functions.find((fn: CompileFunction) => fn.name === "big");
      if (!big || big.line !== 2 || big.column !== 13 || !big.sourceFile.endsWith("main.mjs"))
        throw new Error(`big must be one record at main.mjs:2:13: ${JSON.stringify(big)}`);
      for (const field of ["lexTimeNs", "parseTimeNs", "compileTimeNs", "tokens", "instructions"] as const)
        if (!(big[field] > 0)) throw new Error(`big must record ${field}: ${JSON.stringify(big)}`);
      if (big.counters.bindingNameLookups < 8)
        throw new Error(`big's pattern must count its binding-name lookups: ${JSON.stringify(big.counters)}`);
      const twice = compile.functions.find((fn: CompileFunction) => fn.name === "twice");
      if (!twice?.sourceFile.endsWith("dep.mjs") || !(twice.compileTimeNs > 0))
        throw new Error(`the imported module's function must be profiled: ${JSON.stringify(twice)}`);
      nodes.push(big.counters.nodesCompiled);
    }
    if (nodes[1] < nodes[0] * 1.8)
      throw new Error(`nodes compiled must grow with the source: ${nodes.join(" -> ")}`);

    // `all` stays opcodes and functions: the compile section is present and empty.
    const source = join(tmp, "n50", "main.mjs");
    const output = join(tmp, "all.json");
    const all = Bun.spawnSync([resolve(RUNNER), source, "--profile=all", `--profile-output=${output}`],
      { stdout: "pipe", stderr: "pipe", timeout: 120_000 });
    if (all.exitCode !== 0) throw new Error(`--profile=all exited ${all.exitCode}: ${all.stderr.toString()}`);
    if (all.stdout.toString().includes("Compile Profile"))
      throw new Error("--profile=all must not print the compile profile");
    const allProfile = JSON.parse(readFileSync(output, "utf-8"));
    if (allProfile.compile?.functions?.length !== 0 || allProfile.compile?.files?.length !== 0)
      throw new Error(`--profile=all must leave the compile section empty: ${JSON.stringify(allProfile.compile)}`);
  } finally {
    clean(tmp);
  }
}

if (import.meta.main) {
  verifyAllocationProfiles();
  console.log("Allocation profiling checks passed (loader, benchmark, nested calls, exceptions, async, generators).");
  verifyCompileProfiles();
  console.log("Compile profiling checks passed (per-function records, imported modules, shims excluded, all unchanged).");
}
