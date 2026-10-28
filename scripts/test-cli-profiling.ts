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
  "localListScans", "parameterNameProbes", "bindingNameLookups",
  "bindingNameProbes", "directEvalRecordProbes", "typeArgumentProbeTokens",
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

    // Function-constructor code is a unit of its own per source: two
    // sources stay apart even at the same positions, and a repeated source
    // adds to its own records.
    const dynamic = join(tmp, "dynamic.js");
    const dynamicOutput = join(tmp, "dynamic.json");
    writeFileSync(dynamic, [
      'const a = new Function("return 1;");',
      'const b = new Function("return 2 + 3;");',
      'const c = new Function("return 1;");',
      'const d = new Function("x", "0;");',
      'const e = new Function("x,0;");',
      'const f = new Function("x//", "//\\0return 1;");',
      'const g = new Function("x//\\0//", "return 1;");',
      "console.log(a() + b() + c(), d(1), e, f, g);",
    ].join("\n"));
    const dyn = Bun.spawnSync([resolve(RUNNER), dynamic, "--unsafe-function-constructor",
      "--profile=compile", `--profile-output=${dynamicOutput}`],
      { stdout: "pipe", stderr: "pipe", timeout: 120_000 });
    if (dyn.exitCode !== 0) throw new Error(`dynamic profiling exited ${dyn.exitCode}: ${dyn.stderr.toString()}`);
    const units: string[] = JSON.parse(readFileSync(dynamicOutput, "utf-8")).compile.files
      .map((file: { sourceFile: string }) => file.sourceFile).filter((name: string) => name.startsWith("<Function>#"));
    if (units.length !== 6)
      throw new Error(`each Function-constructor source must be its own unit: ${JSON.stringify(units)}`);

    // The validation parses of the parameters and the body are units of their
    // own: a nested function keeps one record in the wrapper's unit, at the
    // wrapper's position, even after multi-line parameters.
    const nested = join(tmp, "nested.js");
    const nestedOutput = join(tmp, "nested.json");
    writeFileSync(nested, 'const h = new Function("x\\n", "return () => x;");\nconsole.log(h(4)());\n');
    const nest = Bun.spawnSync([resolve(RUNNER), nested, "--unsafe-function-constructor",
      "--profile=compile", `--profile-output=${nestedOutput}`],
      { stdout: "pipe", stderr: "pipe", timeout: 120_000 });
    if (nest.exitCode !== 0) throw new Error(`nested dynamic profiling exited ${nest.exitCode}: ${nest.stderr.toString()}`);
    const nestedProfile = JSON.parse(readFileSync(nestedOutput, "utf-8")).compile;
    const wrapperUnit = nestedProfile.files.find((file: { sourceFile: string }) => file.sourceFile.startsWith("<Function>#"));
    const arrows = nestedProfile.functions.filter((fn: CompileFunction) =>
      fn.sourceFile === wrapperUnit?.sourceFile && fn.name !== "<module>" && fn.name !== "anonymous");
    if (arrows.length !== 1 || !(arrows[0].parseTimeNs > 0) || !(arrows[0].compileTimeNs > 0))
      throw new Error(`the nested arrow must be one record with its parse and compile: ${JSON.stringify(arrows)}`);
    for (const kind of ["<Function:params>#", "<Function:body>#"])
      if (!nestedProfile.files.some((file: { sourceFile: string }) => file.sourceFile.startsWith(kind)))
        throw new Error(`the validation parse ${kind} must be its own unit: ${JSON.stringify(nestedProfile.files)}`);

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

/** Each counter grows with the source shape that exercises it, so a
 * refactor that stops counting an operation fails here. The shapes are
 * compiled at three sizes, each twice the last: a counter linear in the
 * size (or faster) grows by at least about twice as much over the second
 * step as over the first, whatever constant work it also counts. */
export function verifyCompileCounterGrowth(): void {
  const tmp = mkdtemp("goccia-compile-counters-");
  try {
    const shapes = (n: number): string => {
      const range = Array.from({ length: n }, (_, i) => i);
      return [
        `const chain = (p) => ${range.map(() => "p").join(" + ")};`,
        `const params = (${range.map((i) => `a${i}`).join(", ")}) => a0;`,
        `const pattern = () => { const { ${range.map((i) => `b${i}`).join(", ")} } = {}; return b0; };`,
        `const evalScope = () => { let ${range.map((i) => `e${i}`).join(",")}; eval(""); };`,
        `const constructors = (X) => ${range.map(() => "new X < ").join("")}0;`,
        "const outer = (p) => {",
        ...range.map((i) => `  let x${i} = p + ${i};`),
        "  const inner = () => {",
        ...range.map((i) => `    const y${i} = x${i};`),
        "    return y0;",
        "  };",
        "  return inner;",
        "};",
        "const sw = (d) => {",
        "  switch (d) {",
        ...range.map((i) => `    case ${i}: const s${i} = ${i}; break;`),
        "  }",
        "};",
        "console.log(chain(1), params(1), pattern(), outer(1)(), sw(1));",
      ].join("\n");
    };
    const totals: Record<string, number>[] = [];
    // Every size keeps more than 64 locals in a scope, past the point where
    // lookups switch from a scan to a hashed index.
    for (const n of [80, 160, 320]) {
      const source = join(tmp, `shapes-${n}.js`);
      const output = join(tmp, `shapes-${n}.json`);
      writeFileSync(source, shapes(n));
      const proc = Bun.spawnSync([resolve(RUNNER), source, "--profile=compile", `--profile-output=${output}`],
        { stdout: "pipe", stderr: "pipe", timeout: 120_000 });
      if (proc.exitCode !== 0)
        throw new Error(`counter shapes exited ${proc.exitCode}: ${proc.stderr.toString()}`);
      totals.push(JSON.parse(readFileSync(output, "utf-8")).compile.counters);
    }
    for (const counter of COMPILE_COUNTERS) {
      const [small, medium, large] = totals.map((total) => total[counter]);
      const [first, second] = [medium - small, large - medium];
      if (!(first > 0) || second < first * 1.8)
        throw new Error(`${counter} must count its operation and grow with the source: ${small} -> ${medium} -> ${large}`);
    }

    // The benchmark runner profiles the compile of a benchmark file too.
    const bench = join(tmp, "bench.mjs");
    const output = join(tmp, "bench.json");
    writeFileSync(bench, [
      'import { bench } from "goccia:microbench";',
      "const work = (n) => n * 2;",
      'bench("work", () => work(2));',
    ].join("\n"));
    const proc = Bun.spawnSync([resolve(BENCHRUNNER), bench, "--profile=compile",
      `--profile-output=${output}`, "--no-progress", "--format=compact-json"],
      { stdout: "pipe", stderr: "pipe", timeout: 120_000 });
    if (proc.exitCode !== 0)
      throw new Error(`benchmark compile profiling exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    const work = JSON.parse(readFileSync(output, "utf-8")).compile.functions
      .find((fn: CompileFunction) => fn.name === "work");
    if (!work || !(work.compileTimeNs > 0) || !(work.parseTimeNs > 0))
      throw new Error(`the benchmark runner must profile the benchmark's functions: ${JSON.stringify(work)}`);
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
  verifyCompileProfiles();
  console.log("Compile profiling checks passed (per-function records, imported modules, shims excluded, all unchanged).");
  verifyCompileCounterGrowth();
  console.log("Compile counter checks passed (every counter grows with its shape, benchmark runner).");
  verifyRecursiveTotals();
  console.log("Recursive total-time checks passed (direct, mutual, callback, generator, async).");
}
