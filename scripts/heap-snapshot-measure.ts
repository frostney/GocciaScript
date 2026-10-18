#!/usr/bin/env bun
/**
 * heap-snapshot-measure.ts
 *
 * Measures the heap snapshot prototype (docs/spikes/heap-snapshot-prototype.md)
 * against replaying the same program from its start.
 *
 * MANUAL USE ONLY: a prototype measurement tool, not wired into CI.
 *
 *   bun scripts/heap-snapshot-measure.ts workloads <probe> [options]
 *     For every workload: runs it uninterrupted (the replay), takes a
 *     snapshot at its pause point, compresses the snapshot, restores it in a
 *     fresh process and checks that the output before the pause plus the
 *     output after the restore equals the uninterrupted output.
 *     <probe> is a GocciaHeapSnapshotProbe binary (./build.pas heapsnapshotprobe;
 *     --prod for timings).
 *       --reps=N          repetitions per mode (default 5); medians are reported
 *       --awfy-dir=DIR    are-we-fast-yet/benchmarks/JavaScript: adds AWFY
 *                         programs that pause after a number of iterations
 *       --only=a,b        run only these workloads
 *       --json=FILE       also write the raw results as JSON
 *
 *   bun scripts/heap-snapshot-measure.ts census <dir>
 *     Summarizes the live-class census the test runner writes with
 *     GOCCIA_HEAP_CENSUS=<dir> ./build/GocciaTestRunner -P tests: at each
 *     sampled await point and idle point, could the prototype's codecs cover
 *     every class alive?
 */

import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

type Stats = Record<string, number | string>;

interface Workload {
  name: string;
  file: string;
  pause: number;
  compat?: boolean;
  note: string;
}

interface ProbeResult {
  status: number | null;
  stdout: string;
  stats: Stats[];
  stderr: string;
}

const ROOT = resolve(import.meta.dir, "..");

const median = (values: number[]): number => {
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2;
};

const runProbe = (probe: string, args: string[]): ProbeResult => {
  const proc = spawnSync(probe, args, { encoding: "utf8", timeout: 600_000, maxBuffer: 64 * 1024 * 1024 });
  const stats: Stats[] = [];
  for (const line of (proc.stderr ?? "").split("\n")) {
    if (line.startsWith("stats ")) stats.push(JSON.parse(line.slice(6)));
  }
  return { status: proc.status, stdout: proc.stdout ?? "", stats, stderr: proc.stderr ?? "" };
};

const failure = (result: ProbeResult): string =>
  result.stderr.split("\n").filter((l) => l && !l.startsWith("stats ")).join(" ").slice(0, 300);

const builtinWorkloads = (): Workload[] => {
  const dir = join(ROOT, "fixtures/heap-snapshot/workloads");
  return [
    { name: "fixture", file: join(ROOT, "fixtures/heap-snapshot/paused.mjs"), pause: 1, note: "closures, async function between awaits, .then chain, class with private field, Map, typed array" },
    { name: "agent-loop@1", file: join(dir, "agent-loop.mjs"), pause: 1, note: "agent loop: history, tool closures, class tasks, suspended generator inside for-of" },
    { name: "agent-loop@3", file: join(dir, "agent-loop.mjs"), pause: 3, note: "the same, third host call" },
    { name: "top-level-await", file: join(dir, "top-level-await.mjs"), pause: 1, note: "module body suspended by a top-level await" },
    { name: "lazy-builtins", file: join(dir, "lazy-builtins.mjs"), pause: 1, note: "references to lazily materialized built-ins (JSON, Temporal)" },
    { name: "ssr", file: join(dir, "ssr.mjs"), pause: 1, note: "React-like render, one async component suspended, Promise.all" },
    { name: "markdown", file: join(dir, "markdown.mjs"), pause: 1, note: "markdown/MDX pipeline: 60 documents, ASTs, RegExps, embeds waiting on props" },
    { name: "json-data", file: join(dir, "json-data.mjs"), pause: 1, note: "4,000 orders through JSON, indexed in Maps, waiting on a rate" },
    { name: "image-text", file: join(dir, "image-text.mjs"), pause: 1, note: "192x192 blur and Sobel in typed arrays, word counts, waiting on a threshold" },
  ];
};

// A variant written to a temporary file: the same program at another size.
const scaledWorkload = (tmp: string, base: Workload, name: string, from: string, to: string, note: string): Workload => {
  const source = readFileSync(base.file, "utf8");
  if (!source.includes(from)) throw new Error(`${base.file} does not contain ${from}`);
  const file = join(tmp, `${name}.mjs`);
  writeFileSync(file, source.replace(from, to));
  return { ...base, name, file, note };
};

// AWFY programs with a pause: K iterations of the benchmark, then a host call,
// then one more verified iteration. Replay redoes the K iterations; the paused
// heap holds only what the benchmark object keeps.
const awfyWorkloads = (tmp: string, awfyDir: string): Workload[] => {
  // eslint-disable-next-line @typescript-eslint/no-require-imports
  const driver = require(join(ROOT, "scripts/awfy-driver.js"));
  // [name, module, iterations before the pause, inner iterations per call]
  const programs: Array<[string, string, number, number]> = [
    ["Richards", "richards", 20, 1],
    ["DeltaBlue", "deltablue", 20, 1],
    ["Havlak", "havlak", 1, 1],
    ["CD", "cd", 4, 2],
    ["Json", "json", 20, 1],
  ];
  return programs.map(([name, module, iterations, inner]) => {
    const bundle: string = driver.buildAwfyBundle({ awfyDir, benchmarkName: name, moduleName: module, innerIterations: 1 });
    const cut = bundle.indexOf("const __suite = __awfyRequire(");
    const suiteLine = bundle.slice(cut).split("\n")[0];
    const source = `${bundle.slice(0, cut)}${suiteLine}
const __bench = __suite.newInstance();
let __verified = 0;
for (let __i = 0; __i < ${iterations}; __i += 1) {
  if (__bench.innerBenchmarkLoop(${inner})) __verified += 1;
}
print("before pause", __verified);
const __answer = await hostCall("resume", 0);
print("after pause", __verified + __answer, __bench.innerBenchmarkLoop(${inner}));
`;
    const file = join(tmp, `awfy-${module}.mjs`);
    writeFileSync(file, source);
    return { name: `awfy-${module}`, file, pause: 1, compat: true, note: `AWFY ${name}: ${iterations} iteration(s) before the pause` };
  });
};

const measureWorkload = (probe: string, workload: Workload, reps: number, tmp: string) => {
  const extra = workload.compat ? ["--compat"] : [];
  const snapshotFile = join(tmp, `${workload.name}.ghs`);

  const runs = Array.from({ length: reps }, () => runProbe(probe, ["run", workload.file, ...extra]));
  const bad = runs.find((r) => r.status !== 0);
  if (bad) return { name: workload.name, note: workload.note, error: `run failed: ${failure(bad)}` };
  const reference = runs[0].stdout;
  const replayMs = median(runs.map((r) => Number(r.stats.find((s) => s.event === "pause" && s.index === workload.pause)?.ms)));
  const endMs = median(runs.map((r) => Number(r.stats.find((s) => s.event === "end")?.ms)));

  const snapshots = Array.from({ length: reps }, () =>
    runProbe(probe, ["snapshot", workload.file, snapshotFile, `--pause=${workload.pause}`, ...extra]));
  const badSnapshot = snapshots.find((r) => r.status !== 0);
  if (badSnapshot) {
    return { name: workload.name, note: workload.note, replayMs, endMs, error: `snapshot refused: ${failure(badSnapshot)}` };
  }
  const snap = (key: string) => median(snapshots.map((r) => Number(r.stats.find((s) => s.event === "snapshot")?.[key])));
  const raw = readFileSync(snapshotFile);
  const gzipBytes = Bun.gzipSync(raw, { level: 6 }).byteLength;
  const zstdStart = performance.now();
  const zstdBytes = Bun.zstdCompressSync(raw, { level: 3 }).byteLength;
  const zstdMs = performance.now() - zstdStart;

  const restores = Array.from({ length: reps }, () =>
    runProbe(probe, ["restore", workload.file, snapshotFile, ...extra]));
  const badRestore = restores.find((r) => r.status !== 0);
  if (badRestore) {
    return { name: workload.name, note: workload.note, replayMs, endMs, error: `restore failed: ${failure(badRestore)}` };
  }
  const correct = restores.every((r, i) => snapshots[i % snapshots.length].stdout + r.stdout === reference);
  const rest = (key: string) => median(restores.map((r) => Number(r.stats.find((s) => s.event === "restore")?.[key])));

  return {
    name: workload.name,
    note: workload.note,
    replayMs,
    endMs,
    liveObjects: snap("liveObjects"),
    liveHeapBytes: snap("liveHeapBytes"),
    objects: snap("objects"),
    builtinRefs: snap("builtinRefs"),
    rawBytes: raw.byteLength,
    gzipBytes,
    zstdBytes,
    zstdMs,
    snapshotBuiltinTableMs: snap("builtinTableMs"),
    serializeMs: snap("serializeMs"),
    accountingMs: snap("accountingMs"),
    restoreBuiltinTableMs: rest("builtinTableMs"),
    compiledMs: rest("compiledMs"),
    restoreMs: rest("restoreMs"),
    resumedMs: rest("resumedMs"),
    correct,
  };
};

const kib = (bytes: number) => (bytes / 1024).toFixed(bytes < 10240 ? 1 : 0);
const ms = (value: number) => value.toFixed(value < 10 ? 2 : 1);

const workloadsCommand = (args: string[]) => {
  const probe = resolve(args[0] ?? "");
  if (!args[0] || !statSync(probe, { throwIfNoEntry: false })) {
    console.error("Usage: bun scripts/heap-snapshot-measure.ts workloads <probe> [--reps=N] [--awfy-dir=DIR] [--only=a,b] [--json=FILE]");
    return 2;
  }
  const option = (name: string) => args.find((a) => a.startsWith(`--${name}=`))?.slice(name.length + 3);
  const reps = Number(option("reps") ?? 5);
  const tmp = mkdtempSync(join(tmpdir(), "goccia-heap-snapshot-"));
  try {
    const base = builtinWorkloads();
    const json = base.find((w) => w.name === "json-data")!;
    const workloads = [
      ...base,
      scaledWorkload(tmp, json, "json-data-x4", "length: 4000", "length: 16000", "the same with 16,000 orders"),
    ];
    const awfyDir = option("awfy-dir");
    if (awfyDir) workloads.push(...awfyWorkloads(tmp, awfyDir));
    const only = option("only")?.split(",");
    const selected = only ? workloads.filter((w) => only.includes(w.name)) : workloads;

    const results = selected.map((w) => {
      console.error(`measuring ${w.name}`);
      return measureWorkload(probe, w, reps, tmp);
    });

    console.log(`Probe: ${probe}, ${reps} repetitions per mode, medians.`);
    console.log("");
    console.log("| Workload | Live objects | Live heap KiB | Snapshot objects | Raw KiB | gzip KiB | zstd KiB | Serialize ms | Restore ms | Restore total ms | Replay ms | Replay / restore | Output equal |");
    console.log("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |");
    for (const r of results as Array<Record<string, unknown>>) {
      if (r.error) {
        console.log(`| ${r.name} | | | | | | | | | | ${r.replayMs !== undefined ? ms(r.replayMs as number) : ""} | | ${r.error} |`);
        continue;
      }
      const n = (k: string) => r[k] as number;
      console.log(`| ${r.name} | ${n("liveObjects")} | ${kib(n("liveHeapBytes"))} | ${n("objects")} | ${kib(n("rawBytes"))} | ${kib(n("gzipBytes"))} | ${kib(n("zstdBytes"))} | ${ms(n("serializeMs"))} | ${ms(n("restoreMs"))} | ${ms(n("resumedMs"))} | ${ms(n("replayMs"))} | ${(n("replayMs") / n("resumedMs")).toFixed(1)}x | ${r.correct ? "yes" : "NO"} |`);
    }
    const jsonFile = option("json");
    if (jsonFile) writeFileSync(jsonFile, `${JSON.stringify({ probe, reps, results }, null, 2)}\n`);
    return (results as Array<Record<string, unknown>>).some((r) => r.correct === false) ? 1 : 0;
  } finally {
    rmSync(tmp, { recursive: true, force: true });
  }
};

// The classes a snapshot can write (RegisterSnapshotCodec), the primitives it
// writes inline, and the engine objects it names instead of writing.
const CODEC_CLASSES = new Set([
  "TGocciaArrayBufferValue", "TGocciaArrayIteratorValue", "TGocciaArrayValue", "TGocciaBytecodeCell",
  "TGocciaBytecodeFunctionValue", "TGocciaBytecodeGeneratorObjectValue", "TGocciaInstanceValue",
  "TGocciaMapValue", "TGocciaObjectValue", "TGocciaPromiseReactionCapability", "TGocciaPromiseValue",
  "TGocciaSetValue", "TGocciaSymbolValue", "TGocciaTypedArrayValue", "TGocciaVMAsyncAwaitContinuationValue",
  "TGocciaVMClassValue", "TGocciaVMLiteralObjectValue", "TPromiseAllHandler", "TPromiseAllRejectHandler",
  "TPromiseAllState",
]);
const INLINE_CLASSES = new Set(["TGocciaStringLiteralValue", "TGocciaNumberLiteralValue", "TGocciaBooleanLiteralValue", "TGocciaBigIntValue"]);
// Covered only when its callback is registered (promise resolving functions,
// Promise.all element functions); every other native closure refuses.
const CONDITIONAL_CLASSES = new Set(["TGocciaNativeFunctionValue"]);
const NAMED_CLASSES = new Set(["TGocciaScope", "TGocciaFunctionConstructorClassValue"]);

const censusCommand = (args: string[]) => {
  const dir = args[0];
  if (!dir) {
    console.error("Usage: bun scripts/heap-snapshot-measure.ts census <dir>");
    return 2;
  }
  const samples: Array<{ file: string; point: string; total: number; classes: Record<string, number> }> = [];
  for (const name of readdirSync(dir)) {
    if (!name.endsWith(".jsonl")) continue;
    for (const line of readFileSync(join(dir, name), "utf8").split("\n")) {
      if (line.trim()) samples.push(JSON.parse(line));
    }
  }
  const kinds = [
    ["await points", (p: string) => p.startsWith("await-")],
    ["module idle", (p: string) => p === "module-idle"],
    ["tests done", (p: string) => p === "tests-done"],
  ] as const;
  console.log("| Point | Samples | Files | All classes have codecs | ... counting native closures as covered | Median live objects |");
  console.log("| --- | ---: | ---: | ---: | ---: | ---: |");
  const blockers = new Map<string, number>();
  for (const [label, match] of kinds) {
    const subset = samples.filter((s) => match(s.point));
    const files = new Set(subset.map((s) => s.file)).size;
    let strict = 0;
    let lenient = 0;
    for (const s of subset) {
      const uncovered = Object.keys(s.classes).filter((c) => !CODEC_CLASSES.has(c) && !INLINE_CLASSES.has(c) && !NAMED_CLASSES.has(c));
      if (uncovered.every((c) => CONDITIONAL_CLASSES.has(c))) lenient += 1;
      if (uncovered.length === 0) strict += 1;
      if (label === "await points") for (const c of uncovered) blockers.set(c, (blockers.get(c) ?? 0) + 1);
    }
    const pct = (n: number) => (subset.length ? `${((100 * n) / subset.length).toFixed(1)}%` : "-");
    console.log(`| ${label} | ${subset.length} | ${files} | ${pct(strict)} | ${pct(lenient)} | ${subset.length ? median(subset.map((s) => s.total)) : "-"} |`);
  }
  console.log("");
  console.log("Classes without a codec, by the number of await-point samples they occur in:");
  console.log("");
  for (const [c, n] of [...blockers.entries()].sort((a, b) => b[1] - a[1]).slice(0, 25)) console.log(`- ${c}: ${n}`);
  return 0;
};

const [command, ...rest] = process.argv.slice(2);
if (command === "workloads") process.exit(workloadsCommand(rest));
else if (command === "census") process.exit(censusCommand(rest));
else {
  console.error("Usage: bun scripts/heap-snapshot-measure.ts workloads <probe> [options] | census <dir>");
  process.exit(2);
}
