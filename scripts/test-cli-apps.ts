#!/usr/bin/env bun
/**
 * test-cli-apps.ts
 *
 * App-specific features: GocciaScriptLoader (JSON output, --global/--globals,
 * JavaScript host environments,
 * coverage, source maps), GocciaScriptLoaderBare (core-engine-only stdin/file
 * execution, CLI print, and runtime-global absence), GocciaBundler (compile,
 * roundtrip, stdin, directory, .gbc rejection, source maps),
 * GocciaBenchmarkRunner (file, stdin, bytecode), GocciaREPL (banner,
 * evaluation, ASI, error recovery, bytecode), GocciaSandboxRunner
 * (seed baselines, sandbox fs, shell, nested execution, diffs).
 */

import { $ } from "bun";
import {
  writeFileSync,
  readFileSync,
  existsSync,
  mkdirSync,
  realpathSync,
  chmodSync,
  rmSync,
  symlinkSync,
} from "fs";
import { join, resolve } from "path";
import { fileURLToPath } from "url";
import {
  LOADER,
  BARE,
  SANDBOXRUNNER,
  REPL,
  TESTRUNNER,
  BUNDLER,
  BENCHRUNNER,
  FUZZHARNESS,
} from "./test-cli/binaries";
import { containsLine, normalizeLineEndings, runLoaderJson } from "./test-cli/assertions";
import { makeTmpFactory, clean } from "./test-cli/tmpdir";
import { runWithPeakRss, assertPeakRssBelow, assertPeakRssAbove } from "./test-cli/rss";

const makeTmp = makeTmpFactory("goccia-apps-");

/**
 * Run one named section, recording a failure instead of aborting the run.
 *
 * This script is a straight sequence of assertions that throw, so the first
 * failure used to end the run and every later section went unexamined. On
 * platforms exercised only in CI that meant one defect per round: during the
 * 0.11.0 stack the Windows job reported a coverage assertion, then — after a
 * fix and another twenty-minute round — a second one 550 lines further down.
 * Collecting failures turns those rounds into one.
 *
 * Output on a green run is unchanged: the header prints, the body runs, and
 * nothing is recorded. A failing section prints its error where it happened
 * and the run continues; every failure is repeated at the end and the process
 * exits non-zero.
 */
const sectionFailures: { name: string; error: unknown }[] = [];

async function section(
  name: string,
  body: () => Promise<void> | void,
): Promise<void> {
  console.log(name);
  try {
    await body();
  } catch (error) {
    sectionFailures.push({ name, error });
    const message = error instanceof Error ? error.message : String(error);
    console.log(`  FAILED: ${message}`);
  }
}


/**
 * Re-key a coverage report by basename.
 *
 * Report keys are canonical coverage paths: repo-relative when the file sits
 * under a repository root, absolute otherwise, always with '/' separators. A
 * test that knows only the native path it passed on the command line cannot
 * reconstruct that key — on Windows the separators differ, and whether the
 * key is relative depends on where the temp directory happens to live. The
 * basename is the one part both spellings agree on. Splitting on both
 * separators keeps the helper honest if a key ever reaches it unnormalized.
 *
 * Basenames are only unique because every fixture in this file gives its
 * sources distinct names. Nothing enforces that, so a later test adding
 * `helpers/shared.js` next to `shared.js` would land both on one key and every
 * assertion after it would silently read whichever the report happened to list
 * last — a passing test measuring the wrong file. Collisions therefore throw
 * here rather than resolving arbitrarily.
 *
 * Null-prototype maps with own-property checks, not `in` on a plain object: a
 * source named `constructor` or `toString` would otherwise inherit a truthy
 * hit and be reported as a collision that never happened, and a `__proto__`
 * key assigned onto a plain object sets the prototype instead of an entry.
 */
function readCoverageByBasename(path: string): Record<string, any> {
  const raw = JSON.parse(readFileSync(path, "utf-8"));
  const byBasename: Record<string, any> = Object.create(null);
  const sources: Record<string, string> = Object.create(null);
  for (const [file, entry] of Object.entries(raw)) {
    const basename = file.split(/[\\/]/).pop() as string;
    if (Object.hasOwn(byBasename, basename)) {
      throw new Error(
        `Coverage report ${path} has two files named ${basename} ` +
          `(${sources[basename]} and ${file}); this helper keys by basename, ` +
          `so give the fixtures distinct names or match on the full key.`,
      );
    }
    byBasename[basename] = entry;
    sources[basename] = file;
  }
  return byBasename;
}

function coverageEntryFor(reportPath: string, sourcePath: string): any {
  const basename = sourcePath.split(/[\\/]/).pop() as string;
  return readCoverageByBasename(reportPath)[basename];
}

const MICROBENCH_MODULE_IMPORT = 'import { bench, group } from "goccia:microbench";';

function microbenchModule(lines: string[]): string {
  return [MICROBENCH_MODULE_IMPORT, ...lines, ""].join("\n");
}

function microbenchModuleWithExports(exports: string, lines: string[]): string {
  return [`import { ${exports} } from "goccia:microbench";`, ...lines, ""].join("\n");
}

async function withFetchTestServer(
  callback: (baseUrl: string) => void | Promise<void>,
): Promise<void> {
  const server = Bun.serve({
    hostname: "127.0.0.1",
    port: 0,
    fetch(request) {
      if (request.method === "HEAD")
        return new Response(null, { status: 200 });
      return new Response("ok", { status: 200 });
    },
  });
  try {
    await callback(`http://127.0.0.1:${server.port}`);
  } finally {
    server.stop(true);
  }
}

async function runLoaderJsonAsync(
  source: string,
  extraArgs?: string[],
  opts?: { timeout?: number },
): Promise<{ exitCode: number | null; json: any; stderr: string }> {
  const hasOutputFlag = extraArgs?.some((a) => a.startsWith("--output="));
  const proc = Bun.spawn(
    [
      LOADER,
      ...(hasOutputFlag ? [] : ["--output=json"]),
      ...(extraArgs ?? []),
    ],
    {
      stdin: "pipe",
      stdout: "pipe",
      stderr: "pipe",
    },
  );
  proc.stdin.write(source);
  proc.stdin.end();

  let timeout: ReturnType<typeof setTimeout> | undefined;
  if (opts?.timeout != null)
    timeout = setTimeout(() => proc.kill(), opts.timeout);
  try {
    const [exitCode, stdout, stderr] = await Promise.all([
      proc.exited,
      new Response(proc.stdout).text(),
      new Response(proc.stderr).text(),
    ]);
    let json: any;
    try {
      json = JSON.parse(stdout);
    } catch (e: any) {
      throw new Error(
        `runLoaderJsonAsync: failed to parse JSON (exitCode=${exitCode}): ${e.message}\nstderr: ${stderr}\nstdout: ${stdout}`,
      );
    }
    return { exitCode, json, stderr };
  } finally {
    if (timeout !== undefined)
      clearTimeout(timeout);
  }
}

function assertValidSourceMap(path: string): void {
  const raw = readFileSync(path, "utf-8");
  const map = JSON.parse(raw);
  if (map.version !== 3) throw new Error(`Source map version should be 3, got ${map.version}`);
  if (!Array.isArray(map.sources) || map.sources.length === 0) throw new Error("Source map should have non-empty sources");
  if (typeof map.mappings !== "string" || map.mappings.length === 0) throw new Error("Source map should have non-empty mappings");
}

function readJsonLines(path: string): any[] {
  return readFileSync(path, "utf-8")
    .split(/\r?\n/)
    .filter(Boolean)
    .map((line) => JSON.parse(line));
}

function assertCommonJsonReport(json: any, label: string, expectedFileCount: number): void {
  if (json.fileName !== undefined) throw new Error(`${label} top-level fileName should be omitted`);
  if (typeof json.build?.version !== "string") throw new Error(`${label} build.version should be present`);
  if (typeof json.build?.date !== "string") throw new Error(`${label} build.date should be present`);
  if (typeof json.stdout !== "string") throw new Error(`${label} stdout should always be present`);
  if (typeof json.stderr !== "string") throw new Error(`${label} stderr should always be present`);
  if (!Array.isArray(json.output)) throw new Error(`${label} output should be an array`);
  if (!Array.isArray(json.files)) throw new Error(`${label} files should be an array`);
  if (json.files.length !== expectedFileCount) throw new Error(`${label} files length should be ${expectedFileCount}, got ${json.files.length}`);
  if (typeof json.timing?.total_ns !== "number") throw new Error(`${label} top-level timing.total_ns should be present`);
  if ("total_ms" in json.timing) throw new Error(`${label} top-level timing should not include millisecond fields`);
  if (typeof json.memory?.gc?.liveBytes !== "number") throw new Error(`${label} top-level memory.gc.liveBytes should be present`);
  if (typeof json.memory?.heap?.endAllocatedBytes !== "number") throw new Error(`${label} top-level memory.heap.endAllocatedBytes should be present`);
  if (typeof json.workers?.used !== "number") throw new Error(`${label} workers.used should be present`);
  if (typeof json.workers?.available !== "number") throw new Error(`${label} workers.available should be present`);
}

function assertCommonJsonFile(file: any, label: string, fileName: string, ok = true): void {
  if (file?.fileName !== fileName) throw new Error(`${label} fileName mismatch: ${file?.fileName}`);
  if ("file" in file) throw new Error(`${label} per-file should not include duplicate "file" alias`);
  if (file?.ok !== ok) throw new Error(`${label} ok should be ${ok}, got ${file?.ok}`);
  if (typeof file?.stdout !== "string") throw new Error(`${label} stdout should always be present`);
  if (typeof file?.stderr !== "string") throw new Error(`${label} stderr should always be present`);
  if (!Array.isArray(file?.output)) throw new Error(`${label} output should be an array`);
  if (typeof file?.timing?.total_ns !== "number") throw new Error(`${label} timing.total_ns should be present`);
  if ("total_ms" in file.timing) throw new Error(`${label} timing should not include millisecond fields`);
  if (ok && file.error !== null) throw new Error(`${label} error should be null`);
}

function assertPreservesBodyFailure(outputPath: string, label: string): void {
  const json = JSON.parse(readFileSync(outputPath, "utf-8"));
  const message = json.files?.[0]?.benchmarks?.[0]?.error;
  if (typeof message !== "string" || !message.includes("body failure") || message.includes("cleanup failure"))
    throw new Error(`${label} should preserve body failure, got ${JSON.stringify(message)}`);
}

// ============================================================================
// GocciaScriptLoader
// ============================================================================

// -- JSON output (interpreted + bytecode) ---------------------------------------

await section("Loader: JSON output (interpreted)...", async () => {
  const { json } = runLoaderJson("console.log('hi'); 2 + 2;\n");
  const file = json.files?.[0];
  if (json.ok !== true) throw new Error(`JSON ok should be true, got ${json.ok}`);
  if (json.fileName !== undefined) throw new Error(`JSON fileName should only be present per-file, got ${json.fileName}`);
  if (file?.result !== 4) throw new Error(`JSON file result should be 4, got ${file?.result}`);
  if (file?.fileName !== "<stdin>") throw new Error(`JSON fileName should be <stdin>, got ${file?.fileName}`);
  if (!json.output?.includes("hi")) throw new Error(`JSON output should contain "hi"`);
  if (!json.stdout?.includes("hi")) throw new Error(`JSON stdout should contain "hi"`);
  if (typeof json.stderr !== "string") throw new Error("JSON stderr should always be present");
  if (typeof json.build?.version !== "string") throw new Error("JSON build.version should be present");
  if (typeof json.build?.date !== "string") throw new Error("JSON build.date should be present");
  if (typeof json.memory?.gc?.liveBytes !== "number") throw new Error("JSON memory.gc.liveBytes should be present");
  if (typeof json.memory?.gc?.allocatedDuringRunBytes !== "number") throw new Error("JSON memory.gc.allocatedDuringRunBytes should be present");
  if (typeof json.memory?.gc?.limitBytes !== "number") throw new Error("JSON memory.gc.limitBytes should be present");
  if ("maxBytes" in json.memory.gc) throw new Error("JSON memory.gc.maxBytes should not be present; use limitBytes");
  if (typeof json.memory?.heap?.endAllocatedBytes !== "number") throw new Error("JSON memory.heap.endAllocatedBytes should be present");
  if (typeof json.workers?.used !== "number") throw new Error("JSON workers.used should be present");
  if (typeof json.timing?.total_ns !== "number") throw new Error("JSON timing.total_ns should be present");
  if ("total_ms" in json.timing) throw new Error("JSON timing should not include millisecond fields");
  if (typeof file?.timing?.total_ns !== "number") throw new Error("JSON per-file timing.total_ns should be present");
  if ("total_ms" in file.timing) throw new Error("JSON per-file timing should not include millisecond fields");
});

await section("Loader: JSON output (bytecode)...", async () => {
  const { json } = runLoaderJson("console.log('hi'); 2 + 2;\n", ["--mode=bytecode"]);
  const file = json.files?.[0];
  if (json.ok !== true) throw new Error(`Bytecode JSON ok should be true, got ${json.ok}`);
  if (file?.result !== 4) throw new Error(`Bytecode JSON file result should be 4, got ${file?.result}`);
  if (!json.output?.includes("hi")) throw new Error(`Bytecode JSON output should contain "hi"`);
  if (!json.stdout?.includes("hi")) throw new Error(`Bytecode JSON stdout should contain "hi"`);
  if (typeof json.stderr !== "string") throw new Error("Bytecode JSON stderr should always be present");
  if (typeof json.memory?.gc?.peakLiveBytes !== "number") throw new Error("Bytecode JSON memory.gc.peakLiveBytes should be present");
});

await section("Loader: bytecode TypedArray.from roots mapper during iterator GC...", async () => {
  const source = `
let i = 0;
const iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        i++;
        Goccia.gc();
        if (i > 2) return { done: true };
        return { done: false, value: { value: i } };
      }
    };
  }
};
const ta = Uint8Array.from(
  iterable,
  ({ mapper(item) { return item.value + this.offset; } }).mapper,
  { offset: 0 },
);
ta[0] * 10 + ta[1];
`;
  const { exitCode, json, stderr } = runLoaderJson(source, ["--mode=bytecode"]);
  if (exitCode !== 0) throw new Error(`TypedArray.from GC repro exited ${exitCode}: ${stderr}`);
  const result = json.files?.[0]?.result;
  if (result !== 12) throw new Error(`TypedArray.from GC repro expected 12, got ${result}`);
});

await section("Loader: JSON undefined result...", async () => {
  const { json } = runLoaderJson("undefined;\n");
  if (json.ok !== true) throw new Error(`JSON undefined run should succeed, got ${json.ok}`);
  if (json.files?.[0]?.error !== null) throw new Error("JSON undefined result should not imply an error");
  if (json.files?.[0]?.result !== null) throw new Error(`JSON undefined result should serialize as null, got ${json.files?.[0]?.result}`);
});

await section("Loader: JSON stdout/stderr split...", async () => {
  const { json } = runLoaderJson("console.log('out'); console.error('err'); 1;\n");
  if (!json.stdout?.includes("out")) throw new Error(`JSON stdout should contain "out", got ${json.stdout}`);
  if (!json.stderr?.includes("err")) throw new Error(`JSON stderr should contain "err", got ${json.stderr}`);
  if (!json.output?.includes("out") || !json.output?.includes("Error: err")) {
    throw new Error(`JSON output should include both streams, got ${json.output}`);
  }
});

await section("Loader: JSON multi-file structure...", async () => {
  const tmp = makeTmp();
  try {
    const first = join(tmp, "first.js");
    const second = join(tmp, "second.js");
    writeFileSync(first, "console.log('first out'); 11;\n");
    writeFileSync(second, "console.error('second err'); 22;\n");

    const proc = Bun.spawnSync([LOADER, "--output=json", "--jobs=2", first, second], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Loader multi-file JSON exited ${proc.exitCode}: ${proc.stderr.toString()}`);

    const json = JSON.parse(proc.stdout.toString());
    assertCommonJsonReport(json, "Loader multi-file JSON", 2);
    if (json.ok !== true) throw new Error(`Loader multi-file top-level ok should be true, got ${json.ok}`);
    if (json.error !== null) throw new Error("Loader multi-file top-level error should be null");
    if (!json.stdout.includes("first out")) throw new Error(`Loader multi-file stdout should include first file output, got ${json.stdout}`);
    if (!json.stderr.includes("second err")) throw new Error(`Loader multi-file stderr should include second file error output, got ${json.stderr}`);
    if (!json.output.includes("first out") || !json.output.includes("Error: second err"))
      throw new Error(`Loader multi-file output should include both streams, got ${json.output}`);
    if (json.workers.used !== 2) throw new Error(`Loader multi-file workers.used should be 2, got ${json.workers.used}`);

    assertCommonJsonFile(json.files[0], "Loader first file", first);
    assertCommonJsonFile(json.files[1], "Loader second file", second);
    if (json.files[0].result !== 11) throw new Error(`Loader first result should be 11, got ${json.files[0].result}`);
    if (json.files[1].result !== 22) throw new Error(`Loader second result should be 22, got ${json.files[1].result}`);
    if (!json.files[0].output.includes("first out")) throw new Error(`Loader first file output mismatch: ${json.files[0].output}`);
    if (!json.files[1].stderr.includes("second err")) throw new Error(`Loader second file stderr mismatch: ${json.files[1].stderr}`);
    if (typeof json.files[0].memory?.gc?.liveBytes !== "number")
      throw new Error("Loader first file worker memory should be present");
    if (typeof json.files[1].memory?.gc?.liveBytes !== "number")
      throw new Error("Loader second file worker memory should be present");
    if (json.memory.gc.allocatedDuringRunBytes <= 0)
      throw new Error("Loader multi-file top-level memory should include worker GC allocations");
    if (json.memory.gc.liveBytes > json.memory.gc.limitBytes * (json.workers.used + 1))
      throw new Error("Loader multi-file top-level live memory should not double-count per-file worker snapshots");
  } finally {
    clean(tmp);
  }
});

await section("Loader: JSON source-load failure stays per-file...", async () => {
  const tmp = makeTmp();
  try {
    const unreadable = join(tmp, "unreadable.js");
    const valid = join(tmp, "valid.js");
    writeFileSync(unreadable, "1;\n");
    writeFileSync(valid, "2 + 2;\n");
    if (process.platform !== "win32") {
      chmodSync(unreadable, 0o000);
      const proc = Bun.spawnSync([LOADER, "--output=json", "--jobs=1", unreadable, valid], {
        stdout: "pipe",
        stderr: "pipe",
      });
      chmodSync(unreadable, 0o600);
      if (proc.exitCode === 0) throw new Error("Unreadable source file should fail the run");
      const json = JSON.parse(proc.stdout.toString());
      const unreadableFile = json.files?.[0];
      const validFile = json.files?.[1];
      if (json.ok !== false) throw new Error(`Unreadable source JSON run should fail, got ${json.ok}`);
      if (unreadableFile?.ok !== false) throw new Error(`Unreadable source file should be marked failed, got ${unreadableFile?.ok}`);
      if (unreadableFile?.fileName !== unreadable) throw new Error(`Unreadable source fileName mismatch: ${unreadableFile?.fileName}`);
      if (typeof unreadableFile?.error?.message !== "string") throw new Error("Unreadable source file should include shared error object");
      if (validFile?.ok !== true || validFile?.result !== 4) throw new Error("Valid file should still run after unreadable source file");
    }
  } finally {
    clean(tmp);
  }
});

await section("Loader: compact-json omits build, memory, stdout, stderr...", async () => {
  const { exitCode, json, stderr } = runLoaderJson("console.log('hi'); console.error('warn'); 2 + 2;\n", ["--output=compact-json"]);
  if (exitCode !== 0) throw new Error(`compact-json exited ${exitCode}: ${stderr}`);
  if ("build" in json) throw new Error("compact-json should omit top-level build");
  if ("memory" in json) throw new Error("compact-json should omit top-level memory");
  if ("stdout" in json) throw new Error("compact-json should omit top-level stdout");
  if ("stderr" in json) throw new Error("compact-json should omit top-level stderr");
  if (json.ok !== true) throw new Error(`compact-json ok should be true, got ${json.ok}`);
  if (!Array.isArray(json.output)) throw new Error("compact-json output should be an array");
  if (!json.output.includes("hi") || !json.output.includes("Error: warn")) {
    throw new Error(`compact-json output should preserve normalized lines, got ${JSON.stringify(json.output)}`);
  }
  if (json.error !== null) throw new Error("compact-json error should be null");
  if (typeof json.timing?.total_ns !== "number") throw new Error("compact-json timing should be present");
  if (typeof json.workers?.used !== "number") throw new Error("compact-json workers should be present");
  if (!Array.isArray(json.files) || json.files.length !== 1) throw new Error("compact-json files should have one entry");
  const file = json.files[0];
  if ("memory" in file) throw new Error("compact-json per-file memory should be omitted");
  if ("stdout" in file) throw new Error("compact-json per-file stdout should be omitted");
  if ("stderr" in file) throw new Error("compact-json per-file stderr should be omitted");
  if ("file" in file) throw new Error("compact-json per-file should not include duplicate \"file\" alias");
  if (file.fileName !== "<stdin>") throw new Error(`compact-json fileName should be <stdin>, got ${file.fileName}`);
  if (file.result !== 4) throw new Error(`compact-json file result should be 4, got ${file.result}`);
  if (typeof file.timing?.total_ns !== "number") throw new Error("compact-json per-file timing should be present");
});

await section("Loader: compact-json error path omits build, memory, stdout, stderr...", async () => {
  const { exitCode, json } = runLoaderJson("throw new Error('boom');\n", ["--output=compact-json"]);
  if (exitCode === 0) throw new Error("compact-json error path should set non-zero exit code");
  if ("build" in json) throw new Error("compact-json error should omit top-level build");
  if ("memory" in json) throw new Error("compact-json error should omit top-level memory");
  if ("stdout" in json) throw new Error("compact-json error should omit top-level stdout");
  if ("stderr" in json) throw new Error("compact-json error should omit top-level stderr");
  if (json.ok !== false) throw new Error(`compact-json error ok should be false, got ${json.ok}`);
  if (json.error?.type !== "Error") throw new Error(`compact-json error type should be Error, got ${json.error?.type}`);
  if (json.error?.message !== "boom") throw new Error(`compact-json error message should be boom, got ${json.error?.message}`);
  const file = json.files?.[0];
  if (!file) throw new Error("compact-json error should still include per-file entry");
  if ("memory" in file) throw new Error("compact-json error per-file memory should be omitted");
  if ("stdout" in file) throw new Error("compact-json error per-file stdout should be omitted");
  if ("stderr" in file) throw new Error("compact-json error per-file stderr should be omitted");
  if ("file" in file) throw new Error("compact-json error per-file should not include duplicate \"file\" alias");
  if (file.ok !== false) throw new Error(`compact-json error per-file ok should be false, got ${file.ok}`);
  if (file.result !== null) throw new Error(`compact-json error per-file result should be null, got ${file.result}`);
});

await section("Loader: compact-json multi-file omits build, memory, stdout, stderr...", async () => {
  const tmp = makeTmp();
  try {
    const first = join(tmp, "first.js");
    const second = join(tmp, "second.js");
    writeFileSync(first, "11;\n");
    writeFileSync(second, "22;\n");

    const proc = Bun.spawnSync([LOADER, "--output=compact-json", "--jobs=2", first, second], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`compact-json multi-file exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    const json = JSON.parse(proc.stdout.toString());
    if ("build" in json) throw new Error("compact-json multi-file should omit top-level build");
    if ("memory" in json) throw new Error("compact-json multi-file should omit top-level memory");
    if ("stdout" in json) throw new Error("compact-json multi-file should omit top-level stdout");
    if ("stderr" in json) throw new Error("compact-json multi-file should omit top-level stderr");
    if (!Array.isArray(json.files) || json.files.length !== 2) throw new Error("compact-json multi-file should have two entries");
    for (const [idx, file] of (json.files as any[]).entries()) {
      if ("memory" in file) throw new Error(`compact-json multi-file files[${idx}] memory should be omitted`);
      if ("stdout" in file) throw new Error(`compact-json multi-file files[${idx}] stdout should be omitted`);
      if ("stderr" in file) throw new Error(`compact-json multi-file files[${idx}] stderr should be omitted`);
      if ("file" in file) throw new Error(`compact-json multi-file files[${idx}] should not include duplicate "file" alias`);
    }
    const byFileName = new Map<string, any>(
      (json.files as any[]).map((f) => [f.fileName, f]),
    );
    const firstFile = byFileName.get(first);
    const secondFile = byFileName.get(second);
    if (!firstFile) throw new Error(`compact-json multi-file missing entry for ${first}`);
    if (!secondFile) throw new Error(`compact-json multi-file missing entry for ${second}`);
    if (firstFile.result !== 11) throw new Error(`compact-json multi-file ${first} result should be 11, got ${firstFile.result}`);
    if (secondFile.result !== 22) throw new Error(`compact-json multi-file ${second} result should be 22, got ${secondFile.result}`);
    if (json.workers?.used !== 2) throw new Error(`compact-json multi-file workers.used should be 2, got ${json.workers?.used}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: parallel human-readable output preserves console output...", async () => {
  const tmp = makeTmp();
  try {
    const first = join(tmp, "parallel-first.js");
    const second = join(tmp, "parallel-second.js");
    writeFileSync(first, "console.log('parallel first out'); 1;\n");
    writeFileSync(second, "console.log('parallel second out'); 2;\n");

    const proc = Bun.spawnSync([LOADER, "--jobs=2", first, second], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Loader parallel output exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    const stdout = proc.stdout.toString();
    if (!stdout.includes("parallel first out")) throw new Error(`Loader parallel stdout should include first file output, got ${stdout}`);
    if (!stdout.includes("parallel second out")) throw new Error(`Loader parallel stdout should include second file output, got ${stdout}`);
  } finally {
    clean(tmp);
  }
});

// -- --print --------------------------------------------------------------------

await section("Loader: silent (no result line) by default...", async () => {
  const proc = Bun.spawnSync([LOADER], {
    stdin: new TextEncoder().encode("const r = 'this contains the word error'; r;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Loader default exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  const out = proc.stdout.toString();
  if (out.includes("Result:"))
    throw new Error(`Loader should not print "Result:" prefix anymore, got: ${out}`);
  if (out.includes("this contains the word error"))
    throw new Error(`Loader default should not print script value, got: ${out}`);
  if (!out.includes("Running script"))
    throw new Error(`Loader default should still print timing banner, got: ${out}`);
});

await section("Loader: --print emits bare value (no 'Result:' prefix)...", async () => {
  const proc = Bun.spawnSync([LOADER, "--print"], {
    stdin: new TextEncoder().encode("const r = 'this contains the word error'; r;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Loader --print exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  const out = proc.stdout.toString();
  if (out.includes("Result:"))
    throw new Error(`Loader --print must not prefix with "Result:", got: ${out}`);
  if (!containsLine(out, "this contains the word error"))
    throw new Error(`Loader --print should emit bare value on its own line, got: ${out}`);
});

await section("Loader: --print emits 'undefined' when result is undefined...", async () => {
  const proc = Bun.spawnSync([LOADER, "--print"], {
    stdin: new TextEncoder().encode("undefined;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Loader --print undefined exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  const out = proc.stdout.toString();
  if (!containsLine(out, "undefined"))
    throw new Error(`Loader --print should emit "undefined" (matches node -p), got: ${out}`);
});

await section("Loader: --print honored from goccia.json...", async () => {
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"print": true}\n');
    const file = join(tmp, "test.js");
    writeFileSync(file, "1 + 1;\n");
    const proc = Bun.spawnSync([LOADER, file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Loader config print exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    const out = proc.stdout.toString();
    if (!containsLine(out, "2"))
      throw new Error(`goccia.json print=true should emit value, got: ${out}`);
  } finally {
    clean(tmp);
  }
});

// ============================================================================
// GocciaScriptLoaderBare
// ============================================================================

await section("Bare Loader: stdin default path...", async () => {
  const proc = Bun.spawnSync([BARE, "--print"], {
    stdin: new TextEncoder().encode("const x = 2 + 2; x;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare stdin exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "4") throw new Error(`Bare stdin expected 4, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: stdin dash path...", async () => {
  const proc = Bun.spawnSync([BARE, "--print", "-"], {
    stdin: new TextEncoder().encode("21 * 2;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare stdin dash exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "42") throw new Error(`Bare stdin dash expected 42, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: quoted source names survive argv parsing...", async () => {
  const sourceName = 'quoted "source".js';
  const proc = Bun.spawnSync([BARE, `--source-name=${sourceName}`], {
    stdin: new TextEncoder().encode("const = ;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const output = proc.stdout.toString() + proc.stderr.toString();
  if (proc.exitCode === 0 || !output.includes(sourceName))
    throw new Error(`Bare quoted source name was not preserved: ${output}`);
});

await section("Bare Loader: input file...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "bare.js");
    writeFileSync(file, "40 + 2;\n");
    const proc = Bun.spawnSync([BARE, "--print", file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Bare file exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (proc.stdout.toString().trim() !== "42") throw new Error(`Bare file expected 42, got: ${proc.stdout.toString()}`);
  } finally {
    clean(tmp);
  }
});

await section("Bare Loader: print global...", async () => {
  const proc = Bun.spawnSync([BARE], {
    stdin: new TextEncoder().encode("print('hello', 7); undefined;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare print exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "hello 7") throw new Error(`Bare print expected hello 7, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: --stack-size bounds deep non-tail recursion with RangeError...", async () => {
  const src =
    "const f = (n) => (n === 0 ? 0 : 1 + f(n - 1)); try { f(100000); print('NO THROW'); } catch (e) { print(e.constructor.name); }\n";
  const proc = Bun.spawnSync([BARE, "--mode=bytecode", "--stack-size=1000"], {
    stdin: new TextEncoder().encode(src),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --stack-size exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "RangeError")
    throw new Error(`Bare --stack-size expected RangeError, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: proper tail calls reuse the frame (deep strict tail recursion completes)...", async () => {
  // Without proper tail calls this 100k-deep recursion would exceed --stack-size;
  // a tail call in strict-mode code reuses the current frame, so it runs in O(1)
  // stack and completes well under the 1000-frame limit.
  const src =
    "const f = (n) => { 'use strict'; return n === 0 ? 'done' : f(n - 1); }; print(f(100000));\n";
  const proc = Bun.spawnSync([BARE, "--mode=bytecode", "--stack-size=1000"], {
    stdin: new TextEncoder().encode(src),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare tail-call exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "done")
    throw new Error(`Bare tail-call expected 'done', got: ${proc.stdout.toString()} / ${proc.stderr.toString()}`);
});

await section("Bare Loader: tail-call optimization stays strict-mode only...", async () => {
  // The same tail recursion in sloppy-mode code is NOT a proper tail call, so it
  // is bounded by --stack-size and throws RangeError.
  const src =
    "const f = (n) => (n === 0 ? 'done' : f(n - 1)); try { print(f(100000)); } catch (e) { print(e.constructor.name); }\n";
  const proc = Bun.spawnSync([BARE, "--mode=bytecode", "--stack-size=1000"], {
    stdin: new TextEncoder().encode(src),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare sloppy tail-call exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "RangeError")
    throw new Error(`Bare sloppy tail-call expected RangeError, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: native re-entry recursion throws RangeError instead of crashing...", async () => {
  // Recursion through a native callback (Array.prototype.forEach) and through a
  // generator resume re-enters the VM on a native stack frame. Both must be
  // bounded (RangeError), not overflow the native stack (SIGSEGV / non-zero
  // signal exit).
  const cases = [
    "let d = 0; const rec = () => { d++; [0].forEach(rec); }; try { rec(); print('NO THROW'); } catch (e) { print(e.constructor.name); }\n",
    "let d = 0; function* g() { d++; for (const x of g()) {} yield 1; } try { for (const x of g()) {} print('NO THROW'); } catch (e) { print(e.constructor.name); }\n",
  ];
  for (const src of cases) {
    const proc = Bun.spawnSync(
      [BARE, "--mode=bytecode", "--compat-function", "--compat-traditional-for-loop"],
      { stdin: new TextEncoder().encode(src), stdout: "pipe", stderr: "pipe" },
    );
    if (proc.exitCode !== 0)
      throw new Error(`Bare native re-entry exited ${proc.exitCode} (signal ${proc.signalCode}): ${proc.stderr.toString()}`);
    if (proc.stdout.toString().trim() !== "RangeError")
      throw new Error(`Bare native re-entry expected RangeError, got: ${proc.stdout.toString()}`);
  }
});

await section("Bare Loader: no runtime globals...", async () => {
  const source = [
    "typeof print + ':' +",
    "typeof globalThis.print + ':' +",
    "typeof console + ':' +",
    "typeof FFI + ':' +",
    "typeof Goccia + ':' +",
    "(Goccia.semver === undefined);",
    "",
  ].join("\n");
  const proc = Bun.spawnSync([BARE, "--print"], {
    stdin: new TextEncoder().encode(source),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare runtime-global check exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  const output = proc.stdout.toString().trim();
  if (output !== "function:function:undefined:undefined:object:true")
    throw new Error(`Bare runtime-global check mismatch, got: ${output}`);
});

await section("Bare Loader: module source type...", async () => {
  const proc = Bun.spawnSync([BARE, "--print", "--source-type=module"], {
    stdin: new TextEncoder().encode("this === undefined;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare module source exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "true") throw new Error(`Bare module source expected true, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: .mjs module inference...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "entry.mjs");
    writeFileSync(file, "this === undefined;\n");

    const proc = Bun.spawnSync([BARE, "--print", file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Bare .mjs source exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (proc.stdout.toString().trim() !== "true") throw new Error(`Bare .mjs source expected true, got: ${proc.stdout.toString()}`);

    const scriptOverride = Bun.spawnSync([BARE, "--print", file, "--source-type=script"], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (scriptOverride.exitCode !== 0)
      throw new Error(`Bare .mjs script override exited ${scriptOverride.exitCode}: ${scriptOverride.stderr.toString()}`);
    if (scriptOverride.stdout.toString().trim() !== "false")
      throw new Error(`Bare .mjs script override expected false, got: ${scriptOverride.stdout.toString()}`);
  } finally {
    clean(tmp);
  }
});

// --mode option: bare loader defaults to interpreter mode; both values must execute.
await section("Bare Loader: --mode=interpreted...", async () => {
  const proc = Bun.spawnSync([BARE, "--print", "--mode=interpreted"], {
    stdin: new TextEncoder().encode("21 * 2;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --mode=interpreted exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "42") throw new Error(`Bare --mode=interpreted expected 42, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: interpreted for-in scope survives Goccia.gc...", async () => {
  const source = [
    "function f() {",
    "  let obj = { p: 1, r: 3, s: 4 };",
    "  let seen = '';",
    "  for (let key in obj) {",
    "    Goccia.gc();",
    "    seen = seen + key;",
    "  }",
    "  return seen;",
    "}",
    "print(f());",
    "",
  ].join("\n");
  const proc = Bun.spawnSync([
    BARE,
    "--mode=interpreted",
    "--compat-asi",
    "--compat-function",
    "--compat-for-in-loop",
    "--compat-non-strict-mode",
  ], {
    stdin: new TextEncoder().encode(source),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0)
    throw new Error(`Bare interpreted for-in Goccia.gc exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "prs")
    throw new Error(`Bare interpreted for-in Goccia.gc expected prs, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: --mode=bytecode...", async () => {
  const proc = Bun.spawnSync([BARE, "--print", "--mode=bytecode"], {
    stdin: new TextEncoder().encode("21 * 2;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --mode=bytecode exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "42") throw new Error(`Bare --mode=bytecode expected 42, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: bytecode top-level declarations back globalThis...", async () => {
  const source = [
    "var x = 1;",
    "function f() {}",
    "print(",
    "  Object.prototype.hasOwnProperty.call(globalThis, 'x') + ':' +",
    "  Object.prototype.hasOwnProperty.call(globalThis, 'f') + ':' +",
    "  typeof f",
    ");",
    "",
  ].join("\n");
  const proc = Bun.spawnSync([
    BARE,
    "--mode=bytecode",
    "--compat-var",
    "--compat-function",
    "--compat-non-strict-mode",
  ], {
    stdin: new TextEncoder().encode(source),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode global-backed top-level exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "true:true:function")
    throw new Error(`Bare bytecode global-backed top-level mismatch, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: test262 host marker is hidden by default...", async () => {
  const proc = Bun.spawnSync([BARE], {
    stdin: new TextEncoder().encode("print(typeof Goccia.test262Host); print(typeof Goccia.test262);\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0)
    throw new Error(`Bare default test262 marker probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== "undefined\nundefined")
    throw new Error(`Bare default should hide test262 host hooks, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: --test262-host exposes Goccia test262 hooks...", async () => {
  const proc = Bun.spawnSync([BARE, "--test262-host", "--compat-loose-equality"], {
    stdin: new TextEncoder().encode([
      "print(Goccia.test262Host);",
      "print(typeof Goccia.test262);",
      "print(typeof Goccia.test262.createRealm);",
      "print(typeof Goccia.test262.evalScript);",
      "const htmlDDA = Goccia.test262.isHTMLDDA;",
      "print(typeof htmlDDA);",
      "print(Boolean(htmlDDA));",
      "print(htmlDDA == null);",
      "print(htmlDDA == undefined);",
      "print(htmlDDA === undefined);",
      "const realm = Goccia.test262.createRealm();",
      "print(realm.global.Object !== Object);",
      "print(typeof realm.global.eval);",
      "print(realm.global.eval('1 + 2'));",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = [
    "true",
    "object",
    "function",
    "function",
    "undefined",
    "false",
    "true",
    "true",
    "false",
    "true",
    "function",
    "3",
  ].join("\n");
  if (proc.exitCode !== 0)
    throw new Error(`Bare --test262-host hook probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare --test262-host should expose realm hooks, got: ${proc.stdout.toString()}`);
});

await section("test262 runner: engine timeout is classified as TIMEOUT...", async () => {
  const tmp = makeTmp();
  try {
    const suite = join(tmp, "suite");
    const harness = join(suite, "harness");
    const tests = join(suite, "test", "built-ins");
    mkdirSync(harness, { recursive: true });
    mkdirSync(tests, { recursive: true });
    writeFileSync(join(harness, "sta.js"), "");
    writeFileSync(join(harness, "assert.js"), "");

    const timeoutTest = join(tests, "timeout-loop.js");
    writeFileSync(timeoutTest, "while (true) {}\n");
    const timeoutOut = join(tmp, "timeout-result.json");
    const timeoutProc = Bun.spawnSync(
      [
        "bun",
        "scripts/run_test262_suite.ts",
        "--suite-dir", suite,
        "--categories", "built-ins",
        "--filter", "built-ins/timeout-loop.js",
        "--mode=bytecode",
        "--jobs=1",
        "--timeout-ms=50",
        "--output", timeoutOut,
      ],
      { stdout: "pipe", stderr: "pipe", timeout: 10_000 },
    );
    if (timeoutProc.exitCode !== 1)
      throw new Error(`test262 timeout fixture should exit 1, got ${timeoutProc.exitCode}: ${timeoutProc.stderr.toString()}`);
    const timeoutJson = JSON.parse(readFileSync(timeoutOut, "utf8"));
    if (timeoutJson.summary.timeouts !== 1 || timeoutJson.summary.failed !== 0)
      throw new Error(`engine timeout should be TIMEOUT, got summary ${JSON.stringify(timeoutJson.summary)}`);
    if (timeoutJson.results?.[0]?.status !== "TIMEOUT")
      throw new Error(`engine timeout result should be TIMEOUT, got ${timeoutJson.results?.[0]?.status}`);

    const thrownErrorTest = join(tests, "timeout-like-error.js");
    writeFileSync(thrownErrorTest, 'throw new Error("file timed out after 50ms");\n');
    const thrownErrorOut = join(tmp, "timeout-like-error-result.json");
    const thrownErrorProc = Bun.spawnSync(
      [
        "bun",
        "scripts/run_test262_suite.ts",
        "--suite-dir", suite,
        "--categories", "built-ins",
        "--filter", "built-ins/timeout-like-error.js",
        "--mode=bytecode",
        "--jobs=1",
        "--timeout-ms=50",
        "--output", thrownErrorOut,
      ],
      { stdout: "pipe", stderr: "pipe", timeout: 10_000 },
    );
    if (thrownErrorProc.exitCode !== 1)
      throw new Error(`test262 thrown timeout-like error should exit 1, got ${thrownErrorProc.exitCode}: ${thrownErrorProc.stderr.toString()}`);
    const thrownErrorJson = JSON.parse(readFileSync(thrownErrorOut, "utf8"));
    if (thrownErrorJson.summary.failed !== 1 || thrownErrorJson.summary.timeouts !== 0)
      throw new Error(`timeout-like user Error should remain FAIL, got summary ${JSON.stringify(thrownErrorJson.summary)}`);
    if (thrownErrorJson.results?.[0]?.status !== "FAIL")
      throw new Error(`timeout-like user Error result should be FAIL, got ${thrownErrorJson.results?.[0]?.status}`);

    const markerErrorTest = join(tests, "timeout-marker-error.js");
    writeFileSync(markerErrorTest, 'throw new Error("\\nGocciaTest262:Timeout:50");\n');
    const markerErrorOut = join(tmp, "timeout-marker-error-result.json");
    const markerErrorProc = Bun.spawnSync(
      [
        "bun",
        "scripts/run_test262_suite.ts",
        "--suite-dir", suite,
        "--categories", "built-ins",
        "--filter", "built-ins/timeout-marker-error.js",
        "--mode=bytecode",
        "--jobs=1",
        "--timeout-ms=50",
        "--output", markerErrorOut,
      ],
      { stdout: "pipe", stderr: "pipe", timeout: 10_000 },
    );
    if (markerErrorProc.exitCode !== 1)
      throw new Error(`test262 marker-like user Error should exit 1, got ${markerErrorProc.exitCode}: ${markerErrorProc.stderr.toString()}`);
    const markerErrorJson = JSON.parse(readFileSync(markerErrorOut, "utf8"));
    if (markerErrorJson.summary.failed !== 1 || markerErrorJson.summary.timeouts !== 0)
      throw new Error(`marker-like user Error should remain FAIL, got summary ${JSON.stringify(markerErrorJson.summary)}`);
    if (markerErrorJson.results?.[0]?.status !== "FAIL")
      throw new Error(`marker-like user Error result should be FAIL, got ${markerErrorJson.results?.[0]?.status}`);
  } finally {
    clean(tmp);
  }
});

await section("Bare Loader: --test262-host child realms expose host records...", async () => {
  const proc = Bun.spawnSync([BARE, "--test262-host"], {
    stdin: new TextEncoder().encode([
      "const child = Goccia.test262.createRealm();",
      "print(typeof child.evalScript);",
      "print(typeof child.createRealm);",
      "child.evalScript('globalThis.childRealmValue = 42;');",
      "print(child.global.childRealmValue);",
      "print(typeof globalThis.childRealmValue);",
      "const grandchild = child.createRealm();",
      "print(typeof grandchild.evalScript);",
      "print(typeof grandchild.createRealm);",
      "print(grandchild.global.Object !== child.global.Object);",
      "print(grandchild.global.Object !== Object);",
      "grandchild.evalScript('globalThis.grandchildRealmValue = 43;');",
      "print(grandchild.global.grandchildRealmValue);",
      "print(typeof child.global.grandchildRealmValue);",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = [
    "function",
    "function",
    "42",
    "undefined",
    "function",
    "function",
    "true",
    "true",
    "43",
    "undefined",
  ].join("\n");
  if (proc.exitCode !== 0)
    throw new Error(`Bare --test262-host child realm probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare --test262-host child realm hooks got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: --test262-host child realm globals expose host hooks...", async () => {
  const proc = Bun.spawnSync([BARE, "--test262-host"], {
    stdin: new TextEncoder().encode([
      "const child = Goccia.test262.createRealm();",
      "print(child.global.Goccia.test262Host);",
      "print(typeof child.global.Goccia.test262);",
      "print(typeof child.global.Goccia.test262.createRealm);",
      "print(typeof child.global.Goccia.test262.evalScript);",
      "print(typeof child.global.eval);",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = [
    "true",
    "object",
    "function",
    "function",
    "function",
  ].join("\n");
  if (proc.exitCode !== 0)
    throw new Error(`Bare --test262-host child global hook probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare --test262-host child global hooks got: ${proc.stdout.toString()}`);
});

console.log("Bare Loader: cross-realm weak constructors use the newTarget realm prototype...");
for (const { label, args } of [
  { label: "interpreted", args: [BARE, "--test262-host", "--compat-function"] },
  { label: "bytecode", args: [BARE, "--test262-host", "--compat-function", "--mode=bytecode"] },
]) {
  const proc = Bun.spawnSync(args, {
    stdin: new TextEncoder().encode([
      "const child = Goccia.test262.createRealm();",
      "child.evalScript('function NewTarget() {} NewTarget.prototype = null; globalThis.NewTarget = NewTarget;');",
      "const newTarget = child.global.NewTarget;",
      "const weakRef = Reflect.construct(WeakRef, [{}], newTarget);",
      "print(Object.getPrototypeOf(weakRef) === child.global.WeakRef.prototype);",
      "const registry = Reflect.construct(FinalizationRegistry, [() => {}], newTarget);",
      "print(Object.getPrototypeOf(registry) === child.global.FinalizationRegistry.prototype);",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0)
    throw new Error(`Bare ${label} cross-realm weak constructor probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== "true\ntrue")
    throw new Error(`Bare ${label} cross-realm weak constructor prototype mismatch: ${proc.stdout.toString()}`);
}

await section("Bare Loader: bytecode --test262-host eval is direct eval...", async () => {
  const proc = Bun.spawnSync([BARE, "--test262-host", "--mode=bytecode"], {
    stdin: new TextEncoder().encode([
      "{",
      "  let x = 41;",
      "  print(eval('x + 1'));",
      "  eval('x = 7');",
      "  print(x);",
      "  const shadow = () => { const eval = (source) => 'shadow:' + source; return eval('x'); };",
      "  print(shadow());",
      "}",
      "{",
      "  let y = 3;",
      "  const f = () => eval('y + 4');",
      "  print(f());",
      "}",
      "{",
      "  const update = () => { let local = 1; print(eval('local = 2; local;')); print(local); return local; };",
      "  print(update());",
      "}",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = "42\n7\nshadow:x\n7\n2\n2\n2";
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode direct eval probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare bytecode direct eval got: ${proc.stdout.toString()}`);
});

console.log("Bare Loader: rejected eval source preserves primitive singletons...");
for (const { label, args } of [
  {
    label: "interpreted",
    args: [BARE, "--test262-host", "--compat-function", "--unsafe-function-constructor"],
  },
  {
    label: "bytecode",
    args: [
      BARE,
      "--test262-host",
      "--compat-function",
      "--unsafe-function-constructor",
      "--mode=bytecode",
    ],
  },
]) {
  const proc = Bun.spawnSync(args, {
    stdin: new TextEncoder().encode([
      "function rejectsSyntaxError(callback) {",
      "  try { callback(); } catch (error) { return error instanceof SyntaxError; }",
      "  return false;",
      "}",
      "const source = 'null, [true && a] = [];';",
      "const indirectEval = eval;",
      "print(rejectsSyntaxError(() => Function(source)));",
      "print(rejectsSyntaxError(() => eval(source)));",
      "print(rejectsSyntaxError(() => indirectEval(source)));",
      "print(null === null);",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0)
    throw new Error(`Bare ${label} rejected eval source probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== "true\ntrue\ntrue\ntrue")
    throw new Error(`Bare ${label} rejected eval source probe got: ${proc.stdout.toString()}`);
}

await section("Bare Loader: bytecode --test262-host eval keeps sloppy var declarations in the caller environment...", async () => {
  const proc = Bun.spawnSync([
    BARE,
    "--test262-host",
    "--mode=bytecode",
    "--compat-var",
    "--compat-function",
    "--compat-non-strict-mode",
  ], {
    stdin: new TextEncoder().encode([
      "var y = 42;",
      "function globalY() { return y; }",
      "function testY() {",
      "  const f = eval(",
      "    'var y = 5;' +",
      "    'function actY(action) {' +",
      "    '  switch (action) {' +",
      "    \"    case 'get': return y;\" +",
      "    \"    case 'set': y = 2; return;\" +",
      "    \"    case 'delete': return eval('delete y');\" +",
      "    '  }' +",
      "    '}' +",
      "    'actY;'",
      "  );",
      "  print([f('get'), y, globalY()].join(','));",
      "  y = 8;",
      "  print([f('get'), y, globalY()].join(','));",
      "  f('set');",
      "  print([f('get'), y, globalY()].join(','));",
      "  print(f('delete'));",
      "  print([f('get'), y, globalY()].join(','));",
      "}",
      "testY();",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = [
    "5,5,42",
    "8,8,42",
    "2,2,42",
    "true",
    "42,42,42",
  ].join("\n");
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode sloppy eval var probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare bytecode sloppy eval var probe got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: bytecode --test262-host eval exposes later sloppy vars to existing closures...", async () => {
  const proc = Bun.spawnSync([
    BARE,
    "--test262-host",
    "--mode=bytecode",
    "--compat-var",
    "--compat-function",
    "--compat-non-strict-mode",
  ], {
    stdin: new TextEncoder().encode([
      "var y = 42;",
      "function testY() {",
      "  const before = () => y;",
      "  eval('var y = 5;');",
      "  const after = () => y;",
      "  print([before(), after(), y].join(','));",
      "  return before;",
      "}",
      "const beforeClosure = testY();",
      "print(beforeClosure());",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = [
    "5,5,5",
    "5",
  ].join("\n");
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode sloppy eval pre-closure probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare bytecode sloppy eval pre-closure probe got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: bytecode --test262-host eval var declarations shadow outer upvalues...", async () => {
  const proc = Bun.spawnSync([
    BARE,
    "--test262-host",
    "--mode=bytecode",
    "--compat-var",
    "--compat-function",
    "--compat-non-strict-mode",
  ], {
    stdin: new TextEncoder().encode([
      "function outerNormal() {",
      "  var x = 1;",
      "  function inner() {",
      "    eval('var x = 2;');",
      "    return x;",
      "  }",
      "  print([inner(), x].join(','));",
      "}",
      "function outerArrow() {",
      "  var x = 1;",
      "  const inner = () => { eval('var x = 2;'); return x; };",
      "  print([inner(), x].join(','));",
      "}",
      "outerNormal();",
      "outerArrow();",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = [
    "2,1",
    "2,1",
  ].join("\n");
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode sloppy eval upvalue shadow probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare bytecode sloppy eval upvalue shadow probe got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: bytecode --test262-host eval keeps nested variable environments isolated...", async () => {
  const proc = Bun.spawnSync([
    BARE,
    "--test262-host",
    "--mode=bytecode",
    "--compat-var",
    "--compat-function",
    "--compat-non-strict-mode",
  ], {
    stdin: new TextEncoder().encode([
      "function outer() {",
      "  eval('var outerOnly = 1;');",
      "  function inner() {",
      "    eval('var innerOnly = 2;');",
      "    return [innerOnly, outerOnly].join(':');",
      "  }",
      "  print([inner(), typeof innerOnly, outerOnly].join(','));",
      "}",
      "outer();",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = "2:1,undefined,1";
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode nested eval environment probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare bytecode nested eval environment probe got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: bytecode --test262-host eval preserves lexical upvalue precedence...", async () => {
  const proc = Bun.spawnSync([
    BARE,
    "--test262-host",
    "--mode=bytecode",
    "--compat-var",
    "--compat-function",
    "--compat-non-strict-mode",
  ], {
    stdin: new TextEncoder().encode([
      "function lexicalShadow() {",
      "  eval('var x = \"eval\";');",
      "  {",
      "    let x = 'lexical';",
      "    const read = () => x;",
      "    print([read(), x].join(','));",
      "  }",
      "}",
      "function writeOuter() {",
      "  var x = 'outer';",
      "  function inner() {",
      "    const set = () => { x = 'set'; };",
      "    const readDeep = () => () => x;",
      "    eval('var x = \"eval\";');",
      "    set();",
      "    print([readDeep()(), x].join(','));",
      "  }",
      "  inner();",
      "  print(x);",
      "}",
      "lexicalShadow();",
      "writeOuter();",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = [
    "lexical,lexical",
    "set,set",
    "outer",
  ].join("\n");
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode eval lexical precedence probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare bytecode eval lexical precedence probe got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: bytecode assignment retains its resolved eval environment reference...", async () => {
  const proc = Bun.spawnSync([
    BARE,
    "--test262-host",
    "--mode=bytecode",
    "--compat-var",
    "--compat-function",
    "--compat-non-strict-mode",
  ], {
    stdin: new TextEncoder().encode([
      "function simpleAssignment() {",
      "  var x = 0;",
      "  function inner() {",
      "    x = (eval('var x = 2;'), 1);",
      "    return x;",
      "  }",
      "  print([inner(), x].join(','));",
      "}",
      "function compoundAssignment() {",
      "  var x = 0;",
      "  function inner() {",
      "    x += (eval('var x = 2;'), 1);",
      "    return x;",
      "  }",
      "  print([inner(), x].join(','));",
      "}",
      "var globalX = 0;",
      "function globalBackedAssignment() {",
      "  globalX = (eval('var globalX = 2;'), 1);",
      "  return globalX;",
      "}",
      "simpleAssignment();",
      "compoundAssignment();",
      "print([globalBackedAssignment(), globalX].join(','));",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = [
    "2,1",
    "2,1",
    "2,1",
  ].join("\n");
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode eval assignment reference probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare bytecode eval assignment reference probe got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: --test262-host generator parameter eval uses the parameter var environment...", async () => {
  const source = [
    "var x = 'outside';",
    "var declaredBefore, declaredAfter;",
    "function* declared(",
    "  _ = declaredBefore = function() { return x; },",
    "  __ = (eval('var x = \"inside\";'), declaredAfter = function() { return x; })",
    ") {}",
    "declared().next();",
    "print([declaredBefore(), declaredAfter(), x].join(','));",
    "var expressionBefore, expressionAfter;",
    "(function*(",
    "  _ = expressionBefore = function() { return x; },",
    "  ...[__ = (eval('var x = \"inside\";'), expressionAfter = function() { return x; })]",
    ") {})().next();",
    "print([expressionBefore(), expressionAfter(), x].join(','));",
    "var methodBefore, methodAfter;",
    "({",
    "  *method(",
    "    _ = methodBefore = function() { return x; },",
    "    ...[__ = (eval('var x = \"inside\";'), methodAfter = function() { return x; })]",
    "  ) {}",
    "}).method().next();",
    "print([methodBefore(), methodAfter(), x].join(','));",
    "",
  ].join("\n");
  const expected = [
    "inside,inside,outside",
    "inside,inside,outside",
    "inside,inside,outside",
  ].join("\n");
  for (const mode of [
    { label: "interpreted", args: [BARE] },
    { label: "bytecode", args: [BARE, "--mode=bytecode"] },
  ]) {
    const proc = Bun.spawnSync([
      ...mode.args,
      "--test262-host",
      "--compat-var",
      "--compat-function",
      "--compat-non-strict-mode",
    ], {
      stdin: new TextEncoder().encode(source),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0)
      throw new Error(`Bare ${mode.label} generator parameter eval probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
      throw new Error(`Bare ${mode.label} generator parameter eval probe got: ${proc.stdout.toString()}`);
  }
});

await section("Bare Loader: --test262-host Annex B eval preserves with-object properties...", async () => {
  const source = [
    "function checkAnnexBEval() {",
    "  function g() { return 'outer-g'; }",
    "  var object = { g: function() { return 'with-g'; } };",
    "  with (object) {",
    "    eval('{ function g() { return \"eval-g\"; } }');",
    "  }",
    "  print([g(), object.g()].join(','));",
    "}",
    "checkAnnexBEval();",
    "",
  ].join("\n");
  for (const mode of [
    { label: "interpreted", args: [BARE] },
    { label: "bytecode", args: [BARE, "--mode=bytecode"] },
  ]) {
    const proc = Bun.spawnSync([
      ...mode.args,
      "--test262-host",
      "--compat-var",
      "--compat-function",
      "--compat-non-strict-mode",
    ], {
      stdin: new TextEncoder().encode(source),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0)
      throw new Error(`Bare ${mode.label} Annex B eval probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (normalizeLineEndings(proc.stdout.toString()).trim() !== "eval-g,with-g")
      throw new Error(`Bare ${mode.label} Annex B eval probe got: ${proc.stdout.toString()}`);
  }
});

await section("Bare Loader: --test262-host eval reports strict delete identifier as SyntaxError...", async () => {
  const source = [
    "try {",
    "  eval('\"use strict\"; delete x');",
    "  print('no error');",
    "} catch (e) {",
    "  print(e.name);",
    "}",
    "",
  ].join("\n");
  for (const mode of [
    { label: "interpreted", args: [BARE, "--test262-host"] },
    { label: "bytecode", args: [BARE, "--test262-host", "--mode=bytecode"] },
  ]) {
    const proc = Bun.spawnSync(mode.args, {
      stdin: new TextEncoder().encode(source),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0)
      throw new Error(`Bare ${mode.label} strict delete eval probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (normalizeLineEndings(proc.stdout.toString()).trim() !== "SyntaxError")
      throw new Error(`Bare ${mode.label} strict delete eval probe got: ${proc.stdout.toString()}`);
  }
});

await section("Bare Loader: --test262-host eval validates destructuring pattern early errors...", async () => {
  const source = [
    "const cases = [",
    "  'let { [super.x]: y } = {};',",
    "  'let { a = super.x } = {};',",
    "  'let { a = super() } = {};',",
    "  'let { a = new.target } = {};',",
    "  '({ [super.x]: y } = {});',",
    "  '({ a = super.x } = {});'",
    "];",
    "for (const source of cases) {",
    "  try {",
    "    eval(source);",
    "    print('no error');",
    "  } catch (e) {",
    "    print(e.name);",
    "  }",
    "}",
    "",
  ].join("\n");
  const expected = [
    "SyntaxError",
    "SyntaxError",
    "SyntaxError",
    "SyntaxError",
    "SyntaxError",
    "SyntaxError",
  ].join("\n");
  for (const mode of [
    { label: "interpreted", args: [BARE, "--test262-host"] },
    { label: "bytecode", args: [BARE, "--test262-host", "--mode=bytecode"] },
  ]) {
    const proc = Bun.spawnSync(mode.args, {
      stdin: new TextEncoder().encode(source),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0)
      throw new Error(`Bare ${mode.label} eval destructuring early-error probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
      throw new Error(`Bare ${mode.label} eval destructuring early errors got: ${proc.stdout.toString()}`);
  }
});

await section("Bare Loader: --test262-host eval rejects arguments in class field initializers...", async () => {
  const source = [
    "let instanceExecuted = false;",
    "try {",
    "  class C { x = eval('instanceExecuted = true; arguments;'); }",
    "  new C();",
    "  print('instance:none');",
    "} catch (e) {",
    "  print('instance:' + e.name);",
    "}",
    "print(instanceExecuted);",
    "",
    "let privateExecuted = false;",
    "try {",
    "  class C { #x = eval('privateExecuted = true; arguments;'); }",
    "  new C();",
    "  print('private:none');",
    "} catch (e) {",
    "  print('private:' + e.name);",
    "}",
    "print(privateExecuted);",
    "",
    "let arrowExecuted = false;",
    "class ArrowField { x = () => eval('arrowExecuted = true; arguments;'); }",
    "try {",
    "  new ArrowField().x();",
    "  print('arrow:none');",
    "} catch (e) {",
    "  print('arrow:' + e.name);",
    "}",
    "print(arrowExecuted);",
    "",
    "let staticExecuted = false;",
    "try {",
    "  class StaticField { static x = eval('staticExecuted = true; arguments;'); }",
    "  print('static:none');",
    "} catch (e) {",
    "  print('static:' + e.name);",
    "}",
    "print(staticExecuted);",
    "",
    "let staticArrowExecuted = false;",
    "class StaticArrowField { static x = () => eval('staticArrowExecuted = true; arguments;'); }",
    "try {",
    "  StaticArrowField.x();",
    "  print('static-arrow:none');",
    "} catch (e) {",
    "  print('static-arrow:' + e.name);",
    "}",
    "print(staticArrowExecuted);",
    "",
  ].join("\n");
  const expected = [
    "instance:SyntaxError",
    "false",
    "private:SyntaxError",
    "false",
    "arrow:SyntaxError",
    "false",
    "static:SyntaxError",
    "false",
    "static-arrow:SyntaxError",
    "false",
  ].join("\n");
  for (const mode of [
    { label: "interpreted", args: [BARE, "--test262-host"] },
    { label: "bytecode", args: [BARE, "--test262-host", "--mode=bytecode"] },
  ]) {
    const proc = Bun.spawnSync(mode.args, {
      stdin: new TextEncoder().encode(source),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0)
      throw new Error(`Bare ${mode.label} eval class-field arguments probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
      throw new Error(`Bare ${mode.label} eval class-field arguments got: ${proc.stdout.toString()}`);
  }
});

await section("Bare Loader: --test262-host eval rejects arguments in generator method defaults...", async () => {
  const source = [
    "const cases = [",
    "  { label: 'generator', run: () => ({ *method(value = eval('var value = 42')) { yield value; } }).method() },",
    "  { label: 'async-generator', run: () => ({ async *method(value = eval('var value = 42')) { yield value; } }).method() }",
    "];",
    "for (const item of cases) {",
    "  try {",
    "    item.run();",
    "    print(item.label + ':none');",
    "  } catch (e) {",
    "    print(item.label + ':' + e.name);",
    "  }",
    "}",
    "",
  ].join("\n");
  const expected = [
    "generator:SyntaxError",
    "async-generator:SyntaxError",
  ].join("\n");
  for (const mode of [
    { label: "interpreted", args: [BARE, "--test262-host", "--compat-var", "--compat-non-strict-mode"] },
    { label: "bytecode", args: [BARE, "--test262-host", "--mode=bytecode", "--compat-var", "--compat-non-strict-mode"] },
  ]) {
    const proc = Bun.spawnSync(mode.args, {
      stdin: new TextEncoder().encode(source),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0)
      throw new Error(`Bare ${mode.label} eval generator-method arguments probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
      throw new Error(`Bare ${mode.label} eval generator-method arguments got: ${proc.stdout.toString()}`);
  }
});

await section("Bare Loader: --test262-host eval super permissions stop at ordinary function boundary...", async () => {
  const source = [
    "class Base { method() { return 11; } }",
    "class Derived extends Base {",
    "  method() {",
    "    function inner() {",
    "      try {",
    "        return eval('super.method()');",
    "      } catch (e) {",
    "        return e.name;",
    "      }",
    "    }",
    "    return inner();",
    "  }",
    "}",
    "print(new Derived().method());",
    "",
  ].join("\n");
  for (const mode of [
    { label: "interpreted", args: [BARE, "--test262-host", "--compat-function", "--compat-non-strict-mode"] },
    { label: "bytecode", args: [BARE, "--test262-host", "--mode=bytecode", "--compat-function", "--compat-non-strict-mode"] },
  ]) {
    const proc = Bun.spawnSync(mode.args, {
      stdin: new TextEncoder().encode(source),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0)
      throw new Error(`Bare ${mode.label} eval ordinary-boundary probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (proc.stdout.toString().trim() !== "SyntaxError")
      throw new Error(`Bare ${mode.label} eval ordinary-boundary got: ${proc.stdout.toString()}`);
  }
});

await section("Bare Loader: bytecode --test262-host eval inherits arrow lexical super and new.target...", async () => {
  const proc = Bun.spawnSync([BARE, "--test262-host", "--mode=bytecode"], {
    stdin: new TextEncoder().encode([
      "class Base {",
      "  constructor() { this.x = 1; }",
      "  m() { return 7; }",
      "}",
      "class Derived extends Base {",
      "  constructor() {",
      "    (() => eval('super()'))();",
      "    this.nt = (() => eval('new.target === Derived'))();",
      "  }",
      "  method() { return (() => eval('super.m()'))(); }",
      "  detached() { return () => eval('super.m()'); }",
      "}",
      "const d = new Derived();",
      "print(d.method());",
      "print(d.detached()());",
      "print(d.x);",
      "print(d.nt);",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  const expected = "7\n7\n1\ntrue";
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode eval arrow lexical probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== expected)
    throw new Error(`Bare bytecode eval arrow lexical got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: bytecode direct eval creates top-level sloppy var...", async () => {
  const proc = Bun.spawnSync([
    BARE,
    "--test262-host",
    "--mode=bytecode",
    "--compat-var",
    "--compat-non-strict-mode",
  ], {
    stdin: new TextEncoder().encode([
      "print(eval('var t262EvalGlobal = 33; this === globalThis'));",
      "print(t262EvalGlobal);",
      "print(globalThis.t262EvalGlobal);",
      "const topLevelArrow = () => eval('this === globalThis');",
      "print(topLevelArrow());",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode sloppy direct eval var probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== "true\n33\n33\ntrue")
    throw new Error(`Bare bytecode sloppy direct eval var got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: bytecode module direct eval keeps module this binding...", async () => {
  const proc = Bun.spawnSync([
    BARE,
    "--test262-host",
    "--mode=bytecode",
    "--source-type=module",
  ], {
    stdin: new TextEncoder().encode([
      "print(eval('this === undefined'));",
      "const topLevelArrow = () => eval('this === undefined');",
      "print(topLevelArrow());",
      "",
    ].join("\n")),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0)
    throw new Error(`Bare bytecode module direct eval this probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== "true\ntrue")
    throw new Error(`Bare bytecode module direct eval this got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: --mode default is interpreted...", async () => {
  const proc = Bun.spawnSync([BARE, "--help"], {
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --help exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  const help = proc.stdout.toString();
  if (!help.includes("--mode=interpreted|bytecode"))
    throw new Error(`Bare --help should document --mode, got: ${help}`);
  if (!help.includes("default: interpreted"))
    throw new Error(`Bare --help should document interpreted as default, got: ${help}`);
  if (!help.includes("--test262-host"))
    throw new Error(`Bare --help should document --test262-host, got: ${help}`);
});

await section("Bare Loader: --mode invalid value rejected...", async () => {
  const proc = Bun.spawnSync([BARE, "--mode=foo"], {
    stdin: new TextEncoder().encode("1;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode === 0) throw new Error(`Bare --mode=foo should fail, got exit 0`);
  const stderr = proc.stderr.toString();
  if (!stderr.includes("Invalid --mode value: foo"))
    throw new Error(`Bare --mode=foo should report invalid value, got stderr: ${stderr}`);
});

// -- --print --------------------------------------------------------------------

await section("Bare Loader: silent by default (no script result printed)...", async () => {
  const proc = Bun.spawnSync([BARE], {
    stdin: new TextEncoder().encode("const r = 'this contains the word error'; r;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare default exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString() !== "")
    throw new Error(`Bare default should produce empty stdout (matches node script.js), got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: --print emits bare value...", async () => {
  const proc = Bun.spawnSync([BARE, "--print"], {
    stdin: new TextEncoder().encode("const r = 'this contains the word error'; r;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --print exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "this contains the word error")
    throw new Error(`Bare --print should emit bare value, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: --print emits 'undefined' (matches node -p)...", async () => {
  const proc = Bun.spawnSync([BARE, "--print"], {
    stdin: new TextEncoder().encode("undefined;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --print undefined exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "undefined")
    throw new Error(`Bare --print undefined should emit "undefined", got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: print() output independent of --print flag...", async () => {
  const proc = Bun.spawnSync([BARE], {
    stdin: new TextEncoder().encode("print('explicit'); 'last value';\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare default+print() exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "explicit")
    throw new Error(`Bare default should emit print() output but no result, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: --help documents --print...", async () => {
  const proc = Bun.spawnSync([BARE, "--help"], {
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --help exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (!proc.stdout.toString().includes("--print"))
    throw new Error(`Bare --help should document --print, got: ${proc.stdout.toString()}`);
});

// -- Promise.then microtask drain (Bare) ----------------------------------------
// Top-level .then callbacks must fire via WaitForRuntimeIdle post-execution drain.
// Regression: ExecuteProgram freed the bytecode module before the drain, leaving
// closures with dangling template pointers (Range check error on FCode access).

await section("Bare Loader: Promise.then drain (interpreted)...", async () => {
  const proc = Bun.spawnSync([BARE, "--mode=interpreted"], {
    stdin: new TextEncoder().encode('Promise.resolve(42).then(v => print("then-" + v));\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare Promise drain interpreted exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "then-42")
    throw new Error(`Bare Promise drain interpreted expected then-42, got: ${proc.stdout.toString()}`);
});

await section("Bare Loader: Promise.then drain (bytecode)...", async () => {
  const proc = Bun.spawnSync([BARE, "--mode=bytecode"], {
    stdin: new TextEncoder().encode('Promise.resolve(42).then(v => print("then-" + v));\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare Promise drain bytecode exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "then-42")
    throw new Error(`Bare Promise drain bytecode expected then-42, got: ${proc.stdout.toString()}`);
});

// -- Promise.then microtask drain (Loader) --------------------------------------

await section("Loader: Promise.then drain (interpreted)...", async () => {
  const proc = Bun.spawnSync([LOADER, "--mode=interpreted"], {
    stdin: new TextEncoder().encode('Promise.resolve(42).then(v => console.log("then-" + v));\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Loader Promise drain interpreted exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (!proc.stdout.toString().includes("then-42"))
    throw new Error(`Loader Promise drain interpreted expected then-42, got: ${proc.stdout.toString()}`);
});

await section("Loader: Promise.then drain (bytecode)...", async () => {
  const proc = Bun.spawnSync([LOADER, "--mode=bytecode"], {
    stdin: new TextEncoder().encode('Promise.resolve(42).then(v => console.log("then-" + v));\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Loader Promise drain bytecode exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (!proc.stdout.toString().includes("then-42"))
    throw new Error(`Loader Promise drain bytecode expected then-42, got: ${proc.stdout.toString()}`);
});

await section("Loader: --audit-log records capability decisions with source locations...", async () => {
  const tmp = makeTmp();
  try {
    for (const mode of ["interpreted", "bytecode"] as const) {
      const audit = join(tmp, `capabilities-${mode}.jsonl`);
      const proc = Bun.spawnSync(
        [
          LOADER,
          `--mode=${mode}`,
          "--unsafe-ffi",
          "--unsafe-shadowrealm",
          `--audit-log=${audit}`,
        ],
        {
          stdin: new TextEncoder().encode([
            'try { Function("return 1")(); } catch (e) {}',
            'const holder = { ctor: Function };',
            'try { holder.ctor("return 1")(); } catch (e) {}',
            'try { FFI.open("./missing"); } catch (e) {}',
            'new ShadowRealm().evaluate("new ShadowRealm(); 1");',
            "",
          ].join("\n")),
          stdout: "pipe",
          stderr: "pipe",
        },
      );
      if (proc.exitCode !== 0)
        throw new Error(`Loader audit ${mode} exited ${proc.exitCode}: ${proc.stderr.toString()}`);
      const events = readJsonLines(audit);
      const expected = [
        ["function.constructor", "deny", "Function", "<stdin>", 1],
        ["function.constructor", "deny", "Function", "<stdin>", 3],
        ["ffi.open", "allow", "./missing", "<stdin>", 4],
        ["shadow-realm.construct", "allow", "ShadowRealm", "<stdin>", 5],
        ["shadow-realm.construct", "allow", "ShadowRealm", "<shadow-realm-eval>", 1],
      ];
      if (events.length !== expected.length)
        throw new Error(`Loader audit ${mode} expected ${expected.length} events, got ${JSON.stringify(events)}`);
      expected.forEach(([kind, decision, subject, file, line], index) => {
        const event = events[index];
        if (event.schemaVersion !== 1 ||
            event.kind !== kind ||
            event.decision !== decision ||
            event.subject !== subject ||
            event.source?.file !== file ||
            event.source?.line !== line ||
            typeof event.source?.column !== "number")
          throw new Error(`Loader audit ${mode} event ${index} mismatch: ${JSON.stringify(event)}`);
      });

      const allowAudit = join(tmp, `function-allow-${mode}.jsonl`);
      const allow = Bun.spawnSync(
        [
          LOADER,
          `--mode=${mode}`,
          "--unsafe-function-constructor",
          `--audit-log=${allowAudit}`,
        ],
        {
          stdin: new TextEncoder().encode('Function("return 1")();\n'),
          stdout: "pipe",
          stderr: "pipe",
        },
      );
      if (allow.exitCode !== 0)
        throw new Error(`Loader Function audit ${mode} exited ${allow.exitCode}: ${allow.stderr.toString()}`);
      const allowEvents = readJsonLines(allowAudit);
      if (allowEvents.length !== 1 ||
          allowEvents[0].kind !== "function.constructor" ||
          allowEvents[0].decision !== "allow" ||
          allowEvents[0].source?.line !== 1)
        throw new Error(`Loader Function allow audit ${mode} mismatch: ${JSON.stringify(allowEvents)}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("Loader: --audit-log fails closed when the output cannot be opened...", async () => {
  const tmp = makeTmp();
  try {
    const proc = Bun.spawnSync([LOADER, `--audit-log=${tmp}`], {
      stdin: new TextEncoder().encode("1;\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode === 0)
      throw new Error("Loader audit log open failure should be fatal");
  } finally {
    clean(tmp);
  }
});

await section("Loader: --log and --audit-log reject the same output path...", async () => {
  const tmp = makeTmp();
  try {
    const shared = join(tmp, "combined.log");
    const alias = `${tmp}/./combined.log`;
    const proc = Bun.spawnSync(
      [LOADER, `--log=${shared}`, `--audit-log=${alias}`],
      {
        stdin: new TextEncoder().encode('console.log("hello");\n'),
        stdout: "pipe",
        stderr: "pipe",
      },
    );
    if (proc.exitCode === 0)
      throw new Error("Loader should reject colliding log output paths");
    const diagnostic = proc.stdout.toString() + proc.stderr.toString();
    if (!diagnostic.includes("must write to different files"))
      throw new Error(`Loader collision error mismatch: ${diagnostic}`);
    if (existsSync(shared))
      throw new Error("Loader should validate colliding paths before opening either file");
  } finally {
    clean(tmp);
  }
});

// -- --global / --globals -------------------------------------------------------

await section("Loader: --host-environment module controls time, zone, and random streams...", async () => {
  const tmp = makeTmp();
  try {
    const providerPath = join(tmp, "host-environment.js");
    writeFileSync(
      providerPath,
      [
        "let epoch = 1700000000000000000n;",
        "let monotonic = 0n;",
        "export const epochNanoseconds = () => { const value = epoch; epoch += 1000000n; return value; };",
        "export const monotonicNanoseconds = () => { const value = monotonic; monotonic += 1000000n; return value; };",
        'export const timeZoneIdentifier = () => "Europe/London";',
        "export const random = (streamId) => streamId === 0n ? 0.25 : 0.75;",
        "",
      ].join("\n"),
    );

    const source = [
      "const child = new ShadowRealm();",
      "[",
      "  Date.now(),",
      "  Temporal.Now.instant().epochNanoseconds.toString(),",
      "  Temporal.Now.timeZoneId(),",
      '  new Intl.DateTimeFormat("en").resolvedOptions().timeZone,',
      "  Math.random(),",
      "  child.evaluate(\"Math.random()\"),",
      "  performance.timeOrigin,",
      "  performance.now(),",
      '].join("|");',
      "",
    ].join("\n");
    const expected =
      "1700000000001|1700000000002000000|Europe/London|Europe/London|0.25|0.75|1700000000000|1.5";

    for (const mode of ["interpreted", "bytecode"] as const) {
      const result = runLoaderJson(source, [
        `--host-environment=${providerPath}`,
        "--unsafe-shadowrealm",
        `--mode=${mode}`,
      ]);
      if (result.exitCode !== 0 || result.json.files?.[0]?.result !== expected)
        throw new Error(
          `Loader host environment ${mode} expected ${expected}, got ${result.json.files?.[0]?.result}${result.stderr}`,
        );
    }

    const conflict = runLoaderJson("0;\n", [
      `--host-environment=${providerPath}`,
      "--deterministic",
    ]);
    const conflictOutput = JSON.stringify(conflict.json) + conflict.stderr;
    if (
      conflict.exitCode === 0 ||
      !conflictOutput.includes("cannot be combined with --deterministic")
    )
      throw new Error(
        `Loader should reject conflicting host environment options, got: ${conflictOutput}`,
      );
  } finally {
    clean(tmp);
  }
});

await section("Loader: --global option...", async () => {
  const { json } = runLoaderJson("x + y;\n", ["--global", "x=10", "--global", "y=20"]);
  if (json.files?.[0]?.result !== 30) throw new Error(`--global x+y should be 30, got ${json.files?.[0]?.result}`);
});

await section("Loader: ShadowRealm.importValue inherits the host module aliases...", async () => {
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "real.js"), "export const v = 99;\n");
    const entry = join(tmp, "entry.js");
    writeFileSync(
      entry,
      [
        "const realm = new ShadowRealm();",
        'realm.importValue("aliased", "v").then(',
        '  (x) => console.log("child-resolved: " + x),',
        '  (e) => console.log("child-rejected: " + (e && e.constructor && e.constructor.name)),',
        ");",
        "",
      ].join("\n"),
    );
    const proc = Bun.spawnSync(
      [LOADER, entry, "--unsafe-shadowrealm", "--alias", `aliased=${join(tmp, "real.js")}`],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (proc.exitCode !== 0)
      throw new Error(`Loader ShadowRealm alias import exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    const out = proc.stdout.toString();
    if (!out.includes("child-resolved: 99"))
      throw new Error(`ShadowRealm.importValue should resolve a host alias in the child realm, got: ${out}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: relative aliases use the invocation or config directory...", async () => {
  const tmp = makeTmp();
  try {
    const loader = resolve(LOADER);
    const project = join(tmp, "project");
    mkdirSync(join(project, "api-tests"), { recursive: true });
    mkdirSync(join(project, "src"), { recursive: true });
    writeFileSync(
      join(project, "api-tests", "alias.test.js"),
      [
        'import { value } from "@/value";',
        "console.log(value);",
        'new ShadowRealm().importValue("@/value", "value")',
        '  .then((childValue) => console.log("child-" + childValue));',
        "",
      ].join("\n"),
    );
    writeFileSync(join(project, "src", "value.js"), 'export const value = "project-root";\n');

    for (const mode of ["interpreted", "bytecode"] as const) {
      const cliProc = Bun.spawnSync(
        [
          loader,
          "api-tests/alias.test.js",
          "--source-type=module",
          `--mode=${mode}`,
          "--unsafe-shadowrealm",
          "--alias",
          "@/=./src/",
        ],
        { cwd: project, stdout: "pipe", stderr: "pipe" },
      );
      if (cliProc.exitCode !== 0 ||
          !containsLine(cliProc.stdout.toString(), "project-root") ||
          !containsLine(cliProc.stdout.toString(), "child-project-root"))
        throw new Error(
          `Loader ${mode} relative CLI alias should resolve from the invocation directory: ` +
          `${cliProc.stdout}${cliProc.stderr}`,
        );
    }

    const baseConfigDirectory = join(tmp, "base-config");
    mkdirSync(baseConfigDirectory, { recursive: true });
    writeFileSync(
      join(baseConfigDirectory, "goccia.json"),
      JSON.stringify({ alias: ["@/=./src/"] }),
    );
    writeFileSync(
      join(project, "goccia.json"),
      JSON.stringify({
        extends: "../base-config/goccia.json",
        "source-type": "module",
        "unsafe-shadowrealm": true,
      }),
    );
    for (const mode of ["interpreted", "bytecode"] as const) {
      const configProc = Bun.spawnSync(
        [loader, "project/api-tests/alias.test.js", `--mode=${mode}`],
        { cwd: tmp, stdout: "pipe", stderr: "pipe" },
      );
      if (configProc.exitCode !== 0 ||
          !containsLine(configProc.stdout.toString(), "project-root") ||
          !containsLine(configProc.stdout.toString(), "child-project-root"))
        throw new Error(
          `Loader ${mode} inherited relative config alias should resolve from ` +
          `the active config directory: ${configProc.stdout}${configProc.stderr}`,
        );
    }
  } finally {
    clean(tmp);
  }
});

await section("Loader: --globals file...", async () => {
  const tmp = makeTmp();
  try {
    const globalsPath = join(tmp, "globals.json");
    writeFileSync(globalsPath, JSON.stringify({ name: "goccia" }));
    const { json } = runLoaderJson("name;\n", [`--globals=${globalsPath}`, "--mode=bytecode"]);
    if (json.files?.[0]?.result !== "goccia") throw new Error(`--globals should set name to "goccia", got ${json.files?.[0]?.result}`);
  } finally {
    clean(tmp);
  }
});

/**
 * Builds a throwaway project with a node_modules tree covering the shapes
 * docs/module-resolution.md describes: an exports map with a wildcard, a
 * transitive bare dependency behind "main", and a CommonJS-only package.
 * Returns the project directory.
 */
const writeNodeModulesProject = (tmp: string): string => {
  const project = join(tmp, "project");
  const packageDirectory = (name: string): string => {
    const directory = join(project, "node_modules", name);
    mkdirSync(directory, { recursive: true });
    return directory;
  };

  mkdirSync(project, { recursive: true });
  writeFileSync(
    join(project, "app.js"),
    [
      'import { chain } from "pkg-exports";',
      'import { widen } from "pkg-exports/sub/widen";',
      "console.log(chain() + \":\" + widen(21));",
      "",
    ].join("\n"),
  );

  const exportsPackage = packageDirectory("pkg-exports");
  mkdirSync(join(exportsPackage, "src"), { recursive: true });
  writeFileSync(
    join(exportsPackage, "package.json"),
    JSON.stringify({
      name: "pkg-exports",
      type: "module",
      exports: { ".": { import: "./index.js" }, "./sub/*": "./src/*.ts" },
    }),
  );
  writeFileSync(
    join(exportsPackage, "index.js"),
    ['import { label } from "pkg-main";', "export const chain = () => label;", ""].join("\n"),
  );
  writeFileSync(
    join(exportsPackage, "src", "widen.ts"),
    "export const widen = (value: number): number => value * 2;\n",
  );

  const mainPackage = packageDirectory("pkg-main");
  writeFileSync(
    join(mainPackage, "package.json"),
    JSON.stringify({ name: "pkg-main", type: "module", main: "./entry.js" }),
  );
  writeFileSync(join(mainPackage, "entry.js"), 'export const label = "chained";\n');

  const commonjsPackage = packageDirectory("pkg-commonjs");
  writeFileSync(
    join(commonjsPackage, "package.json"),
    JSON.stringify({ name: "pkg-commonjs", main: "./index.js" }),
  );
  writeFileSync(
    join(commonjsPackage, "index.js"),
    ['const helper = require("./helper.js");', "module.exports = { helper };", ""].join("\n"),
  );
  writeFileSync(join(commonjsPackage, "helper.js"), "module.exports = 1;\n");

  return project;
};

await section("Loader: bare specifiers stay sealed without --allow-node-modules...", async () => {
  const tmp = makeTmp();
  try {
    const project = writeNodeModulesProject(tmp);
    const proc = Bun.spawnSync([resolve(LOADER), "app.js", "--source-type=module"], {
      cwd: project,
      stdout: "pipe",
      stderr: "pipe",
    });
    const out = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode === 0)
      throw new Error(`A bare specifier must fail without the opt-in, got: ${out}`);
    if (!out.includes('Cannot resolve bare module specifier "pkg-exports"'))
      throw new Error(`Expected the sealed-default message, got: ${out}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: --allow-node-modules resolves exports, wildcards, and transitive deps...", async () => {
  const tmp = makeTmp();
  try {
    const project = writeNodeModulesProject(tmp);
    for (const mode of ["interpreted", "bytecode"] as const) {
      const proc = Bun.spawnSync(
        [resolve(LOADER), "app.js", "--source-type=module", `--mode=${mode}`, "--allow-node-modules"],
        { cwd: project, stdout: "pipe", stderr: "pipe" },
      );
      if (proc.exitCode !== 0 || !containsLine(proc.stdout.toString(), "chained:42"))
        throw new Error(
          `--allow-node-modules should resolve the ${mode} run: ${proc.stdout}${proc.stderr}`,
        );
    }

    // The config-file spelling has to reach the resolver too, since a project
    // that needs the capability wants it recorded, not retyped.
    writeFileSync(join(project, "goccia.json"), JSON.stringify({ "allow-node-modules": true }));
    const configProc = Bun.spawnSync([resolve(LOADER), "app.js", "--source-type=module"], {
      cwd: project,
      stdout: "pipe",
      stderr: "pipe",
    });
    if (configProc.exitCode !== 0 || !containsLine(configProc.stdout.toString(), "chained:42"))
      throw new Error(
        `"allow-node-modules": true should resolve from a config file: ${configProc.stdout}${configProc.stderr}`,
      );
  } finally {
    clean(tmp);
  }
});

await section("Loader: --allow-node-modules=<dir> caps the ancestor walk...", async () => {
  const tmp = makeTmp();
  try {
    const project = writeNodeModulesProject(tmp);
    // The ceiling names a directory below the one holding node_modules, so the
    // walk can never reach the packages the unbounded form finds.
    const inner = join(project, "src");
    mkdirSync(inner, { recursive: true });
    writeFileSync(join(inner, "app.js"), 'import "pkg-exports";\n');
    const proc = Bun.spawnSync(
      [resolve(LOADER), "src/app.js", "--source-type=module", `--allow-node-modules=${inner}`],
      { cwd: project, stdout: "pipe", stderr: "pipe" },
    );
    const out = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode === 0)
      throw new Error(`A ceiling below node_modules must not resolve, got: ${out}`);
    if (!out.includes('Module not found: "pkg-exports"'))
      throw new Error(`Expected a not-found failure inside the ceiling, got: ${out}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: a CommonJS package is refused by name, not parsed...", async () => {
  const tmp = makeTmp();
  try {
    const project = writeNodeModulesProject(tmp);
    writeFileSync(join(project, "cjs.js"), 'import "pkg-commonjs";\n');
    const proc = Bun.spawnSync(
      [resolve(LOADER), "cjs.js", "--source-type=module", "--allow-node-modules"],
      { cwd: project, stdout: "pipe", stderr: "pipe" },
    );
    const out = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode === 0) throw new Error(`A CommonJS package must fail, got: ${out}`);
    if (
      !out.includes(
        'Package "pkg-commonjs" resolved to a CommonJS file (index.js); GocciaScript loads only ES modules',
      )
    )
      throw new Error(`Expected the named CommonJS refusal, got: ${out}`);
    if (out.includes("SyntaxError"))
      throw new Error(`A CommonJS package must not reach the parser, got: ${out}`);
    // ADR 0108: the expanded path is a host-only diagnostic line, never part of
    // the message a script can read. Matched by tail rather than by the full
    // path the test built, because the platform may hand back a temp directory
    // through a symlink (macOS /var -> /private/var) that the engine resolves.
    if (!/Resolved to: .*[\\/]node_modules[\\/]pkg-commonjs[\\/]index\.js/.test(out))
      throw new Error(`Expected the host-side resolved-path line, got: ${out}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: --globals JSON5 file...", async () => {
  const tmp = makeTmp();
  try {
    const globalsPath = join(tmp, "globals.json5");
    writeFileSync(globalsPath, [
      "{",
      "  // JSON5 globals allow comments and unquoted keys",
      "  unquoted: 'goccia',",
      "  maxRetries: 3,",
      "  nested: { enabled: true, },",
      "}",
      "",
    ].join("\n"));
    const { exitCode, json, stderr } = runLoaderJson("unquoted + ':' + maxRetries + ':' + nested.enabled;\n", [`--globals=${globalsPath}`]);
    if (exitCode !== 0) throw new Error(`--globals JSON5 exited ${exitCode}: ${stderr}`);
    if (json.files?.[0]?.result !== "goccia:3:true")
      throw new Error(`--globals JSON5 should inject parsed values, got ${json.files?.[0]?.result}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: --globals TOML file...", async () => {
  const tmp = makeTmp();
  try {
    const globalsPath = join(tmp, "globals.toml");
    const nameValue = "Jos\u00e9";
    const quotedKey = "d\u00e9j\u00e0";
    writeFileSync(globalsPath, [
      `name = "${nameValue}"`,
      `"${quotedKey}" = "vu"`,
      "count = 3",
      "",
    ].join("\n"));
    const { exitCode, json, stderr } = runLoaderJson(`name + ':' + globalThis["${quotedKey}"] + ':' + count;\n`, [`--globals=${globalsPath}`, "--mode=bytecode"]);
    if (exitCode !== 0) throw new Error(`--globals TOML exited ${exitCode}: ${stderr}`);
    if (json.files?.[0]?.result !== "Jos\u00e9:vu:3")
      throw new Error(`--globals TOML should inject UTF-8 parsed values, got ${json.files?.[0]?.result}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: --global overrides --globals file...", async () => {
  const tmp = makeTmp();
  try {
    const globalsPath = join(tmp, "globals.json");
    writeFileSync(globalsPath, JSON.stringify({ name: "goccia" }));
    const { json } = runLoaderJson("name;\n", [`--globals=${globalsPath}`, "--global", "name=override"]);
    if (json.files?.[0]?.result !== "override") throw new Error(`--global should override --globals, got ${json.files?.[0]?.result}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: --globals from JS module...", async () => {
  const tmp = makeTmp();
  try {
    const moduleJsPath = join(tmp, "module.js");
    writeFileSync(moduleJsPath, 'export const name = "module-value";\n');
    const { json } = runLoaderJson("name;\n", [`--globals=${moduleJsPath}`]);
    if (json.files?.[0]?.result !== "module-value") throw new Error(`--globals JS module should set name, got ${json.files?.[0]?.result}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: --global cannot override built-in...", async () => {
  const res = await $`echo '1;' | ${LOADER} --global console=1 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Overriding built-in should fail");
  if (!res.text().includes("Cannot override built-in global")) throw new Error("Should mention 'Cannot override built-in global'");
});

// -- Coverage -------------------------------------------------------------------

await section("Loader: coverage summary...", async () => {
  const out = await $`echo 'const x = 1 + 2; x;' | ${LOADER} --coverage 2>&1`.text();
  if (!out.includes("Coverage Summary:")) throw new Error(`Expected "Coverage Summary:", got: ${out}`);
});

await section("Loader: coverage --output=json not corrupted...", async () => {
  const { json } = runLoaderJson("const x = 1 + 2;\nx;\n", ["--coverage"]);
  if (json.ok === undefined) throw new Error(`Coverage --output=json should produce valid JSON with ok field`);
});

{
  const tmp = makeTmp();
  try {
    console.log("Loader: coverage LCOV...");
    const lcovPath = join(tmp, "coverage.lcov");
    await $`echo 'const x = 1 + 2; x;' | ${LOADER} --coverage-format=lcov --coverage-output=${lcovPath}`.quiet();
    if (!existsSync(lcovPath)) throw new Error("LCOV file should exist");
    const lcov = readFileSync(lcovPath, "utf-8");
    if (!lcov.includes("SF:")) throw new Error('LCOV should contain "SF:"');
    if (!lcov.includes("DA:")) throw new Error('LCOV should contain "DA:"');

    console.log("Loader: coverage JSON...");
    const jsonCovPath = join(tmp, "coverage.json");
    await $`echo 'const x = 1 + 2; x;' | ${LOADER} --coverage-format=json --coverage-output=${jsonCovPath}`.quiet();
    if (!existsSync(jsonCovPath)) throw new Error("JSON coverage file should exist");
    const jsonCov = readFileSync(jsonCovPath, "utf-8");
    if (!jsonCov.includes('"path":')) throw new Error('JSON coverage should contain "path":');

    console.log("Loader: function coverage (--coverage implies bytecode, so --mode is a no-op)...");
    const functionSourcePath = join(tmp, "function-coverage.js");
    writeFileSync(
      functionSourcePath,
      [
        "const called = () => 1;",
        "const neverCalled = () => 2;",
        "called();",
        "",
      ].join("\n"),
    );
    for (const modeArgs of [[], ["--mode=bytecode"]]) {
      const modeName = modeArgs.length === 0 ? "default" : "explicit-bytecode";
      const functionLcovPath = join(tmp, `function-${modeName}.lcov`);
      await $`${LOADER} ${modeArgs} --coverage --coverage-format=lcov --coverage-output=${functionLcovPath} ${functionSourcePath}`.quiet();
      const functionLcov = readFileSync(functionLcovPath, "utf-8");
      if (!functionLcov.includes("FN:1,called")) throw new Error(`${modeName} LCOV should define called`);
      if (!functionLcov.includes("FN:2,neverCalled")) throw new Error(`${modeName} LCOV should define neverCalled`);
      if (!functionLcov.includes("FNDA:1,called")) throw new Error(`${modeName} LCOV should count called once`);
      if (!functionLcov.includes("FNDA:0,neverCalled")) throw new Error(`${modeName} LCOV should retain the uncalled function`);
      if (!functionLcov.includes("FNF:") || !functionLcov.includes("FNH:")) {
        throw new Error(`${modeName} LCOV should report function totals`);
      }
      const functionNames = [...functionLcov.matchAll(/^FNDA:\d+,(.*)$/gm)]
        .map((match) => match[1])
        .sort();
      if (functionNames.join(",") !== "called,neverCalled") {
        throw new Error(`${modeName} LCOV should contain only user functions, got ${functionNames.join(", ")}`);
      }
    }
    const functionJsonPath = join(tmp, "function-coverage.json");
    await $`${LOADER} --coverage --coverage-format=json --coverage-output=${functionJsonPath} ${functionSourcePath}`.quiet();
    const functionFile = coverageEntryFor(functionJsonPath, functionSourcePath);
    if (!functionFile) throw new Error("JSON coverage should contain the source file");
    const functionIdsByName = Object.fromEntries(
      Object.entries(functionFile.fnMap).map(([id, entry]: [string, any]) => [entry.name, id]),
    );
    if (functionFile.f[functionIdsByName.called] !== 1) {
      throw new Error("JSON f should count called once");
    }
    if (functionFile.f[functionIdsByName.neverCalled] !== 0) {
      throw new Error("JSON f should retain neverCalled with zero hits");
    }

    console.log("Loader: generator function coverage (--mode is a no-op under --coverage)...");
    const generatorSourcePath = join(tmp, "generator-function-coverage.js");
    writeFileSync(
      generatorSourcePath,
      [
        "function* generatorFunction() { yield 1; }",
        "function* generatorWithDefault(value = 2) { yield value; }",
        "function* nestedGenerator() {",
        "  function innerDeclaration() { return 6; }",
        "  yield 0;",
        "  const innerArrow = () => 7;",
        "  innerDeclaration();",
        "  innerArrow();",
        "  yield 1;",
        "}",
        "async function* asyncGeneratorFunction() { yield 3; }",
        "const holder = {",
        "  *generatorMethod() { yield 4; },",
        "  async *asyncGeneratorMethod() { yield 5; },",
        "};",
        "const generator = generatorFunction();",
        "generator.next();",
        "generator.next();",
        "const defaultGenerator = generatorWithDefault();",
        "defaultGenerator.next();",
        "const nested = nestedGenerator();",
        "nested.next();",
        "nested.next();",
        "nested.next();",
        "asyncGeneratorFunction();",
        "holder.generatorMethod();",
        "holder.asyncGeneratorMethod();",
        "",
      ].join("\n"),
    );
    for (const modeArgs of [[], ["--mode=bytecode"]]) {
      const modeName = modeArgs.length === 0 ? "default" : "explicit-bytecode";
      const generatorLcovPath = join(tmp, `generator-function-${modeName}.lcov`);
      await $`${LOADER} ${modeArgs} --compat-function --coverage --coverage-format=lcov --coverage-output=${generatorLcovPath} ${generatorSourcePath}`.quiet();
      const generatorLcov = readFileSync(generatorLcovPath, "utf-8");
      for (const name of [
        "generatorFunction",
        "generatorWithDefault",
        "nestedGenerator",
        "innerDeclaration",
        "innerArrow",
        "asyncGeneratorFunction",
        "generatorMethod",
        "asyncGeneratorMethod",
      ]) {
        if (!generatorLcov.includes(`FNDA:1,${name}`)) {
          throw new Error(`${modeName} LCOV should count ${name} once`);
        }
      }
    }

    console.log("Loader: uncalled declarations keep names (interpreted + bytecode)...");
    const declarationSourcePath = join(tmp, "function-declaration-coverage.js");
    writeFileSync(
      declarationSourcePath,
      [
        "function ordinaryNeverCalled() { return 1; }",
        "function* generatorNeverCalled() { yield 2; }",
        "",
      ].join("\n"),
    );
    for (const modeArgs of [[], ["--mode=bytecode"]]) {
      const modeName = modeArgs.length === 0 ? "default" : "explicit-bytecode";
      const declarationLcovPath = join(tmp, `function-declaration-${modeName}.lcov`);
      await $`${LOADER} ${modeArgs} --compat-function --coverage --coverage-format=lcov --coverage-output=${declarationLcovPath} ${declarationSourcePath}`.quiet();
      const declarationLcov = readFileSync(declarationLcovPath, "utf-8");
      for (const name of ["ordinaryNeverCalled", "generatorNeverCalled"]) {
        if (!declarationLcov.includes(`FNDA:0,${name}`)) {
          throw new Error(`${modeName} LCOV should retain the name of ${name}`);
        }
      }
    }

    console.log("Loader: LCOV function names cannot inject tracefile records...");
    const escapedFunctionNameSourcePath = join(tmp, "escaped-function-name.js");
    writeFileSync(
      escapedFunctionNameSourcePath,
      // Static string-literal keys, not computed ones: coverage always runs in
      // bytecode mode, where a computed method key is not known at compile time
      // and the function is named "<method>@<line>:<column>". Literal keys carry
      // the real CR/LF and backslash characters this escaping check needs.
      [
        "const holder = {",
        '  "line\\r\\nbreak"() { return 1; },',
        '  "line\\\\r\\\\nbreak"() { return 2; },',
        "};",
        'holder["line\\r\\nbreak"]();',
        'holder["line\\\\r\\\\nbreak"]();',
        "",
      ].join("\n"),
    );
    const escapedFunctionNameLcovPath = join(tmp, "escaped-function-name.lcov");
    await $`${LOADER} --coverage --coverage-format=lcov --coverage-output=${escapedFunctionNameLcovPath} ${escapedFunctionNameSourcePath}`.quiet();
    const escapedFunctionNameLcov = readFileSync(escapedFunctionNameLcovPath, "utf-8");
    const escapedFunctionRecords = escapedFunctionNameLcov.split(/\r?\n/);
    if (!escapedFunctionRecords.some((line) => /^FN:\d+,line\\r\\nbreak$/.test(line)) ||
        !escapedFunctionRecords.includes("FNDA:1,line\\r\\nbreak")) {
      throw new Error("LCOV should escape carriage returns and newlines in function names");
    }
    if (!escapedFunctionRecords.some((line) => /^FN:\d+,line\\\\r\\\\nbreak$/.test(line)) ||
        !escapedFunctionRecords.includes("FNDA:1,line\\\\r\\\\nbreak")) {
      throw new Error("LCOV should preserve literal backslashes in function names");
    }
    if (escapedFunctionRecords.includes("break") ||
        escapedFunctionRecords.filter((line) => line.startsWith("FNDA:1,line")).length !== 2) {
      throw new Error("LCOV function names must not collide or create extra tracefile records");
    }

    console.log("TestRunner: parallel function coverage merges workers...");
    const workerOnePath = join(tmp, "function-worker-one.js");
    const workerTwoPath = join(tmp, "function-worker-two.js");
    writeFileSync(workerOnePath, 'test("worker one", () => { const workerOne = () => 1; expect(workerOne()).toBe(1); });\n');
    writeFileSync(workerTwoPath, 'test("worker two", () => { const workerTwo = () => 2; expect(workerTwo()).toBe(2); });\n');
    const workerLcovPath = join(tmp, "function-workers.lcov");
    await $`${TESTRUNNER} ${workerOnePath} ${workerTwoPath} --jobs=2 --no-progress --coverage --coverage-format=lcov --coverage-output=${workerLcovPath}`.quiet();
    const workerLcov = readFileSync(workerLcovPath, "utf-8");
    if (!workerLcov.includes("FNDA:1,workerOne") || !workerLcov.includes("FNDA:1,workerTwo")) {
      throw new Error("Parallel LCOV should merge function hits from both workers");
    }

    console.log("Loader: coverage order-independent flags...");
    const orderPath = join(tmp, "order.lcov");
    await $`echo 'const x = 1 + 2; x;' | ${LOADER} --coverage-output=${orderPath} --coverage-format=lcov`.quiet();
    if (!existsSync(orderPath)) throw new Error("Order-independent LCOV should exist");
    if (!readFileSync(orderPath, "utf-8").includes("SF:")) throw new Error("Order-independent LCOV should contain SF:");

    console.log("Loader: coverage bytecode...");
    const bcLcovPath = join(tmp, "bc-coverage.lcov");
    await $`echo 'const x = 1 + 2; x;' | ${LOADER} --mode=bytecode --coverage-format=lcov --coverage-output=${bcLcovPath}`.quiet();
    if (!existsSync(bcLcovPath)) throw new Error("Bytecode LCOV should exist");
    if (!readFileSync(bcLcovPath, "utf-8").includes("DA:")) throw new Error("Bytecode LCOV should contain DA:");

    console.log("Loader: coverage implies bytecode and reports imported modules...");
    const implyDir = join(tmp, "coverage-imply");
    mkdirSync(implyDir, { recursive: true });
    const implyHelperPath = join(implyDir, "helper.js");
    const implyEntryPath = join(implyDir, "entry.js");
    writeFileSync(
      implyHelperPath,
      [
        "export const classify = (n) => {",
        "  if (n > 10) {",
        "    return 'big';",
        "  }",
        "  return 'small';",
        "};",
        "",
        "export const unused = (n) => n - 1;",
        "",
      ].join("\n"),
    );
    writeFileSync(
      implyEntryPath,
      [
        "import { classify } from './helper.js';",
        "",
        "const labels = [1, 42].map((v) => classify(v));",
        "globalThis.__labels = labels.join(',');",
        "",
      ].join("\n"),
    );
    const implyReports: Record<string, Record<string, any>> = {};
    for (const [label, modeArgs] of [
      ["default", []],
      ["interpreted", ["--mode=interpreted"]],
      ["bytecode", ["--mode=bytecode"]],
    ] as [string, string[]][]) {
      const implyJsonPath = join(implyDir, `coverage-${label}.json`);
      await $`${LOADER} ${modeArgs} --coverage --coverage-format=json --coverage-output=${implyJsonPath} ${implyEntryPath}`.quiet();
      const report = readCoverageByBasename(implyJsonPath);
      implyReports[label] = report;
      const helper = report["helper.js"];
      if (!helper) {
        throw new Error(`Coverage (${label}) should report the imported module helper.js`);
      }
      const helperFunctionNames = Object.values(helper.fnMap)
        .map((fn: any) => fn.name)
        .sort();
      if (!helperFunctionNames.includes("classify") || !helperFunctionNames.includes("unused")) {
        throw new Error(
          `Coverage (${label}) should report imported-module functions, got ${helperFunctionNames.join(", ")}`,
        );
      }
      if (Object.keys(helper.branchMap).length === 0) {
        throw new Error(`Coverage (${label}) should report imported-module branches`);
      }
      if (!report["entry.js"]) {
        throw new Error(`Coverage (${label}) should still report the entry file`);
      }
    }
    // --coverage forces bytecode, so --mode is irrelevant to the report.
    for (const label of ["default", "interpreted"]) {
      if (JSON.stringify(implyReports[label]) !== JSON.stringify(implyReports.bytecode)) {
        throw new Error(`Coverage with --mode=${label} should match the --mode=bytecode report`);
      }
    }

    console.log("TestRunner: coverage merges an entry file that is also an import...");
    {
      // shared.test.js is BOTH an entry file named on the command line and an
      // import of main.test.js. The entry role used to be keyed by the spelling
      // as typed while the import role was keyed by its resolved absolute path,
      // so the file produced two report records whose hits were never merged.
      // Canonicalizing both roles to one key collapses them into one record.
      const dualDir = join(tmp, "coverage-dual-role");
      mkdirSync(dualDir, { recursive: true });
      writeFileSync(
        join(dualDir, "shared.test.js"),
        [
          "export const bump = (n) => n + 1;",
          "",
          "test('shared bump', () => {",
          "  expect(bump(1)).toBe(2);",
          "});",
          "",
        ].join("\n"),
      );
      writeFileSync(
        join(dualDir, "main.test.js"),
        [
          "import { bump } from './shared.test.js';",
          "",
          "test('main bump', () => {",
          "  expect(bump(5)).toBe(6);",
          "});",
          "",
        ].join("\n"),
      );

      // The command line must use relative spellings: the mismatch only shows
      // up when the typed path differs from the absolute path that import
      // resolution produces. Binary paths are repo-relative, so resolve them
      // before running with a different working directory.
      const runner = resolve(TESTRUNNER);
      const runDual = async (name: string, files: string[]) => {
        const outPath = join(dualDir, name);
        await $`${runner} ${files} --coverage --coverage-format=json --coverage-output=${name} --no-progress --silent`
          .cwd(dualDir)
          .quiet();
        return JSON.parse(readFileSync(outPath, "utf-8")) as Record<string, any>;
      };

      const sharedRecords = (report: Record<string, any>) =>
        Object.keys(report).filter((key) => key.endsWith("shared.test.js"));

      // Each role in isolation, then both together.
      const entryOnly = await runDual("entry-only.json", ["./shared.test.js"]);
      const importOnly = await runDual("import-only.json", ["./main.test.js"]);
      const bothRoles = await runDual("both-roles.json", ["./shared.test.js", "./main.test.js"]);

      const bothKeys = sharedRecords(bothRoles);
      if (bothKeys.length !== 1) {
        throw new Error(
          `A file that is both entry and import should get exactly one coverage record, got ${bothKeys.length}: ${bothKeys.join(", ")}`,
        );
      }

      // Hits must be merged, not one role's counts overwriting the other's.
      const entryHits = entryOnly[sharedRecords(entryOnly)[0]].s;
      const importHits = importOnly[sharedRecords(importOnly)[0]].s;
      const mergedHits = bothRoles[bothKeys[0]].s;
      for (const statement of Object.keys(mergedHits)) {
        const expected = (entryHits[statement] ?? 0) + (importHits[statement] ?? 0);
        if (mergedHits[statement] !== expected) {
          throw new Error(
            `Dual-role statement ${statement} should carry the summed hits of both roles: expected ${expected}, got ${mergedHits[statement]}`,
          );
        }
      }

      // The same physical file named two different ways must land on one key.
      // Compare against the *real* directory: canonicalization is textual and
      // deliberately does not resolve symlinks, and on macOS the system temp
      // directory is reached through the /var -> /private/var symlink.
      const realDualDir = realpathSync(dualDir);
      const absoluteSpelling = await runDual("absolute-spelling.json", [
        join(realDualDir, "shared.test.js"),
        join(realDualDir, "main.test.js"),
      ]);
      if (JSON.stringify(Object.keys(absoluteSpelling).sort()) !== JSON.stringify(Object.keys(bothRoles).sort())) {
        throw new Error(
          `Absolute and relative command-line spellings should produce identical report keys: ${Object.keys(absoluteSpelling).sort().join(", ")} vs ${Object.keys(bothRoles).sort().join(", ")}`,
        );
      }

      // Separators are '/' in every emitted path on every platform: genhtml and
      // Codecov both mishandle the backslashes Windows path resolution yields.
      const dualLcovPath = join(dualDir, "dual.lcov");
      await $`${runner} ./shared.test.js ./main.test.js --coverage --coverage-format=lcov --coverage-output=dual.lcov --no-progress --silent`
        .cwd(dualDir)
        .quiet();
      const sfRecords = readFileSync(dualLcovPath, "utf-8")
        .split("\n")
        .filter((line) => line.startsWith("SF:"));
      if (sfRecords.length === 0) throw new Error("Dual-role LCOV should contain SF: records");
      for (const record of sfRecords) {
        if (record.includes("\\")) {
          throw new Error(`LCOV SF: paths must use '/' separators, got ${record}`);
        }
      }
      for (const key of Object.keys(bothRoles)) {
        if (key.includes("\\")) {
          throw new Error(`JSON coverage keys must use '/' separators, got ${key}`);
        }
        if (bothRoles[key].path.includes("\\")) {
          throw new Error(`JSON coverage "path" must use '/' separators, got ${bothRoles[key].path}`);
        }
      }
    }

    console.log("Loader: coverage reports functions at their declaration line...");
    // LCOV FN: records where a function is declared. The bytecode VM used to
    // report the first executed instruction instead, which lands on the body's
    // first line for any function whose body starts on a later line.
    const declLinePath = join(tmp, "declaration-line.js");
    writeFileSync(
      declLinePath,
      [
        "const oneLine = () => 1;",
        "const multi = (a) => {",
        "  const b = a + 1;",
        "  return b;",
        "};",
        "oneLine();",
        "multi(1);",
        "",
      ].join("\n"),
    );
    const declLineLcovPath = join(tmp, "declaration-line.lcov");
    await $`${LOADER} --coverage --coverage-format=lcov --coverage-output=${declLineLcovPath} ${declLinePath}`.quiet();
    const declLineRecords = readFileSync(declLineLcovPath, "utf-8").split(/\r?\n/);
    for (const expected of ["FN:1,oneLine", "FN:2,multi", "FNDA:1,oneLine", "FNDA:1,multi"]) {
      if (!declLineRecords.includes(expected)) {
        throw new Error(
          `LCOV should report ${expected}, got ${declLineRecords.filter((l) => l.startsWith("FN")).join(" ")}`,
        );
      }
    }
    // The per-call line hit still belongs to the first executed body line.
    if (!declLineRecords.includes("DA:3,1")) {
      throw new Error("Function body's first line should still record a line hit");
    }

    console.log("TestRunner: coverage hit counts are identical across --jobs...");
    const jobsDir = join(tmp, "coverage-jobs");
    mkdirSync(jobsDir, { recursive: true });
    writeFileSync(
      join(jobsDir, "shared.js"),
      [
        "export const step = (n) => {",
        "  const doubled = n * 2;",
        "  return doubled > 4 ? 'high' : 'low';",
        "};",
        "",
      ].join("\n"),
    );
    // Asymmetric arm usage: a merge that records one hit per covered entry
    // instead of summing counts collapses these totals.
    writeFileSync(
      join(jobsDir, "a.test.js"),
      [
        "import { step } from './shared.js';",
        'test("a", () => {',
        "  expect(step(1)).toBe('low');",
        "  expect(step(2)).toBe('low');",
        "  expect(step(5)).toBe('high');",
        "});",
        "",
      ].join("\n"),
    );
    writeFileSync(
      join(jobsDir, "b.test.js"),
      [
        "import { step } from './shared.js';",
        'test("b", () => {',
        "  expect(step(2)).toBe('low');",
        "  expect(step(9)).toBe('high');",
        "});",
        "",
      ].join("\n"),
    );
    const jobsCounts: Record<string, string> = {};
    for (const jobs of ["1", "2", "4"]) {
      const jobsJsonPath = join(jobsDir, `coverage-jobs-${jobs}.json`);
      await $`${TESTRUNNER} ${join(jobsDir, "a.test.js")} ${join(jobsDir, "b.test.js")} --jobs=${jobs} --no-progress --coverage --coverage-format=json --coverage-output=${jobsJsonPath}`.quiet();
      const shared = readCoverageByBasename(jobsJsonPath)["shared.js"];
      if (!shared) throw new Error(`--jobs=${jobs} coverage should report the shared module`);
      jobsCounts[jobs] = JSON.stringify({ s: shared.s, b: shared.b, f: shared.f });
    }
    // A statement executed 5 times must report 5, not "2 workers touched it".
    if (JSON.parse(jobsCounts["1"]).s["2"] !== 5) {
      throw new Error(`--jobs=1 should count the shared statement 5 times, got ${jobsCounts["1"]}`);
    }
    for (const jobs of ["2", "4"]) {
      if (jobsCounts[jobs] !== jobsCounts["1"]) {
        throw new Error(
          `--jobs=${jobs} coverage counts should equal --jobs=1: ${jobsCounts[jobs]} vs ${jobsCounts["1"]}`,
        );
      }
    }

    console.log("Loader: coverage hit counts are identical across --jobs...");
    // The loader runs its own worker pools. Without EnableCoverage plus a merge
    // back into the main tracker, worker hits are silently dropped and the
    // shared module reports only what the main thread executed. The
    // --coverage-output-only run additionally pins that the flag implies
    // --coverage, since the pool reads Enabled to decide whether to merge.
    const loaderJobsDir = join(tmp, "loader-coverage-jobs");
    mkdirSync(loaderJobsDir, { recursive: true });
    writeFileSync(
      join(loaderJobsDir, "shared.js"),
      [
        "export const step = (n) => {",
        "  const doubled = n * 2;",
        "  return doubled > 4 ? 'high' : 'low';",
        "};",
        "",
      ].join("\n"),
    );
    writeFileSync(
      join(loaderJobsDir, "a.js"),
      [
        "import { step } from './shared.js';",
        "console.log(step(1), step(2), step(5));",
        "",
      ].join("\n"),
    );
    writeFileSync(
      join(loaderJobsDir, "b.js"),
      [
        "import { step } from './shared.js';",
        "console.log(step(2), step(9));",
        "",
      ].join("\n"),
    );
    const loaderEntries = [join(loaderJobsDir, "a.js"), join(loaderJobsDir, "b.js")];
    // Both loader worker-pool paths (the plain run and the --output=json run),
    // plus an "implied" run that omits --coverage on purpose.
    for (const label of ["plain", "json", "implied"]) {
      const counts: Record<string, string> = {};
      for (const jobs of ["1", "2", "4"]) {
        const outPath = join(loaderJobsDir, `coverage-${label}-${jobs}.json`);
        const args = [
          resolve(LOADER),
          ...loaderEntries,
          `--jobs=${jobs}`,
          ...(label === "implied" ? [] : ["--coverage"]),
          ...(label === "json" ? ["--output=json"] : []),
          "--coverage-format=json",
          `--coverage-output=${outPath}`,
        ];
        const proc = Bun.spawnSync(args, { stdout: "pipe", stderr: "pipe" });
        if (proc.exitCode !== 0)
          throw new Error(`Loader coverage (${label}, jobs=${jobs}) exited ${proc.exitCode}: ${proc.stderr.toString()}`);
        const report = readCoverageByBasename(outPath);
        if (Object.keys(report).some((f) => f.startsWith("<")))
          throw new Error(`Loader coverage (${label}, jobs=${jobs}) leaked an internal source: ${Object.keys(report).join(", ")}`);
        const shared = report["shared.js"];
        if (!shared)
          throw new Error(`Loader coverage (${label}, jobs=${jobs}) should report the shared module`);
        counts[jobs] = JSON.stringify({ s: shared.s, b: shared.b, f: shared.f });
      }
      // The shared arrow runs 5 times in total across the two entry files.
      if (JSON.parse(counts["1"]).f["1"] !== 5)
        throw new Error(`Loader coverage (${label}) --jobs=1 should count 5 function hits, got ${counts["1"]}`);
      for (const jobs of ["2", "4"])
        if (counts[jobs] !== counts["1"])
          throw new Error(
            `Loader coverage (${label}) --jobs=${jobs} should equal --jobs=1: ${counts[jobs]} vs ${counts["1"]}`,
          );
    }

    console.log("Loader: branch coverage via TestRunner...");
    const branchLcovPath = join(tmp, "branch.lcov");
    await $`${TESTRUNNER} --coverage --coverage-format=lcov --coverage-output=${branchLcovPath} --no-progress tests/language/statements/if/if-else-statements.js`.quiet();
    const branchLcov = readFileSync(branchLcovPath, "utf-8");
    if (!branchLcov.includes("BRDA:")) throw new Error('Branch LCOV should contain "BRDA:"');
    if (!branchLcov.includes("BRF:")) throw new Error('Branch LCOV should contain "BRF:"');
    if (!branchLcov.includes("BRH:")) throw new Error('Branch LCOV should contain "BRH:"');

    const branchJsonPath = join(tmp, "branch.json");
    await $`${TESTRUNNER} --coverage --coverage-format=json --coverage-output=${branchJsonPath} --no-progress tests/language/statements/if/if-else-statements.js`.quiet();
    const branchJson = readFileSync(branchJsonPath, "utf-8");
    if (!branchJson.includes('"branchMap":')) throw new Error('Branch JSON should contain "branchMap":');
    if (!branchJson.includes('"b":')) throw new Error('Branch JSON should contain "b":');

    console.log("TestRunner: parallel coverage excludes internal warm-up sources...");
    const parallelFirst = join(tmp, "parallel-coverage-a.js");
    const parallelSecond = join(tmp, "parallel-coverage-b.js");
    writeFileSync(parallelFirst, 'test("a", () => { expect(1).toBe(1); });\n');
    writeFileSync(parallelSecond, 'test("b", () => { expect(2).toBe(2); });\n');
    for (const mode of ["interpreted", "bytecode"]) {
      const parallelJsonPath = join(tmp, `parallel-${mode}.json`);
      const modeArgs = mode === "bytecode" ? ["--mode=bytecode"] : [];
      const proc = Bun.spawnSync(
        [
          resolve(TESTRUNNER),
          parallelFirst,
          parallelSecond,
          "--no-progress",
          "--no-results",
          "--jobs=2",
          "--coverage",
          "--coverage-format=json",
          `--coverage-output=${parallelJsonPath}`,
          ...modeArgs,
        ],
        { stdout: "pipe", stderr: "pipe" },
      );
      if (proc.exitCode !== 0)
        throw new Error(`Parallel ${mode} coverage exited ${proc.exitCode}: ${proc.stderr.toString()}`);
      const parallelCoverage = JSON.parse(readFileSync(parallelJsonPath, "utf-8"));
      if (Object.hasOwn(parallelCoverage, "<thread-init>"))
        throw new Error(`Parallel ${mode} coverage should exclude internal <thread-init> source`);
      // Raw keys above (the internal-source check needs the literal
      // "<thread-init>"); user sources by basename, since a report key is
      // canonical and a native path is not.
      const parallelByBasename = readCoverageByBasename(parallelJsonPath);
      for (const file of [parallelFirst, parallelSecond]) {
        const basename = file.split(/[\\/]/).pop() as string;
        if (!Object.hasOwn(parallelByBasename, basename))
          throw new Error(`Parallel ${mode} coverage should retain user source ${file}`);
      }
    }

    console.log("Loader: JSX coverage source-map translation...");
    const jsxPath = join(tmp, "coverage-test.jsx");
    writeFileSync(
      jsxPath,
      [
        "const createElement = (t, p, ...c) => ({ t, p, c });",
        "const Greet = (props) => {",
        '  const msg = props.name ? props.name : "world";',
        "  return <div>{msg}</div>;",
        "};",
        'Greet({ name: "hi" });',
        "Greet({});",
        "",
      ].join("\n"),
    );

    const jsxLcovPath = join(tmp, "jsx-coverage.lcov");
    await $`${LOADER} --coverage --coverage-format=lcov --coverage-output=${jsxLcovPath} ${jsxPath}`.quiet();
    const jsxLcov = readFileSync(jsxLcovPath, "utf-8");
    if (!jsxLcov.includes("BRDA:3,")) throw new Error("JSX LCOV should have branch on line 3");
    if (!jsxLcov.includes("FN:2,Greet")) throw new Error("JSX LCOV should map Greet to original line 2");

    const jsxJsonPath = join(tmp, "jsx-coverage.json");
    await $`${LOADER} --coverage --coverage-format=json --coverage-output=${jsxJsonPath} ${jsxPath}`.quiet();
    if (!readFileSync(jsxJsonPath, "utf-8").includes('"line":3')) throw new Error('JSX JSON should have "line":3');
  } finally {
    clean(tmp);
  }
}

// ============================================================================
// GocciaTestRunner
// ============================================================================

await section("TestRunner: Vitest-compatible snapshot lifecycle (interpreted + bytecode)...", async () => {
  const tmp = makeTmp();
  const localEnv = { ...process.env };
  // Keep this list aligned with GocciaTestRunner.IsContinuousIntegration.
  for (const name of [
    "CI",
    "CONTINUOUS_INTEGRATION",
    "APPVEYOR",
    "AWS_APP_ID",
    "SYSTEM_TEAMFOUNDATIONCOLLECTIONURI",
    "INPUT_AZURE_STATIC_WEB_APPS_API_TOKEN",
    "AC_APPCIRCLE",
    "bamboo_planKey",
    "BITBUCKET_COMMIT",
    "BITRISE_IO",
    "BUDDY_WORKSPACE_ID",
    "BUILDKITE",
    "CIRCLECI",
    "CIRRUS_CI",
    "CF_PAGES",
    "WORKERS_CI",
    "K_SERVICE",
    "CLOUD_RUN_JOB",
    "CODEBUILD_BUILD_ARN",
    "CF_BUILD_ID",
    "DRONE",
    "DRONE_BUILD_EVENT",
    "DSARI",
    "GITHUB_ACTIONS",
    "GITLAB_CI",
    "CI_MERGE_REQUEST_ID",
    "GO_PIPELINE_LABEL",
    "LAYERCI",
    "JENKINS_URL",
    "HUDSON_URL",
    "MAGNUM",
    "NETLIFY",
    "NEVERCODE",
    "RENDER",
    "SAILCI",
    "SEMAPHORE",
    "SCREWDRIVER",
    "SHIPPABLE",
    "TDDIUM",
    "STRIDER",
    "TEAMCITY_VERSION",
    "TRAVIS",
    "NOW_BUILDER",
    "APPCENTER_BUILD_ID",
    "STACKBLITZ",
    "STORMKIT",
    "CLEAVR",
    "ZEABUR",
    "CODESPHERE_APP_ID",
    "RAILWAY_PROJECT_ID",
    "RAILWAY_SERVICE_ID",
    "DENO_DEPLOY",
    "DENO_DEPLOYMENT_ID",
    "FIREBASE_APP_HOSTING",
  ]) {
    delete localEnv[name];
  }
  const ciEnv = { ...localEnv, CI: "1" };
  const stringCiEnv = { ...localEnv, CI: "false" };
  const teamCityEnv = { ...localEnv, TEAMCITY_VERSION: "2025.1" };
  const run = (args: string[], env = localEnv, cwd?: string) => Bun.spawnSync(
    [resolve(TESTRUNNER), ...args, "--no-progress", "--no-results", "--silent"],
    { stdout: "pipe", stderr: "pipe", env, cwd },
  );

  try {
    const external = join(tmp, "external.test.js");
    const snapshot = join(tmp, "__snapshots__", "external.test.js.snap");
    const externalSource = [
      'describe("snapshot parity", () => {',
      '  test("external values", () => {',
      '    expect({ z: 1, a: [true, "x"] }).toMatchSnapshot("object");',
      '  });',
      '  test("property shape", () => {',
      '    const key = Symbol("key");',
      '    expect({ id: 42, name: "Ada", [key]: 1 }).toMatchSnapshot({ id: expect.any(Number), [key]: 2 }, "shape");',
      '  });',
      '  test("inherited property shape", () => {',
      '    const received = Object.create({ inherited: "value" });',
      '    received.own = 1;',
      '    expect(received).toMatchSnapshot({ inherited: expect.any(String) });',
      '  });',
      '  test("primitive properties argument", () => {',
      '    expect({ value: 1 }).toMatchSnapshot(42);',
      '  });',
      '  test("sparse property shape", () => {',
      '    expect([undefined]).toMatchSnapshot([,]);',
      '  });',
      '  test("callable property shape", () => {',
      '    const fn = () => {};',
      '    expect({ fn }).toMatchSnapshot({ fn });',
      '  });',
      '  test("function property shape rejection", () => {',
      '    const fn = () => {};',
      '    fn.value = 1;',
      '    expect(() => expect(fn).toMatchSnapshot({ value: 1 })).toThrow("Received value must be an object");',
      '    expect(() => expect(fn).toMatchInlineSnapshot({ value: 1 }, `ignored`)).toThrow("Received value must be an object");',
      '  });',
      '  test("special values", () => {',
      '    const date = new Date(0);',
      '    date.toISOString = () => "instance spoofed";',
      '    Date.prototype.toISOString = () => "prototype spoofed";',
      '    const coercedInvalid = new Date(0);',
      '    coercedInvalid.valueOf = () => NaN;',
      '    Goccia.gc();',
      '    expect({ boxed: new String("ab"), coercedInvalid, date, invalid: new Date(NaN) }).toMatchSnapshot();',
      '  });',
      '  test("custom serializer", () => {',
      '    expect.addSnapshotSerializer({',
      '      test(value) { return value && value.kind === "point"; },',
      '      serialize(value, config, indentation, depth, refs, printer) {',
      '        if (value.child) {',
      '          const recursiveConfig = { ...config, plugins: config.plugins.slice(1) };',
      '          return `Point(${config.plugins.length}; ${printer(value.child, recursiveConfig, indentation, depth, refs)})`;',
      '        }',
      '        return `Point(${value.x}, ${value.y})`;',
      '      },',
      '    });',
      '    expect({ kind: "point", x: 2, y: 3 }).toMatchSnapshot("serializer");',
      '    expect({ kind: "point", child: { kind: "point", x: 9 } }).toMatchSnapshot("serializer recursion");',
      '  });',
      '  test("serializer validation is lazy", () => {',
      '    expect.addSnapshotSerializer(42);',
      '    expect(true).toBe(true);',
      '  });',
      '});',
      '',
    ].join("\n");
    writeFileSync(external, externalSource);

    let proc = run([external]);
    if (proc.exitCode !== 0)
      throw new Error(`Snapshot creation failed: ${proc.stdout}${proc.stderr}`);
    const expectedSnapshot = [
      '// Vitest Snapshot v1, https://vitest.dev/guide/snapshot.html',
      '',
      'exports[`snapshot parity > callable property shape 1`] = `',
      '{',
      '  "fn": [Function],',
      '}',
      '`;',
      '',
      'exports[`snapshot parity > custom serializer > serializer 1`] = `Point(2, 3)`;',
      '',
      'exports[`snapshot parity > custom serializer > serializer recursion 1`] = `',
      'Point(8; {',
      '  "kind": "point",',
      '  "x": 9,',
      '})',
      '`;',
      '',
      'exports[`snapshot parity > external values > object 1`] = `',
      '{',
      '  "a": [',
      '    true,',
      '    "x",',
      '  ],',
      '  "z": 1,',
      '}',
      '`;',
      '',
      'exports[`snapshot parity > inherited property shape 1`] = `',
      '{',
      '  "inherited": Any<String>,',
      '  "own": 1,',
      '}',
      '`;',
      '',
      'exports[`snapshot parity > primitive properties argument 1`] = `',
      '{',
      '  "value": 1,',
      '}',
      '`;',
      '',
      'exports[`snapshot parity > property shape > shape 1`] = `',
      '{',
      '  "id": Any<Number>,',
      '  "name": "Ada",',
      '  Symbol(key): 1,',
      '}',
      '`;',
      '',
      'exports[`snapshot parity > sparse property shape 1`] = `',
      '[',
      '  undefined,',
      ']',
      '`;',
      '',
      'exports[`snapshot parity > special values 1`] = `',
      '{',
      '  "boxed": String {',
      '    "0": "a",',
      '    "1": "b",',
      '  },',
      '  "coercedInvalid": Date { NaN },',
      '  "date": 1970-01-01T00:00:00.000Z,',
      '  "invalid": Date { NaN },',
      '}',
      '`;',
      '',
    ].join("\n");
    if (readFileSync(snapshot, "utf-8") !== expectedSnapshot)
      throw new Error(`External snapshot formatting mismatch:\n${readFileSync(snapshot, "utf-8")}`);

    proc = run([external, "--mode=bytecode"]);
    if (proc.exitCode !== 0)
      throw new Error(`Bytecode snapshot comparison failed: ${proc.stdout}${proc.stderr}`);

    writeFileSync(external, externalSource.replace('z: 1', 'z: 2'));
    proc = run([external]);
    if (proc.exitCode === 0)
      throw new Error("Snapshot mismatch should fail without update mode");
    if (!readFileSync(snapshot, "utf-8").includes('"z": 1'))
      throw new Error("Snapshot mismatch should not write without update mode");

    proc = run([external, "--mode=bytecode", "--update"]);
    if (proc.exitCode !== 0 || !readFileSync(snapshot, "utf-8").includes('"z": 2'))
      throw new Error(`Snapshot --update alias failed: ${proc.stdout}${proc.stderr}`);

    const withoutSerializer = externalSource
      .replace('z: 1', 'z: 2')
      .split('  test("custom serializer"')[0] + '});\n';
    writeFileSync(external, withoutSerializer);
    proc = run([external]);
    if (proc.exitCode !== 0 || !readFileSync(snapshot, "utf-8").includes('custom serializer'))
      throw new Error("Local obsolete snapshots should be retained without failing");
    proc = run([external], ciEnv);
    if (proc.exitCode === 0)
      throw new Error("CI should fail on obsolete snapshots");
    proc = run([external, "-u"]);
    if (proc.exitCode !== 0 || readFileSync(snapshot, "utf-8").includes('custom serializer'))
      throw new Error("-u should prune obsolete snapshots");

    const skipped = join(tmp, "skipped.test.js");
    const skippedSnapshot = join(tmp, "__snapshots__", "skipped.test.js.snap");
    writeFileSync(skipped, 'test("kept while skipped", () => expect("value").toMatchSnapshot());\n');
    proc = run([skipped]);
    if (proc.exitCode !== 0 || !existsSync(skippedSnapshot))
      throw new Error("Skipped snapshot setup failed");
    writeFileSync(skipped, 'test.skip("kept while skipped", () => expect("value").toMatchSnapshot());\n');
    proc = run([skipped, "-u"]);
    if (proc.exitCode !== 0 || !existsSync(skippedSnapshot) || !readFileSync(skippedSnapshot, "utf-8").includes('kept while skipped'))
      throw new Error("-u should preserve snapshots belonging to skipped tests");

    const incomplete = join(tmp, "incomplete.test.js");
    const incompleteSnapshot = join(tmp, "__snapshots__", "incomplete.test.js.snap");
    writeFileSync(incomplete, [
      'test("first", () => expect("first").toMatchSnapshot());',
      'test("second", () => expect("second").toMatchSnapshot());',
      '',
    ].join("\n"));
    proc = run([incomplete]);
    if (proc.exitCode !== 0 || !readFileSync(incompleteSnapshot, "utf-8").includes('second 1'))
      throw new Error("Incomplete snapshot setup failed");
    writeFileSync(incomplete, [
      'test("first", () => expect({ value: 1 }).toMatchSnapshot({ value: 2 }));',
      'test("second", () => expect("second").toMatchSnapshot());',
      '',
    ].join("\n"));
    proc = run([incomplete, "-u", "--exit-on-first-failure"]);
    if (proc.exitCode === 0 || !readFileSync(incompleteSnapshot, "utf-8").includes('second 1'))
      throw new Error("An incomplete -u run should preserve unchecked snapshots");

    const empty = join(tmp, "empty.test.js");
    const emptySnapshot = join(tmp, "__snapshots__", "empty.test.js.snap");
    writeFileSync(empty, 'test("no snapshot", () => expect(true).toBe(true));\n');
    writeFileSync(emptySnapshot, '// Vitest Snapshot v1, https://vitest.dev/guide/snapshot.html\n');
    proc = run([empty, "-u"]);
    if (proc.exitCode !== 0 || existsSync(emptySnapshot))
      throw new Error("-u should delete an existing empty snapshot file");

    const missing = join(tmp, "missing.test.js");
    const missingSnapshot = join(tmp, "__snapshots__", "missing.test.js.snap");
    writeFileSync(missing, 'test("missing", () => expect("value").toMatchSnapshot());\n');
    proc = run([missing], ciEnv);
    if (proc.exitCode === 0 || existsSync(missingSnapshot))
      throw new Error("CI should fail missing snapshots without writing them");
    proc = run([missing], teamCityEnv);
    if (proc.exitCode === 0 || existsSync(missingSnapshot))
      throw new Error("TeamCity should fail missing snapshots without writing them");
    proc = run([missing], stringCiEnv);
    if (proc.exitCode === 0 || existsSync(missingSnapshot))
      throw new Error("Non-empty CI strings should use JavaScript truthiness");
    proc = run([missing, "-u"], teamCityEnv);
    if (proc.exitCode !== 0 || !existsSync(missingSnapshot))
      throw new Error("Explicit snapshot update should override CI detection");

    const inline = join(tmp, "inline.test.js");
    writeFileSync(inline, [
      'test("scalar inline", () => {',
      '  expect("hello").toMatchInlineSnapshot();',
      '});',
      '',
      'test("multiline inline", () => {',
      '  expect({ b: 2, a: 1 }).toMatchInlineSnapshot();',
      '});',
      '',
      'test("property inline", () => {',
      '  expect({ id: 42, name: "Ada" }).toMatchInlineSnapshot({ id: expect.any(Number) });',
      '});',
      '',
    ].join("\n"));
    proc = run([inline]);
    if (proc.exitCode !== 0)
      throw new Error(`Inline snapshot insertion failed: ${proc.stdout}${proc.stderr}`);
    const expectedInline = [
      'test("scalar inline", () => {',
      '  expect("hello").toMatchInlineSnapshot(`"hello"`);',
      '});',
      '',
      'test("multiline inline", () => {',
      '  expect({ b: 2, a: 1 }).toMatchInlineSnapshot(`',
      '    {',
      '      "a": 1,',
      '      "b": 2,',
      '    }',
      '  `);',
      '});',
      '',
      'test("property inline", () => {',
      '  expect({ id: 42, name: "Ada" }).toMatchInlineSnapshot({ id: expect.any(Number) }, `',
      '    {',
      '      "id": Any<Number>,',
      '      "name": "Ada",',
      '    }',
      '  `);',
      '});',
      '',
    ].join("\n");
    if (readFileSync(inline, "utf-8") !== expectedInline)
      throw new Error(`Inline snapshot formatting mismatch:\n${readFileSync(inline, "utf-8")}`);
    proc = run([inline, "--mode=bytecode"]);
    if (proc.exitCode !== 0)
      throw new Error(`Bytecode inline comparison failed: ${proc.stdout}${proc.stderr}`);

    const unicodeInline = join(tmp, "inline-unicode.test.js");
    for (const mode of [[], ["--mode=bytecode"]]) {
      writeFileSync(unicodeInline,
        'test("first", () => expect(true).toBe(true));\u2028' +
        'test("unicode", () => expect("value").toMatchInlineSnapshot\u00a0());');
      proc = run([unicodeInline, ...mode]);
      if (proc.exitCode !== 0 ||
          !readFileSync(unicodeInline, "utf-8").includes(
            'toMatchInlineSnapshot\u00a0(`"value"`)'))
        throw new Error(`Unicode inline snapshot update failed: ${proc.stdout}${proc.stderr}`);
    }

    writeFileSync(inline, expectedInline.replace('`"hello"`', '`"wrong"`'));
    proc = run([inline, "--mode=bytecode", "--update-snapshots"]);
    if (proc.exitCode !== 0 || readFileSync(inline, "utf-8") !== expectedInline)
      throw new Error(`Bytecode inline update failed: ${proc.stdout}${proc.stderr}`);

    const bytecodeCreate = join(tmp, "inline-bytecode-create.test.js");
    writeFileSync(bytecodeCreate, [
      'test("bytecode creates after matcher text in string", () => expect("toMatchInlineSnapshot();created").toMatchInlineSnapshot());',
      'test("bytecode creates after method semicolon", () => expect({ method() { const value = 1; return value; } }).toMatchInlineSnapshot());',
      '',
    ].join("\n"));
    proc = run([bytecodeCreate, "--mode=bytecode"]);
    if (proc.exitCode !== 0 || !readFileSync(bytecodeCreate, "utf-8").includes('toMatchInlineSnapshot(`"toMatchInlineSnapshot();created"`)') || !readFileSync(bytecodeCreate, "utf-8").includes('"method": [Function]'))
      throw new Error(`Bytecode inline creation failed: ${proc.stdout}${proc.stderr}\n${readFileSync(bytecodeCreate, "utf-8")}`);
    proc = run([bytecodeCreate, "--mode=bytecode"]);
    if (proc.exitCode !== 0)
      throw new Error(`Rewritten bytecode inline snapshots should parse and compare: ${proc.stdout}${proc.stderr}`);

    const helper = join(tmp, "inline-helper.js");
    const leftEntry = join(tmp, "inline-left.test.js");
    const rightEntry = join(tmp, "inline-right.test.js");
    writeFileSync(helper, [
      'export const matchLeft = () => {',
      '  expect({ side: "left", nested: [1, 2] }).toMatchInlineSnapshot();',
      '};',
      '',
      'export const matchRight = () => {',
      '  expect({ side: "right", nested: [3, 4] }).toMatchInlineSnapshot();',
      '};',
      '',
    ].join("\n"));
    writeFileSync(leftEntry, [
      'import { matchLeft } from "./inline-helper.js";',
      'test("left helper", () => matchLeft());',
      '',
    ].join("\n"));
    writeFileSync(rightEntry, [
      'import { matchRight } from "./inline-helper.js";',
      'test("right helper", () => matchRight());',
      '',
    ].join("\n"));
    proc = run([leftEntry, rightEntry, "--jobs=2", "--mode=bytecode"]);
    const helperSource = readFileSync(helper, "utf-8");
    if (proc.exitCode !== 0 || !helperSource.includes('"side": "left"') || !helperSource.includes('"side": "right"'))
      throw new Error(`Parallel imported inline snapshot update failed: ${proc.stdout}${proc.stderr}\n${helperSource}`);
    proc = run([leftEntry, rightEntry, "--jobs=2", "--mode=bytecode"]);
    if (proc.exitCode !== 0)
      throw new Error(`Rewritten imported inline snapshots should compare: ${proc.stdout}${proc.stderr}`);

    const conflictingHelper = join(tmp, "inline-conflict-helper.js");
    const conflictingLeft = join(tmp, "inline-conflict-left.test.js");
    const conflictingRight = join(tmp, "inline-conflict-right.test.js");
    const conflictingSource = 'export const match = value => expect(value).toMatchInlineSnapshot();\n';
    writeFileSync(conflictingHelper, conflictingSource);
    writeFileSync(conflictingLeft, [
      'import { match } from "./inline-conflict-helper.js";',
      'test("left conflict", () => match("left"));',
      '',
    ].join("\n"));
    writeFileSync(conflictingRight, [
      'import { match } from "./inline-conflict-helper.js";',
      'test("right conflict", () => match("right"));',
      '',
    ].join("\n"));
    proc = run([conflictingLeft, conflictingRight, "--jobs=2", "--mode=bytecode"]);
    if (proc.exitCode === 0 || readFileSync(conflictingHelper, "utf-8") !== conflictingSource)
      throw new Error(`Conflicting shared inline snapshots should fail without writing: ${proc.stdout}${proc.stderr}`);
    proc = run([
      conflictingLeft,
      conflictingRight,
      "--jobs=2",
      "--mode=bytecode",
      "--output=compact-json",
    ]);
    const conflictResult = JSON.parse(proc.stdout.toString());
    if (proc.exitCode === 0 || conflictResult.failed !== 1 ||
        conflictResult.totalTests !== 3 ||
        !conflictResult.error?.message?.includes("Conflicting inline snapshots") ||
        conflictResult.files.some((file: { failed: number; totalTests: number }) =>
          file.failed !== 0 || file.totalTests !== 1))
      throw new Error(`Conflicting snapshots should be one process-level failure: ${proc.stdout}${proc.stderr}`);

    const attributionDir = join(tmp, "snapshot-attribution");
    mkdirSync(join(attributionDir, "dir"), { recursive: true });
    writeFileSync(join(attributionDir, "foo.js"), 'test("shallow", () => expect(true).toBe(true));\n');
    writeFileSync(join(attributionDir, "dir", "foo.js"), conflictingSource);
    writeFileSync(join(attributionDir, "left.test.js"), [
      'import { match } from "./dir/foo.js";',
      'test("left", () => match("left"));',
      '',
    ].join("\n"));
    writeFileSync(join(attributionDir, "right.test.js"), [
      'import { match } from "./dir/foo.js";',
      'test("right", () => match("right"));',
      '',
    ].join("\n"));
    proc = run([
      "foo.js",
      "dir/foo.js",
      "left.test.js",
      "right.test.js",
      "--jobs=4",
      "--mode=bytecode",
      "--output=compact-json",
    ], localEnv, attributionDir);
    const attributionResult = JSON.parse(proc.stdout.toString());
    const shallowResult = attributionResult.files.find(
      (file: { fileName: string }) => file.fileName === "foo.js",
    );
    const nestedResult = attributionResult.files.find(
      (file: { fileName: string }) => file.fileName === "dir/foo.js",
    );
    if (proc.exitCode === 0 || shallowResult?.failed !== 0 ||
        nestedResult?.failed !== 1 ||
        !nestedResult.error?.message?.includes("Conflicting inline snapshots"))
      throw new Error(`Snapshot errors should prefer the longest matching path: ${proc.stdout}${proc.stderr}`);

    const multifileExternal = join(tmp, "snapshot-multi.test.js");
    writeFileSync(multifileExternal, [
      'test("part one", () => expect("one").toMatchSnapshot());',
      '---',
      'test("part two", () => expect("two").toMatchSnapshot());',
      '',
    ].join("\n"));
    proc = run([multifileExternal, "--multifile", "--jobs=1"]);
    const multifilePartOneSnapshot = join(tmp, "__snapshots__", "snapshot-multi.test[part1].js.snap");
    const multifilePartTwoSnapshot = join(tmp, "__snapshots__", "snapshot-multi.test[part2].js.snap");
    if (proc.exitCode !== 0 || !existsSync(multifilePartOneSnapshot) || !existsSync(multifilePartTwoSnapshot))
      throw new Error(`Multifile external snapshots failed: ${proc.stdout}${proc.stderr}`);

    const multifileInline = join(tmp, "inline-multi.test.js");
    writeFileSync(multifileInline, [
      'test("inline part one", () => expect({ part: "one", nested: [1, 2] }).toMatchInlineSnapshot());',
      '---',
      'test("inline part two", () => expect({ part: "two", nested: [3, 4] }).toMatchInlineSnapshot());',
      '',
    ].join("\n"));
    proc = run([multifileInline, "--multifile", "--jobs=2", "--mode=bytecode"]);
    const multifileInlineSource = readFileSync(multifileInline, "utf-8");
    if (proc.exitCode !== 0 || !multifileInlineSource.includes('"part": "one"') || !multifileInlineSource.includes('"part": "two"') || !multifileInlineSource.includes("\n---\n"))
      throw new Error(`Multifile inline snapshots failed: ${proc.stdout}${proc.stderr}\n${multifileInlineSource}`);
    proc = run([multifileInline, "--multifile", "--jobs=2", "--mode=bytecode"]);
    if (proc.exitCode !== 0)
      throw new Error(`Rewritten multifile inline snapshots should compare: ${proc.stdout}${proc.stderr}`);

    const bracketed = join(tmp, "literal.test[part1].js");
    writeFileSync(bracketed, 'test("literal bracket", () => expect({ value: 1, nested: [2, 3] }).toMatchInlineSnapshot());\n');
    proc = run([bracketed, "--mode=bytecode"]);
    if (proc.exitCode !== 0 || !readFileSync(bracketed, "utf-8").includes('"nested": ['))
      throw new Error(`A real bracketed filename should not be treated as a multifile section: ${proc.stdout}${proc.stderr}`);
    proc = run([bracketed, "--mode=bytecode"]);
    if (proc.exitCode !== 0)
      throw new Error(`A top-level multiline bracketed snapshot should compare on rerun: ${proc.stdout}${proc.stderr}`);

    const stdinExternal = Bun.spawnSync(
      [resolve(TESTRUNNER), "--no-progress", "--no-results", "--silent"],
      { stdin: new Blob(['test("stdin", () => expect(1).toMatchSnapshot());\n']), stdout: "pipe", stderr: "pipe", env: localEnv },
    );
    if (stdinExternal.exitCode === 0)
      throw new Error("External snapshots from stdin should fail");
    const stdinInline = Bun.spawnSync(
      [resolve(TESTRUNNER), "--mode=bytecode", "--no-progress", "--no-results", "--silent"],
      { stdin: new Blob(['test("stdin", () => expect(1).toMatchInlineSnapshot(`1`));\n']), stdout: "pipe", stderr: "pipe", env: localEnv },
    );
    if (stdinInline.exitCode !== 0)
      throw new Error(`Existing inline snapshots from stdin should compare: ${stdinInline.stdout}${stdinInline.stderr}`);
  } finally {
    clean(tmp);
  }
});

await section("TestRunner: an expired deadline inside a toThrow callable is not a thrown error...", async () => {
  // A per-test deadline is not something the callable threw. If toThrow's
  // generic exception arm absorbs TGocciaTimeoutError the assertion reports a
  // pass and the deadline never unwinds to ExecuteSuite, so the run finishes
  // green while having blown straight past the limit.
  const tmp = makeTmp();
  try {
    const file = join(tmp, "throw-timeout.test.js");
    writeFileSync(
      file,
      [
        // for(;;) and while are gated off by default, so spin through an
        // iterator that never reports done.
        "const forever = {",
        "  [Symbol.iterator]() {",
        "    return { next: () => ({ value: 1, done: false }) };",
        "  },",
        "};",
        "",
        'test("deadline inside toThrow", () => {',
        "  expect(() => {",
        "    for (const value of forever) {",
        "      if (value === 2) break;",
        "    }",
        "  }).toThrow();",
        "});",
        "",
      ].join("\n"),
    );

    for (const modeArgs of [[] as string[], ["--mode=bytecode"]]) {
      const label = modeArgs.length
        ? "toThrow deadline (bytecode)"
        : "toThrow deadline";
      // Report to a file: a failing test still prints its marker line to
      // stdout, so stdout is not parseable JSON here.
      const reportPath = join(tmp, `throw-timeout${modeArgs.length ? "-bc" : ""}.json`);
      const proc = Bun.spawnSync(
        [resolve(TESTRUNNER), file, ...modeArgs, "--test-timeout=300", "--no-progress", `--output=${reportPath}`],
        { stdout: "pipe", stderr: "pipe" },
      );
      if (proc.exitCode === 0)
        throw new Error(`${label}: an expired deadline must not report a passing toThrow`);
      const json = JSON.parse(readFileSync(reportPath, "utf-8"));
      if (json.passed !== 0 || json.failed !== 1)
        throw new Error(`${label}: expected 0 passed / 1 failed, got ${json.passed}/${json.failed}`);
      const failures = (json.files?.[0]?.failedTests ?? []).join("\n");
      if (!failures.includes("TIMEOUT"))
        throw new Error(`${label}: expected the failure to be recorded as a TIMEOUT, got ${failures}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("TestRunner: JSON multi-file structure...", async () => {
  const tmp = makeTmp();
  try {
    const first = join(tmp, "test-a.js");
    const second = join(tmp, "test-b.js");
    const resultsPath = join(tmp, "test-results.json");
    writeFileSync(
      first,
      [
        'describe("a", () => {',
        '  test("passes a", () => { expect(1 + 1).toBe(2); });',
        "});",
        "",
      ].join("\n"),
    );
    writeFileSync(
      second,
      [
        'describe("b", () => {',
        '  test("passes b", () => { expect(2 + 2).toBe(4); });',
        "});",
        "",
      ].join("\n"),
    );

    const proc = Bun.spawnSync([resolve(TESTRUNNER), first, second, "--no-progress", "--jobs=2", `--output=${resultsPath}`], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`TestRunner multi-file JSON exited ${proc.exitCode}: ${proc.stderr.toString()}`);

    const json = JSON.parse(readFileSync(resultsPath, "utf-8"));
    assertCommonJsonReport(json, "TestRunner multi-file JSON", 2);
    if (json.ok !== true) throw new Error(`TestRunner multi-file top-level ok should be true, got ${json.ok}`);
    if (json.error !== null) throw new Error("TestRunner multi-file top-level error should be null");
    if (json.totalFiles !== 2) throw new Error(`TestRunner multi-file totalFiles should be 2, got ${json.totalFiles}`);
    if (json.totalTests !== 2) throw new Error(`TestRunner multi-file totalTests should be 2, got ${json.totalTests}`);
    if (json.passed !== 2 || json.failed !== 0) throw new Error(`TestRunner multi-file pass/fail mismatch: ${json.passed}/${json.failed}`);
    if (json.workers.used !== 2) throw new Error(`TestRunner multi-file workers.used should be 2, got ${json.workers.used}`);
    if (json.memory.gc.allocatedDuringRunBytes <= 0)
      throw new Error("TestRunner multi-file top-level memory should include worker GC allocations");
    if (json.memory.gc.liveBytes > json.memory.gc.limitBytes * (json.workers.used + 1))
      throw new Error("TestRunner multi-file top-level live memory should not double-count per-file worker snapshots");
    if (!Array.isArray(json.results) || json.results.length !== 2) throw new Error("TestRunner multi-file results should mirror files with 2 entries");

    assertCommonJsonFile(json.files[0], "TestRunner first file", first);
    assertCommonJsonFile(json.files[1], "TestRunner second file", second);
    if (json.files[0].passed !== 1 || json.files[0].failed !== 0) throw new Error(`TestRunner first file counts mismatch: ${JSON.stringify(json.files[0])}`);
    if (json.files[1].passed !== 1 || json.files[1].failed !== 0) throw new Error(`TestRunner second file counts mismatch: ${JSON.stringify(json.files[1])}`);
    if (json.files[0].memory !== null || json.files[1].memory !== null)
      throw new Error("TestRunner multi-file per-file memory should be null when top-level memory is aggregated");
    if (json.results[0].fileName !== json.files[0].fileName || json.results[1].fileName !== json.files[1].fileName)
      throw new Error("TestRunner results[] should mirror files[] file names");
  } finally {
    clean(tmp);
  }
});

await section("TestRunner: --output=json emits structured JSON envelope to stdout...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "test-stdout-json.js");
    writeFileSync(
      file,
      [
        'describe("a", () => {',
        '  test("passes", () => { expect(1 + 1).toBe(2); });',
        "});",
        "",
      ].join("\n"),
    );

    const proc = Bun.spawnSync(
      [resolve(TESTRUNNER), file, "--no-progress", "--output=json"],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (proc.exitCode !== 0) throw new Error(`TestRunner --output=json exited ${proc.exitCode}: ${proc.stderr.toString()}`);

    const stdout = proc.stdout.toString();
    if (stdout.includes("Test Results"))
      throw new Error(`TestRunner --output=json should suppress human-readable summary, got: ${stdout}`);
    const json = JSON.parse(stdout);
    assertCommonJsonReport(json, "TestRunner --output=json", 1);
    if (json.ok !== true) throw new Error(`TestRunner --output=json ok should be true, got ${json.ok}`);
    if (json.totalFiles !== 1) throw new Error(`TestRunner --output=json totalFiles should be 1, got ${json.totalFiles}`);
    if (json.passed !== 1 || json.failed !== 0)
      throw new Error(`TestRunner --output=json pass/fail mismatch: ${json.passed}/${json.failed}`);
    assertCommonJsonFile(json.files[0], "TestRunner --output=json file", file);
  } finally {
    clean(tmp);
  }
});

await section("TestRunner: --output=json keeps stdout clean when test throws...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "test-throws.js");
    writeFileSync(file, 'throw new Error("boom");\n');

    const proc = Bun.spawnSync(
      [resolve(TESTRUNNER), file, "--no-progress", "--output=json"],
      { stdout: "pipe", stderr: "pipe" },
    );
    // ExitCode must be non-zero because the test failed; stdout must still be parseable.
    if (proc.exitCode === 0)
      throw new Error(`TestRunner --output=json with throwing test should exit non-zero, got 0`);
    const stdout = proc.stdout.toString();
    if (stdout.includes("Error: boom") && !stdout.startsWith("{"))
      throw new Error(`TestRunner --output=json should not leak diagnostic before JSON, got: ${stdout.slice(0, 200)}`);
    let json: any;
    try {
      json = JSON.parse(stdout);
    } catch (e) {
      throw new Error(`TestRunner --output=json with throwing test should produce parseable JSON, got: ${stdout.slice(0, 200)}`);
    }
    if (json.ok !== false) throw new Error(`TestRunner --output=json with throwing test should mark ok=false, got ${json.ok}`);
    if (json.failed !== 1) throw new Error(`TestRunner --output=json with throwing test should mark failed=1, got ${json.failed}`);
    const errorMessage = json.files?.[0]?.errorMessage;
    if (typeof errorMessage !== "string" || errorMessage.length === 0)
      throw new Error(`TestRunner --output=json with throwing test should populate per-file errorMessage with a non-empty string, got: ${JSON.stringify(errorMessage)}`);
  } finally {
    clean(tmp);
  }
});

// A file-level throw emits no per-test reporter output, so the case above cannot
// catch the reporter markers the testing library writes straight to stdout for
// individual tests. These scenarios cover every marker-producing shape.
for (const modeArgs of [[], ["--mode=bytecode"]]) {
  const modeLabel = modeArgs.length === 0 ? "interpreted" : "bytecode";

  const scenarios: Array<{
    name: string;
    source: string;
    // suiteErrors: describe/hook failures. Vitest keeps these OUT of
    // `failed` (affected tests report as skipped) and fails the file
    // instead, so they are asserted separately. Defaults to 0.
    // totalTests: tests the runner actually entered. Defaults to
    // passed + failed + skipped, which is the only shape that does not hold
    // when collection aborts and the file is discarded whole.
    expected: {
      passed: number;
      failed: number;
      skipped: number;
      suiteErrors?: number;
      totalTests?: number;
    };
    // Reporter markers this shape can leak beyond the shared set below.
    extraMarkers?: string[];
  }> = [
    {
      name: "a test assertion fails",
      source: 'test("fails", () => { expect(1).toBe(2); });\n',
      expected: { passed: 0, failed: 1, skipped: 0 },
    },
    {
      name: "multiple tests fail across suites",
      source: [
        'describe("first", () => {',
        '  test("fails a", () => { expect(1).toBe(2); });',
        "});",
        'describe("second", () => {',
        '  test("fails b", () => { expect("x").toBe("y"); });',
        "});",
        "",
      ].join("\n"),
      expected: { passed: 0, failed: 2, skipped: 0 },
    },
    {
      name: "a run mixes passes, failures, todo and skipped tests",
      source: [
        'describe("mixed", () => {',
        '  test("passes", () => { expect(1 + 1).toBe(2); });',
        '  test("fails", () => { expect(1).toBe(2); });',
        '  test.todo("todo");',
        '  test.skip("skipped", () => {});',
        "});",
        "",
      ].join("\n"),
      expected: { passed: 1, failed: 1, skipped: 2 },
    },
    {
      // A throwing beforeAll aborts its suite: every test in it (and in
      // any descendant suite) reports as SKIPPED, and the hook failure is
      // a suite error rather than a failed test. Matches Vitest exactly.
      name: "a beforeAll hook throws",
      source: [
        'describe("suite", () => {',
        '  beforeAll(() => { throw new Error("beforeAll exploded"); });',
        '  test("t1", () => { expect(1).toBe(1); });',
        '  test("t2", () => { expect(2).toBe(2); });',
        "});",
        "",
      ].join("\n"),
      expected: { passed: 0, failed: 0, skipped: 2, suiteErrors: 1 },
    },
    {
      // A throwing afterAll is a teardown failure: the suite's tests have
      // already run and keep their passes, so nothing is skipped and the
      // hook is recorded as a suite error. Matches Vitest exactly.
      name: "an afterAll hook throws",
      source: [
        'describe("suite", () => {',
        '  afterAll(() => { throw new Error("afterAll exploded"); });',
        '  test("t1", () => { expect(1).toBe(1); });',
        '  test("t2", () => { expect(2).toBe(2); });',
        "});",
        "",
      ].join("\n"),
      expected: { passed: 2, failed: 0, skipped: 0, suiteErrors: 1 },
    },
    {
      // Scope check: a failed beforeAll skips the suite's own tests AND
      // every test in its descendant suites -- the child's test never
      // runs even though the child itself has no failing hook.
      name: "a beforeAll hook throws in a suite with nested child suites",
      source: [
        'describe("parent", () => {',
        '  beforeAll(() => { throw new Error("parent beforeAll exploded"); });',
        '  test("direct", () => { expect(1).toBe(1); });',
        '  describe("child", () => {',
        '    test("nested", () => { expect(2).toBe(2); });',
        "  });",
        "});",
        "",
      ].join("\n"),
      expected: { passed: 0, failed: 0, skipped: 2, suiteErrors: 1 },
    },
    {
      // Scope check, other direction: an inner suite's failed beforeAll
      // must NOT affect the outer suite's own tests or a sibling suite.
      name: "a nested suite's beforeAll throws without affecting siblings",
      source: [
        'describe("outer", () => {',
        '  test("outer-test", () => { expect(1).toBe(1); });',
        '  describe("inner", () => {',
        '    beforeAll(() => { throw new Error("inner beforeAll exploded"); });',
        '    test("inner-test", () => { expect(2).toBe(2); });',
        "  });",
        '  describe("sibling", () => {',
        '    test("sibling-test", () => { expect(3).toBe(3); });',
        "  });",
        "});",
        "",
      ].join("\n"),
      expected: { passed: 2, failed: 0, skipped: 1, suiteErrors: 1 },
    },
    {
      // A describe callback that throws aborts collection for the WHOLE
      // file, exactly as Vitest does: no test runs, not even in the
      // passing suite below. Collection-time; hook and test failures are
      // execution-time and leave collected results intact.
      name: "a describe block throws during registration",
      source: [
        'describe("boom", () => { throw new Error("registration exploded"); });',
        'describe("ok", () => { test("passes", () => { expect(1).toBe(1); }); });',
        "",
      ].join("\n"),
      expected: { passed: 0, failed: 0, skipped: 0, suiteErrors: 1, totalTests: 0 },
      extraMarkers: ["Error in describe block"],
    },
    {
      // The decisive collection-abort property: a suite collected BEFORE
      // the throwing describe is discarded too, so its test never runs.
      // Verified against Vitest, which reports zero tests for this shape.
      name: "a describe block throws after an earlier suite was collected",
      source: [
        'describe("collected first", () => {',
        '  test("earlier", () => { expect(1).toBe(1); });',
        "});",
        'describe("boom", () => { throw new Error("registration exploded"); });',
        'describe("collected last", () => {',
        '  test("later", () => { expect(2).toBe(2); });',
        "});",
        "",
      ].join("\n"),
      expected: { passed: 0, failed: 0, skipped: 0, suiteErrors: 1, totalTests: 0 },
      extraMarkers: ["Error in describe block"],
    },
  ];

  // Both JSON shapes: compact-json shares the envelope's count fields but
  // omits build/memory/stdout/stderr, so it needs its own coverage.
  for (const outputFlag of ["--output=json", "--output=compact-json"]) {
  for (const scenario of scenarios) {
    const label = `TestRunner ${outputFlag} (${modeLabel}) when ${scenario.name}`;
    console.log(`TestRunner: ${outputFlag} keeps stdout clean when ${scenario.name} (${modeLabel})...`);
    const tmp = makeTmp();
    try {
      const file = join(tmp, "test-reporter-markers.js");
      writeFileSync(file, scenario.source);

      const proc = Bun.spawnSync(
        [resolve(TESTRUNNER), file, "--no-progress", outputFlag, ...modeArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      if (proc.exitCode === 0)
        throw new Error(`${label} should exit non-zero because tests failed, got 0`);

      const stdout = proc.stdout.toString();
      for (const marker of ["❌", "📝", "⏸️", "Test Results", ...(scenario.extraMarkers ?? [])]) {
        if (stdout.includes(marker))
          throw new Error(`${label} leaked reporter output ${marker} to stdout, got: ${stdout.slice(0, 200)}`);
      }

      let json: any;
      try {
        json = JSON.parse(stdout);
      } catch {
        throw new Error(`${label} should produce parseable JSON on stdout, got: ${stdout.slice(0, 200)}`);
      }

      if (json.ok !== false) throw new Error(`${label} ok should be false, got ${json.ok}`);
      if (json.passed !== scenario.expected.passed)
        throw new Error(`${label} passed should be ${scenario.expected.passed}, got ${json.passed}`);
      if (json.failed !== scenario.expected.failed)
        throw new Error(`${label} failed should be ${scenario.expected.failed}, got ${json.failed}`);
      if (json.skipped !== scenario.expected.skipped)
        throw new Error(`${label} skipped should be ${scenario.expected.skipped}, got ${json.skipped}`);
      const expectedSuiteErrors = scenario.expected.suiteErrors ?? 0;
      if (json.suiteErrors !== expectedSuiteErrors)
        throw new Error(`${label} suiteErrors should be ${expectedSuiteErrors}, got ${json.suiteErrors}`);
      // A collection abort discards the file, so the tests registered before
      // the throwing describe must not be counted as run either. Without
      // this the retained registrations of the "collected first" shape pass
      // undetected.
      const expectedTotalTests =
        scenario.expected.totalTests ??
        scenario.expected.passed + scenario.expected.failed + scenario.expected.skipped;
      if (json.totalTests !== expectedTotalTests)
        throw new Error(`${label} totalTests should be ${expectedTotalTests}, got ${json.totalTests}`);
      const failedTests = json.files?.[0]?.failedTests;
      // failedTests stays the human-visible detail channel for BOTH
      // failed tests and suite errors, so it carries one entry each.
      const expectedDetails = scenario.expected.failed + expectedSuiteErrors;
      if (!Array.isArray(failedTests) || failedTests.length !== expectedDetails)
        throw new Error(`${label} should report ${expectedDetails} failedTests entries, got: ${JSON.stringify(failedTests)}`);
    } finally {
      clean(tmp);
    }
  }
  }

  // The worker-merge path aggregates counts differently from the single-file
  // path, so pin the describe-error accounting there too: the throwing file
  // must flip its own ok and the top-level ok while the clean sibling file
  // stays ok, and the merged `failed` must carry the registration error.
  {
    const label = `TestRunner --output=json --jobs (${modeLabel}) when a describe block throws`;
    console.log(`TestRunner: --output=json counts a throwing describe under --jobs (${modeLabel})...`);
    const tmp = makeTmp();
    try {
      const bad = join(tmp, "test-describe-throws.js");
      const good = join(tmp, "test-describe-clean.js");
      writeFileSync(bad, [
        'describe("boom", () => { throw new Error("registration exploded"); });',
        'describe("ok", () => { test("passes", () => { expect(1).toBe(1); }); });',
        "",
      ].join("\n"));
      writeFileSync(good, 'describe("clean", () => { test("passes", () => { expect(2).toBe(2); }); });\n');

      const proc = Bun.spawnSync(
        [resolve(TESTRUNNER), bad, good, "--jobs=2", "--no-progress", "--output=json", ...modeArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      if (proc.exitCode === 0)
        throw new Error(`${label} should exit non-zero because a describe block threw, got 0`);

      const stdout = proc.stdout.toString();
      let json: any;
      try {
        json = JSON.parse(stdout);
      } catch {
        throw new Error(`${label} should produce parseable JSON on stdout, got: ${stdout.slice(0, 200)}`);
      }

      if (json.ok !== false) throw new Error(`${label} ok should be false, got ${json.ok}`);
      if (json.failed !== 0) throw new Error(`${label} merged failed should be 0, got ${json.failed}`);
      if (json.suiteErrors !== 1) throw new Error(`${label} merged suiteErrors should be 1, got ${json.suiteErrors}`);
      // Only the clean sibling contributes a pass: collection aborted for
      // the throwing file, so its own passing suite never ran either.
      if (json.passed !== 1) throw new Error(`${label} merged passed should be 1, got ${json.passed}`);

      const badResult = json.files?.find((f: any) => String(f.fileName).endsWith("test-describe-throws.js"));
      const goodResult = json.files?.find((f: any) => String(f.fileName).endsWith("test-describe-clean.js"));
      if (badResult?.ok !== false)
        throw new Error(`${label} the throwing file should report ok=false, got ${badResult?.ok}`);
      if (badResult?.suiteErrors !== 1)
        throw new Error(`${label} the throwing file should report suiteErrors=1, got ${badResult?.suiteErrors}`);
      if (badResult?.passed !== 0)
        throw new Error(`${label} the throwing file should run no tests at all, got passed=${badResult?.passed}`);
      // Same guard as the single-file scenarios: a retained registration
      // would surface here as a non-zero total for the discarded file.
      if (badResult?.totalTests !== 0)
        throw new Error(`${label} the throwing file should report totalTests=0, got ${badResult?.totalTests}`);
      if (goodResult?.passed !== 1)
        throw new Error(`${label} the clean sibling file should keep its pass, got ${goodResult?.passed}`);
      if (!badResult?.failedTests?.some((t: string) => t.includes('Describe "boom"')))
        throw new Error(`${label} should keep the describe error visible in failedTests, got: ${JSON.stringify(badResult?.failedTests)}`);
      if (goodResult?.ok !== true)
        throw new Error(`${label} the clean sibling file should stay ok, got ${goodResult?.ok}`);
    } finally {
      clean(tmp);
    }
  }

  // Body-execution guard. A failed beforeEach produces IDENTICAL counts
  // whether or not the body runs (the test fails either way), so counts
  // cannot detect a regression here -- only the body's side effects can.
  // Vitest and bun both skip the body; running it executes user code
  // against a fixture the hook failed to build.
  {
    const label = `TestRunner (${modeLabel}) beforeEach failure skips the test body`;
    console.log(`TestRunner: a failed beforeEach does not run the test body (${modeLabel})...`);
    const tmp = makeTmp();
    try {
      const file = join(tmp, "test-beforeeach-body.js");
      writeFileSync(file, [
        'describe("suite", () => {',
        '  beforeEach(() => { console.log("RAN:beforeEach"); throw new Error("beforeEach exploded"); });',
        '  test("t1", () => { console.log("RAN:body1"); expect(1).toBe(1); });',
        '  test("t2", () => { console.log("RAN:body2"); expect(2).toBe(2); });',
        "});",
        "",
      ].join("\n"));

      const proc = Bun.spawnSync(
        [resolve(TESTRUNNER), file, "--no-progress", ...modeArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      const all = proc.stdout.toString() + proc.stderr.toString();
      if (!all.includes("RAN:beforeEach"))
        throw new Error(`${label} should still run the beforeEach hook, got: ${all.slice(0, 300)}`);
      if (all.includes("RAN:body1") || all.includes("RAN:body2"))
        throw new Error(`${label} must NOT execute the test body after the hook failed, got: ${all.slice(0, 300)}`);
      if (proc.exitCode === 0)
        throw new Error(`${label} should exit non-zero, got 0`);

      // The skipped body writes no detail line of its own, so the entry is
      // built from the fallback. Without the hook's message carried into it,
      // the JSON payload names the failed test and never says why.
      const jsonProc = Bun.spawnSync(
        [resolve(TESTRUNNER), file, "--no-progress", "--output=json", ...modeArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      const json = JSON.parse(jsonProc.stdout.toString());
      const details: string[] = json.files?.[0]?.failedTests ?? [];
      if (details.length !== 2)
        throw new Error(`${label} should report both tests as failed, got: ${JSON.stringify(details)}`);
      for (const detail of details)
        if (!detail.includes("beforeEach exploded"))
          throw new Error(
            `${label} failedTests entry must carry the hook failure message, got: ${JSON.stringify(details)}`,
          );
    } finally {
      clean(tmp);
    }
  }

  // Uncatchable-limit teardown guard. afterEach / onTestFinished are GUEST code:
  // on an ordinary failure they MUST run (fixtures still need tearing down), but
  // once a hard memory limit has fired the guest may not execute past it. The
  // per-test finally distinguishes the two through the in-flight exception, so
  // pin BOTH directions here. Each test prints "RAN:body" before its outcome and
  // "RAN:afterEach" from the hook; the body marker proves stdout was captured up
  // to the abort, so an ABSENT afterEach marker means the hook was skipped, not
  // merely buffered away.
  {
    const label = `TestRunner (${modeLabel}) afterEach vs uncatchable limit`;

    // (a) Ordinary failing test: afterEach still runs.
    console.log(`TestRunner: afterEach runs after an ordinary test failure (${modeLabel})...`);
    {
      const tmp = makeTmp();
      try {
        const file = join(tmp, "afterEach-normal.test.js");
        writeFileSync(file, [
          'describe("suite", () => {',
          '  afterEach(() => { console.log("RAN:afterEach"); });',
          '  test("t", () => { console.log("RAN:body"); expect(1).toBe(2); });',
          "});",
          "",
        ].join("\n"));

        const proc = Bun.spawnSync(
          [resolve(TESTRUNNER), file, "--no-progress", ...modeArgs],
          { stdout: "pipe", stderr: "pipe" },
        );
        const all = proc.stdout.toString() + proc.stderr.toString();
        if (!all.includes("RAN:body"))
          throw new Error(`${label} (normal) should run the test body, got: ${all.slice(0, 300)}`);
        if (!all.includes("RAN:afterEach"))
          throw new Error(`${label} (normal) must run afterEach on an ordinary failure, got: ${all.slice(0, 300)}`);
        if (proc.exitCode === 0)
          throw new Error(`${label} (normal) should exit non-zero for the failing test, got 0`);
      } finally {
        clean(tmp);
      }
    }

    // (b) Test that trips --max-memory: afterEach is skipped and the run aborts
    // uncatchably. 100M array elements far exceed the 64 MiB budget; the refusal
    // raises the uncatchable TGocciaMemoryLimitError, which must tear the run
    // down without running the guest hook.
    console.log(`TestRunner: afterEach is skipped when a memory limit aborts the run (${modeLabel})...`);
    {
      const tmp = makeTmp();
      try {
        const file = join(tmp, "afterEach-memory.test.js");
        writeFileSync(file, [
          'describe("suite", () => {',
          '  afterEach(() => { console.log("RAN:afterEach"); });',
          '  test("t", () => { console.log("RAN:body"); const a = new Array(100000000); expect(a.length).toBe(100000000); });',
          "});",
          "",
        ].join("\n"));

        const proc = Bun.spawnSync(
          [resolve(TESTRUNNER), file, "--no-progress", "--max-memory=67108864", ...modeArgs],
          { stdout: "pipe", stderr: "pipe" },
        );
        const all = proc.stdout.toString() + proc.stderr.toString();
        if (!all.includes("RAN:body"))
          throw new Error(`${label} (memory) should run the body up to the refusal, got: ${all.slice(0, 400)}`);
        if (!/memory budget/.test(all))
          throw new Error(`${label} (memory) should abort on the memory budget, got: ${all.slice(0, 400)}`);
        if (all.includes("RAN:afterEach"))
          throw new Error(`${label} (memory) must NOT run afterEach after an uncatchable limit, got: ${all.slice(0, 400)}`);
        if (proc.exitCode === 0)
          throw new Error(`${label} (memory) should abort with a non-zero exit, got 0`);
      } finally {
        clean(tmp);
      }
    }
  }

  // The worker-merge path aggregates counts separately from the single-file
  // path, so pin the hook-failure accounting there too: the file with the
  // throwing beforeAll must flip its own ok and the top-level ok, while the
  // clean sibling file stays ok and the merged `failed` carries the hook.
  {
    const label = `TestRunner --output=json --jobs (${modeLabel}) when a beforeAll hook throws`;
    console.log(`TestRunner: --output=json counts a throwing beforeAll under --jobs (${modeLabel})...`);
    const tmp = makeTmp();
    try {
      const bad = join(tmp, "test-hook-throws.js");
      const good = join(tmp, "test-hook-clean.js");
      writeFileSync(bad, [
        'describe("suite", () => {',
        '  beforeAll(() => { throw new Error("beforeAll exploded"); });',
        '  test("t1", () => { expect(1).toBe(1); });',
        "});",
        "",
      ].join("\n"));
      writeFileSync(good, 'describe("clean", () => { test("passes", () => { expect(2).toBe(2); }); });\n');

      const proc = Bun.spawnSync(
        [resolve(TESTRUNNER), bad, good, "--jobs=2", "--no-progress", "--output=json", ...modeArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      if (proc.exitCode === 0)
        throw new Error(`${label} should exit non-zero because a beforeAll hook threw, got 0`);

      const stdout = proc.stdout.toString();
      let json: any;
      try {
        json = JSON.parse(stdout);
      } catch {
        throw new Error(`${label} should produce parseable JSON on stdout, got: ${stdout.slice(0, 200)}`);
      }

      if (json.ok !== false) throw new Error(`${label} ok should be false, got ${json.ok}`);
      if (json.failed !== 0) throw new Error(`${label} merged failed should be 0, got ${json.failed}`);
      if (json.suiteErrors !== 1) throw new Error(`${label} merged suiteErrors should be 1, got ${json.suiteErrors}`);
      if (json.skipped !== 1) throw new Error(`${label} merged skipped should be 1, got ${json.skipped}`);

      const badResult = json.files?.find((f: any) => String(f.fileName).endsWith("test-hook-throws.js"));
      const goodResult = json.files?.find((f: any) => String(f.fileName).endsWith("test-hook-clean.js"));
      if (badResult?.ok !== false)
        throw new Error(`${label} the hook-failing file should report ok=false, got ${badResult?.ok}`);
      if (badResult?.suiteErrors !== 1)
        throw new Error(`${label} the hook-failing file should report suiteErrors=1, got ${badResult?.suiteErrors}`);
      if (badResult?.skipped !== 1)
        throw new Error(`${label} the hook-failing file's test should report as skipped, got ${badResult?.skipped}`);
      if (!badResult?.failedTests?.some((t: string) => t.includes('Hook "beforeAll"')))
        throw new Error(`${label} should keep the hook error visible in failedTests, got: ${JSON.stringify(badResult?.failedTests)}`);
      if (goodResult?.ok !== true)
        throw new Error(`${label} the clean sibling file should stay ok, got ${goodResult?.ok}`);
    } finally {
      clean(tmp);
    }
  }

  // A suite error never enters `failed`, so a bail keyed on `failed` alone
  // walks straight past a file that already died. Both the sequential loop
  // and the worker pool have to stop, and the human summary must not call
  // the run green.
  {
    const label = `TestRunner (${modeLabel}) --exit-on-first-failure on a suite error`;
    console.log(`TestRunner: --exit-on-first-failure stops on a suite error (${modeLabel})...`);
    const tmp = makeTmp();
    try {
      // "a-" sorts first so the failing file is the one the runner reaches
      // first on both paths.
      const failing = join(tmp, "a-suite-error.test.js");
      writeFileSync(failing, [
        'describe("suite", () => {',
        '  beforeAll(() => { throw new Error("beforeAll exploded"); });',
        '  test("t1", () => { expect(1).toBe(1); });',
        "});",
        "",
      ].join("\n"));

      // Slow enough that a second worker cannot drain the whole queue in the
      // window before the first file's failure cancels it.
      const laterFiles: string[] = [];
      for (const index of [1, 2, 3, 4, 5]) {
        const later = join(tmp, `b-later-${index}.test.js`);
        writeFileSync(later, [
          `test("later ${index}", () => {`,
          // Array methods, not a traditional for loop: those need
          // --compat-traditional-for-loop and would fail the file for the
          // wrong reason.
          "  const total = Array.from({ length: 200000 }, (_, i) => i)",
          "    .reduce((sum, value) => sum + value, 0);",
          `  console.log("RAN:later-${index}");`,
          "  expect(total > 0).toBe(true);",
          "});",
          "",
        ].join("\n"));
        laterFiles.push(later);
      }

      // Sequential: the loop must break before the next file is opened.
      const sequential = Bun.spawnSync(
        [resolve(TESTRUNNER), failing, ...laterFiles, "--jobs=1", "--no-progress",
          "--exit-on-first-failure", ...modeArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      const sequentialOut = sequential.stdout.toString() + sequential.stderr.toString();
      if (sequential.exitCode === 0)
        throw new Error(`${label} sequential should exit non-zero, got 0`);
      for (const index of [1, 2, 3, 4, 5])
        if (sequentialOut.includes(`RAN:later-${index}`))
          throw new Error(
            `${label} sequential kept running files after the suite error: ${sequentialOut.slice(0, 400)}`,
          );
      // The summary must not claim the run passed when only a suite errored.
      if (sequentialOut.includes("All tests passed"))
        throw new Error(`${label} printed the all-passed summary for a suite error: ${sequentialOut.slice(0, 400)}`);

      // Parallel: the pool must cancel the queue, so at least one queued file
      // never runs and never appears as a synthesised failure.
      const parallel = Bun.spawnSync(
        [resolve(TESTRUNNER), failing, ...laterFiles, "--jobs=2", "--no-progress",
          "--exit-on-first-failure", "--output=json", ...modeArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      if (parallel.exitCode === 0)
        throw new Error(`${label} parallel should exit non-zero, got 0`);
      const parallelJson = JSON.parse(parallel.stdout.toString());
      if (parallelJson.ok !== false)
        throw new Error(`${label} parallel ok should be false, got ${parallelJson.ok}`);
      if (!Array.isArray(parallelJson.files) || parallelJson.files.length >= 6)
        throw new Error(
          `${label} parallel ran every queued file instead of bailing, got ${parallelJson.files?.length} file results`,
        );
      // Cancelled files are omitted, not reported as failures of their own.
      if (parallelJson.failed !== 0)
        throw new Error(
          `${label} parallel should not synthesise failures for cancelled files, got failed=${parallelJson.failed}`,
        );
      if (parallelJson.suiteErrors !== 1)
        throw new Error(`${label} parallel suiteErrors should be 1, got ${parallelJson.suiteErrors}`);
    } finally {
      clean(tmp);
    }
  }
}

await section("TestRunner: --output=json keeps stdout clean when script logs to console...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "test-with-log.js");
    writeFileSync(
      file,
      [
        'console.log("THIS WOULD LEAK");',
        'console.error("THIS TOO");',
        'test("ok", () => { expect(1).toBe(1); });',
        "",
      ].join("\n"),
    );

    const proc = Bun.spawnSync(
      [resolve(TESTRUNNER), file, "--no-progress", "--output=json"],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (proc.exitCode !== 0) throw new Error(`TestRunner --output=json with console output exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    const stdout = proc.stdout.toString();
    if (stdout.includes("THIS WOULD LEAK") || stdout.includes("THIS TOO"))
      throw new Error(`TestRunner --output=json should suppress test-script console output, got: ${stdout.slice(0, 200)}`);
    const json = JSON.parse(stdout);
    if (json.ok !== true) throw new Error(`TestRunner --output=json with console output ok should be true, got ${json.ok}`);
  } finally {
    clean(tmp);
  }
});

await section("TestRunner: --output=json keeps stdout clean when --coverage is enabled...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "test-coverage.js");
    writeFileSync(file, 'test("ok", () => { expect(1 + 1).toBe(2); });\n');

    const proc = Bun.spawnSync(
      [resolve(TESTRUNNER), file, "--no-progress", "--output=json", "--coverage"],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (proc.exitCode !== 0) throw new Error(`TestRunner --output=json --coverage exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    const stdout = proc.stdout.toString();
    if (stdout.includes("Coverage Summary"))
      throw new Error(`TestRunner --output=json should suppress coverage summary on stdout, got: ${stdout.slice(0, 200)}`);
    const json = JSON.parse(stdout);
    if (json.ok !== true) throw new Error(`TestRunner --output=json --coverage ok should be true, got ${json.ok}`);
  } finally {
    clean(tmp);
  }
});

await section("TestRunner: --output=compact-json omits build, memory, stdout, stderr...", async () => {
  const tmp = makeTmp();
  try {
    const first = join(tmp, "test-compact-a.js");
    const second = join(tmp, "test-compact-b.js");
    writeFileSync(
      first,
      [
        'describe("a", () => {',
        '  test("passes a", () => { expect(1 + 1).toBe(2); });',
        "});",
        "",
      ].join("\n"),
    );
    writeFileSync(
      second,
      [
        'describe("b", () => {',
        '  test("passes b", () => { expect(2 + 2).toBe(4); });',
        "});",
        "",
      ].join("\n"),
    );

    const proc = Bun.spawnSync(
      [resolve(TESTRUNNER), first, second, "--no-progress", "--jobs=2", "--output=compact-json"],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (proc.exitCode !== 0) throw new Error(`TestRunner --output=compact-json exited ${proc.exitCode}: ${proc.stderr.toString()}`);

    const stdout = proc.stdout.toString();
    if (stdout.includes("Test Results"))
      throw new Error(`TestRunner --output=compact-json should suppress human-readable summary, got: ${stdout}`);
    const json = JSON.parse(stdout);
    if ("build" in json) throw new Error("TestRunner --output=compact-json should omit top-level build");
    if ("memory" in json) throw new Error("TestRunner --output=compact-json should omit top-level memory");
    if ("stdout" in json) throw new Error("TestRunner --output=compact-json should omit top-level stdout");
    if ("stderr" in json) throw new Error("TestRunner --output=compact-json should omit top-level stderr");
    if (json.ok !== true) throw new Error(`TestRunner --output=compact-json ok should be true, got ${json.ok}`);
    if (json.totalFiles !== 2) throw new Error(`TestRunner --output=compact-json totalFiles should be 2, got ${json.totalFiles}`);
    if (json.passed !== 2 || json.failed !== 0)
      throw new Error(`TestRunner --output=compact-json pass/fail mismatch: ${json.passed}/${json.failed}`);
    if (typeof json.timing?.total_ns !== "number")
      throw new Error("TestRunner --output=compact-json timing should be present");
    if (typeof json.workers?.used !== "number")
      throw new Error("TestRunner --output=compact-json workers should be present");
    if (!Array.isArray(json.files) || json.files.length !== 2)
      throw new Error("TestRunner --output=compact-json files should have two entries");
    for (const [idx, file] of (json.files as any[]).entries()) {
      if ("memory" in file) throw new Error(`TestRunner --output=compact-json files[${idx}] memory should be omitted`);
      if ("stdout" in file) throw new Error(`TestRunner --output=compact-json files[${idx}] stdout should be omitted`);
      if ("stderr" in file) throw new Error(`TestRunner --output=compact-json files[${idx}] stderr should be omitted`);
      if (typeof file.fileName !== "string")
        throw new Error(`TestRunner --output=compact-json files[${idx}] fileName should be present`);
      if (typeof file.timing?.total_ns !== "number")
        throw new Error(`TestRunner --output=compact-json files[${idx}] timing should be present`);
      if (file.passed !== 1 || file.failed !== 0)
        throw new Error(`TestRunner --output=compact-json files[${idx}] pass/fail mismatch: ${JSON.stringify(file)}`);
    }
  } finally {
    clean(tmp);
  }
});

// -- Source maps (Loader) -------------------------------------------------------

{
  const tmp = makeTmp();
  try {
    const jsxSource = [
      "const createElement = (t, p, ...c) => ({ t, p, c });",
      'const el = <div id="test">hello</div>;',
      "el;",
      "",
    ].join("\n");

    console.log("Loader: source map bytecode...");
    const jsxPath = join(tmp, "test.jsx");
    writeFileSync(jsxPath, jsxSource);
    await $`${LOADER} --source-map --mode=bytecode ${jsxPath}`.quiet();
    const mapPath = jsxPath.replace(/\.jsx$/, ".jsx.map");
    if (!existsSync(mapPath)) throw new Error("Source map should exist");
    assertValidSourceMap(mapPath);

    console.log("Loader: source map custom path...");
    const customMapPath = join(tmp, "custom.map");
    await $`${LOADER} --source-map=${customMapPath} --mode=bytecode ${jsxPath}`.quiet();
    if (!existsSync(customMapPath)) throw new Error("Custom source map should exist");
    assertValidSourceMap(customMapPath);

    console.log("Loader: source map interpreted...");
    const interpJsxPath = join(tmp, "interp.jsx");
    writeFileSync(interpJsxPath, jsxSource);
    await $`${LOADER} --source-map ${interpJsxPath}`.quiet();
    const interpMapPath = interpJsxPath.replace(/\.jsx$/, ".jsx.map");
    if (!existsSync(interpMapPath)) throw new Error("Interpreted source map should exist");
    assertValidSourceMap(interpMapPath);

    console.log("Loader: no --source-map -> no .map...");
    const noMapJsxPath = join(tmp, "nomap.jsx");
    writeFileSync(noMapJsxPath, jsxSource);
    await $`${LOADER} ${noMapJsxPath}`.quiet();
    const noMapPath = noMapJsxPath.replace(/\.jsx$/, ".jsx.map");
    if (existsSync(noMapPath)) throw new Error("No .map file should exist without --source-map");

    console.log("Loader: stdin --source-map rejection...");
    const stdinRes = await $`echo 'const x = 1;' | ${LOADER} --source-map 2>&1`.nothrow();
    const stdinOut = stdinRes.text().toLowerCase();
    if (!stdinOut.includes("error") && !stdinOut.includes("cannot") && !stdinOut.includes("require")) {
      throw new Error(`Stdin --source-map should produce an error, got: ${stdinRes.text()}`);
    }
  } finally {
    clean(tmp);
  }
}

// ============================================================================
// GocciaBundler
// ============================================================================

{
  const tmp = makeTmp();
  try {
    console.log("Bundler: single file compile + roundtrip...");
    const singleFile = join(tmp, "single.js");
    writeFileSync(singleFile, "const x = 2 + 2;\nx;\n");
    const singleOut = await $`${BUNDLER} ${singleFile} 2>&1`.text();
    const singleGbc = singleFile.replace(/\.js$/, ".gbc");
    if (!existsSync(singleGbc)) throw new Error(".gbc should exist");
    if (!singleOut.includes("Compiled to")) throw new Error('Output should contain "Compiled to"');

    // Roundtrip
    const roundtripOut = await $`${LOADER} --print ${singleGbc} 2>&1`.text();
    if (!containsLine(roundtripOut, "4")) throw new Error(`Roundtrip should print 4 on its own line, got: ${roundtripOut}`);

    console.log("Bundler: custom --output path...");
    const customOut = join(tmp, "custom.gbc");
    const customSrc = join(tmp, "custom.js");
    writeFileSync(customSrc, "const y = 3 + 3;\ny;\n");
    await $`${BUNDLER} ${customSrc} --output=${customOut}`.quiet();
    if (!existsSync(customOut)) throw new Error("Custom --output .gbc should exist");

    console.log("Bundler: compatibility arguments object roundtrip...");
    const argumentsSrc = join(tmp, "arguments.js");
    const argumentsOut = join(tmp, "arguments.gbc");
    writeFileSync(
      argumentsSrc,
      [
        "var count = function() { return arguments.length; };",
        "count(1, 2);",
        "",
      ].join("\n"),
    );
    await $`${BUNDLER} ${argumentsSrc} --output=${argumentsOut} --compat-var --compat-function --compat-arguments-object`.quiet();
    const argumentsRoundtrip = await $`${LOADER} --print ${argumentsOut} 2>&1`.text();
    if (!containsLine(argumentsRoundtrip, "2"))
      throw new Error(`Arguments-object roundtrip should print 2, got: ${argumentsRoundtrip}`);

    console.log("Bundler: repeated NaN constants compile + roundtrip...");
    const nanSrc = join(tmp, "nan-constants.js");
    const nanOut = join(tmp, "nan-constants.gbc");
    writeFileSync(
      nanSrc,
      [
        'console.log("NaN =", NaN);',
        'console.log("NaN2 =", NaN);',
        "",
      ].join("\n"),
    );
    await $`${BUNDLER} ${nanSrc} --output=${nanOut}`.quiet();
    if (!existsSync(nanOut)) throw new Error("Repeated NaN constants should compile to .gbc");
    const nanRoundtrip = await $`${LOADER} ${nanOut} 2>&1`.text();
    if (!nanRoundtrip.includes("NaN = NaN") || !nanRoundtrip.includes("NaN2 = NaN"))
      throw new Error(`Repeated NaN roundtrip should print both lines, got: ${nanRoundtrip}`);

    console.log("Bundler: stdin compile with --output...");
    const stdinOut = join(tmp, "stdin.gbc");
    await $`echo 'const z = 5 + 5; z;' | ${BUNDLER} --output=${stdinOut}`.quiet();
    if (!existsSync(stdinOut)) throw new Error("Stdin --output .gbc should exist");
    const stdinRoundtrip = await $`${LOADER} --print ${stdinOut} 2>&1`.text();
    if (!containsLine(stdinRoundtrip, "10")) throw new Error(`Stdin roundtrip should print 10 on its own line, got: ${stdinRoundtrip}`);

    console.log("Bundler: stdin without --output should fail...");
    const stdinNoOutput = await $`echo '1 + 1;' | ${BUNDLER} 2>&1`.nothrow();
    if (stdinNoOutput.exitCode === 0) throw new Error("Stdin without --output should exit non-zero");

    console.log("Bundler: directory compile...");
    const dirSrc = join(tmp, "dir-src");
    mkdirSync(dirSrc);
    writeFileSync(join(dirSrc, "a.js"), "1 + 1;\n");
    writeFileSync(join(dirSrc, "b.js"), "2 + 2;\n");
    await $`${BUNDLER} ${dirSrc}`.quiet();
    if (!existsSync(join(dirSrc, "a.gbc"))) throw new Error("Directory compile should create a.gbc");
    if (!existsSync(join(dirSrc, "b.gbc"))) throw new Error("Directory compile should create b.gbc");

    console.log("Bundler: multiple files...");
    const multiA = join(tmp, "multi-a.js");
    const multiB = join(tmp, "multi-b.js");
    writeFileSync(multiA, "10 + 10;\n");
    writeFileSync(multiB, "20 + 20;\n");
    await $`${BUNDLER} ${multiA} ${multiB}`.quiet();
    if (!existsSync(join(tmp, "multi-a.gbc"))) throw new Error("multi-a.gbc should exist");
    if (!existsSync(join(tmp, "multi-b.gbc"))) throw new Error("multi-b.gbc should exist");

    console.log("Bundler: .gbc rejection...");
    const gbcInput = join(tmp, "reject.gbc");
    writeFileSync(gbcInput, "not real bytecode");
    const gbcReject = await $`${BUNDLER} ${gbcInput} 2>&1`.nothrow();
    if (gbcReject.exitCode === 0) throw new Error(".gbc input should be rejected");

    // -- Bundler source maps --

    const jsxSource = [
      "const createElement = (t, p, ...c) => ({ t, p, c });",
      'const el = <div id="test">hello</div>;',
      "el;",
      "",
    ].join("\n");

    console.log("Bundler: --source-map option...");
    const smSrc = join(tmp, "sm.jsx");
    writeFileSync(smSrc, jsxSource);
    await $`${BUNDLER} ${smSrc} --source-map`.quiet();
    const smMap = join(tmp, "sm.map");
    if (!existsSync(join(tmp, "sm.gbc"))) throw new Error("--source-map: .gbc should exist");
    if (!existsSync(smMap)) throw new Error("--source-map: .map should exist");
    if (existsSync(join(tmp, "sm.jsx.map"))) throw new Error("--source-map should not write map beside source extension");
    assertValidSourceMap(smMap);

    console.log("Bundler: --source-map defaults beside custom --output...");
    const smOutSrc = join(tmp, "sm-output.jsx");
    const smOutDir = join(tmp, "maps");
    const smOutGbc = join(smOutDir, "out.gbc");
    const smOutMap = join(smOutDir, "out.map");
    mkdirSync(smOutDir, { recursive: true });
    writeFileSync(smOutSrc, jsxSource);
    await $`${BUNDLER} ${smOutSrc} --output=${smOutGbc} --source-map`.quiet();
    if (!existsSync(smOutGbc)) throw new Error("--source-map with --output: .gbc should exist");
    if (!existsSync(smOutMap)) throw new Error("--source-map with --output: .map should exist beside .gbc");
    if (existsSync(join(tmp, "sm-output.map")) || existsSync(join(tmp, "sm-output.jsx.map")))
      throw new Error("--source-map with --output should not write .map beside source");
    assertValidSourceMap(smOutMap);

    console.log("Bundler: --source-map=<custom path>...");
    const smCustomSrc = join(tmp, "sm-custom.jsx");
    const smCustomMap = join(tmp, "custom-output.map");
    writeFileSync(smCustomSrc, jsxSource);
    await $`${BUNDLER} ${smCustomSrc} --source-map=${smCustomMap}`.quiet();
    if (!existsSync(smCustomMap)) throw new Error("Custom map should exist");
    assertValidSourceMap(smCustomMap);

    console.log("Bundler: no --source-map -> no .map...");
    const noSmSrc = join(tmp, "no-sm.jsx");
    writeFileSync(noSmSrc, jsxSource);
    await $`${BUNDLER} ${noSmSrc}`.quiet();
    if (existsSync(join(tmp, "no-sm.map"))) throw new Error("No .map file should exist without --source-map");
    if (existsSync(join(tmp, "no-sm.jsx.map"))) throw new Error("No .map file should exist without --source-map");

    console.log("Bundler: stdin --source-map --output...");
    const stdinSmOut = join(tmp, "stdin-sm.gbc");
    await $`echo ${jsxSource} | ${BUNDLER} --source-map --output=${stdinSmOut}`.quiet();
    const stdinSmMap = join(tmp, "stdin-sm.map");
    if (!existsSync(stdinSmOut)) throw new Error("Stdin --source-map: .gbc should exist");
    if (!existsSync(stdinSmMap)) throw new Error("Stdin --source-map: .map should exist beside .gbc");
    assertValidSourceMap(stdinSmMap);
  } finally {
    clean(tmp);
  }
}

// ============================================================================
// GocciaBenchmarkRunner
// ============================================================================

{
  const tmp = makeTmp();
  const benchEnv = {
    ...process.env,
    GOCCIA_BENCH_CALIBRATION_MS: "50",
    GOCCIA_BENCH_ROUNDS: "3",
  } as Record<string, string>;

  try {
    const stdinSource = microbenchModule([
      'group("stdin", () => {',
      '  bench("sum", () => 1 + 1);',
      "});",
    ]);

    console.log("BenchmarkRunner: file benchmark (interpreted)...");
    const fileOut = join(tmp, "file-interp.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "benchmarks/fibonacci.js", "--source-type=module", "--no-progress", "--format=json", `--output=${fileOut}`],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode !== 0) throw new Error(`File benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const fileJson = readFileSync(fileOut, "utf-8");
    if (!fileJson.includes('"fileName":')) throw new Error('File JSON should contain "fileName":');
    if (fileJson.includes('"file":')) throw new Error('File JSON should not contain duplicate "file" alias');
    if (!fileJson.includes('"totalBenchmarks":')) throw new Error('File JSON should contain "totalBenchmarks":');
    {
      const json = JSON.parse(fileJson);
      if (typeof json.build?.version !== "string") throw new Error("Benchmark JSON build.version should be present");
      if (json.fileName !== undefined) throw new Error(`Benchmark JSON fileName should only be present per-file, got ${json.fileName}`);
      if (json.files?.[0]?.fileName !== "benchmarks/fibonacci.js") throw new Error(`Benchmark JSON fileName mismatch: ${json.files?.[0]?.fileName}`);
      if (!Array.isArray(json.output)) throw new Error("Benchmark JSON output should be an array");
      if (json.error !== null) throw new Error("Benchmark JSON error should be null");
      if (typeof json.timing?.total_ns !== "number") throw new Error("Benchmark JSON timing.total_ns should be present");
      if (typeof json.memory?.gc?.limitBytes !== "number") throw new Error("Benchmark JSON memory.gc.limitBytes should be present");
      if ("maxBytes" in json.memory.gc) throw new Error("Benchmark JSON memory.gc.maxBytes should not be present; use limitBytes");
      if (typeof json.memory?.heap?.endAllocatedBytes !== "number") throw new Error("Benchmark JSON memory.heap.endAllocatedBytes should be present");
      if (typeof json.workers?.used !== "number") throw new Error("Benchmark JSON workers.used should be present");
    }

    console.log("BenchmarkRunner: file benchmark (bytecode)...");
    const fileBcOut = join(tmp, "file-bc.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "benchmarks/fibonacci.js", "--source-type=module", "--no-progress", "--format=json", `--output=${fileBcOut}`, "--mode=bytecode"],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode !== 0) throw new Error(`Bytecode file benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const fileBcJson = readFileSync(fileBcOut, "utf-8");
    if (!fileBcJson.includes('"fileName":')) throw new Error('Bytecode file JSON should contain "fileName":');
    if (fileBcJson.includes('"file":')) throw new Error('Bytecode file JSON should not contain duplicate "file" alias');
    {
      const parsed = JSON.parse(fileBcJson);
      const valid = parsed.files
        .flatMap((file: { benchmarks: Array<Record<string, unknown>> }) => file.benchmarks)
        .filter((bench: Record<string, unknown>) =>
          !("error" in bench) &&
          typeof bench.opsPerSec === "number" && bench.opsPerSec > 0 &&
          typeof bench.meanMs === "number" && bench.meanMs > 0 &&
          typeof bench.iterations === "number" && bench.iterations > 0
        );
      if (valid.length === 0) throw new Error("Bytecode benchmark JSON should contain at least one valid result");
    }

    console.log("BenchmarkRunner: microbench API is module-only and accepts wrappers...");
    const moduleOnlyOut = join(tmp, "module-only.json");
    const moduleOnlyBytecodeOut = join(tmp, "module-only-bytecode.json");
    const moduleOnlySource = microbenchModuleWithExports(
      "bench as microBench, group as microGroup, summary, boxplot",
      [
        'if (typeof bench !== "undefined") throw new Error("ambient bench should not exist");',
        'if (typeof group !== "undefined") throw new Error("ambient group should not exist");',
        'if (typeof suite !== "undefined") throw new Error("ambient suite should not exist");',
        'if (typeof runBenchmarks !== "undefined") throw new Error("ambient runBenchmarks should not exist");',
        "summary(() => {",
        "  boxplot(() => {",
        '    microGroup("module-only", () => {',
        '      microBench("sum", () => 1 + 1);',
        '      microBench("array map", () => [1, 2, 3, 4].map((value) => value + 1));',
        '      microBench("special numbers", () => Number.isNaN(NaN) && Number.isFinite(Infinity));',
        "    });",
        "  });",
        "});",
        'microBench("outside wrappers", () => 2 + 2);',
      ],
    );
    for (const run of [
      { output: moduleOnlyOut, modeArguments: [] },
      { output: moduleOnlyBytecodeOut, modeArguments: ["--mode=bytecode"] },
    ]) {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--source-type=module", "--no-progress", "--format=json", `--output=${run.output}`, ...run.modeArguments],
        {
          stdin: new TextEncoder().encode(moduleOnlySource),
          stdout: "pipe",
          stderr: "pipe",
          env: benchEnv,
          timeout: 120_000,
        },
      );
      if (proc.exitCode !== 0) throw new Error(`Module-only microbench API exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    for (const outputFile of [moduleOnlyOut, moduleOnlyBytecodeOut]) {
      const json = JSON.parse(readFileSync(outputFile, "utf-8"));
      if (json.files?.[0]?.benchmarks?.[0]?.name !== "sum")
        throw new Error(`Module-only microbench JSON should contain benchmark name "sum", got: ${JSON.stringify(json.files?.[0]?.benchmarks)}`);
      if (json.totalBenchmarks !== 4)
        throw new Error(`Module-only microbench JSON should contain totalBenchmarks: 4, got ${json.totalBenchmarks}`);
      for (const benchmark of json.files[0].benchmarks) {
        if (!(benchmark.sampleCount > 0 && benchmark.sampleCount <= benchmark.iterations && benchmark.sampleCount <= 10_000))
          throw new Error(`Benchmark sampleCount should be bounded by calibrated iterations and the fixed cap: ${JSON.stringify(benchmark)}`);
        if (!(benchmark.minSampleMs <= benchmark.p25Ms &&
              benchmark.p25Ms <= benchmark.medianMs &&
              benchmark.medianMs <= benchmark.p75Ms &&
              benchmark.p75Ms <= benchmark.p99Ms &&
              benchmark.p99Ms <= benchmark.p999Ms &&
              benchmark.p999Ms <= benchmark.maxSampleMs))
          throw new Error(`Benchmark percentiles should be ordered: ${JSON.stringify(benchmark)}`);
        if (benchmark.name === "outside wrappers") {
          if (benchmark.summaryScope !== null || benchmark.boxplotScope !== null || benchmark.relative !== null)
            throw new Error(`Unwrapped benchmark should stay outside scoped views: ${JSON.stringify(benchmark)}`);
        } else {
          if (benchmark.summaryScope !== 1 || benchmark.boxplotScope !== 1)
            throw new Error(`Benchmark wrapper scopes should be retained: ${JSON.stringify(benchmark)}`);
          if (typeof benchmark.relative?.median !== "number" ||
              typeof benchmark.relative?.low !== "number" ||
              typeof benchmark.relative?.high !== "number" ||
              typeof benchmark.relative?.inconclusive !== "boolean")
            throw new Error(`Benchmark relative comparison should be structured: ${JSON.stringify(benchmark)}`);
        }
      }

      const scopedBenchmarks = json.files[0].benchmarks.filter(
        (benchmark: Record<string, unknown>) => benchmark.summaryScope === 1,
      );
      const baseline = scopedBenchmarks.reduce(
        (fastest: any, benchmark: any) => benchmark.medianMs < fastest.medianMs ? benchmark : fastest,
      );
      if (baseline.relative.median !== 1 || baseline.relative.low !== 1 ||
          baseline.relative.high !== 1 || baseline.relative.inconclusive !== false)
        throw new Error(`Summary baseline should compare exactly to itself: ${JSON.stringify(baseline)}`);
      for (const benchmark of scopedBenchmarks) {
        if (benchmark === baseline) continue;
        const { low, high } = benchmark.relative;
        if (!Number.isFinite(low) || !Number.isFinite(high) || low > high)
          throw new Error(`Summary relative bounds should be finite and ordered: ${JSON.stringify(benchmark)}`);
        const expectedInconclusive = low <= 1 && high >= 1;
        if (benchmark.relative.inconclusive !== expectedInconclusive)
          throw new Error(`Summary overlap classification should match its range: ${JSON.stringify(benchmark)}`);
      }

    }

    const consoleProc = Bun.spawnSync(
      [resolve(BENCHRUNNER), "--source-type=module", "--no-progress"],
      {
        stdin: new TextEncoder().encode(moduleOnlySource),
        stdout: "pipe",
        stderr: "pipe",
        env: benchEnv,
        timeout: 120_000,
      },
    );
    if (consoleProc.exitCode !== 0)
      throw new Error(`Module-only microbench console exit ${consoleProc.exitCode}: ${consoleProc.stderr.toString()}`);
    const consoleOutput = consoleProc.stdout.toString();
    for (const expected of ["p75", "p99", "p999", "boxplot", "summary", "fastest:"])
      if (!consoleOutput.includes(expected))
        throw new Error(`Module-only microbench console should contain ${expected}: ${consoleOutput}`);

    console.log("BenchmarkRunner: async summary and boxplot callbacks retain scopes...");
    const asyncScopeSource = microbenchModuleWithExports(
      "bench, summary, boxplot",
      [
        "await boxplot(async () => {",
        "  await Promise.resolve();",
        "  await summary(async () => {",
        "    await Promise.resolve();",
        '    bench("async fast", () => 1 + 1);',
        '    bench("async slow", () => [1, 2, 3, 4].map((value) => value + 1));',
        "  });",
        "});",
      ],
    );
    for (const run of [
      { output: join(tmp, "async-scopes.json"), modeArguments: [] },
      { output: join(tmp, "async-scopes-bytecode.json"), modeArguments: ["--mode=bytecode"] },
    ]) {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--source-type=module", "--no-progress", "--format=json", `--output=${run.output}`, ...run.modeArguments],
        {
          stdin: new TextEncoder().encode(asyncScopeSource),
          stdout: "pipe",
          stderr: "pipe",
          env: benchEnv,
          timeout: 120_000,
        },
      );
      if (proc.exitCode !== 0)
        throw new Error(`Async microbench scopes exit ${proc.exitCode}: ${proc.stderr.toString()}`);
      const benchmarks = JSON.parse(readFileSync(run.output, "utf-8")).files?.[0]?.benchmarks;
      if (!Array.isArray(benchmarks) || benchmarks.length !== 2 ||
          benchmarks.some((benchmark: any) => benchmark.summaryScope !== 1 || benchmark.boxplotScope !== 1))
        throw new Error(`Async microbench callbacks should retain both scopes: ${JSON.stringify(benchmarks)}`);
    }

    console.log("BenchmarkRunner: deterministic profile mode...");
    const profileBench = join(tmp, "profile-deterministic.js");
    const profileOut = join(tmp, "profile.json");
    writeFileSync(profileBench, microbenchModule([
      'let count = 0;',
      "const profiledBenchmark = {",
      "  *run() {",
      "    const state = { value: 1 };",
      "    yield async () => {",
      "      count = count + await Promise.resolve(state.value);",
      "    };",
      '      if (count !== 1) { throw new Error("expected one deterministic run, got " + count); }',
      "  },",
      "}.run;",
      'group("profile", () => {',
      '  bench("runs once", profiledBenchmark);',
      "});",
    ]));
    {
      const proc = Bun.spawnSync(
        [
          resolve(BENCHRUNNER),
          profileBench,
          "--source-type=module",
          "--profile-deterministic",
          "--profile=all",
          `--profile-output=${profileOut}`,
          "--no-progress",
          "--format=compact-json",
        ],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode !== 0) throw new Error(`Deterministic profile benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
      const report = JSON.parse(proc.stdout.toString());
      const bench = report.files?.[0]?.benchmarks?.[0];
      if (bench?.iterations !== 1)
        throw new Error(`Deterministic profile report should record one iteration, got ${bench?.iterations}`);
    }
    {
      const profile = JSON.parse(readFileSync(profileOut, "utf-8"));
      if (!Array.isArray(profile.opcodes) || profile.opcodes.length === 0)
        throw new Error("Deterministic profile should include opcode counts");
      if (!Array.isArray(profile.functions) || profile.functions.length === 0)
        throw new Error("Deterministic profile should include function counts");
      if (!profile.functions.some((fn: Record<string, unknown>) => typeof fn.allocations === "number"))
        throw new Error("Deterministic profile should include function allocation counts");
    }
    const scriptRunProfileBench = join(tmp, "profile-deterministic-script-run.js");
    const scriptRunProfileOut = join(tmp, "profile-script-run.json");
    writeFileSync(scriptRunProfileBench, microbenchModuleWithExports("bench, group, run", [
      "let count = 0;",
      'group("profile-script-run", () => {',
      '  bench("reruns deterministically", () => { count++; });',
      "});",
      "run();",
    ]));
    {
      const proc = Bun.spawnSync(
        [
          resolve(BENCHRUNNER),
          scriptRunProfileBench,
          "--source-type=module",
          "--profile-deterministic",
          "--profile=all",
          `--profile-output=${scriptRunProfileOut}`,
          "--no-progress",
          "--format=compact-json",
        ],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode !== 0) throw new Error(`Deterministic script-run profile benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
      const report = JSON.parse(proc.stdout.toString());
      const bench = report.files?.[0]?.benchmarks?.[0];
      if (bench?.iterations !== 1)
        throw new Error(`Deterministic script-run profile report should record one iteration, got ${bench?.iterations}`);
    }
    {
      const profile = JSON.parse(readFileSync(scriptRunProfileOut, "utf-8"));
      if (!Array.isArray(profile.opcodes) || profile.opcodes.length === 0)
        throw new Error("Deterministic script-run profile should include opcode counts");
      if (!Array.isArray(profile.functions) || profile.functions.length === 0)
        throw new Error("Deterministic script-run profile should include function counts");
    }

    console.log("BenchmarkRunner: file benchmark JSON output...");
    if (!fileJson.includes('"totalBenchmarks":')) throw new Error('JSON should contain totalBenchmarks');

    console.log("BenchmarkRunner: multi-file JSON structure...");
    const benchA = join(tmp, "bench-a.js");
    const benchB = join(tmp, "bench-b.js");
    const multiBenchOut = join(tmp, "bench-multi.json");
    writeFileSync(benchA, microbenchModule(['group("a", () => { bench("one", () => 1 + 1); });']));
    writeFileSync(benchB, microbenchModule(['group("b", () => { bench("two", () => 2 + 2); });']));
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), benchA, benchB, "--source-type=module", "--no-progress", "--jobs=2", "--format=json", `--output=${multiBenchOut}`],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode !== 0) throw new Error(`Multi-file benchmark JSON exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    {
      const json = JSON.parse(readFileSync(multiBenchOut, "utf-8"));
      assertCommonJsonReport(json, "Benchmark multi-file JSON", 2);
      if (json.ok !== true) throw new Error(`Benchmark multi-file top-level ok should be true, got ${json.ok}`);
      if (json.error !== null) throw new Error("Benchmark multi-file top-level error should be null");
      if (json.totalBenchmarks !== 2) throw new Error(`Benchmark multi-file totalBenchmarks should be 2, got ${json.totalBenchmarks}`);
      if (json.workers.used !== 2) throw new Error(`Benchmark multi-file workers.used should be 2, got ${json.workers.used}`);
      if (json.memory.gc.allocatedDuringRunBytes <= 0)
        throw new Error("Benchmark multi-file top-level memory should include worker GC allocations");
      if (json.memory.gc.collections <= 0)
        throw new Error("Benchmark multi-file top-level memory should include worker GC collections");
      if (json.memory.gc.liveBytes > json.memory.gc.limitBytes * (json.workers.used + 1))
        throw new Error("Benchmark multi-file top-level live memory should not double-count per-file worker snapshots");
      assertCommonJsonFile(json.files[0], "Benchmark first file", benchA);
      assertCommonJsonFile(json.files[1], "Benchmark second file", benchB);
      if (json.files[0].benchmarks?.[0]?.name !== "one") throw new Error(`Benchmark first file entry mismatch: ${JSON.stringify(json.files[0].benchmarks)}`);
      if (json.files[1].benchmarks?.[0]?.name !== "two") throw new Error(`Benchmark second file entry mismatch: ${JSON.stringify(json.files[1].benchmarks)}`);
      if (json.files[0].memory !== null || json.files[1].memory !== null)
        throw new Error("Benchmark multi-file per-file memory should be null when top-level memory is aggregated");
    }

    console.log("BenchmarkRunner: --format=compact-json omits build, memory, stdout, stderr...");
    const compactBenchOut = join(tmp, "bench-compact.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), benchA, benchB, "--source-type=module", "--no-progress", "--jobs=2", "--format=compact-json", `--output=${compactBenchOut}`],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode !== 0) throw new Error(`Benchmark --format=compact-json exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    {
      const json = JSON.parse(readFileSync(compactBenchOut, "utf-8"));
      if ("build" in json) throw new Error("Benchmark --format=compact-json should omit top-level build");
      if ("memory" in json) throw new Error("Benchmark --format=compact-json should omit top-level memory");
      if ("stdout" in json) throw new Error("Benchmark --format=compact-json should omit top-level stdout");
      if ("stderr" in json) throw new Error("Benchmark --format=compact-json should omit top-level stderr");
      if (json.ok !== true) throw new Error(`Benchmark --format=compact-json ok should be true, got ${json.ok}`);
      if (json.totalBenchmarks !== 2) throw new Error(`Benchmark --format=compact-json totalBenchmarks should be 2, got ${json.totalBenchmarks}`);
      if (typeof json.timing?.total_ns !== "number") throw new Error("Benchmark --format=compact-json timing should be present");
      if (typeof json.workers?.used !== "number") throw new Error("Benchmark --format=compact-json workers should be present");
      if (!Array.isArray(json.files) || json.files.length !== 2) throw new Error("Benchmark --format=compact-json files should have two entries");
      for (const [idx, file] of (json.files as any[]).entries()) {
        if ("memory" in file) throw new Error(`Benchmark --format=compact-json files[${idx}] memory should be omitted`);
        if ("stdout" in file) throw new Error(`Benchmark --format=compact-json files[${idx}] stdout should be omitted`);
        if ("stderr" in file) throw new Error(`Benchmark --format=compact-json files[${idx}] stderr should be omitted`);
        if (typeof file.fileName !== "string") throw new Error(`Benchmark --format=compact-json files[${idx}] fileName should be present`);
        if (!Array.isArray(file.benchmarks) || file.benchmarks.length !== 1)
          throw new Error(`Benchmark --format=compact-json files[${idx}] benchmarks length mismatch: ${JSON.stringify(file.benchmarks)}`);
      }
    }

    console.log("BenchmarkRunner: benchmark failure JSON output...");
    const failBench = join(tmp, "benchmark-fail.js");
    const failOut = join(tmp, "benchmark-fail.json");
    writeFileSync(failBench, microbenchModule(['group("fail", () => { bench("boom", () => { throw new Error("boom"); }); });']));
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), failBench, "--source-type=module", "--no-progress", "--format=json", `--output=${failOut}`],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode === 0) throw new Error("Failing benchmark JSON export should fail");
    }
    {
      const json = JSON.parse(readFileSync(failOut, "utf-8"));
      const file = json.files?.[0];
      if (json.ok !== false) throw new Error(`Failing benchmark run should mark top-level ok=false, got ${json.ok}`);
      if (file?.ok !== false) throw new Error(`Failing benchmark file should mark ok=false, got ${file?.ok}`);
      if (typeof file?.error?.message !== "string") throw new Error("Failing benchmark file should include shared error object");
    }
    console.log("BenchmarkRunner: generator cleanup preserves original benchmark failure...");
    const cleanupFailBench = join(tmp, "benchmark-cleanup-fail.js");
    const cleanupFailOut = join(tmp, "benchmark-cleanup-fail.json");
    writeFileSync(cleanupFailBench, microbenchModule([
      'group("cleanup", () => {',
      '  bench("body error wins", function* () {',
      "    try {",
      '      yield () => { throw new Error("body failure"); };',
      "    } finally {",
      '      throw new Error("cleanup failure");',
      "    }",
      "  });",
      "});",
    ]));
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), cleanupFailBench, "--source-type=module", "--no-progress", "--format=json", `--output=${cleanupFailOut}`],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode === 0) throw new Error("Generator cleanup failure preservation benchmark should fail");
    }
    {
      assertPreservesBodyFailure(cleanupFailOut, "Generator cleanup");
    }
    console.log("BenchmarkRunner: deterministic generator cleanup preserves original benchmark failure...");
    const deterministicCleanupFailOut = join(tmp, "benchmark-cleanup-fail-deterministic.json");
    const deterministicCleanupProfileOut = join(tmp, "benchmark-cleanup-fail-profile.json");
    {
      const proc = Bun.spawnSync(
        [
          resolve(BENCHRUNNER),
          cleanupFailBench,
          "--source-type=module",
          "--profile-deterministic",
          "--profile=all",
          `--profile-output=${deterministicCleanupProfileOut}`,
          "--no-progress",
          "--format=json",
          `--output=${deterministicCleanupFailOut}`,
        ],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode === 0) throw new Error("Deterministic generator cleanup failure preservation benchmark should fail");
    }
    {
      assertPreservesBodyFailure(deterministicCleanupFailOut, "Deterministic generator cleanup");
    }

    console.log("BenchmarkRunner: callback timeout is enforced...");
    {
      const timeoutSource = microbenchModule([
        'group("limit", () => {',
        '  bench("loop", () => { while (true) {} });',
        "});",
      ]);
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--source-type=module", "--no-progress", "--timeout=1"],
        {
          stdin: new TextEncoder().encode(timeoutSource),
          stdout: "pipe",
          stderr: "pipe",
          env: benchEnv,
          timeout: 10_000,
        },
      );
      if (proc.exitCode === 0) throw new Error("Benchmark callback timeout should fail");
    }

    console.log("BenchmarkRunner: stdin benchmark (interpreted)...");
    const stdinOutPath = join(tmp, "stdin-interp.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--source-type=module", "--no-progress", "--format=json", `--output=${stdinOutPath}`],
        {
          stdin: new TextEncoder().encode(stdinSource),
          stdout: "pipe",
          stderr: "pipe",
          env: benchEnv,
          timeout: 120_000,
        },
      );
      if (proc.exitCode !== 0) throw new Error(`Stdin benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const stdinJson = readFileSync(stdinOutPath, "utf-8");
    {
      const json = JSON.parse(stdinJson);
      if (json.files?.[0]?.benchmarks?.[0]?.name !== "sum") throw new Error('Stdin JSON should contain benchmark name "sum"');
      if (json.totalBenchmarks !== 1) throw new Error(`Stdin JSON should contain totalBenchmarks: 1, got ${json.totalBenchmarks}`);
    }

    console.log("BenchmarkRunner: script-callable run avoids auto-run double measurement...");
    const repeatedRunOutPath = join(tmp, "repeated-run.json");
    {
      const repeatedRunSource = microbenchModuleWithExports("bench, group, run", [
        "let setupCount = 0;",
        "const failsIfMeasuredTwice = {",
        "  *run() {",
        "    setupCount++;",
        '    if (setupCount > 1) throw new Error("benchmark was measured more than once");',
        "    yield () => 1 + 1;",
        "  },",
        "}.run;",
        'group("twice", () => {',
        '  bench("sum", failsIfMeasuredTwice);',
        "});",
        "run();",
        "Goccia.gc();",
      ]);
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--source-type=module", "--no-progress", "--format=json", `--output=${repeatedRunOutPath}`],
        {
          stdin: new TextEncoder().encode(repeatedRunSource),
          stdout: "pipe",
          stderr: "pipe",
          env: benchEnv,
          timeout: 120_000,
        },
      );
      if (proc.exitCode !== 0) throw new Error(`Repeated benchmark run exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    {
      const json = JSON.parse(readFileSync(repeatedRunOutPath, "utf-8"));
      if (json.files?.[0]?.benchmarks?.[0]?.name !== "sum") throw new Error('Repeated benchmark JSON should contain benchmark name "sum"');
      if (json.totalBenchmarks !== 1) throw new Error(`Repeated benchmark JSON should contain totalBenchmarks: 1, got ${json.totalBenchmarks}`);
    }

    console.log("BenchmarkRunner: stdin benchmark (bytecode)...");
    const stdinBcOutPath = join(tmp, "stdin-bc.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--source-type=module", "--no-progress", "--format=json", `--output=${stdinBcOutPath}`, "--mode=bytecode"],
        {
          stdin: new TextEncoder().encode(stdinSource),
          stdout: "pipe",
          stderr: "pipe",
          env: benchEnv,
          timeout: 120_000,
        },
      );
      if (proc.exitCode !== 0) throw new Error(`Bytecode stdin benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const stdinBcJson = readFileSync(stdinBcOutPath, "utf-8");
    {
      const json = JSON.parse(stdinBcJson);
      if (json.files?.[0]?.benchmarks?.[0]?.name !== "sum") throw new Error('Bytecode stdin JSON should contain benchmark name "sum"');
      if (json.totalBenchmarks !== 1) throw new Error(`Bytecode stdin JSON should contain totalBenchmarks: 1, got ${json.totalBenchmarks}`);
    }

    console.log("BenchmarkRunner: async generator bytecode benchmark...");
    const asyncGeneratorBcOutPath = join(tmp, "async-generator-bc.json");
    {
      const asyncGeneratorSource = microbenchModule([
        'group("async generator", () => {',
        "  const source = { async *values() { yield 1; yield 2; } };",
        '  bench("consume", async () => {',
        "      let sum = 0;",
        "      for await (const value of source.values()) sum = sum + value;",
        "      return sum;",
        "  });",
        "});",
      ]);
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--source-type=module", "--no-progress", "--format=json", `--output=${asyncGeneratorBcOutPath}`, "--mode=bytecode"],
        {
          stdin: new TextEncoder().encode(asyncGeneratorSource),
          stdout: "pipe",
          stderr: "pipe",
          env: benchEnv,
          timeout: 120_000,
        },
      );
      if (proc.exitCode !== 0) throw new Error(`Bytecode async generator benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const asyncGeneratorBcJson = JSON.parse(readFileSync(asyncGeneratorBcOutPath, "utf-8"));
    const asyncGeneratorBench = (asyncGeneratorBcJson.files ?? [])
      .flatMap((file: { benchmarks?: Array<Record<string, unknown>> }) => file.benchmarks ?? [])
      .find((bench: Record<string, unknown>) => bench.name === "consume");
    if (!asyncGeneratorBench) throw new Error('Bytecode async generator JSON should contain benchmark named "consume"');
    if (typeof asyncGeneratorBench.opsPerSec !== "number" || asyncGeneratorBench.opsPerSec <= 0) {
      throw new Error("Bytecode async generator benchmark should report positive opsPerSec");
    }

    console.log("BenchmarkRunner: no valid bytecode benchmarks fail...");
    const emptyBcOutPath = join(tmp, "empty-bc.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--source-type=module", "--no-progress", "--format=json", `--output=${emptyBcOutPath}`, "--mode=bytecode"],
        {
          stdin: new TextEncoder().encode("const value = 1;\n"),
          stdout: "pipe",
          stderr: "pipe",
          env: benchEnv,
          timeout: 120_000,
        },
      );
      if (proc.exitCode === 0) throw new Error("Bytecode benchmark with no valid results should fail");
    }
    {
      const json = JSON.parse(readFileSync(emptyBcOutPath, "utf-8"));
      const file = json.files?.[0];
      if (json.ok !== false) throw new Error(`No-valid benchmark run should mark top-level ok=false, got ${json.ok}`);
      if (file?.ok !== false) throw new Error(`No-valid benchmark file should mark ok=false, got ${file?.ok}`);
      if (typeof file?.error?.message !== "string") throw new Error("No-valid benchmark file should include shared error object");
    }
  } finally {
    clean(tmp);
  }
}

// ============================================================================
// GocciaREPL
// ============================================================================

await section("REPL: banner (interpreted)...", async () => {
  const out = await $`echo '' | ${REPL} 2>&1`.text();
  if (!out.includes("Goccia REPL")) throw new Error(`Banner should contain "Goccia REPL", got: ${out.slice(0, 200)}`);
  if (!out.includes("(interpreted)")) throw new Error(`Banner should contain "(interpreted)", got: ${out.slice(0, 200)}`);
});

await section("REPL: banner (bytecode)...", async () => {
  const out = await $`echo '' | ${REPL} --mode=bytecode 2>&1`.text();
  if (!out.includes("(bytecode)")) throw new Error(`Bytecode banner should contain "(bytecode)", got: ${out.slice(0, 200)}`);
});

await section("REPL: expression evaluation...", async () => {
  const out = await $`echo '2 + 2;' | ${REPL} 2>&1`.text();
  if (!out.includes("4")) throw new Error(`Expression 2+2 should produce 4, got: ${out}`);
});

await section("REPL: ASI mode...", async () => {
  const out = await $`printf 'const x = 5\nx\n' | ${REPL} --compat-asi 2>&1`.text();
  if (!out.includes("5")) throw new Error(`ASI mode should produce 5, got: ${out}`);
});

await section("REPL: error recovery...", async () => {
  const out = await $`printf 'const x = ;\n2 + 2;\n' | ${REPL} 2>&1`.text();
  if (!out.includes("4")) throw new Error(`After error, second expression should produce 4, got: ${out}`);
});

await section("REPL: bytecode evaluation...", async () => {
  const out = await $`echo '2 + 2;' | ${REPL} --mode=bytecode 2>&1`.text();
  if (!out.includes("4")) throw new Error(`Bytecode 2+2 should produce 4, got: ${out}`);
});

await section("REPL: repeated tagged template execution (interpreted + bytecode)...", async () => {
  const src = [
    "globalThis.tag = (strings) => { globalThis.firstTemplate = strings; return strings[0]; }; tag`first`;",
    'globalThis.tag = (strings) => globalThis.firstTemplate === strings ? "stale" : strings[0]; tag`second`;',
  ].join("\n") + "\n";

  for (const [label, args] of [
    ["interpreted", []],
    ["bytecode", ["--mode=bytecode"]],
  ] as const) {
    const proc = Bun.spawnSync([REPL, ...args], {
      stdin: new TextEncoder().encode(src),
      stdout: "pipe",
      stderr: "pipe",
    });
    const out = `${proc.stdout.toString()}${proc.stderr.toString()}`;
    if (proc.exitCode !== 0)
      throw new Error(`REPL ${label} tagged-template run failed: ${out}`);
    if (!out.includes("'first'") || !out.includes("'second'") ||
        out.includes("'stale'"))
      throw new Error(`REPL ${label} should keep repeated parse template sites distinct, got: ${out}`);
  }
});

// ============================================================================
// GocciaSandboxRunner
// ============================================================================

await section("SandboxRunner: fs callback APIs and promises defer filesystem work...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import fs, { exists, readFile, writeFile } from "fs";',
            'writeFile("/callback.txt", "callback", (error) => {',
            '  if (error) { console.log("callback-error:" + error.code); return; }',
            '  readFile("/callback.txt", "utf8", (readError, text) => {',
            '    if (readError) { console.log("read-error:" + readError.code); return; }',
            '    console.log("callback:" + text);',
            '  });',
            '});',
            'Goccia.gc();',
            'console.log("callback-immediate:" + fs.existsSync("/callback.txt"));',
            'const promiseWrite = fs.promises.writeFile("/promise.txt", "promise");',
            'promiseWrite.then(() => exists("/promise.txt", (present) => {',
            '  console.log("exists:" + present);',
            '}));',
            'Goccia.gc();',
            'console.log("promise-immediate:" + fs.existsSync("/promise.txt"));',
          ].join("\n"),
        },
      ],
    }));

    const expected = [
      "callback-immediate:false",
      "promise-immediate:false",
      "callback:callback",
      "exists:true",
    ].join("\n");
    for (const [label, extraArgs] of [
      ["interpreter", []],
      ["bytecode", ["--mode=bytecode"]],
    ] as const) {
      const proc = Bun.spawnSync(
        [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module", ...extraArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      const stdout = normalizeLineEndings(proc.stdout.toString()).trim();
      if (proc.exitCode !== 0)
        throw new Error(`SandboxRunner ${label} fs callback run should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
      if (stdout !== expected)
        throw new Error(`SandboxRunner ${label} fs callback output should be ${JSON.stringify(expected)}, got: ${stdout}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: fs callback overloads use Node-shaped results...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import fs, { appendFile, copyFile, exists, mkdir, readFile, readdir, rename, rm, stat } from "fs";',
            'const capture = (start) => new Promise((resolve) => start((...args) => resolve(args)));',
            'console.log("lengths:" + [fs.readFile.length, fs.writeFile.length, fs.appendFile.length, fs.mkdir.length, fs.readdir.length, fs.stat.length, fs.rm.length, fs.rename.length, fs.copyFile.length, fs.exists.length].join(","));',
            'console.log("promise-lengths:" + [fs.promises.readFile.length, fs.promises.writeFile.length, fs.promises.appendFile.length, fs.promises.mkdir.length, fs.promises.readdir.length, fs.promises.stat.length, fs.promises.rm.length, fs.promises.rename.length, fs.promises.copyFile.length].join(","));',
            'console.log("promises-exists:" + typeof fs.promises.exists);',
            'try { readFile("/source.txt", "utf8"); }',
            'catch (error) { console.log("missing-callback:" + error.name); }',
            'try { readFile("/source.txt", "latin1", () => {}); }',
            'catch (error) { console.log("unsupported-encoding:" + error.name); }',
            'try { mkdir("/bad-mkdir", true, () => {}); }',
            'catch (error) { console.log("mkdir-boolean-options:" + error.name); }',
            'try { rm("/source.txt", true, () => {}); }',
            'catch (error) { console.log("rm-boolean-options:" + error.name); }',
            'const invalidPathPromise = fs.promises.readFile(123);',
            'console.log("promise-validation-return:" + (invalidPathPromise instanceof Promise));',
            'try { await invalidPathPromise; }',
            'catch (error) { console.log("promise-validation-reject:" + error.name); }',
            'const invalidCopyPromise = fs.promises.copyFile("/source.txt", "/bad-copy.txt", 1);',
            'console.log("copy-mode-return:" + (invalidCopyPromise instanceof Promise));',
            'try { await invalidCopyPromise; }',
            'catch (error) { console.log("unsupported-copy-mode:" + error.name); }',
            'const invalidRmPromise = fs.promises.rm("/source.txt", true);',
            'console.log("promise-rm-boolean-return:" + (invalidRmPromise instanceof Promise));',
            'try { await invalidRmPromise; }',
            'catch (error) { console.log("promise-rm-boolean-reject:" + error.name); }',
            'const thrownReason = { source: "encoding-getter" };',
            'const getterPromise = fs.promises.readFile("/source.txt", { get encoding() { throw thrownReason; } });',
            'try { await getterPromise; }',
            'catch (error) { console.log("promise-getter-reason:" + (error === thrownReason)); }',
            'fs.mkdirSync("/existing");',
            'const appendArgs = await capture((callback) => appendFile("/source.txt", "!", callback));',
            'console.log("append-shape:" + (appendArgs.length === 1 && appendArgs[0] === null));',
            'const readOptions = { encoding: "utf8" };',
            'const readPromise = capture((callback) => readFile("/source.txt", readOptions, callback));',
            'readOptions.encoding = "latin1";',
            'const readArgs = await readPromise;',
            'console.log("read-shape:" + (readArgs.length === 2 && readArgs[0] === null && readArgs[1] === "source!"));',
            'const mkdirOptions = { recursive: true };',
            'const mkdirPromise = capture((callback) => mkdir("/existing/first/second", mkdirOptions, callback));',
            'mkdirOptions.recursive = false;',
            'const mkdirArgs = await mkdirPromise;',
            'console.log("mkdir-shape:" + (mkdirArgs.length === 2 && mkdirArgs[0] === null && mkdirArgs[1] === "/existing/first"));',
            'const readdirArgs = await capture((callback) => readdir("/existing/first", callback));',
            'console.log("readdir-shape:" + (readdirArgs.length === 2 && readdirArgs[0] === null && readdirArgs[1][0] === "second"));',
            'const statArgs = await capture((callback) => stat("/source.txt", callback));',
            'console.log("stat-shape:" + (statArgs.length === 2 && statArgs[0] === null && statArgs[1].isFile()));',
            'const copyArgs = await capture((callback) => copyFile("/source.txt", "/copy.txt", 0, callback));',
            'console.log("copy-shape:" + (copyArgs.length === 1 && copyArgs[0] === null));',
            'const renameArgs = await capture((callback) => rename("/copy.txt", "/renamed.txt", callback));',
            'console.log("rename-shape:" + (renameArgs.length === 1 && renameArgs[0] === null));',
            'const rmArgs = await capture((callback) => rm("/renamed.txt", callback));',
            'console.log("rm-shape:" + (rmArgs.length === 1 && rmArgs[0] === null));',
            'const errorArgs = await capture((callback) => readFile("/missing.txt", "utf8", callback));',
            'console.log("error-shape:" + (errorArgs.length === 1 && errorArgs[0] instanceof Error && errorArgs[0].code === "ENOENT"));',
            'const existsArgs = await capture((callback) => exists("/missing.txt", callback));',
            'console.log("exists-shape:" + (existsArgs.length === 1 && existsArgs[0] === false));',
            'const promiseMkdirPath = await fs.promises.mkdir("/promise/child", { recursive: true });',
            'console.log("promise-mkdir:" + promiseMkdirPath);',
          ].join("\n"),
        },
        { path: "/source.txt", text: "source" },
      ],
    }));

    const expected = [
      "lengths:3,4,4,3,3,1,3,3,4,2",
      "promise-lengths:2,3,3,2,2,1,2,2,3",
      "promises-exists:undefined",
      "missing-callback:TypeError",
      "unsupported-encoding:TypeError",
      "mkdir-boolean-options:TypeError",
      "rm-boolean-options:TypeError",
      "promise-validation-return:true",
      "promise-validation-reject:TypeError",
      "copy-mode-return:true",
      "unsupported-copy-mode:TypeError",
      "promise-rm-boolean-return:true",
      "promise-rm-boolean-reject:TypeError",
      "promise-getter-reason:true",
      "append-shape:true",
      "read-shape:true",
      "mkdir-shape:true",
      "readdir-shape:true",
      "stat-shape:true",
      "copy-shape:true",
      "rename-shape:true",
      "rm-shape:true",
      "error-shape:true",
      "exists-shape:true",
      "promise-mkdir:/promise",
    ].join("\n");
    for (const [label, extraArgs] of [
      ["interpreter", []],
      ["bytecode", ["--mode=bytecode"]],
    ] as const) {
      const proc = Bun.spawnSync(
        [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module", ...extraArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      const stdout = normalizeLineEndings(proc.stdout.toString()).trim();
      if (proc.exitCode !== 0)
        throw new Error(`SandboxRunner ${label} fs callback shape run should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
      if (stdout !== expected)
        throw new Error(`SandboxRunner ${label} fs callback shapes should be ${JSON.stringify(expected)}, got: ${stdout}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: deterministic nested engines use stable distinct streams...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "deterministic-seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import { runScript } from "goccia";',
            "const parentRandom = Math.random();",
            'const child = runScript("/child.js");',
            'console.log([parentRandom, child.result].join("|"));',
          ].join("\n"),
        },
        { path: "/child.js", text: "Math.random();" },
      ],
    }));

    const expected = "0.8833108082136426|0.6524484863740322";
    for (const mode of ["interpreted", "bytecode"] as const) {
      for (let run = 0; run < 2; run++) {
        const proc = Bun.spawnSync(
          [
            SANDBOXRUNNER,
            "/main.js",
            `--seed-config=${seed}`,
            "--source-type=module",
            "--deterministic",
            `--mode=${mode}`,
          ],
          { stdout: "pipe", stderr: "pipe" },
        );
        const output = proc.stdout.toString().trim();
        if (proc.exitCode !== 0 || output !== expected)
          throw new Error(
            `SandboxRunner deterministic ${mode} run ${run + 1} expected ${expected}, got ${output}${proc.stderr.toString()}`,
          );
      }
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: inline seeds, fs, $, runScript, and diffs...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    const diff = join(tmp, "diff.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import fs from "fs";',
            'import { $, runScript } from "goccia";',
            'await fs.promises.writeFile("/hello.txt", "hello");',
            'const shellOut = await $`cat /hello.txt`.text();',
            'const spaced = "hello world";',
            'const interpolated = await $`echo ${spaced}`.text();',
            'const quietText = await $`echo hidden`.quiet().text();',
            'const quietRun = await $`echo hidden`.quiet().run();',
            'const child = runScript("/child.js");',
            'const objectChild = runScript("/object-child.js");',
            'const shellChild = await $`goccia /child.js`.text();',
            'const stat = fs.statSync("/hello.txt");',
            'console.log(shellOut.trim());',
            'console.log(interpolated.trim());',
            'console.log("quiet-text:" + (quietText === ""));',
            'console.log("quiet-run:" + (quietRun.stdout === "" && quietRun.stderr === "" && quietRun.ok));',
            'console.log(child.stdout.trim());',
            'console.log(objectChild.result.value);',
            'console.log(objectChild.result.items[1]);',
            'console.log(objectChild.result.nested.ok);',
            'console.log("mtime-ms:" + (stat.mtimeMs > 1000000000000));',
            'console.log(shellChild.trim());',
            'console.log(fs.readFileSync("/child.out", "utf8"));',
            '"sandbox-ok";',
          ].join("\n"),
        },
        {
          path: "/child.js",
          text: [
            'import fs from "fs";',
            'fs.writeFileSync("/child.out", "child-write");',
            'console.log("child");',
            '"child-result";',
          ].join("\n"),
        },
        {
          path: "/object-child.js",
          text: '({ value: 42, items: ["zero", "one"], nested: { ok: true } });',
        },
      ],
    }));

    const proc = Bun.spawnSync(
      [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module", "--diff", `--diff-output=${diff}`],
      { stdout: "pipe", stderr: "pipe" },
    );
    const stdout = normalizeLineEndings(proc.stdout.toString());
    const stderr = proc.stderr.toString();
    if (proc.exitCode !== 0)
      throw new Error(`SandboxRunner interpreter should exit 0, got ${proc.exitCode}: ${stderr}`);
    for (const expected of [
      "hello",
      "hello world",
      "quiet-text:true",
      "quiet-run:true",
      "child",
      "42",
      "one",
      "true",
      "mtime-ms:true",
      "child\nchild",
      "child-write",
    ]) {
      if (!stdout.includes(expected))
        throw new Error(`SandboxRunner interpreter stdout should include ${JSON.stringify(expected)}, got: ${stdout}`);
    }
    const defaultDiff = JSON.parse(readFileSync(diff, "utf-8"));
    const changes = defaultDiff.changes;
    if ("metadataChanges" in defaultDiff)
      throw new Error(`SandboxRunner default diff should omit metadataChanges, got ${JSON.stringify(defaultDiff)}`);
    if (!changes.some((c: any) => c.kind === "create" && c.path === "/hello.txt"))
      throw new Error(`SandboxRunner diff should include /hello.txt create, got ${JSON.stringify(changes)}`);
    if (!changes.some((c: any) => c.kind === "create" && c.path === "/child.out"))
      throw new Error(`SandboxRunner diff should include /child.out create, got ${JSON.stringify(changes)}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: fs Stats expose realm-owned lazy Date metadata in every execution mode...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import fs from "fs";',
            'const intrinsicStat = fs.statSync("/tracked.txt");',
            'globalThis.Date = class ReplacementDate { constructor() { this.replacement = true; } };',
            'const intrinsicMtime = intrinsicStat.mtime;',
            'const intrinsicDateValid = typeof intrinsicMtime.getTime === "function" && !Object.hasOwn(intrinsicMtime, "replacement");',
            'globalThis.Date = Object.getPrototypeOf(intrinsicMtime).constructor;',
            'const syncStat = fs.statSync("/tracked.txt");',
            'const promiseStat = await fs.promises.stat("/tracked.txt");',
            'const checks = (stat) => {',
            'const firstAtime = stat.atime;',
            'const secondAtime = stat.atime;',
            'return [',
            '  stat.atime instanceof Date,',
            '  stat.mtime instanceof Date,',
            '  stat.ctime instanceof Date,',
            '  stat.birthtime instanceof Date,',
            '  stat.atime.getTime() === Math.trunc(stat.atimeMs + 0.5),',
            '  stat.mtime.getTime() === Math.trunc(stat.mtimeMs + 0.5),',
            '  stat.ctime.getTime() === Math.trunc(stat.ctimeMs + 0.5),',
            '  stat.birthtime.getTime() === Math.trunc(stat.birthtimeMs + 0.5),',
            '  typeof stat.atimeMs === "number",',
            '  typeof stat.mtimeMs === "number",',
            '  typeof stat.ctimeMs === "number",',
            '  typeof stat.birthtimeMs === "number",',
            '  stat.isFile(),',
            '  !stat.isDirectory(),',
            '  !stat.isSymbolicLink(),',
            '  firstAtime !== secondAtime,',
            '  !Object.hasOwn(stat, "atime"),',
            '  !Object.hasOwn(stat, "isFile"),',
            '];',
            '};',
            'const valid = (stat) => checks(stat).every(Boolean);',
            'if (!valid(syncStat)) console.log("sync-checks:" + checks(syncStat).join(","));',
            'if (!valid(promiseStat)) console.log("promise-checks:" + checks(promiseStat).join(","));',
            'console.log("sync-stats:" + valid(syncStat));',
            'console.log("promise-stats:" + valid(promiseStat));',
            'console.log("shared-stats-prototype:" + (Object.getPrototypeOf(syncStat) === Object.getPrototypeOf(promiseStat)));',
            'console.log("intrinsic-stats-date:" + intrinsicDateValid);',
          ].join("\n"),
        },
        { path: "/tracked.txt", text: "tracked" },
      ],
    }));

    for (const [label, extraArgs] of [
      ["interpreter", []],
      ["bytecode", ["--mode=bytecode"]],
    ] as const) {
      const proc = Bun.spawnSync(
        [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module", ...extraArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      const stdout = normalizeLineEndings(proc.stdout.toString());
      if (proc.exitCode !== 0)
        throw new Error(`SandboxRunner ${label} Stats run should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
      for (const expected of [
        "sync-stats:true",
        "promise-stats:true",
        "shared-stats-prototype:true",
        "intrinsic-stats-date:true",
      ]) {
        if (!containsLine(stdout, expected))
          throw new Error(`SandboxRunner ${label} Stats stdout should include ${expected}, got: ${stdout}`);
      }
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: metadata diffing is opt-in and separate from content changes...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    const diff = join(tmp, "diff.json");
    const unifiedDiff = join(tmp, "diff.patch");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import fs from "fs";',
            'for (const i of Array.from({ length: 10000 }, (_, index) => index)) { Math.sqrt(i); }',
            'const text = fs.readFileSync("/tracked.txt", "utf8");',
            'fs.writeFileSync("/tracked.txt", text);',
            'fs.mkdirSync("/created");',
          ].join("\n"),
        },
        { path: "/tracked.txt", text: "unchanged" },
      ],
    }));

    const proc = Bun.spawnSync(
      [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module", "--diff-metadata", `--diff-output=${diff}`],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (proc.exitCode !== 0)
      throw new Error(`SandboxRunner metadata diff should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
    const parsed = JSON.parse(readFileSync(diff, "utf-8"));
    if (parsed.metadataChanges.some((change: any) => change.path === "/"))
      throw new Error(`Metadata diff must not expose the implicit root, got ${JSON.stringify(parsed.metadataChanges)}`);
    if (parsed.changes.some((change: any) => change.path === "/tracked.txt"))
      throw new Error(`Timestamp-only writes must not become content modifications, got ${JSON.stringify(parsed.changes)}`);
    const tracked = parsed.metadataChanges.find((change: any) => change.path === "/tracked.txt");
    if (!tracked)
      throw new Error(`Metadata diff should include /tracked.txt, got ${JSON.stringify(parsed.metadataChanges)}`);
    const fields = Object.keys(tracked.changes).sort();
    if (JSON.stringify(fields) !== JSON.stringify(["atimeMs", "ctimeMs", "mtimeMs"]))
      throw new Error(`Metadata diff should contain only changed timestamp fields, got ${JSON.stringify(tracked)}`);
    if ("size" in tracked.changes || "type" in tracked.changes || "birthtimeMs" in tracked.changes)
      throw new Error(`Metadata diff should not duplicate size/type or change birthtime, got ${JSON.stringify(tracked)}`);

    const unifiedProc = Bun.spawnSync(
      [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module", "--diff-metadata", "--diff-format=unified", `--diff-output=${unifiedDiff}`],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (unifiedProc.exitCode !== 0)
      throw new Error(`SandboxRunner unified metadata diff should exit 0, got ${unifiedProc.exitCode}: ${unifiedProc.stderr.toString()}`);
    const unified = readFileSync(unifiedDiff, "utf-8");
    if (!unified.includes("@@ sandbox metadata changed /tracked.txt @@") ||
        unified.includes("@@ sandbox file changed @@") ||
        unified.includes("@@ sandbox metadata changed / @@"))
      throw new Error(`Unified metadata diff should keep timestamp-only changes separate, got: ${unified}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: fs errors are Node-shaped in every execution mode...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import fs from "fs";',
            'const printError = (label, error) => console.log([',
            '  label,',
            '  error instanceof Error,',
            '  Error.isError(error),',
            '  error.name,',
            '  error.code,',
            '  typeof error.errno === "number" && error.errno < 0,',
            '  error.syscall,',
            '  error.path,',
            '  typeof error.dest,',
            '  error.message,',
            '].join("|"));',
            'try { fs.readFileSync("/missing.txt", "utf8"); }',
            'catch (error) { printError("sync", error); }',
            'try { await fs.promises.readFile("/missing.txt", "utf8"); }',
            'catch (error) { printError("promise", error); }',
            'try { fs.renameSync("/missing.txt", "/destination.txt"); }',
            'catch (error) { printError("rename", error); }',
          ].join("\n"),
        },
      ],
    }));

    const expected = [
      "sync|true|true|Error|ENOENT|true|readFile|/missing.txt|undefined|ENOENT: no such file or directory, readFile '/missing.txt'",
      "promise|true|true|Error|ENOENT|true|readFile|/missing.txt|undefined|ENOENT: no such file or directory, readFile '/missing.txt'",
      "rename|true|true|Error|ENOENT|true|rename|/missing.txt|string|ENOENT: no such file or directory, rename '/missing.txt' -> '/destination.txt'",
    ];
    for (const [label, extraArgs] of [
      ["interpreter", []],
      ["bytecode", ["--mode=bytecode"]],
    ] as const) {
      const proc = Bun.spawnSync(
        [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module", ...extraArgs],
        { stdout: "pipe", stderr: "pipe" },
      );
      const stdout = normalizeLineEndings(proc.stdout.toString()).trim();
      if (proc.exitCode !== 0)
        throw new Error(`SandboxRunner ${label} fs error run should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
      const actual = stdout.split("\n");
      if (JSON.stringify(actual) !== JSON.stringify(expected))
        throw new Error(`SandboxRunner ${label} fs errors should be Node-shaped, got: ${stdout}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: aliases and import maps resolve sandbox module paths...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    const importMap = join(tmp, "import-map.json");
    writeFileSync(importMap, JSON.stringify({ imports: { "#lib/": "/lib/", "#rel/": "./lib/" } }));
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/alias-main.js",
          text: [
            'import { label } from "@lib/alias.js";',
            'console.log(label);',
          ].join("\n"),
        },
        {
          path: "/map-main.js",
          text: [
            'import { label } from "#lib/map.js";',
            'import { relativeLabel } from "#rel/relative.js";',
            'console.log(label);',
            'console.log(relativeLabel);',
          ].join("\n"),
        },
        { path: "/lib/alias.js", text: 'export const label = "alias-ok";' },
        { path: "/lib/map.js", text: 'export const label = "map-ok";' },
        { path: "/lib/relative.js", text: 'export const relativeLabel = "relative-ok";' },
      ],
    }));

    const aliasProc = Bun.spawnSync(
      [SANDBOXRUNNER, "/alias-main.js", `--seed-config=${seed}`, "--source-type=module", "--alias", "@lib/=/lib/"],
      { stdout: "pipe", stderr: "pipe" },
    );
    const aliasStdout = normalizeLineEndings(aliasProc.stdout.toString());
    if (aliasProc.exitCode !== 0)
      throw new Error(`SandboxRunner alias import should exit 0, got ${aliasProc.exitCode}: ${aliasProc.stderr.toString()}`);
    if (!containsLine(aliasStdout, "alias-ok"))
      throw new Error(`SandboxRunner alias import should print alias-ok, got: ${aliasStdout}`);

    const importMapProc = Bun.spawnSync(
      [SANDBOXRUNNER, "/map-main.js", `--seed-config=${seed}`, "--source-type=module", `--import-map=${importMap}`],
      { stdout: "pipe", stderr: "pipe" },
    );
    const importMapStdout = normalizeLineEndings(importMapProc.stdout.toString());
    if (importMapProc.exitCode !== 0)
      throw new Error(`SandboxRunner import map should exit 0, got ${importMapProc.exitCode}: ${importMapProc.stderr.toString()}`);
    if (!containsLine(importMapStdout, "map-ok"))
      throw new Error(`SandboxRunner import map should print map-ok, got: ${importMapStdout}`);
    if (!containsLine(importMapStdout, "relative-ok"))
      throw new Error(`SandboxRunner import map should print relative-ok, got: ${importMapStdout}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: Windows-style sandbox paths normalize to virtual paths...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: String.raw`\main.js`,
          text: [
            'import fs from "fs";',
            'import { $, runScript } from "goccia";',
            'fs.writeFileSync("\\\\hello.txt", "hello");',
            'console.log(fs.readFileSync("/hello.txt", "utf8"));',
            'console.log((await $("cat \'\\\\hello.txt\'").text()).trim());',
            'const child = runScript("\\\\child.js", {',
            '  sandbox: true,',
            '  seed: ["\\\\child.js", { from: "\\\\hello.txt", to: "\\\\copied\\\\" }],',
            '  diff: true,',
            '});',
            'console.log(child.stdout.trim());',
            'const shellChild = await $("goccia \'\\\\child.js\'").text();',
            'console.log(shellChild.trim());',
          ].join("\n"),
        },
        {
          path: String.raw`\child.js`,
          text: [
            'import fs from "fs";',
            'if (fs.existsSync("\\\\copied\\\\hello.txt")) console.log(fs.readFileSync("\\\\copied\\\\hello.txt", "utf8"));',
            'else console.log("child-shared");',
          ].join("\n"),
        },
      ],
    }));

    const proc = Bun.spawnSync(
      [SANDBOXRUNNER, String.raw`\main.js`, `--seed-config=${seed}`, "--source-type=module"],
      { stdout: "pipe", stderr: "pipe" },
    );
    const stdout = normalizeLineEndings(proc.stdout.toString());
    if (proc.exitCode !== 0)
      throw new Error(`SandboxRunner Windows-style paths should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
    for (const expected of ["hello", "child-shared"]) {
      if (!containsLine(stdout, expected))
        throw new Error(`SandboxRunner Windows-style paths should include ${JSON.stringify(expected)}, got: ${stdout}`);
    }
    const helloCount = stdout.split("\n").filter((line) => line === "hello").length;
    if (helloCount !== 3)
      throw new Error(`SandboxRunner Windows-style paths should print hello three times, got ${helloCount} in: ${stdout}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: seed config rejects null source values...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        { path: "/bad.txt", text: null },
        { path: "/main.js", text: "1;" },
      ],
    }));

    const proc = Bun.spawnSync(
      [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`],
      { stdout: "pipe", stderr: "pipe" },
    );
    const output = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode === 0)
      throw new Error("SandboxRunner null seed text should fail");
    if (!output.includes('seed config entry requires "text"'))
      throw new Error(`SandboxRunner null seed text should report the required text field, got: ${output}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: unified diff includes deleted seeded files...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    const diff = join(tmp, "diff.patch");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import fs from "fs";',
            'fs.rmSync("/remove.txt");',
          ].join("\n"),
        },
        { path: "/remove.txt", text: "gone" },
      ],
    }));

    const proc = Bun.spawnSync(
      [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module", "--diff-format=unified", `--diff-output=${diff}`],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (proc.exitCode !== 0)
      throw new Error(`SandboxRunner unified delete diff should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
    const diffText = readFileSync(diff, "utf-8");
    if (!diffText.includes("--- /remove.txt") || !diffText.includes("@@ sandbox file deleted @@") || !diffText.includes("-gone"))
      throw new Error(`SandboxRunner unified delete diff should include deleted file content, got: ${diffText}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: bytecode uses the same sandbox runtime modules...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    const diff = join(tmp, "diff.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import fs from "fs";',
            'import { $, runScript } from "goccia";',
            'await fs.promises.writeFile("/byte.txt", "bytecode");',
            'console.log((await $`cat /byte.txt`.text()).trim());',
            'const child = runScript("/byte-child.js", { sandbox: true, seed: ["/byte-child.js"], diff: true });',
            'console.log(child.stdout.trim());',
            'console.log(child.diff.includes(\'"path": "/byte-child.txt"\'));',
            'console.log(fs.existsSync("/byte-child.txt"));',
          ].join("\n"),
        },
        {
          path: "/byte-child.js",
          text: [
            'import fs from "fs";',
            'fs.writeFileSync("/byte-child.txt", "child-bytecode");',
            'console.log("byte-child");',
          ].join("\n"),
        },
      ],
    }));

    const proc = Bun.spawnSync(
      [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module", "--mode=bytecode", `--diff-output=${diff}`],
      { stdout: "pipe", stderr: "pipe" },
    );
    const stdout = normalizeLineEndings(proc.stdout.toString());
    if (proc.exitCode !== 0)
      throw new Error(`SandboxRunner bytecode should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (!containsLine(stdout, "bytecode"))
      throw new Error(`SandboxRunner bytecode stdout should include bytecode, got: ${stdout}`);
    if (!containsLine(stdout, "byte-child"))
      throw new Error(`SandboxRunner bytecode nested stdout should include byte-child, got: ${stdout}`);
    if (!containsLine(stdout, "true") || !containsLine(stdout, "false"))
      throw new Error(`SandboxRunner bytecode nested diff/isolation booleans missing, got: ${stdout}`);
    const changes = JSON.parse(readFileSync(diff, "utf-8")).changes;
    if (!changes.some((c: any) => c.kind === "create" && c.path === "/byte.txt"))
      throw new Error(`SandboxRunner bytecode diff should include /byte.txt create, got ${JSON.stringify(changes)}`);
    if (changes.some((c: any) => c.path === "/byte-child.txt"))
      throw new Error(`SandboxRunner bytecode parent diff should not include nested child writes, got ${JSON.stringify(changes)}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: nested sandbox execution seeds from parent VFS without leaking writes...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import fs from "fs";',
            'import { $, runScript } from "goccia";',
            'const child = runScript("/child.js", {',
            '  sandbox: true,',
            '  seed: [',
            '    "/child.js",',
            '    "/parent.txt",',
            '    { from: "/parent.txt", to: "/out/" },',
            '    { path: "/inline.txt", text: "inline-child" },',
            '    { path: "/bin.dat", base64: "BAUG" },',
            '  ],',
            '  diff: true,',
            '});',
            'const noWrite = runScript("/readonly.js", { sandbox: true, seed: ["/readonly.js"], diff: true, diffFormat: "unified" });',
            'const metadataOnly = runScript("/metadata.js", { sandbox: true, seed: ["/metadata.js", "/metadata.txt"], diffMetadata: true });',
            'console.log(child.stdout.trim());',
            'console.log(child.diff.includes(\'"path": "/child-only.txt"\'));',
            'console.log(noWrite.diff === "");',
            'console.log(JSON.parse(metadataOnly.diff).changes.length === 0);',
            'console.log(JSON.parse(metadataOnly.diff).metadataChanges.some((change) => change.path === "/metadata.txt" && "ctimeMs" in change.changes && "mtimeMs" in change.changes));',
            'console.log(fs.existsSync("/child-only.txt"));',
            'console.log(fs.readFileSync("/parent.txt", "utf8"));',
            'const shellChild = await $`goccia --sandbox --seed /child.js --seed /parent.txt --seed /parent.txt=/shell-out/ --diff-metadata /child.js`.text();',
            'console.log(shellChild.includes("parent-seed"));',
            'console.log(shellChild.includes("shell-out:parent-seed"));',
            'console.log(shellChild.includes(\'"path": "/child-only.txt"\'));',
            'console.log(shellChild.includes(\'"metadataChanges"\'));',
            'console.log(fs.existsSync("/child-only.txt"));',
          ].join("\n"),
        },
        {
          path: "/child.js",
          text: [
            'import fs from "fs";',
            'console.log(fs.readFileSync("/parent.txt", "utf8"));',
            'if (fs.existsSync("/inline.txt")) console.log(fs.readFileSync("/inline.txt", "utf8"));',
            'if (fs.existsSync("/bin.dat")) console.log(fs.readFileSync("/bin.dat").length);',
            'if (fs.existsSync("/out/parent.txt")) console.log("out:" + fs.readFileSync("/out/parent.txt", "utf8"));',
            'if (fs.existsSync("/shell-out/parent.txt")) console.log("shell-out:" + fs.readFileSync("/shell-out/parent.txt", "utf8"));',
            'fs.writeFileSync("/parent.txt", "child-mutated");',
            'fs.writeFileSync("/child-only.txt", "secret");',
          ].join("\n"),
        },
        { path: "/readonly.js", text: "1;" },
        {
          path: "/metadata.js",
          text: [
            'import fs from "fs";',
            'for (const i of Array.from({ length: 10000 }, (_, index) => index)) { Math.sqrt(i); }',
            'const text = fs.readFileSync("/metadata.txt", "utf8");',
            'fs.writeFileSync("/metadata.txt", text);',
          ].join("\n"),
        },
        { path: "/metadata.txt", text: "metadata" },
        { path: "/parent.txt", text: "parent-seed" },
      ],
    }));

    const proc = Bun.spawnSync(
      [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module"],
      { stdout: "pipe", stderr: "pipe" },
    );
    const stdout = normalizeLineEndings(proc.stdout.toString());
    if (proc.exitCode !== 0)
      throw new Error(`SandboxRunner nested sandbox should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
    for (const expected of [
      "parent-seed",
      "inline-child",
      "out:parent-seed",
      "3",
      "true",
      "false",
    ]) {
      if (!containsLine(stdout, expected))
        throw new Error(`SandboxRunner nested sandbox stdout should include line ${JSON.stringify(expected)}, got: ${stdout}`);
    }
    if (!stdout.includes("parent-seed\ninline-child\n3\nout:parent-seed"))
      throw new Error(`runScript child stdout should include inline seeded files, got: ${stdout}`);
    const falseCount = stdout.split("\n").filter((line) => line === "false").length;
    if (falseCount !== 2)
      throw new Error(`child writes should stay out of parent VFS twice, got ${falseCount} false lines in: ${stdout}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: seed config imports host paths relative to the config file...", async () => {
  const tmp = makeTmp();
  try {
    const project = join(tmp, "project");
    mkdirSync(project, { recursive: true });
    writeFileSync(join(project, "data.txt"), "from-host");
    writeFileSync(join(tmp, "target-file.txt"), "from-file-target");
    writeFileSync(join(tmp, "existing-dir-file.txt"), "from-existing-dir");
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        { from: "./project", to: "/" },
        { path: "/existing-dir/.keep", text: "" },
        { from: "./target-file.txt", to: "/target-dir/" },
        { from: "./existing-dir-file.txt", to: "/existing-dir" },
        { path: "/bin.dat", base64: "AQID" },
        {
          path: "/main.js",
          text: [
            'import fs from "fs";',
            'console.log(fs.readFileSync("/data.txt", "utf8"));',
            'console.log(fs.readFileSync("/target-dir/target-file.txt", "utf8"));',
            'console.log(fs.readFileSync("/existing-dir/existing-dir-file.txt", "utf8"));',
            'console.log(fs.readFileSync("/bin.dat").length);',
          ].join("\n"),
        },
      ],
    }));

    const proc = Bun.spawnSync(
      [resolve(SANDBOXRUNNER), "/main.js", `--seed-config=${seed}`, "--source-type=module"],
      { stdout: "pipe", stderr: "pipe", cwd: "/" },
    );
    const stdout = normalizeLineEndings(proc.stdout.toString());
    if (proc.exitCode !== 0)
      throw new Error(`SandboxRunner host seed should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (!containsLine(stdout, "from-host"))
      throw new Error(`SandboxRunner host seed stdout should include imported host text, got: ${stdout}`);
    if (!containsLine(stdout, "from-file-target"))
      throw new Error(`SandboxRunner host seed stdout should include file copied under trailing slash target, got: ${stdout}`);
    if (!containsLine(stdout, "from-existing-dir"))
      throw new Error(`SandboxRunner host seed stdout should include file copied under existing target directory, got: ${stdout}`);
    if (!containsLine(stdout, "3"))
      throw new Error(`SandboxRunner host seed stdout should include base64 byte length, got: ${stdout}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: --audit-log reports root escapes without changing clamped access...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "audit-seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/main.js",
          text: [
            'import fs from "fs";',
            'console.log(fs.readFileSync("../../secret.txt", "utf8"));',
          ].join("\n"),
        },
        { path: "/secret.txt", text: "inside-jail" },
      ],
    }));

    for (const mode of ["interpreted", "bytecode"] as const) {
      const audit = join(tmp, `sandbox-audit-${mode}.jsonl`);
      const proc = Bun.spawnSync(
        [
          SANDBOXRUNNER,
          "/main.js",
          `--seed-config=${seed}`,
          "--source-type=module",
          `--mode=${mode}`,
          `--audit-log=${audit}`,
        ],
        { stdout: "pipe", stderr: "pipe" },
      );
      if (proc.exitCode !== 0)
        throw new Error(`Sandbox audit ${mode} exited ${proc.exitCode}: ${proc.stderr.toString()}`);
      if (!containsLine(proc.stdout.toString(), "inside-jail"))
        throw new Error(`Sandbox audit ${mode} changed clamped access: ${proc.stdout.toString()}`);
      const events = readJsonLines(audit);
      if (events.length !== 1 ||
          events[0].kind !== "sandbox.fs.path" ||
          events[0].decision !== "deny" ||
          events[0].subject !== "../../secret.txt" ||
          events[0].source?.file !== "/main.js" ||
          events[0].source?.line !== 2)
        throw new Error(`Sandbox audit ${mode} mismatch: ${JSON.stringify(events)}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: seed directory rejects nested host symlink (no leak)...", async () => {
  const tmp = makeTmp();
  try {
    if (process.platform !== "win32") {
      const seedDir = join(tmp, "seedDir");
      mkdirSync(seedDir, { recursive: true });
      writeFileSync(join(tmp, "outside.txt"), "outside-secret");
      symlinkSync("../outside.txt", join(seedDir, "leak.txt"));
      const mainJs = join(tmp, "main.js");
      writeFileSync(mainJs, [
        'import fs from "fs";',
        'console.log(fs.readFileSync("/leak.txt", "utf8"));',
      ].join("\n"));

      const proc = Bun.spawnSync(
        [SANDBOXRUNNER, "/main.js", `--seed=${seedDir}=/`, `--seed=${mainJs}=/`, "--source-type=module"],
        { stdout: "pipe", stderr: "pipe" },
      );
      const output = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode === 0)
        throw new Error(`SandboxRunner nested symlink seed should fail, exited 0: ${output}`);
      if (!output.includes("is a symlink (not supported)"))
        throw new Error(`SandboxRunner nested symlink seed should report the symlink rejection, got: ${output}`);
      if (output.includes("outside-secret"))
        throw new Error(`SandboxRunner nested symlink seed leaked host contents, got: ${output}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: direct seed argument rejects host symlink (no leak)...", async () => {
  const tmp = makeTmp();
  try {
    if (process.platform !== "win32") {
      writeFileSync(join(tmp, "outside.txt"), "outside-secret");
      const link = join(tmp, "link.txt");
      symlinkSync("outside.txt", link);
      const mainJs = join(tmp, "main.js");
      writeFileSync(mainJs, [
        'import fs from "fs";',
        'console.log(fs.readFileSync("/leak.txt", "utf8"));',
      ].join("\n"));

      const proc = Bun.spawnSync(
        [SANDBOXRUNNER, "/main.js", `--seed=${link}=/leak.txt`, `--seed=${mainJs}=/`, "--source-type=module"],
        { stdout: "pipe", stderr: "pipe" },
      );
      const output = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode === 0)
        throw new Error(`SandboxRunner direct symlink seed should fail, exited 0: ${output}`);
      if (!output.includes("is a symlink (not supported)"))
        throw new Error(`SandboxRunner direct symlink seed should report the symlink rejection, got: ${output}`);
      if (output.includes("outside-secret"))
        throw new Error(`SandboxRunner direct symlink seed leaked host contents, got: ${output}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: seed-config from directory rejects nested host symlink (no leak)...", async () => {
  const tmp = makeTmp();
  try {
    if (process.platform !== "win32") {
      const seedDir = join(tmp, "seedDir");
      mkdirSync(seedDir, { recursive: true });
      writeFileSync(join(tmp, "outside.txt"), "outside-secret");
      symlinkSync("../outside.txt", join(seedDir, "leak.txt"));
      const seed = join(tmp, "seed.json");
      writeFileSync(seed, JSON.stringify({
        files: [
          { from: "./seedDir", to: "/" },
          {
            path: "/main.js",
            text: [
              'import fs from "fs";',
              'console.log(fs.readFileSync("/leak.txt", "utf8"));',
            ].join("\n"),
          },
        ],
      }));

      const proc = Bun.spawnSync(
        [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module"],
        { stdout: "pipe", stderr: "pipe" },
      );
      const output = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode === 0)
        throw new Error(`SandboxRunner seed-config symlink should fail, exited 0: ${output}`);
      if (!output.includes("is a symlink (not supported)"))
        throw new Error(`SandboxRunner seed-config symlink should report the symlink rejection, got: ${output}`);
      if (output.includes("outside-secret"))
        throw new Error(`SandboxRunner seed-config symlink leaked host contents, got: ${output}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: trailing slash on a symlinked-directory seed is still rejected (no leak)...", async () => {
  const tmp = makeTmp();
  try {
    if (process.platform !== "win32") {
      const outsideDir = join(tmp, "outsideDir");
      mkdirSync(outsideDir, { recursive: true });
      writeFileSync(join(outsideDir, "secret.txt"), "outside-secret");
      const linkDir = join(tmp, "linkDir");
      symlinkSync("./outsideDir", linkDir);
      const mainJs = join(tmp, "main.js");
      writeFileSync(mainJs, [
        'import fs from "fs";',
        'console.log(fs.readFileSync("/secret.txt", "utf8"));',
      ].join("\n"));

      // A trailing slash must not let POSIX lstat() follow the symlinked leaf.
      const proc = Bun.spawnSync(
        [SANDBOXRUNNER, "/main.js", `--seed=${linkDir}/=/`, `--seed=${mainJs}=/`, "--source-type=module"],
        { stdout: "pipe", stderr: "pipe" },
      );
      const output = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode === 0)
        throw new Error(`SandboxRunner trailing-slash symlink seed should fail, exited 0: ${output}`);
      if (!output.includes("is a symlink (not supported)"))
        throw new Error(`SandboxRunner trailing-slash symlink seed should report the symlink rejection, got: ${output}`);
      if (output.includes("outside-secret"))
        throw new Error(`SandboxRunner trailing-slash symlink seed leaked host contents, got: ${output}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: Windows directory junction seed is rejected (no leak)...", async () => {
  const tmp = makeTmp();
  try {
    // Windows-only: file symlinks need elevation on CI runners, but a directory
    // junction is a reparse point that needs none, so it exercises the Windows
    // branch of the guard (FileGetAttr + faSymLink) that the win32-skipped tests
    // above cannot.
    if (process.platform === "win32") {
      const outsideDir = join(tmp, "outsideDir");
      mkdirSync(outsideDir, { recursive: true });
      writeFileSync(join(outsideDir, "secret.txt"), "outside-secret");
      const junction = join(tmp, "junction");
      symlinkSync(outsideDir, junction, "junction");
      const mainJs = join(tmp, "main.js");
      writeFileSync(mainJs, [
        'import fs from "fs";',
        'console.log(fs.readFileSync("/secret.txt", "utf8"));',
      ].join("\n"));

      const proc = Bun.spawnSync(
        [SANDBOXRUNNER, "/main.js", `--seed=${junction}=/`, `--seed=${mainJs}=/`, "--source-type=module"],
        { stdout: "pipe", stderr: "pipe" },
      );
      const output = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode === 0)
        throw new Error(`SandboxRunner junction seed should fail, exited 0: ${output}`);
      if (!output.includes("is a symlink (not supported)"))
        throw new Error(`SandboxRunner junction seed should report the symlink rejection, got: ${output}`);
      if (output.includes("outside-secret"))
        throw new Error(`SandboxRunner junction seed leaked host contents, got: ${output}`);
    }
  } finally {
    clean(tmp);
  }
});

// ============================================================================
// --allowed-host option
// ============================================================================

await section("Loader: --allowed-host blocks unlisted host...", async () => {
  const tmp = makeTmp();
  try {
    const audit = join(tmp, "blocked-fetch-audit.jsonl");
    const res = await $`echo 'fetch("http://user:password@blocked.test/private?token=secret");' | ${LOADER} --allowed-host=example.com --audit-log=${audit} 2>&1`.nothrow();
    if (res.exitCode === 0) throw new Error("Fetch to unlisted host should fail");
    if (!res.text().includes("blocked.test")) throw new Error(`Error should mention blocked host, got: ${res.text()}`);
    const events = readJsonLines(audit);
    if (events.length !== 1 ||
        events[0].kind !== "fetch.host" ||
        events[0].decision !== "deny" ||
        events[0].subject !== "blocked.test" ||
        JSON.stringify(events).includes("password") ||
        JSON.stringify(events).includes("token=secret"))
      throw new Error(`Blocked fetch audit event mismatch: ${JSON.stringify(events)}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: no --allowed-host blocks all fetch...", async () => {
  const res = await $`echo 'fetch("http://example.com");' | ${LOADER} 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Fetch without --allowed-host should fail");
  if (!res.text().includes("allowed hosts")) throw new Error(`Error should mention allowed hosts, got: ${res.text()}`);
});

await section("Loader: --allowed-host multiple hosts...", async () => {
  // Both hosts in the list; blocked.test is not
  const res = await $`echo 'fetch("http://blocked.test");' | ${LOADER} --allowed-host=example.com --allowed-host=other.com 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Fetch to unlisted host should fail with multiple --allowed-host");
  if (!res.text().includes("blocked.test")) throw new Error(`Error should mention blocked host, got: ${res.text()}`);
});

console.log("Loader: local fetch smoke with --allowed-host...");
await withFetchTestServer(async (baseUrl) => {
  const tmp = makeTmp();
  const audit = join(tmp, "fetch-audit.jsonl");
  try {
    const { exitCode, json, stderr } = await runLoaderJsonAsync(
      `const response = await fetch("${baseUrl}/", { method: "HEAD" });\nresponse.status;\n`,
      ["--compat-asi", "--allowed-host=127.0.0.1", `--audit-log=${audit}`],
      { timeout: 10_000 },
    );
    if (exitCode !== 0) throw new Error(`Local fetch should exit 0, got ${exitCode}: ${stderr}`);
    if (json.ok !== true) throw new Error(`Local fetch JSON ok should be true, got ${json.ok}`);
    if (json.files?.[0]?.result !== 200) throw new Error(`Local fetch status should be 200, got ${json.files?.[0]?.result}`);
    const events = readJsonLines(audit);
    if (events.length !== 2 ||
        events[0].kind !== "fetch.host" ||
        events[0].decision !== "allow" ||
        events[0].subject !== "127.0.0.1" ||
        events[1].kind !== "fetch.dispatch" ||
        events[1].decision !== "allow")
      throw new Error(`Local fetch audit events mismatch: ${JSON.stringify(events)}`);
  } finally {
    clean(tmp);
  }
});

// ============================================================================
// --multifile (all runners)
// ============================================================================

await section("Loader: --multifile splits a single file into N section results...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "multifile-loader.js");
    writeFileSync(
      file,
      'console.log("section A:", 1 + 1);\n' +
      "---\n" +
      'console.log("section B:", 2 + 2);\n' +
      "---\n" +
      'console.log("section C:", 3 + 3);\n',
    );
    const proc = Bun.spawnSync([LOADER, "--multifile", "--output=json", file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Loader --multifile should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
    const json = JSON.parse(proc.stdout.toString());
    if (!Array.isArray(json.files) || json.files.length !== 3)
      throw new Error(`Loader --multifile should produce 3 file entries, got ${json.files?.length}`);
    for (let i = 0; i < 3; i++) {
      const expected = `${file.replace(/\.js$/, "")}[part${i + 1}].js`;
      if (json.files[i].fileName !== expected)
        throw new Error(`Loader --multifile file ${i} fileName mismatch: expected ${expected}, got ${json.files[i].fileName}`);
      if (json.files[i].ok !== true)
        throw new Error(`Loader --multifile section ${i + 1} should succeed`);
    }
    if (!json.stdout.includes("section A: 2") || !json.stdout.includes("section C: 6"))
      throw new Error(`Loader --multifile stdout missing section output: ${json.stdout}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: --multifile on stdin produces <stdin>[partN] entries...", async () => {
  const { exitCode, json } = runLoaderJson(
    "console.log('a');\n---\nconsole.log('b');\n---\nconsole.log('c');\n",
    ["--multifile"],
  );
  if (exitCode !== 0) throw new Error(`Loader --multifile stdin should exit 0, got ${exitCode}`);
  if (json.files?.length !== 3) throw new Error(`Loader --multifile stdin should produce 3 entries, got ${json.files?.length}`);
  for (let i = 0; i < 3; i++) {
    const expected = `<stdin>[part${i + 1}]`;
    if (json.files[i].fileName !== expected)
      throw new Error(`Loader --multifile stdin file ${i} fileName mismatch: expected ${expected}, got ${json.files[i].fileName}`);
  }
});

await section("Loader: --multifile with no separator runs file as a single section...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "no-sep.js");
    writeFileSync(file, "console.log('only section');\n");
    const proc = Bun.spawnSync([LOADER, "--multifile", "--output=json", file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Loader --multifile no-sep should exit 0, got ${proc.exitCode}`);
    const json = JSON.parse(proc.stdout.toString());
    if (json.files?.length !== 1) throw new Error(`Loader --multifile no-sep should produce 1 entry, got ${json.files?.length}`);
    if (json.files[0].fileName !== file)
      throw new Error(`Loader --multifile no-sep should keep original file name, got ${json.files[0].fileName}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: --multifile drops leading/trailing separators...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "edge.js");
    writeFileSync(file, "---\nconsole.log('a');\n---\nconsole.log('b');\n---\n");
    const proc = Bun.spawnSync([LOADER, "--multifile", "--output=json", file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Loader --multifile edge should exit 0, got ${proc.exitCode}`);
    const json = JSON.parse(proc.stdout.toString());
    if (json.files?.length !== 2) throw new Error(`Loader --multifile edge should produce 2 entries, got ${json.files?.length}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: --multifile dispatches sections in parallel with --jobs...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "parallel.js");
    writeFileSync(file, "1;\n---\n2;\n---\n3;\n");
    const proc = Bun.spawnSync([LOADER, "--multifile", "--jobs=3", file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Loader --multifile --jobs should exit 0, got ${proc.exitCode}`);
    const out = proc.stdout.toString();
    if (!/Running 3 files with 3 workers/.test(out))
      throw new Error(`Loader --multifile --jobs should report 3-worker dispatch, got: ${out}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: --source-map with --multifile is rejected...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "sm.js");
    const sm = join(tmp, "sm.map");
    writeFileSync(file, "1;\n---\n2;\n");
    const proc = Bun.spawnSync([LOADER, "--multifile", `--source-map=${sm}`, file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode === 0) throw new Error("Loader --source-map with --multifile should fail");
    const message = proc.stdout.toString() + proc.stderr.toString();
    if (!message.includes("--multifile") || !message.includes("source-map"))
      throw new Error(`Loader --source-map with --multifile error message should mention both options, got: ${message}`);
  } finally {
    clean(tmp);
  }
});

await section("TestRunner: --multifile splits a single test file into N file results...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "multifile-tests.js");
    writeFileSync(
      file,
      'describe("section A", () => { test("a", () => { expect(1).toBe(1); }); });\n' +
      "---\n" +
      'describe("section B", () => { test("b", () => { expect(2).toBe(2); }); });\n' +
      "---\n" +
      'describe("section C", () => { test("c", () => { expect(3).toBe(3); }); });\n',
    );
    const out = join(tmp, "results.json");
    const proc = Bun.spawnSync(
      [TESTRUNNER, "--multifile", "--no-progress", "--no-results", `--output=${out}`, file],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (proc.exitCode !== 0) throw new Error(`TestRunner --multifile should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
    const json = JSON.parse(readFileSync(out, "utf-8"));
    if (json.totalFiles !== 3) throw new Error(`TestRunner --multifile totalFiles should be 3, got ${json.totalFiles}`);
    if (json.totalTests !== 3) throw new Error(`TestRunner --multifile totalTests should be 3, got ${json.totalTests}`);
    if (json.passed !== 3) throw new Error(`TestRunner --multifile passed should be 3, got ${json.passed}`);
  } finally {
    clean(tmp);
  }
});

await section("Bundler: --multifile compiles each section as a separate .gbc...", async () => {
  const tmp = makeTmp();
  const out = join(tmp, "out");
  try {
    mkdirSync(out, { recursive: true });
    const file = join(tmp, "bundle.js");
    writeFileSync(file, "const a = 1;\nconsole.log(a);\n---\nconsole.log(2 + 2);\n");
    const proc = Bun.spawnSync([BUNDLER, "--multifile", `--output=${out}/`, file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Bundler --multifile should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
    const part1 = join(out, "bundle[part1].gbc");
    const part2 = join(out, "bundle[part2].gbc");
    if (!existsSync(part1)) throw new Error(`Bundler --multifile should emit ${part1}`);
    if (!existsSync(part2)) throw new Error(`Bundler --multifile should emit ${part2}`);
    // Each .gbc should run independently in the script loader.
    const r1 = Bun.spawnSync([LOADER, part1], { stdout: "pipe" });
    if (r1.exitCode !== 0 || !r1.stdout.toString().includes("1"))
      throw new Error(`Bundler --multifile part1 .gbc should run successfully`);
  } finally {
    clean(tmp);
  }
});

await section("Bundler: --multifile rejects --output=<file>...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "rejected.js");
    writeFileSync(file, "1;\n---\n2;\n");
    const single = join(tmp, "single.gbc");
    const proc = Bun.spawnSync([BUNDLER, "--multifile", `--output=${single}`, file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode === 0) throw new Error("Bundler --multifile with --output=<file> should fail");
    const message = proc.stdout.toString() + proc.stderr.toString();
    if (!message.includes("--multifile") || !message.includes("--output"))
      throw new Error(`Bundler --multifile --output=<file> message should mention both options, got: ${message}`);
  } finally {
    clean(tmp);
  }
});

await section("BenchmarkRunner: --multifile produces multiple file entries...", async () => {
  const tmp = makeTmp();
  const benchEnv = {
    ...process.env,
    GOCCIA_BENCH_CALIBRATION_MS: "50",
    GOCCIA_BENCH_ROUNDS: "3",
  } as Record<string, string>;
  try {
    const file = join(tmp, "bench.js");
    writeFileSync(
      file,
      microbenchModule(['group("A", () => { bench("a", () => 1 + 1); });']) +
      "---\n" +
      microbenchModule(['group("B", () => { bench("b", () => 2 + 2); });']),
    );
    const proc = Bun.spawnSync([BENCHRUNNER, "--multifile", "--source-type=module", "--no-progress", "--format=json", file], {
      stdout: "pipe",
      stderr: "pipe",
      env: benchEnv,
      timeout: 120_000,
    });
    if (proc.exitCode !== 0) throw new Error(`BenchmarkRunner --multifile should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
    const json = JSON.parse(proc.stdout.toString());
    if (!Array.isArray(json.files) || json.files.length !== 2)
      throw new Error(`BenchmarkRunner --multifile should produce 2 file entries, got ${json.files?.length}`);
    if (!json.files.some((f: any) => f.fileName?.includes("[part1]")))
      throw new Error(`BenchmarkRunner --multifile should have a [part1] entry`);
    if (!json.files.some((f: any) => f.fileName?.includes("[part2]")))
      throw new Error(`BenchmarkRunner --multifile should have a [part2] entry`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: goccia.json multifile=true works without --multifile flag...", async () => {
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), JSON.stringify({ multifile: true }));
    const file = join(tmp, "config-driven.js");
    writeFileSync(file, "1;\n---\n2;\n");
    const proc = Bun.spawnSync([LOADER, "--output=json", file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) throw new Error(`Loader config-driven multifile should exit 0, got ${proc.exitCode}`);
    const json = JSON.parse(proc.stdout.toString());
    if (json.files?.length !== 2)
      throw new Error(`Loader config-driven multifile should produce 2 entries, got ${json.files?.length}`);
  } finally {
    clean(tmp);
  }
});

console.log("Loader: --module virtual modules use the ordinary module pipeline...");
for (const mode of ["interpreted", "bytecode"] as const) {
  const source = [
    'import bytes from "host:asset";',
    'import source moduleSource from "host:pkg/main";',
    'import defer * as deferred from "host:deferred";',
    'import special from "host:with space";',
    'import { url, resolved } from "host:pkg/main";',
    'url + "|" + resolved() + "|" + bytes[2] + "|" + deferred.value + "|" + special + "|" + typeof moduleSource;',
  ].join("\n");
  const proc = Bun.spawnSync(
    [
      LOADER,
      "-",
      "--source-type=module",
      `--mode=${mode}`,
      "--print",
      "--experimental-js-module-source",
      "--module",
      'host:asset={"type":"bytes","content":"AQID"}',
      "--module",
      'host:pkg/dep=export const value = 7;',
      "--module",
      'host:pkg/main=export const url = import.meta.url; export const resolved = () => import.meta.resolve("./dep");',
      "--module",
      'host:deferred=export const value = 11;',
      "--module",
      'host:with space=export default 5;',
    ],
    { stdin: new TextEncoder().encode(source), stdout: "pipe", stderr: "pipe" },
  );
  if (proc.exitCode !== 0)
    throw new Error(
      `--module ${mode} exited ${proc.exitCode}: ${proc.stdout.toString()}${proc.stderr.toString()}`,
    );
  if (!containsLine(proc.stdout.toString(), "host:pkg/main|host:pkg/dep|3|11|5|object"))
    throw new Error(`--module ${mode} did not preserve module phases, addresses, and bytes: ${proc.stdout.toString()}`);
}

await section("Loader: dynamic import and ShadowRealm use configured virtual modules...", async () => {
  const source = [
    'import("host:dynamic").then(ns => console.log("dynamic:" + ns.value));',
    'import { count } from "host:realm-state";',
    'console.log("parent-state:" + count);',
    'new ShadowRealm().importValue("host:realm", "value").then(value => console.log("realm:" + value));',
    'new ShadowRealm().importValue("host:realm-state", "count").then(value => console.log("child-state:" + value));',
  ].join("\n");
  const proc = Bun.spawnSync(
    [
      LOADER,
      "-",
      "--source-type=module",
      "--unsafe-shadowrealm",
      "--module",
      'host:dynamic=export const value = 9;',
      "--module",
      'host:realm=export const value = 13;',
      "--module",
      'host:realm-state=import.meta.count = (import.meta.count ?? 0) + 1; export const count = import.meta.count;',
    ],
    { stdin: new TextEncoder().encode(source), stdout: "pipe", stderr: "pipe" },
  );
  if (proc.exitCode !== 0)
    throw new Error(`Dynamic/ShadowRealm virtual modules failed: ${proc.stderr.toString()}`);
  const output = proc.stdout.toString();
  if (!output.includes("dynamic:9") || !output.includes("realm:13") ||
      !output.includes("parent-state:1") || !output.includes("child-state:1"))
    throw new Error(`Dynamic/ShadowRealm virtual modules produced unexpected output: ${output}`);
});

await section("Loader: hierarchical virtual module addresses preserve canonical URLs...", async () => {
  const proc = Bun.spawnSync(
    [
      LOADER,
      "-",
      "--source-type=module",
      "--print",
      "--module",
      'https://example.test/pkg/main?redirect/a/../b=export default import.meta.url + "|" + import.meta.resolve("./dep");',
    ],
    {
      stdin: new TextEncoder().encode(
        'import value from "https://example.test/pkg/main?redirect/a/../b"; value;',
      ),
      stdout: "pipe",
      stderr: "pipe",
    },
  );
  if (proc.exitCode !== 0)
    throw new Error(`Hierarchical virtual address failed: ${proc.stderr.toString()}`);
  if (!containsLine(proc.stdout.toString(),
      "https://example.test/pkg/main?redirect/a/../b|https://example.test/pkg/dep"))
    throw new Error(`Hierarchical virtual address was not preserved: ${proc.stdout.toString()}`);
});

await section("Loader: virtual import.meta.resolve uses aliases for bare specifiers...", async () => {
  const tmp = makeTmp();
  try {
    const dependency = join(tmp, "dependency.mjs");
    writeFileSync(dependency, "export default 1;\n");
    const proc = Bun.spawnSync(
      [
        LOADER,
        "-",
        "--source-type=module",
        "--print",
        "--alias",
        `dependency=${dependency}`,
        "--module",
        'host:resolver=export default import.meta.resolve("dependency");',
      ],
      {
        stdin: new TextEncoder().encode(
          'import value from "host:resolver"; value;',
        ),
        stdout: "pipe",
        stderr: "pipe",
      },
    );
    if (proc.exitCode !== 0)
      throw new Error(`Virtual bare resolution failed: ${proc.stderr.toString()}`);
    const resolvedURL = normalizeLineEndings(proc.stdout.toString())
      .split("\n")
      .find((line) => line.startsWith("file:"));
    const normalizePath = (path: string) => {
      const canonical = realpathSync(path);
      return process.platform === "win32" ? canonical.toLowerCase() : canonical;
    };
    if (resolvedURL === undefined ||
        normalizePath(fileURLToPath(resolvedURL)) !== normalizePath(dependency))
      throw new Error(
        `Virtual bare resolution skipped aliases: expected ${dependency}, got ${resolvedURL ?? "no URL"}`,
      );
  } finally {
    clean(tmp);
  }
});

await section("Loader: attributed virtual modules reinterpret their stored content...", async () => {
  const proc = Bun.spawnSync(
    [
      LOADER,
      "-",
      "--source-type=module",
      "--print",
      "--module",
      "host:source=A",
    ],
    {
      stdin: new TextEncoder().encode(
        'import bytes from "host:source" with { type: "bytes" }; bytes[0];',
      ),
      stdout: "pipe",
      stderr: "pipe",
    },
  );
  if (proc.exitCode !== 0)
    throw new Error(`Attributed virtual module failed: ${proc.stderr.toString()}`);
  if (!containsLine(proc.stdout.toString(), "65"))
    throw new Error(`Attributed virtual module should expose source bytes: ${proc.stdout.toString()}`);
});

await section("Loader: virtual definitions validate eagerly but JavaScript parses lazily...", async () => {
  const unused = Bun.spawnSync(
    [LOADER, "-", "--module", "host:unused=!!! not valid JavaScript !!!"],
    { stdin: new TextEncoder().encode("1;"), stdout: "pipe", stderr: "pipe" },
  );
  if (unused.exitCode !== 0)
    throw new Error(`Unused invalid virtual source should not fail startup: ${unused.stderr.toString()}`);

  const invalidBytes = Bun.spawnSync(
    [LOADER, "-", "--module", 'host:bad={"type":"bytes","content":"%%%"}'],
    { stdin: new TextEncoder().encode("1;"), stdout: "pipe", stderr: "pipe" },
  );
  const invalidBytesOutput = invalidBytes.stdout.toString() + invalidBytes.stderr.toString();
  if (invalidBytes.exitCode === 0 || !invalidBytesOutput.includes("invalid base64"))
    throw new Error(`Invalid virtual bytes should fail configuration: ${invalidBytesOutput}`);

  const runtimeCollision = Bun.spawnSync(
    [LOADER, "-", "--module", "goccia:csv=export default 1;"],
    { stdin: new TextEncoder().encode("1;"), stdout: "pipe", stderr: "pipe" },
  );
  const collisionOutput = runtimeCollision.stdout.toString() + runtimeCollision.stderr.toString();
  if (runtimeCollision.exitCode === 0 || !collisionOutput.includes("runtime module"))
    throw new Error(`Runtime module collision should be a configuration error: ${collisionOutput}`);
});

await section("SandboxRunner: virtual modules share the CLI surface and cannot shadow host modules...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [{
        path: "/main.js",
        text: 'import value from "host:configured"; console.log(value);',
      }],
    }));
    const configured = Bun.spawnSync(
      [
        SANDBOXRUNNER,
        "/main.js",
        `--seed-config=${seed}`,
        "--source-type=module",
        "--module",
        "host:configured=export default 17;",
      ],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (configured.exitCode !== 0 ||
        normalizeLineEndings(configured.stdout.toString()).trim() !== "17")
      throw new Error(`Sandbox virtual module configuration failed: ${configured.stdout.toString()}${configured.stderr.toString()}`);

    const hostConfigDir = join(tmp, "host-config");
    const sandboxEntry = join(hostConfigDir, "main.js");
    const isolationSeed = join(tmp, "isolation-seed.json");
    mkdirSync(hostConfigDir, { recursive: true });
    writeFileSync(
      join(hostConfigDir, "goccia.json"),
      JSON.stringify({ modules: "missing-modules.json" }),
    );
    writeFileSync(isolationSeed, JSON.stringify({
      files: [{ path: sandboxEntry, text: 'console.log("isolated");' }],
    }));
    const isolated = Bun.spawnSync(
      [
        SANDBOXRUNNER,
        sandboxEntry,
        `--seed-config=${isolationSeed}`,
      ],
      { stdout: "pipe", stderr: "pipe" },
    );
    if (isolated.exitCode !== 0 ||
        normalizeLineEndings(isolated.stdout.toString()).trim() !== "isolated")
      throw new Error(`Sandbox consulted host config for a virtual path: ${isolated.stdout.toString()}${isolated.stderr.toString()}`);
    // Skipping the discovered config is deliberate, and silence about it is
    // not: this is the one binary whose ignored config fails open.
    if (!isolated.stderr.toString().includes("ignoring discovered configuration"))
      throw new Error(`Sandbox skipped a discoverable config without saying so: ${isolated.stderr.toString()}`);

    const manifest = join(tmp, "modules.mjs");
    const manifestSource = join(tmp, "module-map.mjs");
    writeFileSync(
      manifestSource,
      'export default {"host:configured": {content: "export default 19;"}};\n',
    );
    writeFileSync(
      manifest,
      'import modules from "./module-map.mjs"; export default modules;\n',
    );
    for (const mode of ["interpreted", "bytecode"] as const) {
      const configuredFromManifest = Bun.spawnSync(
        [
          SANDBOXRUNNER,
          "/main.js",
          `--seed-config=${seed}`,
          "--source-type=module",
          `--mode=${mode}`,
          "--modules",
          manifest,
        ],
        { stdout: "pipe", stderr: "pipe" },
      );
      if (configuredFromManifest.exitCode !== 0 ||
          normalizeLineEndings(configuredFromManifest.stdout.toString()).trim() !== "19")
        throw new Error(`Sandbox executable module manifest ${mode} failed: ${configuredFromManifest.stdout.toString()}${configuredFromManifest.stderr.toString()}`);
    }

    const hostCollision = Bun.spawnSync(
      [
        SANDBOXRUNNER,
        "/main.js",
        `--seed-config=${seed}`,
        "--source-type=module",
        "--module",
        "fs=export default 1;",
      ],
      { stdout: "pipe", stderr: "pipe" },
    );
    const hostCollisionOutput =
      hostCollision.stdout.toString() + hostCollision.stderr.toString();
    if (hostCollision.exitCode === 0 ||
        !hostCollisionOutput.includes("host module"))
      throw new Error(`Host module collision should be a configuration error: ${hostCollisionOutput}`);
  } finally {
    clean(tmp);
  }
});

// ============================================================================
// No-argument stdin policy (clig.dev)
//
// The interactive-terminal branch cannot be exercised here — Bun.spawn has no
// pty — so the decision itself is covered by the Pascal unit tests in
// source/app/Goccia.CLI.Stdin.Test.pas.  What matters for CI is that every
// NON-terminal path is byte-for-byte unchanged: piped stdin, closed stdin,
// and an explicit "-" must all still read the program from standard input.
// See docs/contributing/cli-conventions.md.
// ============================================================================

{
  const tmp = makeTmp();
  const stdinBenchEnv = {
    ...process.env,
    GOCCIA_BENCH_CALIBRATION_MS: "50",
    GOCCIA_BENCH_ROUNDS: "3",
  } as Record<string, string>;

  // Each binary needs input it can actually accept: the benchmark runner
  // fails a run with zero benchmarks, and the bundler requires an explicit
  // --output when its source came from stdin.
  //
  // Exit 0 alone would not prove anything: a regression that drops stdin on
  // the floor and executes nothing still exits 0. Every entry therefore
  // carries a postcondition only a real execution of *this* source can
  // satisfy — a marker the program prints, or, for the bundler (which
  // prints nothing), the artifact it writes. `reset` runs before each
  // invocation so a stale artifact cannot stand in for a fresh one.
  const bundlerOut = join(tmp, "stdin-policy.gbc");
  const STDIN_MARKER = "stdin-policy-marker";
  type SpawnResult = {
    stdout: { toString(): string };
    stderr: { toString(): string };
  };
  const expectMarker = (name: string, run: string, proc: SpawnResult) => {
    const output = proc.stdout.toString();
    if (!output.includes(STDIN_MARKER))
      throw new Error(
        `${name} ${run}: stdin source did not run (no ${JSON.stringify(STDIN_MARKER)} in stdout): ${output}${proc.stderr.toString()}`,
      );
  };

  const stdinApps = [
    {
      name: "GocciaScriptLoader",
      bin: LOADER,
      args: [] as string[],
      source: `console.log("${STDIN_MARKER}");\n`,
      env: undefined as Record<string, string> | undefined,
      reset: undefined as (() => void) | undefined,
      verify: expectMarker,
    },
    {
      name: "GocciaScriptLoaderBare",
      bin: BARE,
      args: [],
      // The bare loader has no console; `print` is its output global.
      source: `print("${STDIN_MARKER}");\n`,
      env: undefined,
      reset: undefined,
      verify: expectMarker,
    },
    {
      name: "GocciaTestRunner",
      bin: TESTRUNNER,
      args: ["--no-progress"],
      // The marker proves the source ran; the passing test proves the suite
      // was registered and executed rather than merely parsed.
      source: [
        'test("stdin suite", () => {',
        `  console.log("${STDIN_MARKER}");`,
        "  expect(1 + 1).toBe(2);",
        "});",
        "",
      ].join("\n"),
      env: undefined,
      reset: undefined,
      verify: (name: string, run: string, proc: SpawnResult) => {
        expectMarker(name, run, proc);
        const output = proc.stdout.toString();
        if (!output.includes("Test Results Passed: 1"))
          throw new Error(
            `${name} ${run}: stdin suite did not run a passing test: ${output}${proc.stderr.toString()}`,
          );
      },
    },
    {
      name: "GocciaBenchmarkRunner",
      bin: BENCHRUNNER,
      args: ["--source-type=module", "--no-progress"],
      source: microbenchModule([`bench("${STDIN_MARKER}", () => 1);`]),
      env: stdinBenchEnv,
      reset: undefined,
      // The benchmark name only reaches the report if the benchmark was
      // registered and measured.
      verify: expectMarker,
    },
    {
      name: "GocciaBundler",
      bin: BUNDLER,
      args: [`--output=${bundlerOut}`],
      source: `console.log("${STDIN_MARKER}");\n`,
      env: undefined,
      reset: () => rmSync(bundlerOut, { force: true }),
      // The bundler compiles rather than runs, so its evidence is the
      // artifact: it must exist and, when executed, produce the marker that
      // only the stdin source could have put there.
      verify: (name: string, run: string, proc: SpawnResult) => {
        if (!existsSync(bundlerOut))
          throw new Error(
            `${name} ${run}: no bundle written from stdin source: ${proc.stdout.toString()}${proc.stderr.toString()}`,
          );
        const roundtrip = Bun.spawnSync([LOADER, bundlerOut], {
          stdout: "pipe",
          stderr: "pipe",
          timeout: 120_000,
        });
        const roundtripOutput = roundtrip.stdout.toString();
        if (roundtrip.exitCode !== 0 || !roundtripOutput.includes(STDIN_MARKER))
          throw new Error(
            `${name} ${run}: the bundle does not carry the stdin source (exit ${roundtrip.exitCode}): ${roundtripOutput}${roundtrip.stderr.toString()}`,
          );
      },
    },
  ];

  try {
    console.log("Stdin policy: piped stdin with no arguments still runs...");
    for (const app of stdinApps) {
      app.reset?.();
      const proc = Bun.spawnSync([app.bin, ...app.args], {
        stdin: new TextEncoder().encode(app.source),
        stdout: "pipe",
        stderr: "pipe",
        env: app.env,
        timeout: 120_000,
      });
      if (proc.exitCode !== 0)
        throw new Error(
          `${app.name} with piped stdin exited ${proc.exitCode}: ${proc.stdout.toString()}${proc.stderr.toString()}`,
        );
      app.verify(app.name, "with piped stdin", proc);
    }

    console.log('Stdin policy: explicit "-" with piped stdin still runs...');
    for (const app of stdinApps) {
      app.reset?.();
      const proc = Bun.spawnSync([app.bin, ...app.args, "-"], {
        stdin: new TextEncoder().encode(app.source),
        stdout: "pipe",
        stderr: "pipe",
        env: app.env,
        timeout: 120_000,
      });
      if (proc.exitCode !== 0)
        throw new Error(
          `${app.name} with explicit "-" exited ${proc.exitCode}: ${proc.stdout.toString()}${proc.stderr.toString()}`,
        );
      app.verify(app.name, 'with explicit "-"', proc);
    }

    console.log("Stdin policy: closed stdin with no arguments is not a usage error...");
    for (const app of stdinApps) {
      // No `stdin` option: Bun attaches the null device, which is not a
      // terminal, so the implicit-stdin path must still be taken.  The run
      // may fail on its empty program, but it must never be the usage exit.
      const proc = Bun.spawnSync([app.bin, ...app.args], {
        stdout: "pipe",
        stderr: "pipe",
        env: app.env,
        timeout: 120_000,
      });
      if (proc.exitCode === 2)
        throw new Error(
          `${app.name} treated closed stdin as an interactive terminal: ${proc.stderr.toString()}`,
        );
      const output = proc.stdout.toString() + proc.stderr.toString();
      if (output.includes("standard input is a terminal"))
        throw new Error(`${app.name} printed the terminal hint for closed stdin: ${output}`);
    }
  } finally {
    clean(tmp);
  }
}

console.log("Stdin policy: --help documents the stdin rule and escape hatch...");
for (const app of [
  { name: "GocciaScriptLoader", bin: LOADER },
  { name: "GocciaScriptLoaderBare", bin: BARE },
  { name: "GocciaTestRunner", bin: TESTRUNNER },
  { name: "GocciaBenchmarkRunner", bin: BENCHRUNNER },
  { name: "GocciaBundler", bin: BUNDLER },
]) {
  const proc = Bun.spawnSync([app.bin, "--help"], {
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0)
    throw new Error(`${app.name} --help exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  const help = normalizeLineEndings(proc.stdout.toString());
  // The EOF key sequence is platform-specific: a Unix console ends input on
  // Ctrl-D, a Windows console on Ctrl-Z followed by Enter, and the help text
  // renders whichever one the local console honours (Goccia.CLI.Stdin).
  const endOfInputKeys = process.platform === "win32" ? "Ctrl-Z then Enter" : "Ctrl-D";
  for (const needle of ["Input:", `${app.name} < app.js`, '"-"', endOfInputKeys, "exits 2"]) {
    if (!help.includes(needle))
      throw new Error(`${app.name} --help is missing ${JSON.stringify(needle)}:\n${help}`);
  }
  if (proc.stderr.toString() !== "")
    throw new Error(`${app.name} --help wrote to stderr: ${proc.stderr.toString()}`);
}

console.log("Stdin policy: GocciaREPL and GocciaSandboxRunner opt out...");
for (const app of [
  { name: "GocciaREPL", bin: REPL },
  { name: "GocciaSandboxRunner", bin: SANDBOXRUNNER },
]) {
  const proc = Bun.spawnSync([app.bin, "--help"], { stdout: "pipe", stderr: "pipe" });
  // Without these the test also passes when --help fails outright and prints
  // nothing: an empty stdout trivially lacks "Input:". Same postconditions as
  // the stdin-defaulting binaries above.
  if (proc.exitCode !== 0)
    throw new Error(`${app.name} --help exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stderr.toString() !== "")
    throw new Error(`${app.name} --help wrote to stderr: ${proc.stderr.toString()}`);
  const help = normalizeLineEndings(proc.stdout.toString());
  if (!help.includes(app.name))
    throw new Error(`${app.name} --help did not print its own usage:\n${help}`);
  if (help.includes("Input:"))
    throw new Error(`${app.name} should not advertise the stdin rule:\n${help}`);
}

await section("TestRunner: vitest compatibility shim and its off-switch...", async () => {
  const tmp = makeTmp();
  try {
    const suitePath = join(tmp, "vitest-import.test.js");
    writeFileSync(
      suitePath,
      [
        'import { vi } from "vitest";',
        'test("vi.fn is available", () => {',
        "  const fn = vi.fn();",
        "  fn(1);",
        "  expect(fn).toHaveBeenCalledWith(1);",
        "});",
        "",
      ].join("\n"),
    );

    const enabled = Bun.spawnSync([TESTRUNNER, suitePath, "--no-progress"]);
    const enabledOutput = new TextDecoder().decode(enabled.stdout);
    // Output alone does not pin the contract: the process status is what CI
    // and every caller act on, so assert it on both invocations.
    if (enabled.exitCode !== 0)
      throw new Error(
        `TestRunner with the default shim exited ${enabled.exitCode}:\n${enabledOutput}${new TextDecoder().decode(enabled.stderr)}`,
      );
    if (!enabledOutput.includes("Passed: 1"))
      throw new Error(
        `TestRunner should resolve the bare vitest specifier by default:\n${enabledOutput}`,
      );

    const disabled = Bun.spawnSync([
      TESTRUNNER,
      suitePath,
      "--no-progress",
      "--no-vitest-compat",
    ]);
    const disabledOutput =
      new TextDecoder().decode(disabled.stdout) +
      new TextDecoder().decode(disabled.stderr);
    if (disabled.exitCode === 0)
      throw new Error(
        `--no-vitest-compat should fail the run, but it exited 0:\n${disabledOutput}`,
      );
    if (!disabledOutput.includes('Cannot resolve bare module specifier "vitest"'))
      throw new Error(
        `--no-vitest-compat should leave the vitest specifier unresolvable:\n${disabledOutput}`,
      );
  } finally {
    clean(tmp);
  }
});

// ============================================================================
// goccia:test availability per binary
// ============================================================================
//
// The testing API has two halves that install independently: the `goccia:test`
// module namespace, which every host attaching the loader runtime profile
// gets, and the testing globals, which only GocciaTestRunner injects. These
// cases pin both directions — the import must work where the profile is
// applied, and no binary but the runner may grow a testing global.

await section("Loader: goccia:test is importable and injects no globals...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "self-test.js");
    writeFileSync(
      file,
      [
        'import { describe, test, expect, mock, runTests } from "goccia:test";',
        "",
        "// Importing the module must not publish anything globally.",
        'for (const name of ["describe", "test", "it", "expect", "mock", "spyOn",',
        '  "beforeAll", "beforeEach", "afterEach", "afterAll", "onTestFinished",',
        '  "runTests", "__gocciaTest262Describe", "__gocciaTest262Test"]) {',
        "  if (globalThis[name] !== undefined)",
        '    throw new Error("leaked testing global: " + name);',
        "}",
        "",
        'describe("loader suite", () => {',
        '  test("registers and runs", () => { expect(1 + 1).toBe(2); });',
        '  test("mock is wired to the same registry", () => {',
        "    const fn = mock(() => 42);",
        "    expect(fn()).toBe(42);",
        "    expect(fn).toHaveBeenCalledTimes(1);",
        "  });",
        "});",
        "",
        "// Nothing drives execution in a loader script: runTests is the entry point.",
        "const results = runTests({ showTestResults: false });",
        'console.log("passed:" + results.passed);',
        'console.log("failed:" + results.failed);',
        'console.log("run:" + results.totalRunTests);',
        "",
      ].join("\n"),
    );

    for (const [label, extraArgs] of [
      ["interpreted", []],
      ["bytecode", ["--mode=bytecode"]],
    ] as const) {
      const proc = Bun.spawnSync([LOADER, file, ...extraArgs], {
        stdout: "pipe",
        stderr: "pipe",
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode !== 0)
        throw new Error(`Loader goccia:test (${label}) exited ${proc.exitCode}:\n${out}`);
      if (!containsLine(out, "passed:2"))
        throw new Error(`Loader goccia:test (${label}) should run 2 tests:\n${out}`);
      if (!containsLine(out, "failed:0"))
        throw new Error(`Loader goccia:test (${label}) should report no failures:\n${out}`);
      if (!containsLine(out, "run:2"))
        throw new Error(`Loader goccia:test (${label}) should report 2 run tests:\n${out}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("Loader: a failing imported suite is only fatal if the script says so...", async () => {
  const tmp = makeTmp();
  try {
    // A loader script has no runner to interpret results, so runTests reports
    // rather than decides. The process status stays 0 unless the script throws.
    const reporting = join(tmp, "reporting.js");
    writeFileSync(
      reporting,
      [
        'import { test, expect, runTests } from "goccia:test";',
        'test("fails", () => { expect(1).toBe(2); });',
        "const results = runTests({ showTestResults: false });",
        'console.log("failed:" + results.failed);',
        "",
      ].join("\n"),
    );
    const lenient = Bun.spawnSync([LOADER, reporting], { stdout: "pipe", stderr: "pipe" });
    const lenientOut = lenient.stdout.toString() + lenient.stderr.toString();
    if (lenient.exitCode !== 0)
      throw new Error(`A failing imported suite should not fail the loader by itself, got ${lenient.exitCode}:\n${lenientOut}`);
    if (!containsLine(lenientOut, "failed:1"))
      throw new Error(`Loader runTests should report the failure:\n${lenientOut}`);

    const strict = join(tmp, "strict.js");
    writeFileSync(
      strict,
      [
        'import { test, expect, runTests } from "goccia:test";',
        'test("fails", () => { expect(1).toBe(2); });',
        "const results = runTests({ showTestResults: false });",
        'if (results.failed > 0) throw new Error(results.failed + " test(s) failed");',
        "",
      ].join("\n"),
    );
    const failing = Bun.spawnSync([LOADER, strict], { stdout: "pipe", stderr: "pipe" });
    const failingOut = failing.stdout.toString() + failing.stderr.toString();
    if (failing.exitCode === 0)
      throw new Error(`Throwing on a failed suite should fail the loader:\n${failingOut}`);
    if (!failingOut.includes("1 test(s) failed"))
      throw new Error(`Loader should surface the thrown suite error:\n${failingOut}`);
  } finally {
    clean(tmp);
  }
});

await section("Loader: the bare vitest specifier stays unresolvable...", async () => {
  // The compatibility shim is a GocciaTestRunner default, not a loader one.
  // Having goccia:test available must not drag `vitest` along with it.
  const tmp = makeTmp();
  try {
    const file = join(tmp, "vitest-import.js");
    writeFileSync(file, 'import { vi } from "vitest";\n');
    const proc = Bun.spawnSync([LOADER, file], { stdout: "pipe", stderr: "pipe" });
    const out = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode === 0)
      throw new Error(`Loader should not resolve the bare vitest specifier:\n${out}`);
    if (!out.includes('Cannot resolve bare module specifier "vitest"'))
      throw new Error(`Loader should report the vitest specifier as unresolvable:\n${out}`);
  } finally {
    clean(tmp);
  }
});

await section("Bare Loader: goccia:test is absent along with the rest of the runtime...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "import-test.js");
    writeFileSync(file, 'import { expect } from "goccia:test";\n');
    const proc = Bun.spawnSync([BARE, file], { stdout: "pipe", stderr: "pipe" });
    const out = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode === 0)
      throw new Error(`Bare loader attaches no runtime, so goccia:test must not resolve:\n${out}`);
    if (!out.includes('Cannot resolve bare module specifier "goccia:test"'))
      throw new Error(`Bare loader should report goccia:test as unresolvable:\n${out}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: goccia:test is importable and injects no globals...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(
      seed,
      JSON.stringify({
        files: [
          {
            path: "/main.js",
            text: [
              'import { test, expect, runTests } from "goccia:test";',
              'if (globalThis.describe !== undefined) throw new Error("leaked describe");',
              'if (globalThis.expect !== undefined) throw new Error("leaked expect");',
              'test("runs inside the sandbox", () => { expect(2 + 2).toBe(4); });',
              "const results = runTests({ showTestResults: false });",
              'console.log("sandbox-passed:" + results.passed);',
            ].join("\n"),
          },
        ],
      }),
    );
    const proc = Bun.spawnSync(
      [SANDBOXRUNNER, "/main.js", `--seed-config=${seed}`, "--source-type=module"],
      { stdout: "pipe", stderr: "pipe" },
    );
    const out = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode !== 0)
      throw new Error(`SandboxRunner goccia:test exited ${proc.exitCode}:\n${out}`);
    if (!containsLine(out, "sandbox-passed:1"))
      throw new Error(`SandboxRunner should run the imported suite:\n${out}`);
  } finally {
    clean(tmp);
  }
});

await section("TestRunner: globals stay injected and share the imported registry...", async () => {
  const tmp = makeTmp();
  try {
    const suitePath = join(tmp, "globals.test.js");
    writeFileSync(
      suitePath,
      [
        'import { expect as importedExpect, test as importedTest } from "goccia:test";',
        'describe("runner globals", () => {',
        '  test("the whole API is global", () => {',
        '    for (const name of ["describe", "test", "it", "expect", "mock", "spyOn",',
        '      "beforeAll", "beforeEach", "afterEach", "afterAll", "onTestFinished",',
        '      "runTests", "__gocciaTest262Describe", "__gocciaTest262Test"]) {',
        '      expect(typeof globalThis[name]).toBe("function");',
        "    }",
        "  });",
        '  test("import and global are the same function", () => {',
        "    expect(importedExpect).toBe(globalThis.expect);",
        "    expect(importedTest).toBe(globalThis.test);",
        "  });",
        "});",
        "",
      ].join("\n"),
    );
    const proc = Bun.spawnSync([TESTRUNNER, suitePath, "--no-progress"], {
      stdout: "pipe",
      stderr: "pipe",
    });
    const out = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode !== 0)
      throw new Error(`TestRunner globals regression exited ${proc.exitCode}:\n${out}`);
    if (!out.includes("Passed: 2"))
      throw new Error(`TestRunner should keep injecting the testing globals:\n${out}`);
  } finally {
    clean(tmp);
  }
});

// ── GocciaFuzzHarness ──────────────────────────────────────────────────
//
// The harness's contract is entirely in its exit code: every engine-modelled
// outcome must be 0, and only an unmodelled fault may be nonzero. If that
// inverts, a fuzz campaign either reports nothing or reports everything, and
// in both cases it is worthless. These sections pin the contract.

await section("Fuzz Harness: engine-modelled outcomes exit zero...", async () => {
  const tmp = makeTmp();
  try {
    // One input per outcome class the engine models. All must exit 0.
    const cases: Record<string, string> = {
      completed: "const x = 1 + 1;",
      "parse-error": "const = = = {{{",
      "runtime-error": "undefinedIdentifier;",
      "script-throw": "null.x;",
      timeout: "while (true) {}",
      "module-denied": 'import { a } from "./nope.js";',
      "deep-recursion": "const f = () => f(); f();",
      // Requests ~800 MB of element storage in one step, past the harness's
      // 256 MB budget. The refusal raises the uncatchable TGocciaMemoryLimitError;
      // the harness must classify it as a bounded outcome (exit 0), not let it
      // escape the typed ladder into an unexpected-fault report.
      "memory-limit": "const a = new Array(100000000); a.length;",
    };
    for (const [name, source] of Object.entries(cases)) {
      const file = join(tmp, `${name}.js`);
      writeFileSync(file, source);
      const proc = Bun.spawnSync([FUZZHARNESS, "--verbose", file], {
        stdout: "pipe",
        stderr: "pipe",
      });
      const output = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode !== 0)
        throw new Error(`Fuzz harness treated "${name}" as a fault (exit ${proc.exitCode}):\n${output}`);
      // Both executors must report, otherwise a mode is silently skipped.
      if (!output.includes("[interpreter]") || !output.includes("[bytecode]"))
        throw new Error(`Fuzz harness did not run both executors for "${name}":\n${output}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("Fuzz Harness: unmodelled fault exits nonzero with a backtrace...", async () => {
  const proc = Bun.spawnSync([FUZZHARNESS, "--self-test-fault"], {
    stdout: "pipe",
    stderr: "pipe",
  });
  const output = proc.stdout.toString() + proc.stderr.toString();
  if (proc.exitCode === 0)
    throw new Error(`Fuzz harness fault path exited 0; findings would be invisible:\n${output}`);
  if (!output.includes("unexpected fault"))
    throw new Error(`Fuzz harness fault path did not report the fault:\n${output}`);
  // Frames print as symbols on Linux and bare $ addresses on macOS; either
  // proves a backtrace was captured before the handler unwound.
  if (!/\$[0-9A-F]{8}|\.pas|\.dpr/i.test(output))
    throw new Error(`Fuzz harness fault path produced no backtrace:\n${output}`);
});

await section("Fuzz Harness: modules cannot reach the host filesystem...", async () => {
  const tmp = makeTmp();
  try {
    // A real, readable file on disk. The harness must still refuse it: fuzz
    // input driving host file reads is the property this guards.
    const secret = join(tmp, "secret.js");
    writeFileSync(secret, "globalThis.__leaked = 1; export const a = 1;\n");
    const entry = join(tmp, "entry.js");
    writeFileSync(entry, `import { a } from ${JSON.stringify(secret)};\n`);
    const proc = Bun.spawnSync([FUZZHARNESS, "--verbose", entry], {
      stdout: "pipe",
      stderr: "pipe",
    });
    const output = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode !== 0)
      throw new Error(`Fuzz harness faulted on a module import (exit ${proc.exitCode}):\n${output}`);
    if (!output.includes("module-denied"))
      throw new Error(`Fuzz harness loaded a host file instead of denying it:\n${output}`);
  } finally {
    clean(tmp);
  }
});

await section("Fuzz Harness: stdin input path...", async () => {
  const proc = Bun.spawnSync([FUZZHARNESS, "--verbose", "-"], {
    stdin: new TextEncoder().encode("const x = 1;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const output = proc.stdout.toString() + proc.stderr.toString();
  if (proc.exitCode !== 0)
    throw new Error(`Fuzz harness stdin path exited ${proc.exitCode}:\n${output}`);
  if (!output.includes("completed"))
    throw new Error(`Fuzz harness stdin path did not run the input:\n${output}`);
});

await section("Fuzz Harness: oversized file is rejected without materializing it...", async () => {
  // The AFL common case is a file argument (`@@`). The size gate must fire on
  // the file's on-disk length BEFORE the whole file is read, otherwise a
  // pathologically large input OOMs the harness and the fuzzer misreads that
  // as a crash. A 256 MiB file proves it: rejecting it before read keeps peak
  // RSS at the harness's small startup floor, while materializing it would push
  // resident memory past the file size. The RSS ceiling sits well below both.
  const tmp = makeTmp();
  try {
    const huge = join(tmp, "oversized.js");
    const oversizeBytes = 256 * 1024 * 1024;
    writeFileSync(huge, Buffer.alloc(oversizeBytes, 0x61)); // 'a'

    const rejected = await runWithPeakRss([FUZZHARNESS, "--verbose", huge]);
    if (rejected.exitCode !== 0)
      throw new Error(`Fuzz harness oversized file exited ${rejected.exitCode}:\n${rejected.output}`);
    if (!rejected.output.includes("input-rejected"))
      throw new Error(`Fuzz harness did not reject the oversized file:\n${rejected.output}`);
    // Reading the 256 MiB file would peak well above 256 MiB; the pre-read gate
    // keeps it at startup scale. 128 MiB is far below the materialized cost and
    // clear of the harness's own startup.
    assertPeakRssBelow(rejected, "rejected oversized fuzz file", 128 * 1024 * 1024);
  } finally {
    clean(tmp);
  }
});

await section("Fuzz Harness: oversized stdin is rejected like an oversized file...", async () => {
  // The size bound must guard both interfaces. A fuzzer can pipe a multi-MB
  // input through `-`; it must be rejected before the lexer, not lexed.
  const huge = "a".repeat(2 * 1024 * 1024) + ";\n";
  const proc = Bun.spawnSync([FUZZHARNESS, "--verbose", "-"], {
    stdin: new TextEncoder().encode(huge),
    stdout: "pipe",
    stderr: "pipe",
  });
  const output = proc.stdout.toString() + proc.stderr.toString();
  if (proc.exitCode !== 0)
    throw new Error(`Fuzz harness oversized stdin exited ${proc.exitCode}:\n${output}`);
  if (!output.includes("input-rejected"))
    throw new Error(`Fuzz harness did not reject oversized stdin before lexing:\n${output}`);
});

// ── Memory budget (WP-3) ───────────────────────────────────────────────
//
// The budget's whole promise is that it bounds the process. A limit that is
// only noticed after the allocation has already happened bounds nothing, and
// it prints the same error and exits the same way either case, so every
// refusal below asserts peak resident memory as well as the error. See
// scripts/test-cli/rss.ts for how that is measured and where it cannot be.

await section("Memory budget: single large allocation is refused before it happens...", async () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "alloc.js");
    // ~800 MB of pointer storage requested in one step.
    writeFileSync(file, "const a = new Array(100000000); print('len ' + a.length);\n");

    const refused = await runWithPeakRss([BARE, "--max-memory=67108864", file]);
    if (refused.exitCode === 0)
      throw new Error(`Allocation past the budget was permitted:\n${refused.output}`);
    if (!/exceed the memory budget/i.test(refused.output))
      throw new Error(`Expected a memory-budget error, got:\n${refused.output}`);
    // Measured ~16 MiB here; the same script when the gate lets it through
    // peaks near 1.4 GB, so the ceiling sits an order of magnitude below the
    // failure mode and well clear of interpreter startup.
    assertPeakRssBelow(refused, "refused Array(100000000)", 192 * 1024 * 1024);

    // The same script under a budget that accommodates it must still work,
    // otherwise the gate is just broken rather than enforcing anything.
    const permitted = Bun.spawnSync([BARE, "--max-memory=2147483648", file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    const permittedOut = permitted.stdout.toString() + permitted.stderr.toString();
    if (permitted.exitCode !== 0 || !permittedOut.includes("len 100000000"))
      throw new Error(`Allocation within budget was refused:\n${permittedOut}`);
  } finally {
    clean(tmp);
  }
});

// Property storage grows in doublings rather than in one step, so the refusal
// arrives after several successful growths rather than on the first request —
// the assertion is that the script cannot outrun the budget, not that any
// single write is refused. Keys are produced by nested loops over two small
// arrays so the driver itself stays far inside the budget: a keys array would
// have to hold every key string alive and could exhaust the budget on its own,
// which would prove nothing about property storage.
await section("Memory budget: property storage growth is refused before it happens...", async () => {
  const tmp = makeTmp();
  try {
    const runaway = join(tmp, "properties-runaway.js");
    writeFileSync(
      runaway,
      "const outer = Array.from({ length: 2000 }, (_, i) => i);\n" +
        "const inner = Array.from({ length: 2000 }, (_, i) => i);\n" +
        "const target = {};\n" +
        "for (const a of outer) {\n" +
        "  for (const b of inner) {\n" +
        '    target["k" + (a * 2000 + b)] = b;\n' +
        "  }\n" +
        "}\n" +
        'print("kept " + Object.keys(target).length);\n',
    );

    // Same shape, small enough that its property storage fits the same budget.
    const bounded = join(tmp, "properties-bounded.js");
    writeFileSync(
      bounded,
      'const keys = Array.from({ length: 20000 }, (_, i) => "k" + i);\n' +
        "const target = {};\n" +
        "for (const k of keys) {\n" +
        "  target[k] = 1;\n" +
        "}\n" +
        'print("kept " + keys.length);\n',
    );

    for (const modeArgs of [[], ["--mode=bytecode"]]) {
      const label = modeArgs.length > 0 ? "bytecode" : "interpreter";

      const refused = await runWithPeakRss([
        BARE,
        "--max-memory=16777216",
        ...modeArgs,
        runaway,
      ]);
      if (refused.exitCode === 0)
        throw new Error(
          `Property growth past the budget was permitted (${label}):\n${refused.output}`,
        );
      if (!/exceed the memory budget/i.test(refused.output))
        throw new Error(`Expected a memory-budget error (${label}), got:\n${refused.output}`);
      // Measured ~143 MiB interpreted and ~80 MiB compiled. The ceiling is far
      // above both and far below the ~1 GB the same 4M-property loop reaches
      // when nothing refuses it — most of what is resident here is descriptor
      // and key-string storage the budget never sees (ADR 0106 Amendment 1),
      // not the entry array the gate does bound.
      //
      // The collecting gate (ADR 0110) raises the interpreted figure, and the
      // headroom above is what absorbs it. Measured on one machine, production
      // builds, before -> after: interpreted 80.5 -> 101.7 MiB, compiled
      // 73.8 -> 73.9 MiB. The cause is visible in the refusal itself — before,
      // the interpreted run was refused a 4,718,496-byte doubling and the
      // compiled run an 18,874,272-byte one; after, both are refused the same
      // 18,874,272-byte doubling. So the interpreted run now carries two more
      // doublings' worth of descriptors and key strings (4,718,496 -> 9,437,088
      // -> 18,874,272) when it is finally
      // refused, and those are exactly the storage the budget does not see.
      // Compiled is unchanged because it already reached that doubling. Note
      // which mode was unlucky here is the opposite of the 4 MiB case in
      // scripts/test-cli.ts: pre-H4 the loser was whichever mode happened to
      // arrive with a dirtier heap, which is the point of the change.
      assertPeakRssBelow(refused, `refused property growth (${label})`, 384 * 1024 * 1024);

      const permitted = Bun.spawnSync([BARE, "--max-memory=16777216", ...modeArgs, bounded], {
        stdout: "pipe",
        stderr: "pipe",
      });
      const permittedOut = permitted.stdout.toString() + permitted.stderr.toString();
      if (permitted.exitCode !== 0 || !permittedOut.includes("kept 20000"))
        throw new Error(`Property growth within budget was refused (${label}):\n${permittedOut}`);
    }
  } finally {
    clean(tmp);
  }
});

// The gate is per-allocation, and the budget's used-figure does not grow with
// property descriptors, so spreading the same properties across many objects
// escapes it entirely: the section above refuses one 4M-property object, and
// this one runs 480k properties to completion at eight times the same budget.
//
// This asserts the hole, not a guarantee. It is here because ADR 0106 states
// the gap in measured bytes and a prose statement rots silently; if the
// aggregate is ever bounded, the RSS floor below fails and the ADR and
// docs/garbage-collector.md have to be revisited rather than left wrong.
await section("Memory budget: aggregated small-object growth is NOT bounded (ADR 0106 A1)...", async () => {
  const tmp = makeTmp();
  try {
    const distributed = join(tmp, "properties-distributed.js");
    writeFileSync(
      distributed,
      'const outer = Array.from({ length: 4000 }, (_, i) => i);\n' +
        'const inner = Array.from({ length: 120 }, (_, i) => "k" + i);\n' +
        "const sink = [];\n" +
        "for (const a of outer) {\n" +
        "  const o = {};\n" +
        "  for (const k of inner) o[k] = a;\n" +
        "  sink.push(o);\n" +
        "}\n" +
        'print("objects " + sink.length);\n',
    );

    for (const modeArgs of [[], ["--mode=bytecode"]]) {
      const label = modeArgs.length > 0 ? "bytecode" : "interpreter";

      const run = await runWithPeakRss([BARE, "--max-memory=16777216", ...modeArgs, distributed]);
      if (run.exitCode !== 0 || !run.output.includes("objects 4000"))
        throw new Error(
          `Distributed property growth is now refused (${label}). That is a real ` +
            `improvement, but ADR 0106 Amendment 1 documents it as permitted — ` +
            `update the ADR and docs/garbage-collector.md, then rewrite this ` +
            `section as a refusal test:\n${run.output}`,
        );
      // Measured ~131 MiB interpreted and ~80 MiB compiled against a 16 MiB
      // budget. The floor is 2x the budget: high enough that a merely-idle
      // process cannot reach it, low enough to survive platform differences in
      // page size and allocator behaviour, and low enough that the sampler
      // fallback undershooting a spike cannot fail it.
      assertPeakRssAbove(run, `distributed property growth (${label})`, 32 * 1024 * 1024);
    }
  } finally {
    clean(tmp);
  }
});
// ── Sandbox runner engine options (WP-5) ───────────────────────────────
//
// The sandbox runner builds its own engine, because it needs its own module
// resolver. It used to build it without applying the engine options, so the
// binary whose entire purpose is running untrusted code silently ignored its
// own resource and network policy flags. These assert the flags reach the
// engine, and — for the budget — that the refusal is a refusal rather than a
// broken gate that rejects everything.

await section("SandboxRunner: --max-memory bounds the sandboxed program...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/alloc.js",
          // 32 MB of pointer storage in one step: enough to sit either side
          // of the two budgets below, small enough that the permitted arm
          // allocates it in milliseconds rather than seconds.
          text: "const a = new Array(4000000); console.log('len ' + a.length);\n",
        },
      ],
    }));

    const refused = Bun.spawnSync(
      [SANDBOXRUNNER, "/alloc.js", `--seed-config=${seed}`, "--max-memory=8388608"],
      { stdout: "pipe", stderr: "pipe" },
    );
    const refusedOut = refused.stdout.toString() + refused.stderr.toString();
    if (refused.exitCode === 0)
      throw new Error(`Sandbox allocation past the budget was permitted:\n${refusedOut}`);
    if (!/memory limit exceeded/i.test(refusedOut))
      throw new Error(`Expected a sandbox memory-budget error, got:\n${refusedOut}`);

    // The same script under a budget that accommodates it must still run,
    // otherwise the option is not applied so much as the runner is broken.
    const permitted = Bun.spawnSync(
      [SANDBOXRUNNER, "/alloc.js", `--seed-config=${seed}`, "--max-memory=67108864"],
      { stdout: "pipe", stderr: "pipe" },
    );
    const permittedOut = permitted.stdout.toString() + permitted.stderr.toString();
    if (permitted.exitCode !== 0 || !permittedOut.includes("len 4000000"))
      throw new Error(`Sandbox allocation within budget was refused:\n${permittedOut}`);
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: --fetch-deny-private-ranges reaches the sandboxed fetch...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "seed.json");
    writeFileSync(seed, JSON.stringify({
      files: [
        {
          path: "/fetch.js",
          // Port 1 on loopback needs no server: with the policy on the request
          // is refused before any connect, with it off it fails at connect.
          text: [
            "try {",
            "  await fetch('http://127.0.0.1:1/');",
            "  console.log('no-error');",
            "} catch (error) {",
            "  console.log('err:' + error.message);",
            "}",
          ].join("\n"),
        },
      ],
    }));
    const args = [
      SANDBOXRUNNER,
      "/fetch.js",
      `--seed-config=${seed}`,
      "--source-type=module",
      "--allowed-host=127.0.0.1",
    ];

    const denied = Bun.spawnSync([...args, "--fetch-deny-private-ranges"], {
      stdout: "pipe",
      stderr: "pipe",
    });
    const deniedOut = denied.stdout.toString() + denied.stderr.toString();
    if (!deniedOut.includes("resolves to private address 127.0.0.1"))
      throw new Error(`Sandbox fetch should be refused by address policy, got:\n${deniedOut}`);

    // Asserted positively: the default arm has to prove the request reached
    // the connect, because an absent substring is also what a script that
    // never ran produces.
    const allowed = Bun.spawnSync(args, { stdout: "pipe", stderr: "pipe" });
    const allowedOut = allowed.stdout.toString() + allowed.stderr.toString();
    if (allowed.exitCode !== 0 ||
        !allowedOut.includes("err:Failed to connect to 127.0.0.1:1"))
      throw new Error(`Sandbox fetch should reach the connect by default, got (exit ${allowed.exitCode}):\n${allowedOut}`);
  } finally {
    clean(tmp);
  }
});

// ── Sandbox run failure taxonomy (WP-5) ────────────────────────────────
//
// `runScript` reports why a nested run ended alongside the message that says
// it in prose, so a host orchestrating children can tell a bug from a
// ceiling without matching on text. The property worth pinning is not that
// the field exists: it is that the guest cannot talk its way into
// "host-error". Everything the child steers — its own throw, a path it
// named, a ceiling it ran into — must classify as its own fault or as the
// limit it hit, or the distinction the field exists to draw is gone.
//
// Both execution modes, because the classification sits in the runner's
// exception ladder and the executors raise through it differently.

const sandboxSeedConfig = (files: Record<string, string>): string =>
  JSON.stringify({
    files: Object.entries(files).map(([path, text]) => ({ path, text })),
  });

const runSandboxKinds = (
  seed: string,
  mode: "interpreted" | "bytecode",
  extraArgs: string[] = [],
): { stdout: string; exitCode: number | null; combined: string } => {
  const proc = Bun.spawnSync(
    [
      SANDBOXRUNNER,
      "/main.js",
      `--seed-config=${seed}`,
      "--source-type=module",
      `--mode=${mode}`,
      ...extraArgs,
    ],
    { stdout: "pipe", stderr: "pipe" },
  );
  return {
    stdout: normalizeLineEndings(proc.stdout.toString()).trim(),
    exitCode: proc.exitCode,
    combined: proc.stdout.toString() + proc.stderr.toString(),
  };
};

await section("SandboxRunner: guest-reachable failures never classify as host faults...", async () => {
  const tmp = makeTmp();
  try {
    const seed = join(tmp, "failure-kinds.json");
    writeFileSync(seed, sandboxSeedConfig({
      "/main.js": [
        'import { runScript } from "goccia";',
        'const ok = runScript("/ok.js");',
        'console.log("success:" + ok.failureKind + ":" + ok.ok);',
        'const thrown = runScript("/throw.js");',
        'console.log("throw:" + thrown.failureKind + ":" + thrown.ok);',
        // An entry path the guest picked, which the VFS does not have.
        'const missing = runScript("/absent.js");',
        'console.log("missing:" + missing.failureKind);',
        // A child seed source the guest picked, which the VFS does not have.
        'const badSeed = runScript("/ok.js", { sandbox: true, seed: ["/ok.js", "/absent.txt"] });',
        'console.log("seed:" + badSeed.failureKind);',
        // The nesting ceiling, observed from the frame one level above it.
        'console.log("nesting:" + runScript("/deep.js").stdout.trim());',
      ].join("\n"),
      "/ok.js": 'console.log("child ran");\n',
      "/throw.js": 'throw new Error("boom");\n',
      "/deep.js": [
        'import { runScript } from "goccia";',
        'const child = runScript("/deep.js");',
        "if (child.ok) console.log(child.stdout.trim());",
        'else console.log(child.failureKind);',
      ].join("\n"),
    }));

    const expected = [
      "success:none:true",
      "throw:script-error:false",
      "missing:script-error",
      "seed:script-error",
      "nesting:resource-limit",
    ].join("\n");
    for (const mode of ["interpreted", "bytecode"] as const) {
      const run = runSandboxKinds(seed, mode);
      if (run.exitCode !== 0)
        throw new Error(`SandboxRunner ${mode} failure-kind run should exit 0, got ${run.exitCode}:\n${run.combined}`);
      if (run.stdout !== expected)
        throw new Error(`SandboxRunner ${mode} failure kinds should be ${JSON.stringify(expected)}, got:\n${run.stdout}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("SandboxRunner: every host-set ceiling reports itself as one...", async () => {
  const tmp = makeTmp();
  try {
    const memorySeed = join(tmp, "memory-kind.json");
    writeFileSync(memorySeed, sandboxSeedConfig({
      "/main.js": [
        'import { runScript } from "goccia";',
        'const child = runScript("/hog.js");',
        'console.log("memory:" + child.failureKind + ":" + child.ok);',
      ].join("\n"),
      "/hog.js": "const a = new Array(4000000); console.log(a.length);\n",
    }));

    const timeoutSeed = join(tmp, "timeout-kind.json");
    writeFileSync(timeoutSeed, sandboxSeedConfig({
      "/main.js": [
        'import { runScript } from "goccia";',
        'const child = runScript("/spin.js");',
        'console.log("timeout:" + child.failureKind + ":" + child.ok);',
      ].join("\n"),
      "/spin.js": "while (true) {}\n",
    }));

    // The child sandbox inherits what the parent VFS has left, so a parent
    // that has spent its node quota cannot seed one at all. That refusal
    // raises out of the nested call rather than returning a result, so it is
    // classified by the frame that called it — here, /filler.js.
    const quotaSeed = join(tmp, "quota-kind.json");
    writeFileSync(quotaSeed, sandboxSeedConfig({
      "/main.js": [
        'import { runScript } from "goccia";',
        'const filler = runScript("/filler.js");',
        'console.log("quota:" + filler.failureKind + ":" + filler.ok);',
        'console.log("filler:" + filler.stdout.trim());',
      ].join("\n"),
      "/filler.js": [
        'import fs from "fs";',
        'import { runScript } from "goccia";',
        'let code = "";',
        'for (const name of Array.from({ length: 200 }, (_, i) => "/f" + i + ".txt")) {',
        '  try { fs.writeFileSync(name, "x"); } catch (error) { code = error.code; }',
        "}",
        'console.log("filled:" + code);',
        'runScript("/ok.js", { sandbox: true, seed: ["/ok.js"] });',
        'console.log("unreachable");',
      ].join("\n"),
      "/ok.js": 'console.log("child ran");\n',
    }));

    for (const mode of ["interpreted", "bytecode"] as const) {
      const memory = runSandboxKinds(memorySeed, mode, ["--max-memory=8388608"]);
      if (memory.stdout !== "memory:resource-limit:false")
        throw new Error(`SandboxRunner ${mode} memory ceiling should classify as a resource limit, got:\n${memory.combined}`);

      // The parent shares the deadline it hands the child, so it may be out
      // of time itself once the child is refused. Its output is captured
      // either way; the exit code is not the assertion.
      const timeout = runSandboxKinds(timeoutSeed, mode, [
        "--compat-while-loops",
        "--timeout=300",
      ]);
      if (!containsLine(timeout.stdout, "timeout:timeout:false"))
        throw new Error(`SandboxRunner ${mode} deadline should classify as a timeout, got:\n${timeout.combined}`);

      const quota = runSandboxKinds(quotaSeed, mode, ["--fs-node-limit=32"]);
      const expectedQuota = ["quota:resource-limit:false", "filler:filled:ENOSPC"].join("\n");
      if (quota.stdout !== expectedQuota)
        throw new Error(`SandboxRunner ${mode} filesystem quota should classify as a resource limit, got:\n${quota.combined}`);
    }
  } finally {
    clean(tmp);
  }
});

if (sectionFailures.length > 0) {
  console.error(`\n${sectionFailures.length} section(s) failed:`);
  for (const failure of sectionFailures) {
    const message =
      failure.error instanceof Error ? failure.error.message : String(failure.error);
    console.error(`  - ${failure.name}`);
    console.error(`      ${message}`);
  }
  process.exit(1);
}

console.log("\nAll test-cli-apps.ts tests passed.");
