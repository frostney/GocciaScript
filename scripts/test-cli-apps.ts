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
} from "./test-cli/binaries";
import { containsLine, normalizeLineEndings, runLoaderJson } from "./test-cli/assertions";
import { makeTmpFactory, clean } from "./test-cli/tmpdir";

const makeTmp = makeTmpFactory("goccia-apps-");

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

console.log("Loader: JSON output (interpreted)...");
{
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
}

console.log("Loader: JSON output (bytecode)...");
{
  const { json } = runLoaderJson("console.log('hi'); 2 + 2;\n", ["--mode=bytecode"]);
  const file = json.files?.[0];
  if (json.ok !== true) throw new Error(`Bytecode JSON ok should be true, got ${json.ok}`);
  if (file?.result !== 4) throw new Error(`Bytecode JSON file result should be 4, got ${file?.result}`);
  if (!json.output?.includes("hi")) throw new Error(`Bytecode JSON output should contain "hi"`);
  if (!json.stdout?.includes("hi")) throw new Error(`Bytecode JSON stdout should contain "hi"`);
  if (typeof json.stderr !== "string") throw new Error("Bytecode JSON stderr should always be present");
  if (typeof json.memory?.gc?.peakLiveBytes !== "number") throw new Error("Bytecode JSON memory.gc.peakLiveBytes should be present");
}

console.log("Loader: bytecode TypedArray.from roots mapper during iterator GC...");
{
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
}

console.log("Loader: JSON undefined result...");
{
  const { json } = runLoaderJson("undefined;\n");
  if (json.ok !== true) throw new Error(`JSON undefined run should succeed, got ${json.ok}`);
  if (json.files?.[0]?.error !== null) throw new Error("JSON undefined result should not imply an error");
  if (json.files?.[0]?.result !== null) throw new Error(`JSON undefined result should serialize as null, got ${json.files?.[0]?.result}`);
}

console.log("Loader: JSON stdout/stderr split...");
{
  const { json } = runLoaderJson("console.log('out'); console.error('err'); 1;\n");
  if (!json.stdout?.includes("out")) throw new Error(`JSON stdout should contain "out", got ${json.stdout}`);
  if (!json.stderr?.includes("err")) throw new Error(`JSON stderr should contain "err", got ${json.stderr}`);
  if (!json.output?.includes("out") || !json.output?.includes("Error: err")) {
    throw new Error(`JSON output should include both streams, got ${json.output}`);
  }
}

console.log("Loader: JSON multi-file structure...");
{
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
}

console.log("Loader: JSON source-load failure stays per-file...");
{
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
}

console.log("Loader: compact-json omits build, memory, stdout, stderr...");
{
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
}

console.log("Loader: compact-json error path omits build, memory, stdout, stderr...");
{
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
}

console.log("Loader: compact-json multi-file omits build, memory, stdout, stderr...");
{
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
}

console.log("Loader: parallel human-readable output preserves console output...");
{
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
}

// -- --print --------------------------------------------------------------------

console.log("Loader: silent (no result line) by default...");
{
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
}

console.log("Loader: --print emits bare value (no 'Result:' prefix)...");
{
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
}

console.log("Loader: --print emits 'undefined' when result is undefined...");
{
  const proc = Bun.spawnSync([LOADER, "--print"], {
    stdin: new TextEncoder().encode("undefined;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Loader --print undefined exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  const out = proc.stdout.toString();
  if (!containsLine(out, "undefined"))
    throw new Error(`Loader --print should emit "undefined" (matches node -p), got: ${out}`);
}

console.log("Loader: --print honored from goccia.json...");
{
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
}

// ============================================================================
// GocciaScriptLoaderBare
// ============================================================================

console.log("Bare Loader: stdin default path...");
{
  const proc = Bun.spawnSync([BARE, "--print"], {
    stdin: new TextEncoder().encode("const x = 2 + 2; x;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare stdin exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "4") throw new Error(`Bare stdin expected 4, got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: stdin dash path...");
{
  const proc = Bun.spawnSync([BARE, "--print", "-"], {
    stdin: new TextEncoder().encode("21 * 2;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare stdin dash exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "42") throw new Error(`Bare stdin dash expected 42, got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: quoted source names survive argv parsing...");
{
  const sourceName = 'quoted "source".js';
  const proc = Bun.spawnSync([BARE, `--source-name=${sourceName}`], {
    stdin: new TextEncoder().encode("const = ;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const output = proc.stdout.toString() + proc.stderr.toString();
  if (proc.exitCode === 0 || !output.includes(sourceName))
    throw new Error(`Bare quoted source name was not preserved: ${output}`);
}

console.log("Bare Loader: input file...");
{
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
}

console.log("Bare Loader: print global...");
{
  const proc = Bun.spawnSync([BARE], {
    stdin: new TextEncoder().encode("print('hello', 7); undefined;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare print exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "hello 7") throw new Error(`Bare print expected hello 7, got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: --stack-size bounds deep non-tail recursion with RangeError...");
{
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
}

console.log("Bare Loader: proper tail calls reuse the frame (deep strict tail recursion completes)...");
{
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
}

console.log("Bare Loader: tail-call optimization stays strict-mode only...");
{
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
}

console.log("Bare Loader: native re-entry recursion throws RangeError instead of crashing...");
{
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
}

console.log("Bare Loader: no runtime globals...");
{
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
}

console.log("Bare Loader: module source type...");
{
  const proc = Bun.spawnSync([BARE, "--print", "--source-type=module"], {
    stdin: new TextEncoder().encode("this === undefined;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare module source exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "true") throw new Error(`Bare module source expected true, got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: .mjs module inference...");
{
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
}

// --mode option: bare loader defaults to interpreter mode; both values must execute.
console.log("Bare Loader: --mode=interpreted...");
{
  const proc = Bun.spawnSync([BARE, "--print", "--mode=interpreted"], {
    stdin: new TextEncoder().encode("21 * 2;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --mode=interpreted exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "42") throw new Error(`Bare --mode=interpreted expected 42, got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: interpreted for-in scope survives Goccia.gc...");
{
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
}

console.log("Bare Loader: --mode=bytecode...");
{
  const proc = Bun.spawnSync([BARE, "--print", "--mode=bytecode"], {
    stdin: new TextEncoder().encode("21 * 2;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --mode=bytecode exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "42") throw new Error(`Bare --mode=bytecode expected 42, got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: bytecode top-level declarations back globalThis...");
{
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
}

console.log("Bare Loader: test262 host marker is hidden by default...");
{
  const proc = Bun.spawnSync([BARE], {
    stdin: new TextEncoder().encode("print(typeof Goccia.test262Host); print(typeof Goccia.test262);\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0)
    throw new Error(`Bare default test262 marker probe exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (normalizeLineEndings(proc.stdout.toString()).trim() !== "undefined\nundefined")
    throw new Error(`Bare default should hide test262 host hooks, got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: --test262-host exposes Goccia test262 hooks...");
{
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
}

console.log("test262 runner: engine timeout is classified as TIMEOUT...");
{
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
}

console.log("Bare Loader: --test262-host child realms expose host records...");
{
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
}

console.log("Bare Loader: --test262-host child realm globals expose host hooks...");
{
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
}

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

console.log("Bare Loader: bytecode --test262-host eval is direct eval...");
{
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
}

console.log("Bare Loader: bytecode --test262-host eval keeps sloppy var declarations in the caller environment...");
{
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
}

console.log("Bare Loader: bytecode --test262-host eval exposes later sloppy vars to existing closures...");
{
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
}

console.log("Bare Loader: bytecode --test262-host eval var declarations shadow outer upvalues...");
{
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
}

console.log("Bare Loader: bytecode --test262-host eval keeps nested variable environments isolated...");
{
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
}

console.log("Bare Loader: bytecode --test262-host eval preserves lexical upvalue precedence...");
{
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
}

console.log("Bare Loader: bytecode assignment retains its resolved eval environment reference...");
{
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
}

console.log("Bare Loader: --test262-host generator parameter eval uses the parameter var environment...");
{
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
}

console.log("Bare Loader: --test262-host Annex B eval preserves with-object properties...");
{
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
}

console.log("Bare Loader: --test262-host eval reports strict delete identifier as SyntaxError...");
{
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
}

console.log("Bare Loader: --test262-host eval validates destructuring pattern early errors...");
{
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
}

console.log("Bare Loader: --test262-host eval rejects arguments in class field initializers...");
{
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
}

console.log("Bare Loader: --test262-host eval rejects arguments in generator method defaults...");
{
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
}

console.log("Bare Loader: --test262-host eval super permissions stop at ordinary function boundary...");
{
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
}

console.log("Bare Loader: bytecode --test262-host eval inherits arrow lexical super and new.target...");
{
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
}

console.log("Bare Loader: bytecode direct eval creates top-level sloppy var...");
{
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
}

console.log("Bare Loader: bytecode module direct eval keeps module this binding...");
{
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
}

console.log("Bare Loader: --mode default is interpreted...");
{
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
}

console.log("Bare Loader: --mode invalid value rejected...");
{
  const proc = Bun.spawnSync([BARE, "--mode=foo"], {
    stdin: new TextEncoder().encode("1;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode === 0) throw new Error(`Bare --mode=foo should fail, got exit 0`);
  const stderr = proc.stderr.toString();
  if (!stderr.includes("Invalid --mode value: foo"))
    throw new Error(`Bare --mode=foo should report invalid value, got stderr: ${stderr}`);
}

// -- --print --------------------------------------------------------------------

console.log("Bare Loader: silent by default (no script result printed)...");
{
  const proc = Bun.spawnSync([BARE], {
    stdin: new TextEncoder().encode("const r = 'this contains the word error'; r;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare default exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString() !== "")
    throw new Error(`Bare default should produce empty stdout (matches node script.js), got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: --print emits bare value...");
{
  const proc = Bun.spawnSync([BARE, "--print"], {
    stdin: new TextEncoder().encode("const r = 'this contains the word error'; r;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --print exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "this contains the word error")
    throw new Error(`Bare --print should emit bare value, got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: --print emits 'undefined' (matches node -p)...");
{
  const proc = Bun.spawnSync([BARE, "--print"], {
    stdin: new TextEncoder().encode("undefined;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --print undefined exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "undefined")
    throw new Error(`Bare --print undefined should emit "undefined", got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: print() output independent of --print flag...");
{
  const proc = Bun.spawnSync([BARE], {
    stdin: new TextEncoder().encode("print('explicit'); 'last value';\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare default+print() exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "explicit")
    throw new Error(`Bare default should emit print() output but no result, got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: --help documents --print...");
{
  const proc = Bun.spawnSync([BARE, "--help"], {
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare --help exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (!proc.stdout.toString().includes("--print"))
    throw new Error(`Bare --help should document --print, got: ${proc.stdout.toString()}`);
}

// -- Promise.then microtask drain (Bare) ----------------------------------------
// Top-level .then callbacks must fire via WaitForRuntimeIdle post-execution drain.
// Regression: ExecuteProgram freed the bytecode module before the drain, leaving
// closures with dangling template pointers (Range check error on FCode access).

console.log("Bare Loader: Promise.then drain (interpreted)...");
{
  const proc = Bun.spawnSync([BARE, "--mode=interpreted"], {
    stdin: new TextEncoder().encode('Promise.resolve(42).then(v => print("then-" + v));\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare Promise drain interpreted exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "then-42")
    throw new Error(`Bare Promise drain interpreted expected then-42, got: ${proc.stdout.toString()}`);
}

console.log("Bare Loader: Promise.then drain (bytecode)...");
{
  const proc = Bun.spawnSync([BARE, "--mode=bytecode"], {
    stdin: new TextEncoder().encode('Promise.resolve(42).then(v => print("then-" + v));\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Bare Promise drain bytecode exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (proc.stdout.toString().trim() !== "then-42")
    throw new Error(`Bare Promise drain bytecode expected then-42, got: ${proc.stdout.toString()}`);
}

// -- Promise.then microtask drain (Loader) --------------------------------------

console.log("Loader: Promise.then drain (interpreted)...");
{
  const proc = Bun.spawnSync([LOADER, "--mode=interpreted"], {
    stdin: new TextEncoder().encode('Promise.resolve(42).then(v => console.log("then-" + v));\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Loader Promise drain interpreted exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (!proc.stdout.toString().includes("then-42"))
    throw new Error(`Loader Promise drain interpreted expected then-42, got: ${proc.stdout.toString()}`);
}

console.log("Loader: Promise.then drain (bytecode)...");
{
  const proc = Bun.spawnSync([LOADER, "--mode=bytecode"], {
    stdin: new TextEncoder().encode('Promise.resolve(42).then(v => console.log("then-" + v));\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) throw new Error(`Loader Promise drain bytecode exited ${proc.exitCode}: ${proc.stderr.toString()}`);
  if (!proc.stdout.toString().includes("then-42"))
    throw new Error(`Loader Promise drain bytecode expected then-42, got: ${proc.stdout.toString()}`);
}

console.log("Loader: --audit-log records capability decisions with source locations...");
{
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
}

console.log("Loader: --audit-log fails closed when the output cannot be opened...");
{
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
}

console.log("Loader: --log and --audit-log reject the same output path...");
{
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
}

// -- --global / --globals -------------------------------------------------------

console.log("Loader: --host-environment module controls time, zone, and random streams...");
{
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
}

console.log("Loader: --global option...");
{
  const { json } = runLoaderJson("x + y;\n", ["--global", "x=10", "--global", "y=20"]);
  if (json.files?.[0]?.result !== 30) throw new Error(`--global x+y should be 30, got ${json.files?.[0]?.result}`);
}

console.log("Loader: ShadowRealm.importValue inherits the host module aliases...");
{
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
}

console.log("Loader: --globals file...");
{
  const tmp = makeTmp();
  try {
    const globalsPath = join(tmp, "globals.json");
    writeFileSync(globalsPath, JSON.stringify({ name: "goccia" }));
    const { json } = runLoaderJson("name;\n", [`--globals=${globalsPath}`, "--mode=bytecode"]);
    if (json.files?.[0]?.result !== "goccia") throw new Error(`--globals should set name to "goccia", got ${json.files?.[0]?.result}`);
  } finally {
    clean(tmp);
  }
}

console.log("Loader: --globals JSON5 file...");
{
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
}

console.log("Loader: --globals TOML file...");
{
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
}

console.log("Loader: --global overrides --globals file...");
{
  const tmp = makeTmp();
  try {
    const globalsPath = join(tmp, "globals.json");
    writeFileSync(globalsPath, JSON.stringify({ name: "goccia" }));
    const { json } = runLoaderJson("name;\n", [`--globals=${globalsPath}`, "--global", "name=override"]);
    if (json.files?.[0]?.result !== "override") throw new Error(`--global should override --globals, got ${json.files?.[0]?.result}`);
  } finally {
    clean(tmp);
  }
}

console.log("Loader: --globals from JS module...");
{
  const tmp = makeTmp();
  try {
    const moduleJsPath = join(tmp, "module.js");
    writeFileSync(moduleJsPath, 'export const name = "module-value";\n');
    const { json } = runLoaderJson("name;\n", [`--globals=${moduleJsPath}`]);
    if (json.files?.[0]?.result !== "module-value") throw new Error(`--globals JS module should set name, got ${json.files?.[0]?.result}`);
  } finally {
    clean(tmp);
  }
}

console.log("Loader: --global cannot override built-in...");
{
  const res = await $`echo '1;' | ${LOADER} --global console=1 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Overriding built-in should fail");
  if (!res.text().includes("Cannot override built-in global")) throw new Error("Should mention 'Cannot override built-in global'");
}

// -- Coverage -------------------------------------------------------------------

console.log("Loader: coverage summary...");
{
  const out = await $`echo 'const x = 1 + 2; x;' | ${LOADER} --coverage 2>&1`.text();
  if (!out.includes("Coverage Summary:")) throw new Error(`Expected "Coverage Summary:", got: ${out}`);
}

console.log("Loader: coverage --output=json not corrupted...");
{
  const { json } = runLoaderJson("const x = 1 + 2;\nx;\n", ["--coverage"]);
  if (json.ok === undefined) throw new Error(`Coverage --output=json should produce valid JSON with ok field`);
}

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
    if (!readFileSync(jsxLcovPath, "utf-8").includes("BRDA:3,")) throw new Error("JSX LCOV should have branch on line 3");

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

console.log("TestRunner: Vitest-compatible snapshot lifecycle (interpreted + bytecode)...");
{
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
}

console.log("TestRunner: JSON multi-file structure...");
{
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
}

console.log("TestRunner: --output=json emits structured JSON envelope to stdout...");
{
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
}

console.log("TestRunner: --output=json keeps stdout clean when test throws...");
{
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
}

console.log("TestRunner: --output=json keeps stdout clean when script logs to console...");
{
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
}

console.log("TestRunner: --output=json keeps stdout clean when --coverage is enabled...");
{
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
}

console.log("TestRunner: --output=compact-json omits build, memory, stdout, stderr...");
{
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
}

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
      if (json.totalBenchmarks !== 3)
        throw new Error(`Module-only microbench JSON should contain totalBenchmarks: 3, got ${json.totalBenchmarks}`);
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

console.log("REPL: banner (interpreted)...");
{
  const out = await $`echo '' | ${REPL} 2>&1`.text();
  if (!out.includes("Goccia REPL")) throw new Error(`Banner should contain "Goccia REPL", got: ${out.slice(0, 200)}`);
  if (!out.includes("(interpreted)")) throw new Error(`Banner should contain "(interpreted)", got: ${out.slice(0, 200)}`);
}

console.log("REPL: banner (bytecode)...");
{
  const out = await $`echo '' | ${REPL} --mode=bytecode 2>&1`.text();
  if (!out.includes("(bytecode)")) throw new Error(`Bytecode banner should contain "(bytecode)", got: ${out.slice(0, 200)}`);
}

console.log("REPL: expression evaluation...");
{
  const out = await $`echo '2 + 2;' | ${REPL} 2>&1`.text();
  if (!out.includes("4")) throw new Error(`Expression 2+2 should produce 4, got: ${out}`);
}

console.log("REPL: ASI mode...");
{
  const out = await $`printf 'const x = 5\nx\n' | ${REPL} --compat-asi 2>&1`.text();
  if (!out.includes("5")) throw new Error(`ASI mode should produce 5, got: ${out}`);
}

console.log("REPL: error recovery...");
{
  const out = await $`printf 'const x = ;\n2 + 2;\n' | ${REPL} 2>&1`.text();
  if (!out.includes("4")) throw new Error(`After error, second expression should produce 4, got: ${out}`);
}

console.log("REPL: bytecode evaluation...");
{
  const out = await $`echo '2 + 2;' | ${REPL} --mode=bytecode 2>&1`.text();
  if (!out.includes("4")) throw new Error(`Bytecode 2+2 should produce 4, got: ${out}`);
}

console.log("REPL: repeated tagged template execution (interpreted + bytecode)...");
{
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
}

// ============================================================================
// GocciaSandboxRunner
// ============================================================================

console.log("SandboxRunner: deterministic nested engines use stable distinct streams...");
{
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
}

console.log("SandboxRunner: inline seeds, fs, $, runScript, and diffs...");
{
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
}

console.log("SandboxRunner: fs Stats expose realm-owned lazy Date metadata in every execution mode...");
{
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
        if (!containsLine(`\n${stdout}`, expected))
          throw new Error(`SandboxRunner ${label} Stats stdout should include ${expected}, got: ${stdout}`);
      }
    }
  } finally {
    clean(tmp);
  }
}

console.log("SandboxRunner: metadata diffing is opt-in and separate from content changes...");
{
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
}

console.log("SandboxRunner: fs errors are Node-shaped in every execution mode...");
{
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
}

console.log("SandboxRunner: aliases and import maps resolve sandbox module paths...");
{
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
    if (!containsLine(`\n${aliasStdout}`, "alias-ok"))
      throw new Error(`SandboxRunner alias import should print alias-ok, got: ${aliasStdout}`);

    const importMapProc = Bun.spawnSync(
      [SANDBOXRUNNER, "/map-main.js", `--seed-config=${seed}`, "--source-type=module", `--import-map=${importMap}`],
      { stdout: "pipe", stderr: "pipe" },
    );
    const importMapStdout = normalizeLineEndings(importMapProc.stdout.toString());
    if (importMapProc.exitCode !== 0)
      throw new Error(`SandboxRunner import map should exit 0, got ${importMapProc.exitCode}: ${importMapProc.stderr.toString()}`);
    if (!containsLine(`\n${importMapStdout}`, "map-ok"))
      throw new Error(`SandboxRunner import map should print map-ok, got: ${importMapStdout}`);
    if (!containsLine(`\n${importMapStdout}`, "relative-ok"))
      throw new Error(`SandboxRunner import map should print relative-ok, got: ${importMapStdout}`);
  } finally {
    clean(tmp);
  }
}

console.log("SandboxRunner: Windows-style sandbox paths normalize to virtual paths...");
{
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
      if (!containsLine(`\n${stdout}`, expected))
        throw new Error(`SandboxRunner Windows-style paths should include ${JSON.stringify(expected)}, got: ${stdout}`);
    }
    const helloCount = stdout.split("\n").filter((line) => line === "hello").length;
    if (helloCount !== 3)
      throw new Error(`SandboxRunner Windows-style paths should print hello three times, got ${helloCount} in: ${stdout}`);
  } finally {
    clean(tmp);
  }
}

console.log("SandboxRunner: seed config rejects null source values...");
{
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
}

console.log("SandboxRunner: unified diff includes deleted seeded files...");
{
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
}

console.log("SandboxRunner: bytecode uses the same sandbox runtime modules...");
{
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
    if (!containsLine(`\n${stdout}`, "bytecode"))
      throw new Error(`SandboxRunner bytecode stdout should include bytecode, got: ${stdout}`);
    if (!containsLine(`\n${stdout}`, "byte-child"))
      throw new Error(`SandboxRunner bytecode nested stdout should include byte-child, got: ${stdout}`);
    if (!containsLine(`\n${stdout}`, "true") || !containsLine(`\n${stdout}`, "false"))
      throw new Error(`SandboxRunner bytecode nested diff/isolation booleans missing, got: ${stdout}`);
    const changes = JSON.parse(readFileSync(diff, "utf-8")).changes;
    if (!changes.some((c: any) => c.kind === "create" && c.path === "/byte.txt"))
      throw new Error(`SandboxRunner bytecode diff should include /byte.txt create, got ${JSON.stringify(changes)}`);
    if (changes.some((c: any) => c.path === "/byte-child.txt"))
      throw new Error(`SandboxRunner bytecode parent diff should not include nested child writes, got ${JSON.stringify(changes)}`);
  } finally {
    clean(tmp);
  }
}

console.log("SandboxRunner: nested sandbox execution seeds from parent VFS without leaking writes...");
{
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
      if (!containsLine(`\n${stdout}`, expected))
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
}

console.log("SandboxRunner: seed config imports host paths relative to the config file...");
{
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
    if (!containsLine(`\n${stdout}`, "from-host"))
      throw new Error(`SandboxRunner host seed stdout should include imported host text, got: ${stdout}`);
    if (!containsLine(`\n${stdout}`, "from-file-target"))
      throw new Error(`SandboxRunner host seed stdout should include file copied under trailing slash target, got: ${stdout}`);
    if (!containsLine(`\n${stdout}`, "from-existing-dir"))
      throw new Error(`SandboxRunner host seed stdout should include file copied under existing target directory, got: ${stdout}`);
    if (!containsLine(`\n${stdout}`, "3"))
      throw new Error(`SandboxRunner host seed stdout should include base64 byte length, got: ${stdout}`);
  } finally {
    clean(tmp);
  }
}

console.log("SandboxRunner: --audit-log reports root escapes without changing clamped access...");
{
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
      if (!containsLine(`\n${proc.stdout.toString()}`, "inside-jail"))
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
}

console.log("SandboxRunner: seed directory rejects nested host symlink (no leak)...");
{
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
}

console.log("SandboxRunner: direct seed argument rejects host symlink (no leak)...");
{
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
}

console.log("SandboxRunner: seed-config from directory rejects nested host symlink (no leak)...");
{
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
}

console.log("SandboxRunner: trailing slash on a symlinked-directory seed is still rejected (no leak)...");
{
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
}

console.log("SandboxRunner: Windows directory junction seed is rejected (no leak)...");
{
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
}

// ============================================================================
// --allowed-host option
// ============================================================================

console.log("Loader: --allowed-host blocks unlisted host...");
{
  const tmp = makeTmp();
  try {
    const audit = join(tmp, "blocked-fetch-audit.jsonl");
    const res = await $`echo 'fetch("http://blocked.test");' | ${LOADER} --allowed-host=example.com --audit-log=${audit} 2>&1`.nothrow();
    if (res.exitCode === 0) throw new Error("Fetch to unlisted host should fail");
    if (!res.text().includes("blocked.test")) throw new Error(`Error should mention blocked host, got: ${res.text()}`);
    const events = readJsonLines(audit);
    if (events.length !== 1 ||
        events[0].kind !== "fetch.host" ||
        events[0].decision !== "deny" ||
        events[0].subject !== "http://blocked.test")
      throw new Error(`Blocked fetch audit event mismatch: ${JSON.stringify(events)}`);
  } finally {
    clean(tmp);
  }
}

console.log("Loader: no --allowed-host blocks all fetch...");
{
  const res = await $`echo 'fetch("http://example.com");' | ${LOADER} 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Fetch without --allowed-host should fail");
  if (!res.text().includes("allowed hosts")) throw new Error(`Error should mention allowed hosts, got: ${res.text()}`);
}

console.log("Loader: --allowed-host multiple hosts...");
{
  // Both hosts in the list; blocked.test is not
  const res = await $`echo 'fetch("http://blocked.test");' | ${LOADER} --allowed-host=example.com --allowed-host=other.com 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Fetch to unlisted host should fail with multiple --allowed-host");
  if (!res.text().includes("blocked.test")) throw new Error(`Error should mention blocked host, got: ${res.text()}`);
}

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

console.log("Loader: --multifile splits a single file into N section results...");
{
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
}

console.log("Loader: --multifile on stdin produces <stdin>[partN] entries...");
{
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
}

console.log("Loader: --multifile with no separator runs file as a single section...");
{
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
}

console.log("Loader: --multifile drops leading/trailing separators...");
{
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
}

console.log("Loader: --multifile dispatches sections in parallel with --jobs...");
{
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
}

console.log("Loader: --source-map with --multifile is rejected...");
{
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
}

console.log("TestRunner: --multifile splits a single test file into N file results...");
{
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
}

console.log("Bundler: --multifile compiles each section as a separate .gbc...");
{
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
}

console.log("Bundler: --multifile rejects --output=<file>...");
{
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
}

console.log("BenchmarkRunner: --multifile produces multiple file entries...");
{
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
}

console.log("Loader: goccia.json multifile=true works without --multifile flag...");
{
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
}

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

console.log("Loader: dynamic import and ShadowRealm use configured virtual modules...");
{
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
}

console.log("Loader: hierarchical virtual module addresses preserve canonical URLs...");
{
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
}

console.log("Loader: virtual import.meta.resolve uses aliases for bare specifiers...");
{
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
}

console.log("Loader: attributed virtual modules reinterpret their stored content...");
{
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
}

console.log("Loader: virtual definitions validate eagerly but JavaScript parses lazily...");
{
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
}

console.log("SandboxRunner: virtual modules share the CLI surface and cannot shadow host modules...");
{
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
}

console.log("\nAll test-cli-apps.ts tests passed.");
