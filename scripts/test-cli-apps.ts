Warning: truncated output (original token count: 51018)
Total output lines: 4697

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
  chmodSync,
  symlinkSync,
} from "fs";
import { join, resolve } from "path";
import { pathToFileURL } from "url";
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
      throw new Error(`test262 timeout fixture should exit 1, got ${timeoutProc.exitCode}: ${…31018 tokens truncated…ndboxRunner: bytecode uses the same sandbox runtime modules...");
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
  const res = await $`echo 'fetch("http://blocked.test");' | ${LOADER} --allowed-host=example.com 2>&1`.nothrow();
  if (res.exitCode === 0) throw new Error("Fetch to unlisted host should fail");
  if (!res.text().includes("blocked.test")) throw new Error(`Error should mention blocked host, got: ${res.text()}`);
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
  const { exitCode, json, stderr } = await runLoaderJsonAsync(
    `const response = await fetch("${baseUrl}/", { method: "HEAD" });\nresponse.status;\n`,
    ["--compat-asi", "--allowed-host=127.0.0.1"],
    { timeout: 10_000 },
  );
  if (exitCode !== 0) throw new Error(`Local fetch should exit 0, got ${exitCode}: ${stderr}`);
  if (json.ok !== true) throw new Error(`Local fetch JSON ok should be true, got ${json.ok}`);
  if (json.files?.[0]?.result !== 200) throw new Error(`Local fetch status should be 200, got ${json.files?.[0]?.result}`);
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
    if (!containsLine(proc.stdout.toString(), pathToFileURL(dependency).href))
      throw new Error(`Virtual bare resolution skipped aliases: ${proc.stdout.toString()}`);
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

