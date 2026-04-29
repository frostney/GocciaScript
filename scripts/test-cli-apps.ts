#!/usr/bin/env bun
/**
 * test-cli-apps.ts
 *
 * App-specific features: GocciaScriptLoader (JSON output, --global/--globals,
 * coverage, source maps), GocciaBundler (compile, roundtrip, stdin, directory,
 * .gbc rejection, source maps), GocciaBenchmarkRunner (file, stdin, bytecode),
 * GocciaREPL (banner, evaluation, ASI, error recovery, bytecode).
 */

import { $ } from "bun";
import {
  mkdtempSync,
  writeFileSync,
  readFileSync,
  existsSync,
  rmSync,
  mkdirSync,
  chmodSync,
} from "fs";
import { join, resolve } from "path";
import { tmpdir } from "os";

const ext = process.platform === "win32" ? ".exe" : "";
const LOADER = `./build/GocciaScriptLoader${ext}`;
const REPL = `./build/GocciaREPL${ext}`;
const TESTRUNNER = `./build/GocciaTestRunner${ext}`;
const BUNDLER = `./build/GocciaBundler${ext}`;
const BENCHRUNNER = `./build/GocciaBenchmarkRunner${ext}`;

const makeTmp = () => mkdtempSync(join(tmpdir(), "goccia-apps-"));
const clean = (d: string) => rmSync(d, { recursive: true, force: true });

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
  if (file?.ok !== ok) throw new Error(`${label} ok should be ${ok}, got ${file?.ok}`);
  if (typeof file?.stdout !== "string") throw new Error(`${label} stdout should always be present`);
  if (typeof file?.stderr !== "string") throw new Error(`${label} stderr should always be present`);
  if (!Array.isArray(file?.output)) throw new Error(`${label} output should be an array`);
  if (typeof file?.timing?.total_ns !== "number") throw new Error(`${label} timing.total_ns should be present`);
  if ("total_ms" in file.timing) throw new Error(`${label} timing should not include millisecond fields`);
  if (ok && file.error !== null) throw new Error(`${label} error should be null`);
}

// ============================================================================
// GocciaScriptLoader
// ============================================================================

// -- JSON output (interpreted + bytecode) ---------------------------------------

console.log("Loader: JSON output (interpreted)...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json"], {
    stdin: new TextEncoder().encode("console.log('hi'); 2 + 2;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
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
  const proc = Bun.spawnSync([LOADER, "--output=json", "--mode=bytecode"], {
    stdin: new TextEncoder().encode("console.log('hi'); 2 + 2;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
  const file = json.files?.[0];
  if (json.ok !== true) throw new Error(`Bytecode JSON ok should be true, got ${json.ok}`);
  if (file?.result !== 4) throw new Error(`Bytecode JSON file result should be 4, got ${file?.result}`);
  if (!json.output?.includes("hi")) throw new Error(`Bytecode JSON output should contain "hi"`);
  if (!json.stdout?.includes("hi")) throw new Error(`Bytecode JSON stdout should contain "hi"`);
  if (typeof json.stderr !== "string") throw new Error("Bytecode JSON stderr should always be present");
  if (typeof json.memory?.gc?.peakLiveBytes !== "number") throw new Error("Bytecode JSON memory.gc.peakLiveBytes should be present");
}

console.log("Loader: JSON undefined result...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json"], {
    stdin: new TextEncoder().encode("undefined;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
  if (json.ok !== true) throw new Error(`JSON undefined run should succeed, got ${json.ok}`);
  if (json.files?.[0]?.error !== null) throw new Error("JSON undefined result should not imply an error");
  if (json.files?.[0]?.result !== null) throw new Error(`JSON undefined result should serialize as null, got ${json.files?.[0]?.result}`);
}

console.log("Loader: JSON stdout/stderr split...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json"], {
    stdin: new TextEncoder().encode("console.log('out'); console.error('err'); 1;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
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
    const allocatedDuringRun =
      json.files[0].memory.gc.allocatedDuringRunBytes + json.files[1].memory.gc.allocatedDuringRunBytes;
    if (json.memory.gc.allocatedDuringRunBytes !== allocatedDuringRun)
      throw new Error("Loader top-level worker memory should aggregate per-file worker memory");
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

// -- --global / --globals -------------------------------------------------------

console.log("Loader: --global flag...");
{
  const proc = Bun.spawnSync([LOADER, "--global", "x=10", "--global", "y=20", "--output=json"], {
    stdin: new TextEncoder().encode("x + y;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
  if (json.files?.[0]?.result !== 30) throw new Error(`--global x+y should be 30, got ${json.files?.[0]?.result}`);
}

console.log("Loader: --globals file...");
{
  const tmp = makeTmp();
  try {
    const globalsPath = join(tmp, "globals.json");
    writeFileSync(globalsPath, JSON.stringify({ name: "goccia" }));
    const proc = Bun.spawnSync([LOADER, `--globals=${globalsPath}`, "--output=json", "--mode=bytecode"], {
      stdin: new TextEncoder().encode("name;\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    const json = JSON.parse(proc.stdout.toString());
    if (json.files?.[0]?.result !== "goccia") throw new Error(`--globals should set name to "goccia", got ${json.files?.[0]?.result}`);
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
    const proc = Bun.spawnSync([LOADER, `--globals=${globalsPath}`, "--global", "name=override", "--output=json"], {
      stdin: new TextEncoder().encode("name;\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    const json = JSON.parse(proc.stdout.toString());
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
    const proc = Bun.spawnSync([LOADER, `--globals=${moduleJsPath}`, "--output=json"], {
      stdin: new TextEncoder().encode("name;\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    const json = JSON.parse(proc.stdout.toString());
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
  const proc = Bun.spawnSync([LOADER, "--coverage", "--output=json"], {
    stdin: new TextEncoder().encode("const x = 1 + 2;\nx;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const firstLine = proc.stdout.toString().trimStart().split("\n")[0];
  if (!firstLine.startsWith("{")) throw new Error(`Coverage --output=json first line should start with "{", got: ${firstLine}`);
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
    const roundtripOut = await $`${LOADER} ${singleGbc} 2>&1`.text();
    if (!roundtripOut.includes("Result: 4")) throw new Error(`Roundtrip should produce Result: 4, got: ${roundtripOut}`);

    console.log("Bundler: custom --output path...");
    const customOut = join(tmp, "custom.gbc");
    const customSrc = join(tmp, "custom.js");
    writeFileSync(customSrc, "const y = 3 + 3;\ny;\n");
    await $`${BUNDLER} ${customSrc} --output=${customOut}`.quiet();
    if (!existsSync(customOut)) throw new Error("Custom --output .gbc should exist");

    console.log("Bundler: stdin compile with --output...");
    const stdinOut = join(tmp, "stdin.gbc");
    await $`echo 'const z = 5 + 5; z;' | ${BUNDLER} --output=${stdinOut}`.quiet();
    if (!existsSync(stdinOut)) throw new Error("Stdin --output .gbc should exist");
    const stdinRoundtrip = await $`${LOADER} ${stdinOut} 2>&1`.text();
    if (!stdinRoundtrip.includes("Result: 10")) throw new Error(`Stdin roundtrip should produce Result: 10, got: ${stdinRoundtrip}`);

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

    console.log("Bundler: --source-map flag...");
    const smSrc = join(tmp, "sm.jsx");
    writeFileSync(smSrc, jsxSource);
    await $`${BUNDLER} ${smSrc} --source-map`.quiet();
    const smMap = join(tmp, "sm.jsx.map");
    if (!existsSync(join(tmp, "sm.gbc"))) throw new Error("--source-map: .gbc should exist");
    if (!existsSync(smMap)) throw new Error("--source-map: .map should exist");
    assertValidSourceMap(smMap);

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
    if (existsSync(join(tmp, "no-sm.jsx.map"))) throw new Error("No .map file should exist without --source-map");

    console.log("Bundler: stdin --source-map --output...");
    const stdinSmOut = join(tmp, "stdin-sm.gbc");
    await $`echo ${jsxSource} | ${BUNDLER} --source-map --output=${stdinSmOut}`.quiet();
    const stdinSmMap = stdinSmOut + ".map";
    if (!existsSync(stdinSmOut)) throw new Error("Stdin --source-map: .gbc should exist");
    if (!existsSync(stdinSmMap)) throw new Error("Stdin --source-map: .gbc.map should exist");
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
    const stdinSource = 'suite("stdin", () => { bench("sum", { run: () => 1 + 1 }); });\n';

    console.log("BenchmarkRunner: file benchmark (interpreted)...");
    const fileOut = join(tmp, "file-interp.json");
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "benchmarks/fibonacci.js", "--no-progress", "--format=json", `--output=${fileOut}`],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode !== 0) throw new Error(`File benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const fileJson = readFileSync(fileOut, "utf-8");
    if (!fileJson.includes('"file":')) throw new Error('File JSON should contain "file":');
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
        [resolve(BENCHRUNNER), "benchmarks/fibonacci.js", "--no-progress", "--format=json", `--output=${fileBcOut}`, "--mode=bytecode"],
        { stdout: "pipe", stderr: "pipe", env: benchEnv, timeout: 120_000 },
      );
      if (proc.exitCode !== 0) throw new Error(`Bytecode file benchmark exit ${proc.exitCode}: ${proc.stderr.toString()}`);
    }
    const fileBcJson = readFileSync(fileBcOut, "utf-8");
    if (!fileBcJson.includes('"file":')) throw new Error('Bytecode file JSON should contain "file":');
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

    console.log("BenchmarkRunner: file benchmark JSON output...");
    if (!fileJson.includes('"totalBenchmarks":')) throw new Error('JSON should contain totalBenchmarks');

    console.log("BenchmarkRunner: multi-file JSON structure...");
    const benchA = join(tmp, "bench-a.js");
    const benchB = join(tmp, "bench-b.js");
    const multiBenchOut = join(tmp, "bench-multi.json");
    writeFileSync(benchA, 'suite("a", () => { bench("one", { run: () => 1 + 1 }); });\n');
    writeFileSync(benchB, 'suite("b", () => { bench("two", { run: () => 2 + 2 }); });\n');
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), benchA, benchB, "--no-progress", "--jobs=2", "--format=json", `--output=${multiBenchOut}`],
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
      assertCommonJsonFile(json.files[0], "Benchmark first file", benchA);
      assertCommonJsonFile(json.files[1], "Benchmark second file", benchB);
      if (json.files[0].benchmarks?.[0]?.name !== "one") throw new Error(`Benchmark first file entry mismatch: ${JSON.stringify(json.files[0].benchmarks)}`);
      if (json.files[1].benchmarks?.[0]?.name !== "two") throw new Error(`Benchmark second file entry mismatch: ${JSON.stringify(json.files[1].benchmarks)}`);
      if (json.files[0].memory !== null || json.files[1].memory !== null)
        throw new Error("Benchmark multi-file per-file memory should be null when top-level memory is aggregated");
    }

    console.log("BenchmarkRunner: benchmark failure JSON output...");
    const failBench = join(tmp, "benchmark-fail.js");
    const failOut = join(tmp, "benchmark-fail.json");
    writeFileSync(failBench, 'suite("fail", () => { bench("boom", { run: () => { throw new Error("boom"); } }); });\n');
    {
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), failBench, "--no-progress", "--format=json", `--output=${failOut}`],
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

    console.log("BenchmarkRunner: callback timeout is enforced...");
    {
      const timeoutSource = [
        'suite("limit", () => {',
        '  bench("loop", { run: () => { while (true) {} } });',
        "});",
        "",
      ].join("\n");
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--no-progress", "--timeout=1"],
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
        [resolve(BENCHRUNNER), "--no-progress", "--format=json", `--output=${stdinOutPath}`],
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

    console.log("BenchmarkRunner: registered callbacks survive repeated runs...");
    const repeatedRunOutPath = join(tmp, "repeated-run.json");
    {
      const repeatedRunSource = [
        'suite("twice", () => {',
        '  bench("sum", { run: () => 1 + 1 });',
        "});",
        "runBenchmarks();",
        "Goccia.gc();",
        "",
      ].join("\n");
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--no-progress", "--format=json", `--output=${repeatedRunOutPath}`],
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
        [resolve(BENCHRUNNER), "--no-progress", "--format=json", `--output=${stdinBcOutPath}`, "--mode=bytecode"],
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
      const asyncGeneratorSource = [
        'suite("async generator", () => {',
        "  const source = { async *values() { yield 1; yield 2; } };",
        '  bench("consume", {',
        "    run: async () => {",
        "      let sum = 0;",
        "      for await (const value of source.values()) sum = sum + value;",
        "      return sum;",
        "    },",
        "  });",
        "});",
        "",
      ].join("\n");
      const proc = Bun.spawnSync(
        [resolve(BENCHRUNNER), "--no-progress", "--format=json", `--output=${asyncGeneratorBcOutPath}`, "--mode=bytecode"],
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
        [resolve(BENCHRUNNER), "--no-progress", "--format=json", `--output=${emptyBcOutPath}`, "--mode=bytecode"],
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
  const out = await $`printf 'const x = 5\nx\n' | ${REPL} --asi 2>&1`.text();
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

// ============================================================================
// --allowed-host flag
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

console.log("Loader: HTTPS fetch smoke with --allowed-host...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json", "--asi", "--allowed-host=www.gstatic.com"], {
    stdin: new TextEncoder().encode(
      'const response = await fetch("https://www.gstatic.com/generate_204", { method: "HEAD" });\nresponse.status;\n',
    ),
    stdout: "pipe",
    stderr: "pipe",
    timeout: 10_000,
  });
  if (proc.exitedDueToTimeout) throw new Error("HTTPS fetch timed out after 10 seconds");
  if (proc.exitCode !== 0) throw new Error(`HTTPS fetch should exit 0, got ${proc.exitCode}: ${proc.stderr.toString()}`);
  const json = JSON.parse(proc.stdout.toString());
  if (json.ok !== true) throw new Error(`HTTPS fetch JSON ok should be true, got ${json.ok}`);
  if (json.files?.[0]?.result !== 204) throw new Error(`HTTPS fetch status should be 204, got ${json.files?.[0]?.result}`);
}

console.log("\nAll test-cli-apps.ts tests passed.");
