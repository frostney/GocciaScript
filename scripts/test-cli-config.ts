#!/usr/bin/env bun
/**
 * test-cli-config.ts
 *
 * Config file loading and per-file config: goccia.json, goccia.toml,
 * goccia.json5, priority, discovery, timeout, stack-size, imports, extends,
 * TestRunner integration, per-file ASI/compat-var, multi-directory, CLI override.
 */

import { $ } from "bun";
import {
  mkdtempSync,
  writeFileSync,
  readFileSync,
  existsSync,
  rmSync,
  mkdirSync,
} from "fs";
import { join, resolve } from "path";
import { tmpdir } from "os";

const ext = process.platform === "win32" ? ".exe" : "";
const LOADER = `./build/GocciaScriptLoader${ext}`;
const REPL = `./build/GocciaREPL${ext}`;
const TESTRUNNER = `./build/GocciaTestRunner${ext}`;
const BUNDLER = `./build/GocciaBundler${ext}`;
const BENCHRUNNER = `./build/GocciaBenchmarkRunner${ext}`;

const makeTmp = () => mkdtempSync(join(tmpdir(), "goccia-cfg-"));
const clean = (d: string) => rmSync(d, { recursive: true, force: true });

/** Runs a binary with cwd set to a specific directory. */
function runCwd(
  bin: string,
  args: string[],
  cwd: string,
  opts?: { stdin?: string; expectFail?: boolean },
): { stdout: string; stderr: string; exitCode: number; combined: string } {
  const { stdin, expectFail = false } = opts ?? {};
  const absoluteBin = resolve(bin);
  const proc = Bun.spawnSync([absoluteBin, ...args], {
    stdin: stdin != null ? new TextEncoder().encode(stdin) : undefined,
    stdout: "pipe",
    stderr: "pipe",
    cwd,
    env: {
      ...process.env,
      GOCCIA_BENCH_CALIBRATION_MS: "50",
      GOCCIA_BENCH_ROUNDS: "3",
    } as Record<string, string>,
    timeout: 30_000,
  });
  const stdout = proc.stdout.toString();
  const stderr = proc.stderr.toString();
  const exitCode = proc.exitCode;
  const combined = stdout + stderr;
  if (expectFail && exitCode === 0)
    throw new Error(`Expected non-zero exit from ${bin} ${args.join(" ")}, got 0.\nstdout: ${stdout}\nstderr: ${stderr}`);
  if (!expectFail && exitCode !== 0)
    throw new Error(`${bin} ${args.join(" ")} exited with code ${exitCode}.\nstdout: ${stdout}\nstderr: ${stderr}`);
  return { stdout, stderr, exitCode, combined };
}

// -- goccia.json loading --------------------------------------------------------

console.log("goccia.json loading...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"asi": true, "mode": "bytecode"}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 2 + 2\nx\n");

    const out = await $`${LOADER} ${join(tmp, "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`goccia.json should enable bytecode, got: ${out}`);
    if (!out.includes("Result: 4")) throw new Error(`goccia.json should produce Result: 4, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- goccia.toml loading --------------------------------------------------------

console.log("goccia.toml loading...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.toml"), 'asi = true\nmode = "bytecode"\n');
    writeFileSync(join(tmp, "test.js"), "const x = 10\nx\n");

    const out = await $`${LOADER} ${join(tmp, "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`goccia.toml should enable bytecode, got: ${out}`);
    if (!out.includes("Result: 10")) throw new Error(`goccia.toml should produce Result: 10, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- goccia.json5 loading -------------------------------------------------------

console.log("goccia.json5 loading...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json5"), '{asi: true, mode: "bytecode"}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 10\nx\n");

    const out = await $`${LOADER} ${join(tmp, "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`goccia.json5 should enable bytecode, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- Priority: TOML > JSON5 > JSON ---------------------------------------------

console.log("Priority: TOML > JSON5 > JSON...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"mode": "interpreted", "asi": true}\n');
    writeFileSync(join(tmp, "goccia.json5"), '{asi: true, mode: "interpreted"}\n');
    writeFileSync(join(tmp, "goccia.toml"), 'asi = true\nmode = "bytecode"\n');
    writeFileSync(join(tmp, "test.js"), "const x = 1\nx\n");

    const out = await $`${LOADER} ${join(tmp, "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`TOML should win over JSON5 and JSON, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- Discovery from entry file directory ----------------------------------------

console.log("Discovery from entry file directory...");
{
  const tmp = makeTmp();
  try {
    mkdirSync(join(tmp, "src"), { recursive: true });
    writeFileSync(join(tmp, "goccia.json"), '{"asi": true, "mode": "bytecode"}\n');
    writeFileSync(join(tmp, "src", "test.js"), "const x = 7\nx\n");

    const out = await $`${LOADER} ${join(tmp, "src", "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`Config in parent should be discovered, got: ${out}`);
    if (!out.includes("Result: 7")) throw new Error(`Should produce Result: 7, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- Config timeout -------------------------------------------------------------

console.log("Config timeout...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"timeout": 50}\n');
    writeFileSync(
      join(tmp, "test.js"),
      "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n",
    );

    const res = await $`${LOADER} ${join(tmp, "test.js")} 2>&1`.nothrow();
    if (res.exitCode === 0) throw new Error("Timeout config should cause non-zero exit");
    if (!res.text().includes("timed out")) throw new Error(`Should mention "timed out", got: ${res.text()}`);
  } finally {
    clean(tmp);
  }
}

// -- Config stack-size ----------------------------------------------------------

console.log("Config stack-size...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"stack-size": 50, "asi": true}\n');
    writeFileSync(
      join(tmp, "test.js"),
      "let n=0; const f=()=>{n++;f()}; try{f()}catch(e){console.log(n)};\n",
    );

    const out = await $`${LOADER} ${join(tmp, "test.js")} 2>&1`.text();
    if (!out.includes("50")) throw new Error(`Stack-size config should limit to 50, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- Config without imports field -----------------------------------------------

console.log("Config without imports field...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"asi": true}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 42\nx\n");

    const out = await $`${LOADER} ${join(tmp, "test.js")} 2>&1`.text();
    if (!out.includes("Result: 42")) throw new Error(`Config without imports should work, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- Config with imports (module resolution) ------------------------------------

console.log("Config with imports...");
{
  const tmp = makeTmp();
  try {
    mkdirSync(join(tmp, "lib"), { recursive: true });
    writeFileSync(join(tmp, "lib", "utils.js"), "export const value = 99;\n");
    writeFileSync(
      join(tmp, "goccia.json"),
      '{"asi": true, "imports": {"utils": "./lib/utils.js"}}\n',
    );
    writeFileSync(join(tmp, "test.js"), 'import { value } from "utils"\nvalue\n');

    const out = runCwd(LOADER, ["test.js"], tmp);
    if (!out.combined.includes("Result: 99")) throw new Error(`Imports should resolve, got: ${out.combined}`);
  } finally {
    clean(tmp);
  }
}

// -- Config extends (base + child) ----------------------------------------------

console.log("Config extends...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "base.json"), '{"asi": true}\n');
    writeFileSync(join(tmp, "goccia.json"), '{"extends": "base.json", "mode": "bytecode"}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 5\nx\n");

    const out = await $`${LOADER} ${join(tmp, "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`Extends should enable bytecode, got: ${out}`);
    if (!out.includes("Result: 5")) throw new Error(`Extends should inherit ASI, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- Config extends from subdirectory -------------------------------------------

console.log("Config extends from subdirectory...");
{
  const tmp = makeTmp();
  try {
    mkdirSync(join(tmp, "tests", "asi"), { recursive: true });
    writeFileSync(join(tmp, "goccia.json"), '{"asi": true}\n');
    writeFileSync(
      join(tmp, "tests", "asi", "goccia.json"),
      '{"extends": "../../goccia.json", "mode": "bytecode"}\n',
    );
    writeFileSync(join(tmp, "tests", "asi", "test.js"), "const x = 99\nx\n");

    const out = await $`${LOADER} ${join(tmp, "tests", "asi", "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`Subdirectory extends should enable bytecode, got: ${out}`);
    if (!out.includes("Result: 99")) throw new Error(`Subdirectory extends should inherit ASI, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- TestRunner respects config -------------------------------------------------

console.log("TestRunner respects config...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"asi": true, "mode": "bytecode"}\n');
    writeFileSync(
      join(tmp, "test.js"),
      [
        'describe("config", () => {',
        '  test("works", () => {',
        "    expect(2 + 2).toBe(4)",
        "  })",
        "})",
      ].join("\n") + "\n",
    );

    const resultsPath = join(tmp, "results.json");
    runCwd(TESTRUNNER, ["test.js", "--no-progress", `--output=${resultsPath}`], tmp);
    const results = readFileSync(resultsPath, "utf-8");
    if (!results.includes('"mode": "bytecode"')) throw new Error(`TestRunner results should show bytecode mode`);
    if (!results.includes('"passed": 1')) throw new Error(`TestRunner results should show 1 passed`);
    const resultsJson = JSON.parse(results);
    if (typeof resultsJson.build?.version !== "string") throw new Error("TestRunner JSON build.version should be present");
    if (resultsJson.fileName !== undefined) throw new Error(`TestRunner JSON fileName should only be present per-file, got ${resultsJson.fileName}`);
    if (!Array.isArray(resultsJson.output)) throw new Error("TestRunner JSON output should be an array");
    if (resultsJson.error !== null) throw new Error("TestRunner JSON error should be null");
    if (typeof resultsJson.timing?.total_ns !== "number") throw new Error("TestRunner JSON timing.total_ns should be present");
    if (typeof resultsJson.files?.[0]?.timing?.total_ns !== "number") throw new Error("TestRunner JSON per-file timing.total_ns should be present");
    if (typeof resultsJson.memory?.gc?.liveBytes !== "number") throw new Error("TestRunner JSON memory.gc.liveBytes should be present");
    if (typeof resultsJson.workers?.used !== "number") throw new Error("TestRunner JSON workers.used should be present");
  } finally {
    clean(tmp);
  }
}

console.log("TestRunner JSON load errors include per-file error...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "bad.js"), "const broken = ;\n");

    for (const mode of ["interpreted", "bytecode"]) {
      const resultsPath = join(tmp, `load-error-${mode}.json`);
      const args = ["bad.js", "--no-progress", `--output=${resultsPath}`];
      if (mode === "bytecode") args.push("--mode=bytecode");
      runCwd(TESTRUNNER, args, tmp, { expectFail: true });

      const resultsJson = JSON.parse(readFileSync(resultsPath, "utf-8"));
      const file = resultsJson.files?.[0];
      const result = resultsJson.results?.[0];
      if (resultsJson.ok !== false) throw new Error(`TestRunner ${mode} load error should mark ok=false`);
      if (file?.ok !== false) throw new Error(`TestRunner ${mode} load error file should mark ok=false`);
      if (typeof file?.error?.message !== "string" || file.error.message.length === 0)
        throw new Error(`TestRunner ${mode} load error should include files[].error.message`);
      if (file.errorMessage !== file.error.message)
        throw new Error(`TestRunner ${mode} errorMessage should match shared error message`);
      if (typeof result?.error?.message !== "string" || result.error.message.length === 0)
        throw new Error(`TestRunner ${mode} load error should include results[].error.message`);
    }
  } finally {
    clean(tmp);
  }
}

// -- Per-file ASI config across all apps ----------------------------------------

console.log("Per-file ASI config across all apps...");
{
  const tmp = makeTmp();
  try {
    // ASI subdirectory
    const asiDir = join(tmp, "asi");
    mkdirSync(asiDir);
    writeFileSync(join(asiDir, "goccia.json"), '{"asi": true}\n');
    writeFileSync(join(asiDir, "test.js"), "const x = 42\nx\n");
    writeFileSync(
      join(asiDir, "test-runner.js"),
      [
        'describe("asi", () => {',
        '  test("works", () => {',
        "    const v = 1",
        "    expect(v).toBe(1)",
        "  })",
        "})",
      ].join("\n") + "\n",
    );
    writeFileSync(
      join(asiDir, "bench.js"),
      [
        'suite("asi", () => {',
        '  bench("sum", {',
        "    run: () => 1 + 1",
        "  })",
        "})",
      ].join("\n") + "\n",
    );

    // Strict subdirectory (no config)
    const strictDir = join(tmp, "strict");
    mkdirSync(strictDir);
    writeFileSync(join(strictDir, "test.js"), "const y = 99;\ny;\n");
    writeFileSync(join(strictDir, "bad.js"), "const z = 1\nz\n");

    // Loader (interpreted)
    const loaderInterp = await $`${LOADER} ${join(asiDir, "test.js")} 2>&1`.text();
    if (!loaderInterp.includes("Result: 42")) throw new Error(`Loader interp ASI should produce 42, got: ${loaderInterp}`);

    const strictOk = await $`${LOADER} ${join(strictDir, "test.js")} 2>&1`.text();
    if (!strictOk.includes("Result: 99")) throw new Error(`Strict subdir should produce 99, got: ${strictOk}`);

    const strictBad = await $`${LOADER} ${join(strictDir, "bad.js")} 2>&1`.nothrow();
    if (!strictBad.text().includes("SyntaxError")) throw new Error("Strict subdir should reject missing semicolons");

    // Loader (bytecode)
    const loaderBc = await $`${LOADER} ${join(asiDir, "test.js")} --mode=bytecode 2>&1`.text();
    if (!loaderBc.includes("Result: 42")) throw new Error(`Loader bytecode ASI should produce 42, got: ${loaderBc}`);

    const strictBcBad = await $`${LOADER} ${join(strictDir, "bad.js")} --mode=bytecode 2>&1`.nothrow();
    if (!strictBcBad.text().includes("SyntaxError")) throw new Error("Strict bytecode should reject");

    // TestRunner (interpreted)
    const trInterp = await $`${TESTRUNNER} ${join(asiDir, "test-runner.js")} --no-progress 2>&1`.text();
    if (!trInterp.includes("Passed: 1")) throw new Error(`TestRunner interp ASI should pass, got: ${trInterp}`);

    // TestRunner (bytecode)
    const trBc = await $`${TESTRUNNER} ${join(asiDir, "test-runner.js")} --mode=bytecode --no-progress 2>&1`.text();
    if (!trBc.includes("Passed: 1")) throw new Error(`TestRunner bytecode ASI should pass, got: ${trBc}`);

    // Bundler
    await $`${BUNDLER} ${join(asiDir, "test.js")}`.quiet();
    if (!existsSync(join(asiDir, "test.gbc"))) throw new Error("Bundler ASI should compile");

    await $`${BUNDLER} ${join(strictDir, "test.js")}`.quiet();
    if (!existsSync(join(strictDir, "test.gbc"))) throw new Error("Bundler strict should compile");

    const bundleStrictBad = await $`${BUNDLER} ${join(strictDir, "bad.js")} 2>&1`.nothrow();
    if (bundleStrictBad.exitCode === 0) throw new Error("Bundler strict should reject bad.js");

    await $`${BUNDLER} ${join(strictDir, "bad.js")} --asi`.quiet();
    if (!existsSync(join(strictDir, "bad.gbc"))) throw new Error("Bundler --asi override should compile");

    // BenchmarkRunner (interpreted)
    const benchInterp = Bun.spawnSync(
      [resolve(BENCHRUNNER), join(asiDir, "bench.js"), "--no-progress"],
      {
        stdout: "pipe",
        stderr: "pipe",
        env: { ...process.env, GOCCIA_BENCH_CALIBRATION_MS: "50", GOCCIA_BENCH_ROUNDS: "3" } as Record<string, string>,
        timeout: 60_000,
      },
    );
    if (benchInterp.exitCode !== 0) throw new Error(`BenchmarkRunner interp ASI exited ${benchInterp.exitCode}: ${benchInterp.stderr.toString()}`);
    if (!benchInterp.stdout.toString().includes("asi")) throw new Error("BenchmarkRunner interp ASI should mention 'asi'");

    // BenchmarkRunner (bytecode)
    const benchBc = Bun.spawnSync(
      [resolve(BENCHRUNNER), join(asiDir, "bench.js"), "--mode=bytecode", "--no-progress"],
      {
        stdout: "pipe",
        stderr: "pipe",
        env: { ...process.env, GOCCIA_BENCH_CALIBRATION_MS: "50", GOCCIA_BENCH_ROUNDS: "3" } as Record<string, string>,
        timeout: 60_000,
      },
    );
    if (benchBc.exitCode !== 0) throw new Error(`BenchmarkRunner bytecode ASI exited ${benchBc.exitCode}: ${benchBc.stderr.toString()}`);
    if (!benchBc.stdout.toString().includes("asi")) throw new Error("BenchmarkRunner bytecode ASI should mention 'asi'");
  } finally {
    clean(tmp);
  }
}

// -- Per-file compat-var config across all apps ---------------------------------

console.log("Per-file compat-var config across all apps...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"compat-var": true}\n');
    writeFileSync(join(tmp, "test.js"), "var x = 10;\nx;\n");
    writeFileSync(
      join(tmp, "test-runner.js"),
      [
        "var y = 20;",
        'describe("var", () => {',
        '  test("works", () => {',
        "    expect(y).toBe(20);",
        "  });",
        "});",
      ].join("\n") + "\n",
    );
    writeFileSync(
      join(tmp, "bench.js"),
      [
        "var z = 1;",
        'suite("var", () => {',
        '  bench("add", {',
        "    run: () => z + 1,",
        "  });",
        "});",
      ].join("\n") + "\n",
    );

    // Loader (interpreted)
    const loaderOut = await $`${LOADER} ${join(tmp, "test.js")} 2>&1`.text();
    if (!loaderOut.includes("Result: 10")) throw new Error(`Loader interp compat-var should produce 10, got: ${loaderOut}`);

    // Loader (bytecode)
    const loaderBc = await $`${LOADER} ${join(tmp, "test.js")} --mode=bytecode 2>&1`.text();
    if (!loaderBc.includes("Result: 10")) throw new Error(`Loader bytecode compat-var should produce 10, got: ${loaderBc}`);

    // TestRunner (interpreted)
    const trInterp = await $`${TESTRUNNER} ${join(tmp, "test-runner.js")} --no-progress 2>&1`.text();
    if (!trInterp.includes("Passed: 1")) throw new Error(`TestRunner interp compat-var should pass, got: ${trInterp}`);

    // TestRunner (bytecode)
    const trBc = await $`${TESTRUNNER} ${join(tmp, "test-runner.js")} --mode=bytecode --no-progress 2>&1`.text();
    if (!trBc.includes("Passed: 1")) throw new Error(`TestRunner bytecode compat-var should pass, got: ${trBc}`);

    // Bundler
    await $`${BUNDLER} ${join(tmp, "test.js")}`.quiet();
    if (!existsSync(join(tmp, "test.gbc"))) throw new Error("Bundler compat-var should compile");

    // BenchmarkRunner (interpreted)
    const benchInterp = Bun.spawnSync(
      [resolve(BENCHRUNNER), join(tmp, "bench.js"), "--no-progress"],
      {
        stdout: "pipe",
        stderr: "pipe",
        env: { ...process.env, GOCCIA_BENCH_CALIBRATION_MS: "50", GOCCIA_BENCH_ROUNDS: "3" } as Record<string, string>,
        timeout: 60_000,
      },
    );
    if (benchInterp.exitCode !== 0) throw new Error(`BenchmarkRunner interp compat-var exited ${benchInterp.exitCode}: ${benchInterp.stderr.toString()}`);
    if (!benchInterp.stdout.toString().includes("var")) throw new Error("BenchmarkRunner interp compat-var should mention 'var'");

    // BenchmarkRunner (bytecode)
    const benchBc = Bun.spawnSync(
      [resolve(BENCHRUNNER), join(tmp, "bench.js"), "--mode=bytecode", "--no-progress"],
      {
        stdout: "pipe",
        stderr: "pipe",
        env: { ...process.env, GOCCIA_BENCH_CALIBRATION_MS: "50", GOCCIA_BENCH_ROUNDS: "3" } as Record<string, string>,
        timeout: 60_000,
      },
    );
    if (benchBc.exitCode !== 0) throw new Error(`BenchmarkRunner bytecode compat-var exited ${benchBc.exitCode}: ${benchBc.stderr.toString()}`);
    if (!benchBc.stdout.toString().includes("var")) throw new Error("BenchmarkRunner bytecode compat-var should mention 'var'");
  } finally {
    clean(tmp);
  }
}

// -- Multi-directory TestRunner (lenient + strict, both modes) ------------------

console.log("Multi-directory TestRunner...");
{
  const tmp = makeTmp();
  try {
    // Lenient subdirectory: ASI + compat-var
    const lenientDir = join(tmp, "lenient");
    mkdirSync(lenientDir);
    writeFileSync(join(lenientDir, "goccia.json"), '{"asi": true, "compat-var": true}\n');
    writeFileSync(
      join(lenientDir, "test.js"),
      [
        "var x = 1",
        'describe("lenient", () => {',
        '  test("asi + var", () => {',
        "    expect(x).toBe(1)",
        "  })",
        "})",
      ].join("\n") + "\n",
    );

    // Strict subdirectory (no config)
    const strictDir = join(tmp, "strict");
    mkdirSync(strictDir);
    writeFileSync(
      join(strictDir, "test.js"),
      [
        'describe("strict", () => {',
        '  test("works", () => {',
        "    expect(2 + 2).toBe(4);",
        "  });",
        "});",
      ].join("\n") + "\n",
    );

    // TestRunner interpreted: both subdirs
    const trInterp = await $`${TESTRUNNER} ${lenientDir} ${strictDir} --no-progress 2>&1`.text();
    if (!trInterp.includes("Passed: 2")) throw new Error(`TestRunner interp multi-dir should pass 2, got: ${trInterp}`);
    if (!trInterp.includes("Failed: 0")) throw new Error(`TestRunner interp multi-dir should fail 0, got: ${trInterp}`);

    // TestRunner bytecode: both subdirs
    const trBc = await $`${TESTRUNNER} ${lenientDir} ${strictDir} --mode=bytecode --no-progress 2>&1`.text();
    if (!trBc.includes("Passed: 2")) throw new Error(`TestRunner bytecode multi-dir should pass 2, got: ${trBc}`);
    if (!trBc.includes("Failed: 0")) throw new Error(`TestRunner bytecode multi-dir should fail 0, got: ${trBc}`);
  } finally {
    clean(tmp);
  }
}

// -- CLI flag overrides file config ---------------------------------------------

console.log("CLI flag overrides file config...");
{
  const tmp = makeTmp();
  const noConfigDir = makeTmp();
  try {
    // Config enables ASI
    writeFileSync(join(tmp, "goccia.json"), '{"asi": true}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 1\nx\n");

    // No config dir
    writeFileSync(join(noConfigDir, "test.js"), "const x = 1\nx\n");

    // File config ASI works
    const configOut = await $`${LOADER} ${join(tmp, "test.js")} 2>&1`.text();
    if (!configOut.includes("Result: 1")) throw new Error(`File config ASI should work, got: ${configOut}`);

    // Without config should fail
    const noConfigRes = await $`${LOADER} ${join(noConfigDir, "test.js")} 2>&1`.nothrow();
    if (!noConfigRes.text().includes("SyntaxError")) throw new Error("No config should reject");

    // CLI --asi overrides no-config
    const cliAsi = await $`${LOADER} ${join(noConfigDir, "test.js")} --asi 2>&1`.text();
    if (!cliAsi.includes("Result: 1")) throw new Error(`CLI --asi should override, got: ${cliAsi}`);

    // CLI --asi bytecode
    const cliAsiBc = await $`${LOADER} ${join(noConfigDir, "test.js")} --asi --mode=bytecode 2>&1`.text();
    if (!cliAsiBc.includes("Result: 1")) throw new Error(`CLI --asi bytecode should override, got: ${cliAsiBc}`);

    // CLI --mode=interpreted overrides config mode
    writeFileSync(join(tmp, "goccia.json"), '{"asi": true, "mode": "bytecode"}\n');
    const overrideMode = await $`${LOADER} ${join(tmp, "test.js")} --mode=interpreted 2>&1`.text();
    if (!overrideMode.includes("(interpreted)")) throw new Error(`CLI --mode=interpreted should override config, got: ${overrideMode}`);

    // Bundler CLI --asi overrides no-config
    await $`${BUNDLER} ${join(noConfigDir, "test.js")} --asi`.quiet();
    if (!existsSync(join(noConfigDir, "test.gbc"))) throw new Error("Bundler CLI --asi should compile");
  } finally {
    clean(tmp);
    clean(noConfigDir);
  }
}

// -- Config allowed-hosts ---------------------------------------------------------

console.log("Config allowed-hosts blocks unlisted host...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"allowed-hosts": ["example.com"]}\n');
    writeFileSync(join(tmp, "test.js"), 'fetch("http://blocked.test");\n');

    const res = runCwd(LOADER, ["test.js"], tmp, { expectFail: true });
    if (!res.combined.includes("blocked.test")) throw new Error(`Error should mention blocked host, got: ${res.combined}`);
  } finally {
    clean(tmp);
  }
}

console.log("Config allowed-hosts allows listed host...");
{
  const tmp = makeTmp();
  try {
    // 0.0.0.0:1 is unreachable, so the promise rejects with a network error
    // rather than a host-not-allowed TypeError.
    writeFileSync(join(tmp, "goccia.json"), '{"allowed-hosts": ["0.0.0.0"]}\n');
    writeFileSync(
      join(tmp, "test.js"),
      'const p = fetch("http://0.0.0.0:1/"); p.catch(() => {}); typeof p.then;\n',
    );

    const out = runCwd(LOADER, ["test.js"], tmp);
    if (!out.combined.includes("function")) throw new Error(`Allowed host should return promise, got: ${out.combined}`);
  } finally {
    clean(tmp);
  }
}

console.log("Config allowed-hosts per-file overrides root...");
{
  const tmp = makeTmp();
  try {
    // Root config allows example.com
    writeFileSync(join(tmp, "goccia.json"), '{"allowed-hosts": ["example.com"]}\n');
    // Subdirectory config allows only other.com
    const subDir = join(tmp, "sub");
    mkdirSync(subDir);
    writeFileSync(join(subDir, "goccia.json"), '{"allowed-hosts": ["other.com"]}\n');
    writeFileSync(join(subDir, "test.js"), 'fetch("http://example.com");\n');

    // example.com is NOT in the subdirectory config, so it should be blocked
    const res = runCwd(LOADER, [join(subDir, "test.js")], tmp, { expectFail: true });
    if (!res.combined.includes("example.com")) throw new Error(`Per-file config should override root, got: ${res.combined}`);
  } finally {
    clean(tmp);
  }
}

console.log("CLI --allowed-host overrides config allowed-hosts...");
{
  const tmp = makeTmp();
  try {
    // Config allows example.com
    writeFileSync(join(tmp, "goccia.json"), '{"allowed-hosts": ["example.com"]}\n');
    // Script fetches example.com — allowed by config, but NOT by CLI list
    writeFileSync(join(tmp, "test.js"), 'fetch("http://example.com");\n');

    // CLI specifies only other.test — CLI wins outright, so example.com
    // from config should be blocked.
    const res = runCwd(LOADER, ["test.js", "--allowed-host=other.test"], tmp, { expectFail: true });
    if (!res.combined.includes("example.com")) throw new Error(`CLI override should block config-only host, got: ${res.combined}`);
  } finally {
    clean(tmp);
  }
}

console.log("Config allowed-hosts empty array overrides parent via extends...");
{
  const tmp = makeTmp();
  try {
    // Base config allows example.com
    writeFileSync(join(tmp, "base.json"), '{"allowed-hosts": ["example.com"]}\n');
    // Child config extends base but explicitly empties allowed-hosts
    writeFileSync(join(tmp, "goccia.json"), '{"extends": "base.json", "allowed-hosts": []}\n');
    writeFileSync(join(tmp, "test.js"), 'fetch("http://example.com");\n');

    // Empty allowed-hosts in child should override parent — fetch blocked
    const res = runCwd(LOADER, ["test.js"], tmp, { expectFail: true });
    if (!res.combined.includes("allowed hosts")) throw new Error(`Empty allowed-hosts should block fetch, got: ${res.combined}`);
  } finally {
    clean(tmp);
  }
}

console.log("Config allowed-hosts TestRunner integration...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"allowed-hosts": ["example.com"]}\n');
    writeFileSync(
      join(tmp, "test.js"),
      [
        'describe("allowed-hosts", () => {',
        '  test("blocks unlisted host", () => {',
        '    expect(() => fetch("http://blocked.test")).toThrow(TypeError);',
        "  });",
        "});",
      ].join("\n") + "\n",
    );

    const out = runCwd(TESTRUNNER, ["test.js", "--no-progress"], tmp);
    if (!out.combined.includes("Passed: 1")) throw new Error(`TestRunner should pass with allowed-hosts config, got: ${out.combined}`);
  } finally {
    clean(tmp);
  }
}

// -- --config=<file> ------------------------------------------------------------
//
// The --config flag points at a specific config file and skips auto-discovery
// entirely.  All three supported extensions must work because the parser is
// chosen by extension.

console.log("--config=<file> loads .json explicitly...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    // Script lives in tmp with no goccia.* anywhere on its parent chain.
    writeFileSync(join(tmp, "test.js"), "const x = 11\nx\n");
    // Config lives in a sibling directory; auto-discovery would never find it.
    writeFileSync(join(cfgDir, "custom.json"), '{"asi": true, "mode": "bytecode"}\n');

    const out = await $`${LOADER} ${join(tmp, "test.js")} --config=${join(cfgDir, "custom.json")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config=.json should enable bytecode, got: ${out}`);
    if (!out.includes("Result: 11")) throw new Error(`--config=.json should enable ASI (Result: 11), got: ${out}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

console.log("--config=<file> loads .toml explicitly...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    writeFileSync(join(tmp, "test.js"), "const x = 21\nx\n");
    writeFileSync(join(cfgDir, "custom.toml"), 'asi = true\nmode = "bytecode"\n');

    const out = await $`${LOADER} ${join(tmp, "test.js")} --config=${join(cfgDir, "custom.toml")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config=.toml should enable bytecode, got: ${out}`);
    if (!out.includes("Result: 21")) throw new Error(`--config=.toml should enable ASI (Result: 21), got: ${out}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

console.log("--config=<file> loads .json5 explicitly...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    writeFileSync(join(tmp, "test.js"), "const x = 31\nx\n");
    writeFileSync(join(cfgDir, "custom.json5"), '{asi: true, mode: "bytecode"}\n');

    const out = await $`${LOADER} ${join(tmp, "test.js")} --config=${join(cfgDir, "custom.json5")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config=.json5 should enable bytecode, got: ${out}`);
    if (!out.includes("Result: 31")) throw new Error(`--config=.json5 should enable ASI (Result: 31), got: ${out}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

console.log("--config=<file> with relative path resolves against cwd...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "test.js"), "const x = 41\nx\n");
    writeFileSync(join(tmp, "custom.json"), '{"asi": true, "mode": "bytecode"}\n');

    // Relative path; cwd is tmp.
    const out = runCwd(LOADER, ["test.js", "--config=./custom.json"], tmp);
    if (!out.combined.includes("(bytecode)")) throw new Error(`Relative --config should resolve, got: ${out.combined}`);
    if (!out.combined.includes("Result: 41")) throw new Error(`Relative --config should enable ASI, got: ${out.combined}`);
  } finally {
    clean(tmp);
  }
}

// -- --config=<dir> -------------------------------------------------------------
//
// A directory path expands to goccia.{toml,json5,json} inside that directory,
// preserving the same priority as auto-discovery.  The lookup must NOT walk
// upward — that's the point of an explicit override.

console.log("--config=<dir> finds goccia.json...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    writeFileSync(join(tmp, "test.js"), "const x = 51\nx\n");
    writeFileSync(join(cfgDir, "goccia.json"), '{"asi": true, "mode": "bytecode"}\n');

    const out = await $`${LOADER} ${join(tmp, "test.js")} --config=${cfgDir} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config=<dir> with goccia.json should enable bytecode, got: ${out}`);
    if (!out.includes("Result: 51")) throw new Error(`--config=<dir> should enable ASI, got: ${out}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

console.log("--config=<dir> respects priority TOML > JSON5 > JSON...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    writeFileSync(join(tmp, "test.js"), "const x = 61\nx\n");
    // All three present; TOML must win (matches auto-discovery priority).
    writeFileSync(join(cfgDir, "goccia.json"), '{"asi": true, "mode": "interpreted"}\n');
    writeFileSync(join(cfgDir, "goccia.json5"), '{asi: true, mode: "interpreted"}\n');
    writeFileSync(join(cfgDir, "goccia.toml"), 'asi = true\nmode = "bytecode"\n');

    const out = await $`${LOADER} ${join(tmp, "test.js")} --config=${cfgDir} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config=<dir> should pick TOML over JSON5/JSON, got: ${out}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

console.log("--config=<dir> with trailing slash works...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    writeFileSync(join(tmp, "test.js"), "const x = 71\nx\n");
    writeFileSync(join(cfgDir, "goccia.toml"), 'asi = true\nmode = "bytecode"\n');

    // Some shells/users will pass the directory with a trailing slash.
    const out = await $`${LOADER} ${join(tmp, "test.js")} --config=${cfgDir + "/"} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config=<dir>/ should still find config, got: ${out}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

console.log("--config=<dir> does not walk upward...");
{
  const tmp = makeTmp();
  try {
    // Place a config in tmp; pass an empty subdirectory as --config.
    // Auto-discovery would walk up and find tmp/goccia.toml, but the
    // explicit directory form must NOT — it should error out.
    writeFileSync(join(tmp, "goccia.toml"), 'asi = true\nmode = "bytecode"\n');
    const empty = join(tmp, "empty");
    mkdirSync(empty);
    writeFileSync(join(tmp, "test.js"), "const x = 81\nx\n");

    const res = await $`${LOADER} ${join(tmp, "test.js")} --config=${empty} 2>&1`.nothrow();
    if (res.exitCode === 0) throw new Error("--config=<empty-dir> should not silently walk up to parent");
    if (!res.text().includes("No goccia.")) throw new Error(`Error should mention missing goccia.* file, got: ${res.text()}`);
  } finally {
    clean(tmp);
  }
}

// -- --config error handling ----------------------------------------------------

console.log("--config=<missing path> is a hard error...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "test.js"), "const x = 91;\nx;\n");

    const res = await $`${LOADER} ${join(tmp, "test.js")} --config=${join(tmp, "does-not-exist.json")} 2>&1`.nothrow();
    if (res.exitCode === 0) throw new Error("--config pointing to nonexistent path should fail");
    if (!res.text().includes("not found")) throw new Error(`Error should mention "not found", got: ${res.text()}`);
  } finally {
    clean(tmp);
  }
}

// -- --config skips auto-discovery ---------------------------------------------
//
// With --config given, the root-config walk-up MUST be skipped.  We prove
// this by placing a hostile goccia.json near the entry file (which would
// flip mode to interpreted) and showing that the explicit config wins.

console.log("--config skips auto-discovery of nearby goccia.*...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    // Hostile config alongside the script; auto-discovery would pick this up.
    writeFileSync(join(tmp, "goccia.json"), '{"asi": true, "mode": "interpreted"}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 101\nx\n");
    // Explicit config selects bytecode.
    writeFileSync(join(cfgDir, "good.toml"), 'asi = true\nmode = "bytecode"\n');

    const out = await $`${LOADER} ${join(tmp, "test.js")} --config=${join(cfgDir, "good.toml")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config should skip nearby goccia.json, got: ${out}`);
    if (!out.includes("Result: 101")) throw new Error(`--config should still apply ASI, got: ${out}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

// -- CLI args still beat --config -----------------------------------------------

console.log("CLI flags override values from --config...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    writeFileSync(join(tmp, "test.js"), "const x = 111\nx\n");
    // --config says interpreted...
    writeFileSync(join(cfgDir, "custom.toml"), 'asi = true\nmode = "interpreted"\n');

    // ...but a direct --mode=bytecode on the CLI must win.
    const out = await $`${LOADER} ${join(tmp, "test.js")} --config=${join(cfgDir, "custom.toml")} --mode=bytecode 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`CLI --mode should override --config value, got: ${out}`);
    if (!out.includes("Result: 111")) throw new Error(`ASI from --config should still apply, got: ${out}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

// -- --config across the other CLI tools ---------------------------------------
//
// The flag lives in the shared base class, so smoke-test that each consumer
// actually honors it.  The Loader is covered above; here we hit the rest.

console.log("--config works on TestRunner...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    writeFileSync(join(cfgDir, "ci.toml"), 'asi = true\nmode = "bytecode"\n');
    writeFileSync(
      join(tmp, "test.js"),
      [
        'describe("config-flag", () => {',
        '  test("works", () => {',
        "    expect(2 + 2).toBe(4)",
        "  })",
        "})",
      ].join("\n") + "\n",
    );

    const out = runCwd(TESTRUNNER, [join(tmp, "test.js"), "--no-progress", `--config=${join(cfgDir, "ci.toml")}`], tmp);
    if (!out.combined.includes("Passed: 1")) throw new Error(`TestRunner --config should pass, got: ${out.combined}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

console.log("--config works on Bundler...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    // Bad-without-ASI source; --config supplies ASI so compile must succeed.
    writeFileSync(join(tmp, "test.js"), "const x = 1\nx\n");
    writeFileSync(join(cfgDir, "goccia.json"), '{"asi": true}\n');

    // Sanity: without --config, compile rejects (no goccia.* near tmp).
    const noCfg = await $`${BUNDLER} ${join(tmp, "test.js")} 2>&1`.nothrow();
    if (noCfg.exitCode === 0) throw new Error("Bundler without ASI/--config should reject missing semicolons");

    // With --config=<dir>, compile succeeds.
    await $`${BUNDLER} ${join(tmp, "test.js")} --config=${cfgDir}`.quiet();
    if (!existsSync(join(tmp, "test.gbc"))) throw new Error("Bundler --config=<dir> should compile");
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

console.log("--config works on BenchmarkRunner...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    writeFileSync(join(cfgDir, "goccia.toml"), 'asi = true\n');
    writeFileSync(
      join(tmp, "bench.js"),
      [
        // No semicolons — proves --config-supplied ASI is in effect.
        'suite("config-flag", () => {',
        '  bench("sum", {',
        "    run: () => 1 + 1",
        "  })",
        "})",
      ].join("\n") + "\n",
    );

    const proc = Bun.spawnSync(
      [resolve(BENCHRUNNER), join(tmp, "bench.js"), "--no-progress", `--config=${cfgDir}`],
      {
        stdout: "pipe",
        stderr: "pipe",
        env: { ...process.env, GOCCIA_BENCH_CALIBRATION_MS: "50", GOCCIA_BENCH_ROUNDS: "3" } as Record<string, string>,
        timeout: 60_000,
      },
    );
    if (proc.exitCode !== 0)
      throw new Error(`BenchmarkRunner --config exited ${proc.exitCode}: ${proc.stderr.toString()}`);
    if (!proc.stdout.toString().includes("config-flag"))
      throw new Error(`BenchmarkRunner --config output should mention 'config-flag', got: ${proc.stdout.toString()}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

console.log("\nAll test-cli-config.ts tests passed.");
