#!/usr/bin/env bun
/**
 * test-cli-config.ts
 *
 * Config file loading and per-file config: goccia.json, goccia.toml,
 * goccia.json5, priority, discovery, timeout, stack-size, imports, extends,
 * TestRunner integration, per-file engine flags, multi-directory, CLI override.
 */

import { $ } from "bun";
import {
  writeFileSync,
  readFileSync,
  existsSync,
  mkdirSync,
} from "fs";
import { join, resolve } from "path";
import {
  LOADER,
  REPL,
  TESTRUNNER,
  BUNDLER,
  BENCHRUNNER,
} from "./test-cli/binaries";
import { containsLine } from "./test-cli/assertions";
import { makeTmpFactory, clean } from "./test-cli/tmpdir";

const makeTmp = makeTmpFactory("goccia-cfg-");

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

type CompatAcrossAppsCase = {
  name: string;
  config: string;
  loaderSource: string;
  loaderExpectedLine: string;
  testRunnerSource: string;
  benchSource: string;
  benchNeedle: string;
};

function microbenchScript(lines: string[]): string {
  return [
    'import("goccia:microbench").then(({ bench, group }) => {',
    ...lines.map((line) => `  ${line}`),
    "});",
    "",
  ].join("\n");
}

async function runCompatAcrossAppsCase(testCase: CompatAcrossAppsCase): Promise<void> {
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), testCase.config);
    writeFileSync(join(tmp, "test.js"), testCase.loaderSource);
    writeFileSync(join(tmp, "test-runner.js"), testCase.testRunnerSource);
    writeFileSync(join(tmp, "bench.js"), testCase.benchSource);

    const loaderOut = await $`${LOADER} --print ${join(tmp, "test.js")} 2>&1`.text();
    if (!containsLine(loaderOut, testCase.loaderExpectedLine))
      throw new Error(`Loader interp ${testCase.name} should produce ${testCase.loaderExpectedLine} on its own line, got: ${loaderOut}`);

    const loaderBc = await $`${LOADER} --print ${join(tmp, "test.js")} --mode=bytecode 2>&1`.text();
    if (!containsLine(loaderBc, testCase.loaderExpectedLine))
      throw new Error(`Loader bytecode ${testCase.name} should produce ${testCase.loaderExpectedLine} on its own line, got: ${loaderBc}`);

    const trInterp = await $`${TESTRUNNER} ${join(tmp, "test-runner.js")} --no-progress 2>&1`.text();
    if (!trInterp.includes("Passed: 1"))
      throw new Error(`TestRunner interp ${testCase.name} should pass, got: ${trInterp}`);

    const trBc = await $`${TESTRUNNER} ${join(tmp, "test-runner.js")} --mode=bytecode --no-progress 2>&1`.text();
    if (!trBc.includes("Passed: 1"))
      throw new Error(`TestRunner bytecode ${testCase.name} should pass, got: ${trBc}`);

    await $`${BUNDLER} ${join(tmp, "test.js")}`.quiet();
    if (!existsSync(join(tmp, "test.gbc")))
      throw new Error(`Bundler ${testCase.name} should compile`);

    const benchInterp = Bun.spawnSync(
      [resolve(BENCHRUNNER), join(tmp, "bench.js"), "--no-progress"],
      {
        stdout: "pipe",
        stderr: "pipe",
        env: { ...process.env, GOCCIA_BENCH_CALIBRATION_MS: "50", GOCCIA_BENCH_ROUNDS: "3" } as Record<string, string>,
        timeout: 60_000,
      },
    );
    if (benchInterp.exitCode !== 0)
      throw new Error(`BenchmarkRunner interp ${testCase.name} exited ${benchInterp.exitCode}: ${benchInterp.stderr.toString()}`);
    if (!benchInterp.stdout.toString().includes(testCase.benchNeedle))
      throw new Error(`BenchmarkRunner interp ${testCase.name} should mention '${testCase.benchNeedle}'`);

    const benchBc = Bun.spawnSync(
      [resolve(BENCHRUNNER), join(tmp, "bench.js"), "--mode=bytecode", "--no-progress"],
      {
        stdout: "pipe",
        stderr: "pipe",
        env: { ...process.env, GOCCIA_BENCH_CALIBRATION_MS: "50", GOCCIA_BENCH_ROUNDS: "3" } as Record<string, string>,
        timeout: 60_000,
      },
    );
    if (benchBc.exitCode !== 0)
      throw new Error(`BenchmarkRunner bytecode ${testCase.name} exited ${benchBc.exitCode}: ${benchBc.stderr.toString()}`);
    if (!benchBc.stdout.toString().includes(testCase.benchNeedle))
      throw new Error(`BenchmarkRunner bytecode ${testCase.name} should mention '${testCase.benchNeedle}'`);
  } finally {
    clean(tmp);
  }
}

// -- goccia.json loading --------------------------------------------------------

console.log("goccia.json loading...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"compat-asi": true, "mode": "bytecode"}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 2 + 2\nx\n");

    const out = await $`${LOADER} --print ${join(tmp, "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`goccia.json should enable bytecode, got: ${out}`);
    if (!containsLine(out, "4")) throw new Error(`goccia.json should produce 4 on its own line, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- goccia.toml loading --------------------------------------------------------

console.log("goccia.toml loading...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.toml"), 'compat-asi = true\nmode = "bytecode"\n');
    writeFileSync(join(tmp, "test.js"), "const x = 10\nx\n");

    const out = await $`${LOADER} --print ${join(tmp, "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`goccia.toml should enable bytecode, got: ${out}`);
    if (!containsLine(out, "10")) throw new Error(`goccia.toml should produce 10 on its own line, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- goccia.json5 loading -------------------------------------------------------

console.log("goccia.json5 loading...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json5"), '{"compat-asi": true, mode: "bytecode"}\n');
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
    writeFileSync(join(tmp, "goccia.json"), '{"mode": "interpreted", "compat-asi": true}\n');
    writeFileSync(join(tmp, "goccia.json5"), '{"compat-asi": true, mode: "interpreted"}\n');
    writeFileSync(join(tmp, "goccia.toml"), 'compat-asi = true\nmode = "bytecode"\n');
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
    writeFileSync(join(tmp, "goccia.json"), '{"compat-asi": true, "mode": "bytecode"}\n');
    writeFileSync(join(tmp, "src", "test.js"), "const x = 7\nx\n");

    const out = await $`${LOADER} --print ${join(tmp, "src", "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`Config in parent should be discovered, got: ${out}`);
    if (!containsLine(out, "7")) throw new Error(`Should produce 7 on its own line, got: ${out}`);
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
    writeFileSync(join(tmp, "goccia.json"), '{"stack-size": 50, "compat-asi": true}\n');
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
    writeFileSync(join(tmp, "goccia.json"), '{"compat-asi": true}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 42\nx\n");

    const out = await $`${LOADER} --print ${join(tmp, "test.js")} 2>&1`.text();
    if (!containsLine(out, "42")) throw new Error(`Config without imports should work, got: ${out}`);
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
      '{"compat-asi": true, "imports": {"utils": "./lib/utils.js"}}\n',
    );
    writeFileSync(join(tmp, "test.js"), 'import { value } from "utils"\nvalue\n');

    const out = runCwd(LOADER, ["--print", "test.js"], tmp);
    if (!containsLine(out.combined, "99")) throw new Error(`Imports should resolve, got: ${out.combined}`);
  } finally {
    clean(tmp);
  }
}

// -- Config extends (base + child) ----------------------------------------------

console.log("Config extends...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "base.json"), '{"compat-asi": true}\n');
    writeFileSync(join(tmp, "goccia.json"), '{"extends": "base.json", "mode": "bytecode"}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 5\nx\n");

    const out = await $`${LOADER} --print ${join(tmp, "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`Extends should enable bytecode, got: ${out}`);
    if (!containsLine(out, "5")) throw new Error(`Extends should inherit ASI, got: ${out}`);
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
    writeFileSync(join(tmp, "goccia.json"), '{"compat-asi": true}\n');
    writeFileSync(
      join(tmp, "tests", "asi", "goccia.json"),
      '{"extends": "../../goccia.json", "mode": "bytecode"}\n',
    );
    writeFileSync(join(tmp, "tests", "asi", "test.js"), "const x = 99\nx\n");

    const out = await $`${LOADER} --print ${join(tmp, "tests", "asi", "test.js")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`Subdirectory extends should enable bytecode, got: ${out}`);
    if (!containsLine(out, "99")) throw new Error(`Subdirectory extends should inherit ASI, got: ${out}`);
  } finally {
    clean(tmp);
  }
}

// -- TestRunner respects config -------------------------------------------------

console.log("TestRunner respects config...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"compat-asi": true, "mode": "bytecode"}\n');
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
    writeFileSync(join(asiDir, "goccia.json"), '{"compat-asi": true}\n');
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
      microbenchScript([
        'group("asi", () => {',
        '  bench("sum", () => 1 + 1)',
        "})",
      ]),
    );

    // Strict subdirectory (no config)
    const strictDir = join(tmp, "strict");
    mkdirSync(strictDir);
    writeFileSync(join(strictDir, "test.js"), "const y = 99;\ny;\n");
    writeFileSync(join(strictDir, "bad.js"), "const z = 1\nz\n");

    // Loader (interpreted)
    const loaderInterp = await $`${LOADER} --print ${join(asiDir, "test.js")} 2>&1`.text();
    if (!containsLine(loaderInterp, "42")) throw new Error(`Loader interp ASI should produce 42 on its own line, got: ${loaderInterp}`);

    const strictOk = await $`${LOADER} --print ${join(strictDir, "test.js")} 2>&1`.text();
    if (!containsLine(strictOk, "99")) throw new Error(`Strict subdir should produce 99 on its own line, got: ${strictOk}`);

    const strictBad = await $`${LOADER} ${join(strictDir, "bad.js")} 2>&1`.nothrow();
    if (!strictBad.text().includes("SyntaxError")) throw new Error("Strict subdir should reject missing semicolons");

    // Loader (bytecode)
    const loaderBc = await $`${LOADER} --print ${join(asiDir, "test.js")} --mode=bytecode 2>&1`.text();
    if (!containsLine(loaderBc, "42")) throw new Error(`Loader bytecode ASI should produce 42 on its own line, got: ${loaderBc}`);

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

    await $`${BUNDLER} ${join(strictDir, "bad.js")} --compat-asi`.quiet();
    if (!existsSync(join(strictDir, "bad.gbc"))) throw new Error("Bundler --compat-asi override should compile");

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

// -- Per-file unsafe-ffi config across runtime apps ----------------------------

console.log("Per-file unsafe-ffi config across runtime apps...");
{
  const tmp = makeTmp();
  const noConfigDir = makeTmp();
  try {
    writeFileSync(join(tmp, "goccia.json"), '{"unsafe-ffi": true}\n');
    writeFileSync(join(tmp, "test.js"), "typeof FFI;\n");
    writeFileSync(join(noConfigDir, "test.js"), "typeof FFI;\n");
    writeFileSync(
      join(tmp, "test-runner.js"),
      'test("unsafe ffi config", () => { expect(typeof FFI).toBe("object"); });\n',
    );
    writeFileSync(
      join(tmp, "bench.js"),
      microbenchScript([
        'group("unsafe-ffi", () => {',
        '  bench("global", () => {',
        '      if (typeof FFI !== "object") throw new Error("FFI missing");',
        "      return FFI.suffix;",
        "  });",
        "});",
      ]),
    );

    const loaderNoConfig = await $`${LOADER} --print ${join(noConfigDir, "test.js")} 2>&1`.text();
    if (!containsLine(loaderNoConfig, "undefined")) throw new Error(`Loader without unsafe-ffi config should leave FFI undefined, got: ${loaderNoConfig}`);

    const loaderOut = await $`${LOADER} --print ${join(tmp, "test.js")} 2>&1`.text();
    if (!containsLine(loaderOut, "object")) throw new Error(`Loader unsafe-ffi config should expose FFI, got: ${loaderOut}`);

    const loaderBc = await $`${LOADER} --print ${join(tmp, "test.js")} --mode=bytecode 2>&1`.text();
    if (!containsLine(loaderBc, "object")) throw new Error(`Loader bytecode unsafe-ffi config should expose FFI, got: ${loaderBc}`);

    const trInterp = await $`${TESTRUNNER} ${join(tmp, "test-runner.js")} --no-progress 2>&1`.text();
    if (!trInterp.includes("Passed: 1")) throw new Error(`TestRunner unsafe-ffi config should pass, got: ${trInterp}`);

    const trBc = await $`${TESTRUNNER} ${join(tmp, "test-runner.js")} --mode=bytecode --no-progress 2>&1`.text();
    if (!trBc.includes("Passed: 1")) throw new Error(`TestRunner bytecode unsafe-ffi config should pass, got: ${trBc}`);

    const parallelLoaderDir = join(tmp, "loader-parallel");
    mkdirSync(parallelLoaderDir);
    writeFileSync(join(parallelLoaderDir, "goccia.json"), '{"unsafe-ffi": true}\n');
    writeFileSync(
      join(parallelLoaderDir, "a.js"),
      'if (typeof FFI !== "object") throw new Error("FFI missing");\n',
    );
    writeFileSync(
      join(parallelLoaderDir, "b.js"),
      'if (typeof FFI !== "object") throw new Error("FFI missing");\n',
    );
    runCwd(LOADER, ["a.js", "b.js", "--jobs=2", "--output=compact-json"], parallelLoaderDir);

    const parallelTestDir = join(tmp, "test-parallel");
    mkdirSync(parallelTestDir);
    writeFileSync(join(parallelTestDir, "goccia.json"), '{"unsafe-ffi": true}\n');
    writeFileSync(
      join(parallelTestDir, "a.js"),
      'test("unsafe ffi config a", () => { expect(typeof FFI).toBe("object"); });\n',
    );
    writeFileSync(
      join(parallelTestDir, "b.js"),
      'test("unsafe ffi config b", () => { expect(typeof FFI).toBe("object"); });\n',
    );
    const trParallel = runCwd(TESTRUNNER, [".", "--jobs=2", "--no-progress"], parallelTestDir);
    if (!trParallel.combined.includes("Passed: 2"))
      throw new Error(`TestRunner parallel unsafe-ffi config should pass, got: ${trParallel.combined}`);

    for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
      const bench = Bun.spawnSync(
        [resolve(BENCHRUNNER), join(tmp, "bench.js"), "--no-progress", ...modeArgs],
        {
          stdout: "pipe",
          stderr: "pipe",
          env: { ...process.env, GOCCIA_BENCH_CALIBRATION_MS: "50", GOCCIA_BENCH_ROUNDS: "3" } as Record<string, string>,
          timeout: 60_000,
        },
      );
      if (bench.exitCode !== 0)
        throw new Error(`BenchmarkRunner unsafe-ffi config ${modeArgs.join(" ")} exited ${bench.exitCode}: ${bench.stderr.toString()}`);
      if (!bench.stdout.toString().includes("unsafe-ffi"))
        throw new Error(`BenchmarkRunner unsafe-ffi config output should mention 'unsafe-ffi', got: ${bench.stdout.toString()}`);
    }

    const parallelBenchDir = join(tmp, "bench-parallel");
    mkdirSync(parallelBenchDir);
    writeFileSync(join(parallelBenchDir, "goccia.json"), '{"unsafe-ffi": true}\n');
    for (const name of ["a", "b"]) {
      writeFileSync(
        join(parallelBenchDir, `${name}.js`),
        microbenchScript([
          `group("unsafe-ffi-${name}", () => {`,
          '  bench("global", () => {',
          '      if (typeof FFI !== "object") throw new Error("FFI missing");',
          "      return FFI.suffix;",
          "  });",
          "});",
        ]),
      );
    }
    const benchParallel = Bun.spawnSync(
      [resolve(BENCHRUNNER), parallelBenchDir, "--jobs=2", "--no-progress"],
      {
        stdout: "pipe",
        stderr: "pipe",
        env: { ...process.env, GOCCIA_BENCH_CALIBRATION_MS: "50", GOCCIA_BENCH_ROUNDS: "3" } as Record<string, string>,
        timeout: 60_000,
      },
    );
    if (benchParallel.exitCode !== 0)
      throw new Error(`BenchmarkRunner parallel unsafe-ffi config exited ${benchParallel.exitCode}: ${benchParallel.stderr.toString()}`);

    const replOut = runCwd(REPL, [`--config=${join(tmp, "goccia.json")}`], tmp, {
      stdin: "typeof FFI;\n",
    });
    if (!replOut.combined.includes("object")) throw new Error(`REPL unsafe-ffi config should expose FFI, got: ${replOut.combined}`);
  } finally {
    clean(tmp);
    clean(noConfigDir);
  }
}

// -- Per-file compat-var config across all apps --------------------------------

const nonStrictSource = [
  "function f(a) {",
  "  if (this !== globalThis) return -1;",
  "  with ({ extra: 5 }) {",
  "    return arguments.length + extra;",
  "  }",
  "}",
  "f(1, 2);",
].join("\n") + "\n";

for (const testCase of [
  {
    name: "warning-unsupported-features",
    config: '{"warning-unsupported-features": true}\n',
    loaderSource: "while (false) {}\n42;\n",
    loaderExpectedLine: "42",
    testRunnerSource: [
      "while (false) {}",
      'test("warning recovery", () => { expect(2 + 2).toBe(4); });',
    ].join("\n") + "\n",
    benchSource: microbenchScript([
      "while (false) {}",
      'group("warning", () => {',
      '  bench("recovery", () => 1 + 1);',
      "});",
    ]),
    benchNeedle: "warning",
  },
  {
    name: "compat-var",
    config: '{"compat-var": true}\n',
    loaderSource: "var x = 10;\nx;\n",
    loaderExpectedLine: "10",
    testRunnerSource: [
      "var y = 20;",
      'describe("var", () => {',
      '  test("works", () => {',
      "    expect(y).toBe(20);",
      "  });",
      "});",
    ].join("\n") + "\n",
    benchSource: microbenchScript([
      "var z = 1;",
      'group("var", () => {',
      '  bench("add", () => z + 1);',
      "});",
    ]),
    benchNeedle: "var",
  },
  {
    name: "compat-traditional-for-loop",
    config: '{"compat-traditional-for-loop": true}\n',
    loaderSource: "let s = 0;\nfor (let i = 0; i < 5; i++) { s += i; }\ns;\n",
    loaderExpectedLine: "10",
    testRunnerSource: [
      'describe("for", () => {',
      '  test("counts", () => {',
      "    let s = 0;",
      "    for (let i = 0; i < 5; i++) s += i;",
      "    expect(s).toBe(10);",
      "  });",
      "});",
    ].join("\n") + "\n",
    benchSource: microbenchScript([
      'group("for", () => {',
      '  bench("count", () => {',
      "      let s = 0;",
      "      for (let i = 0; i < 10; i++) s += i;",
      "      return s;",
      "  });",
      "});",
    ]),
    benchNeedle: "for",
  },
  {
    name: "compat-while-loops",
    config: '{"compat-while-loops": true}\n',
    loaderSource: "let s = 0;\nlet i = 0;\nwhile (i < 5) { s += i; i++; }\ns;\n",
    loaderExpectedLine: "10",
    testRunnerSource: [
      'describe("while", () => {',
      '  test("counts", () => {',
      "    let s = 0;",
      "    let i = 0;",
      "    do {",
      "      s += i;",
      "      i++;",
      "    } while (i < 5);",
      "    expect(s).toBe(10);",
      "  });",
      "});",
    ].join("\n") + "\n",
    benchSource: microbenchScript([
      'group("while", () => {',
      '  bench("count", () => {',
      "      let s = 0;",
      "      let i = 0;",
      "      while (i < 10) {",
      "        s += i;",
      "        i++;",
      "      }",
      "      return s;",
      "  });",
      "});",
    ]),
    benchNeedle: "while",
  },
  {
    name: "compat-loose-equality",
    config: '{"compat-loose-equality": true}\n',
    loaderSource: '"1" == 1;\n',
    loaderExpectedLine: "true",
    testRunnerSource: 'test("loose equality", () => { expect("1" == 1).toBe(true); });\n',
    benchSource: microbenchScript([
      'group("loose", () => {',
      '  bench("eq", () => "1" == 1);',
      "});",
    ]),
    benchNeedle: "loose",
  },
  {
    name: "compat-non-strict-mode",
    config: '{"compat-function": true, "compat-non-strict-mode": true, "compat-arguments-object": true}\n',
    loaderSource: nonStrictSource,
    loaderExpectedLine: "7",
    testRunnerSource: nonStrictSource + 'test("nonstrict", () => { expect(f(1, 2)).toBe(7); });\n',
    benchSource: microbenchScript([
      nonStrictSource,
      'group("nonstrict", () => {',
      '  bench("call", () => f(1, 2));',
      "});",
    ]),
    benchNeedle: "nonstrict",
  },
]) {
  console.log(`Per-file ${testCase.name} config across all apps...`);
  await runCompatAcrossAppsCase(testCase);
}

// -- compat-traditional-for-loop off -> SyntaxError by default ------------------

console.log("compat-traditional-for-loop OFF errors by default...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(
      join(tmp, "no-flag.js"),
      "for (let i = 0; i < 3; i++) { console.log('should not run'); }\n",
    );
    const defaultRes = await $`${LOADER} ${join(tmp, "no-flag.js")} 2>&1`.nothrow();
    const defaultOutput = defaultRes.text();
    if (defaultRes.exitCode === 0) {
      throw new Error(`Loader without flag should fail for traditional for-loop`);
    }
    if (!defaultOutput.includes("SyntaxError") ||
        !defaultOutput.includes("Traditional 'for(;;)' loops are not supported")) {
      throw new Error(`Loader without flag should emit SyntaxError, got: ${defaultOutput}`);
    }
    const warningRes = await $`${LOADER} ${join(tmp, "no-flag.js")} --warning-unsupported-features 2>&1`.nothrow();
    const warningOutput = warningRes.text();
    if (warningRes.exitCode !== 0) {
      throw new Error(`Loader with warning flag should recover, got: ${warningOutput}`);
    }
    if (!warningOutput.includes("Warning: Traditional 'for(;;)' loops are not supported")) {
      throw new Error(`Loader with warning flag should emit warning, got: ${warningOutput}`);
    }
    if (warningOutput.includes("should not run")) {
      throw new Error(`Loader with warning flag should not execute the loop body, got: ${warningOutput}`);
    }
  } finally {
    clean(tmp);
  }
}

// -- compat-while-loops off -> SyntaxError by default --------------------------

console.log("compat-while-loops OFF errors by default...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(
      join(tmp, "no-flag.js"),
      "let x = 0;\nwhile (x < 3) { console.log('should not run'); x++; }\n",
    );
    const defaultRes = await $`${LOADER} ${join(tmp, "no-flag.js")} 2>&1`.nothrow();
    const defaultOutput = defaultRes.text();
    if (defaultRes.exitCode === 0) {
      throw new Error(`Loader without flag should fail for while loop`);
    }
    if (!defaultOutput.includes("SyntaxError") ||
        !defaultOutput.includes("'while' loops are not supported by default")) {
      throw new Error(`Loader without flag should emit SyntaxError, got: ${defaultOutput}`);
    }
    const warningRes = await $`${LOADER} ${join(tmp, "no-flag.js")} --warning-unsupported-features 2>&1`.nothrow();
    const warningOutput = warningRes.text();
    if (warningRes.exitCode !== 0) {
      throw new Error(`Loader with warning flag should recover, got: ${warningOutput}`);
    }
    if (!warningOutput.includes("Warning: 'while' loops are not supported by default")) {
      throw new Error(`Loader with warning flag should emit warning, got: ${warningOutput}`);
    }
    if (warningOutput.includes("should not run")) {
      throw new Error(`Loader with warning flag should not execute the loop body, got: ${warningOutput}`);
    }
  } finally {
    clean(tmp);
  }
}

// -- Multi-directory TestRunner (lenient + strict, both execution modes) ---------

console.log("Multi-directory TestRunner...");
{
  const tmp = makeTmp();
  try {
    // Lenient subdirectory: ASI + compat-var
    const lenientDir = join(tmp, "lenient");
    mkdirSync(lenientDir);
    writeFileSync(join(lenientDir, "goccia.json"), '{"compat-asi": true, "compat-var": true}\n');
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

console.log("Explicit multi-file TestRunner config isolation...");
{
  const tmp = makeTmp();
  try {
    const argsDir = join(tmp, "arguments");
    const nonStrictDir = join(tmp, "non-strict");
    mkdirSync(argsDir);
    mkdirSync(nonStrictDir);

    writeFileSync(
      join(argsDir, "goccia.json"),
      '{"compat-function": true, "compat-non-strict-mode": true, "compat-arguments-object": true}\n',
    );
    writeFileSync(
      join(argsDir, "test.js"),
      [
        "function count() { return arguments.length; }",
        'test("arguments object enabled", () => {',
        "  expect(count(1, 2)).toBe(2);",
        "});",
      ].join("\n") + "\n",
    );

    writeFileSync(
      join(nonStrictDir, "goccia.json"),
      '{"compat-function": true, "compat-non-strict-mode": true}\n',
    );
    writeFileSync(
      join(nonStrictDir, "test.js"),
      [
        "function probe() { return [typeof arguments, this === globalThis]; }",
        'test("non-strict without implicit arguments", () => {',
        '  expect(probe()).toEqual(["undefined", true]);',
        "});",
      ].join("\n") + "\n",
    );

    const first = join(argsDir, "test.js");
    const second = join(nonStrictDir, "test.js");
    const trInterp = runCwd(TESTRUNNER, [first, second, "--jobs=1", "--no-progress"], tmp);
    if (!trInterp.combined.includes("Passed: 2"))
      throw new Error(`TestRunner interp explicit multi-file config isolation should pass 2, got: ${trInterp.combined}`);
    if (!trInterp.combined.includes("Failed: 0"))
      throw new Error(`TestRunner interp explicit multi-file config isolation should fail 0, got: ${trInterp.combined}`);

    const trBc = runCwd(TESTRUNNER, [first, second, "--jobs=1", "--mode=bytecode", "--no-progress"], tmp);
    if (!trBc.combined.includes("Passed: 2"))
      throw new Error(`TestRunner bytecode explicit multi-file config isolation should pass 2, got: ${trBc.combined}`);
    if (!trBc.combined.includes("Failed: 0"))
      throw new Error(`TestRunner bytecode explicit multi-file config isolation should fail 0, got: ${trBc.combined}`);
  } finally {
    clean(tmp);
  }
}

// -- CLI options override file config -------------------------------------------

console.log("CLI options override file config...");
{
  const tmp = makeTmp();
  const noConfigDir = makeTmp();
  try {
    // Config enables ASI
    writeFileSync(join(tmp, "goccia.json"), '{"compat-asi": true}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 1\nx\n");

    // No config dir
    writeFileSync(join(noConfigDir, "test.js"), "const x = 1\nx\n");

    // File config ASI works
    const configOut = await $`${LOADER} --print ${join(tmp, "test.js")} 2>&1`.text();
    if (!containsLine(configOut, "1")) throw new Error(`File config ASI should work, got: ${configOut}`);

    // Without config should fail
    const noConfigRes = await $`${LOADER} ${join(noConfigDir, "test.js")} 2>&1`.nothrow();
    if (!noConfigRes.text().includes("SyntaxError")) throw new Error("No config should reject");

    // CLI --compat-asi overrides no-config
    const cliAsi = await $`${LOADER} --print ${join(noConfigDir, "test.js")} --compat-asi 2>&1`.text();
    if (!containsLine(cliAsi, "1")) throw new Error(`CLI --compat-asi should override, got: ${cliAsi}`);

    // CLI --compat-asi bytecode
    const cliAsiBc = await $`${LOADER} --print ${join(noConfigDir, "test.js")} --compat-asi --mode=bytecode 2>&1`.text();
    if (!containsLine(cliAsiBc, "1")) throw new Error(`CLI --compat-asi bytecode should override, got: ${cliAsiBc}`);

    // CLI --mode=interpreted overrides config mode
    writeFileSync(join(tmp, "goccia.json"), '{"compat-asi": true, "mode": "bytecode"}\n');
    const overrideMode = await $`${LOADER} ${join(tmp, "test.js")} --mode=interpreted 2>&1`.text();
    if (!overrideMode.includes("(interpreted)")) throw new Error(`CLI --mode=interpreted should override config, got: ${overrideMode}`);

    // Bundler CLI --compat-asi overrides no-config
    await $`${BUNDLER} ${join(noConfigDir, "test.js")} --compat-asi`.quiet();
    if (!existsSync(join(noConfigDir, "test.gbc"))) throw new Error("Bundler CLI --compat-asi should compile");
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

    const out = runCwd(LOADER, ["--print", "test.js"], tmp);
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
// The --config option points at a specific config file and skips auto-discovery
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
    writeFileSync(join(cfgDir, "custom.json"), '{"compat-asi": true, "mode": "bytecode"}\n');

    const out = await $`${LOADER} --print ${join(tmp, "test.js")} --config=${join(cfgDir, "custom.json")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config=.json should enable bytecode, got: ${out}`);
    if (!containsLine(out, "11")) throw new Error(`--config=.json should enable ASI (11 on its own line), got: ${out}`);
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
    writeFileSync(join(cfgDir, "custom.toml"), 'compat-asi = true\nmode = "bytecode"\n');

    const out = await $`${LOADER} --print ${join(tmp, "test.js")} --config=${join(cfgDir, "custom.toml")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config=.toml should enable bytecode, got: ${out}`);
    if (!containsLine(out, "21")) throw new Error(`--config=.toml should enable ASI (21 on its own line), got: ${out}`);
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
    writeFileSync(join(cfgDir, "custom.json5"), '{"compat-asi": true, mode: "bytecode"}\n');

    const out = await $`${LOADER} --print ${join(tmp, "test.js")} --config=${join(cfgDir, "custom.json5")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config=.json5 should enable bytecode, got: ${out}`);
    if (!containsLine(out, "31")) throw new Error(`--config=.json5 should enable ASI (31 on its own line), got: ${out}`);
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
    writeFileSync(join(tmp, "custom.json"), '{"compat-asi": true, "mode": "bytecode"}\n');

    // Relative path; cwd is tmp.
    const out = runCwd(LOADER, ["--print", "test.js", "--config=./custom.json"], tmp);
    if (!out.combined.includes("(bytecode)")) throw new Error(`Relative --config should resolve, got: ${out.combined}`);
    if (!containsLine(out.combined, "41")) throw new Error(`Relative --config should enable ASI, got: ${out.combined}`);
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
    writeFileSync(join(cfgDir, "goccia.json"), '{"compat-asi": true, "mode": "bytecode"}\n');

    const out = await $`${LOADER} --print ${join(tmp, "test.js")} --config=${cfgDir} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config=<dir> with goccia.json should enable bytecode, got: ${out}`);
    if (!containsLine(out, "51")) throw new Error(`--config=<dir> should enable ASI, got: ${out}`);
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
    writeFileSync(join(cfgDir, "goccia.json"), '{"compat-asi": true, "mode": "interpreted"}\n');
    writeFileSync(join(cfgDir, "goccia.json5"), '{"compat-asi": true, mode: "interpreted"}\n');
    writeFileSync(join(cfgDir, "goccia.toml"), 'compat-asi = true\nmode = "bytecode"\n');

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
    writeFileSync(join(cfgDir, "goccia.toml"), 'compat-asi = true\nmode = "bytecode"\n');

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
    writeFileSync(join(tmp, "goccia.toml"), 'compat-asi = true\nmode = "bytecode"\n');
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
    writeFileSync(join(tmp, "goccia.json"), '{"compat-asi": true, "mode": "interpreted"}\n');
    writeFileSync(join(tmp, "test.js"), "const x = 101\nx\n");
    // Explicit config selects bytecode.
    writeFileSync(join(cfgDir, "good.toml"), 'compat-asi = true\nmode = "bytecode"\n');

    const out = await $`${LOADER} --print ${join(tmp, "test.js")} --config=${join(cfgDir, "good.toml")} 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`--config should skip nearby goccia.json, got: ${out}`);
    if (!containsLine(out, "101")) throw new Error(`--config should still apply ASI, got: ${out}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

// -- CLI options still beat --config --------------------------------------------

console.log("CLI options override values from --config...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    writeFileSync(join(tmp, "test.js"), "const x = 111\nx\n");
    // --config says interpreted...
    writeFileSync(join(cfgDir, "custom.toml"), 'compat-asi = true\nmode = "interpreted"\n');

    // ...but a direct --mode=bytecode on the CLI must win.
    const out = await $`${LOADER} --print ${join(tmp, "test.js")} --config=${join(cfgDir, "custom.toml")} --mode=bytecode 2>&1`.text();
    if (!out.includes("(bytecode)")) throw new Error(`CLI --mode should override --config value, got: ${out}`);
    if (!containsLine(out, "111")) throw new Error(`ASI from --config should still apply, got: ${out}`);
  } finally {
    clean(tmp);
    clean(cfgDir);
  }
}

// -- --config across the other CLI tools ---------------------------------------
//
// The option lives in the shared base class, so smoke-test that each consumer
// actually honors it.  The Loader is covered above; here we hit the rest.

console.log("--config works on TestRunner...");
{
  const tmp = makeTmp();
  const cfgDir = makeTmp();
  try {
    writeFileSync(join(cfgDir, "ci.toml"), 'compat-asi = true\nmode = "bytecode"\n');
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
    writeFileSync(join(cfgDir, "goccia.json"), '{"compat-asi": true}\n');

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
    writeFileSync(join(cfgDir, "goccia.toml"), 'compat-asi = true\n');
    writeFileSync(
      join(tmp, "bench.js"),
      microbenchScript([
        // No semicolons — proves --config-supplied ASI is in effect.
        'group("config-flag", () => {',
        '  bench("sum", () => 1 + 1)',
        "})",
      ]),
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

console.log("Virtual modules in goccia.json...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(
      join(tmp, "goccia.json"),
      JSON.stringify({
        "source-type": "module",
        modules: {
          "host:settings": { type: "json", content: { version: 3 } },
          "host:main": {
            content:
              'import settings from "host:settings"; export default settings.version;',
          },
        },
      }),
    );
    writeFileSync(
      join(tmp, "entry.js"),
      'import value from "host:main"; value;\n',
    );
    for (const mode of ["interpreted", "bytecode"] as const) {
      const out = runCwd(
        LOADER,
        [join(tmp, "entry.js"), "--print", `--mode=${mode}`],
        tmp,
      );
      if (!containsLine(out.stdout, "3"))
        throw new Error(`Config virtual module ${mode} expected 3, got: ${out.combined}`);
    }
  } finally {
    clean(tmp);
  }
}

console.log("--modules manifests (JSON, JSON5, TOML, YAML, JavaScript, TypeScript)...");
{
  const tmp = makeTmp();
  try {
    const manifests = new Map<string, string>([
      ["modules.json", '{"host:manifest":{"content":"export default 23;"}}'],
      ["modules.json5", '{"host:manifest": {content: "export default 23;"}}'],
      ["modules.toml", '["host:manifest"]\ncontent = "export default 23;"\n'],
      ["modules.yaml", '"host:manifest":\n  content: "export default 23;"\n'],
      ["modules.js", 'export default {"host:manifest": {content: "export default 23;"}};\n'],
      ["modules.ts", 'export default {"host:manifest": {type: "typescript", content: "export default 23;"}};\n'],
    ]);
    writeFileSync(
      join(tmp, "entry.mjs"),
      'import value from "host:manifest"; value;\n',
    );
    for (const [name, content] of manifests) {
      const path = join(tmp, name);
      writeFileSync(path, content);
      const out = runCwd(
        LOADER,
        [join(tmp, "entry.mjs"), "--print", "--modules", path],
        tmp,
      );
      if (!containsLine(out.stdout, "23"))
        throw new Error(`${name} virtual module manifest expected 23, got: ${out.combined}`);
    }
  } finally {
    clean(tmp);
  }
}

console.log("Executable manifests cannot replace an already loaded module record...");
{
  const tmp = makeTmp();
  try {
    const manifest = join(tmp, "modules.mjs");
    const entry = join(tmp, "entry.mjs");
    writeFileSync(
      manifest,
      'export default {"./modules.mjs": {content: "export default 17;"}};\n',
    );
    writeFileSync(entry, 'import value from "./modules.mjs"; value;\n');
    const out = runCwd(
      LOADER,
      [entry, "--print", "--modules", manifest],
      tmp,
      { expectFail: true },
    );
    if (out.exitCode === 0 || !out.combined.includes("cannot be replaced after it has been loaded"))
      throw new Error(`Loaded module replacement should be rejected: ${out.combined}`);
  } finally {
    clean(tmp);
  }
}

console.log("Virtual module config precedence and inherited manifest origins...");
{
  const tmp = makeTmp();
  const baseDir = join(tmp, "base");
  const appDir = join(tmp, "app");
  try {
    mkdirSync(baseDir, { recursive: true });
    mkdirSync(appDir, { recursive: true });
    writeFileSync(
      join(tmp, "root.json"),
      JSON.stringify({
        modules: {
          "host:choice": { content: "export default 1;" },
        },
      }),
    );
    writeFileSync(
      join(baseDir, "goccia.json"),
      JSON.stringify({ modules: ["modules.json"] }),
    );
    writeFileSync(
      join(baseDir, "modules.json"),
      JSON.stringify({
        "host:inherited": { content: "export default 10;" },
      }),
    );
    writeFileSync(
      join(appDir, "goccia.json"),
      JSON.stringify({
        extends: "../base/goccia.json",
        module: "host:choice=export default 2;",
      }),
    );
    writeFileSync(
      join(appDir, "entry.mjs"),
      'import choice from "host:choice"; import inherited from "host:inherited"; choice + inherited;\n',
    );

    const perFile = runCwd(
      LOADER,
      [join(appDir, "entry.mjs"), "--print", "--config", join(tmp, "root.json")],
      tmp,
    );
    if (!containsLine(perFile.stdout, "12"))
      throw new Error(`Per-file virtual module config should override root and retain inherited origins: ${perFile.combined}`);

    const cli = runCwd(
      LOADER,
      [
        join(appDir, "entry.mjs"),
        "--print",
        "--config",
        join(tmp, "root.json"),
        "--module",
        "host:choice=export default 3;",
      ],
      tmp,
    );
    if (!containsLine(cli.stdout, "13"))
      throw new Error(`CLI virtual module config should override per-file and root config: ${cli.combined}`);
  } finally {
    clean(tmp);
  }
}

console.log("Virtual modules win filesystem collisions with a warning...");
{
  const tmp = makeTmp();
  try {
    writeFileSync(
      join(tmp, "goccia.json"),
      JSON.stringify({
        imports: { "virtual-dep": "./dep.js" },
        modules: {
          "./dep.js": { content: "export default 2;" },
        },
      }),
    );
    writeFileSync(join(tmp, "dep.js"), "export default 1;\n");
    writeFileSync(
      join(tmp, "entry.mjs"),
      'import value from "virtual-dep"; value;\n',
    );
    const out = runCwd(
      LOADER,
      [join(tmp, "entry.mjs"), "--print"],
      tmp,
    );
    if (!containsLine(out.stdout, "2"))
      throw new Error(`Virtual module should win filesystem collision: ${out.combined}`);
    if (!out.stderr.includes("shadows module"))
      throw new Error(`Virtual/filesystem collision should warn: ${out.combined}`);
  } finally {
    clean(tmp);
  }
}

console.log("\nAll test-cli-config.ts tests passed.");
