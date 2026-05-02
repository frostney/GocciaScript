#!/usr/bin/env bun
/**
 * test-cli.ts
 *
 * Common CLI flags tested across all apps: stdin smoke, --help, --unsafe-ffi,
 * --asi, --compat-var, --mode, --timeout, --max-instructions, --max-memory,
 * --stack-size, --log, example scripts.
 */

import { $ } from "bun";
import {
  mkdtempSync,
  writeFileSync,
  readFileSync,
  existsSync,
  rmSync,
} from "fs";
import { join } from "path";
import { tmpdir } from "os";

const ext = process.platform === "win32" ? ".exe" : "";
const LOADER = `./build/GocciaScriptLoader${ext}`;
const BARE = `./build/GocciaScriptLoaderBare${ext}`;
const REPL = `./build/GocciaREPL${ext}`;
const TESTRUNNER = `./build/GocciaTestRunner${ext}`;
const BUNDLER = `./build/GocciaBundler${ext}`;
const BENCHRUNNER = `./build/GocciaBenchmarkRunner${ext}`;

// -- Stdin smoke (Loader interpreted + bytecode) --------------------------------

console.log("Stdin smoke (interpreted)...");
{
  const out = await $`echo 'const x = 2 + 2; x;' | ${LOADER}`.text();
  if (!out.includes("Result: 4")) throw new Error(`Expected Result: 4, got: ${out}`);
}

console.log("Stdin smoke (bytecode)...");
{
  const out = await $`echo 'const x = 2 + 2; x;' | ${LOADER} - --mode=bytecode`.text();
  if (!out.includes("Result: 4")) throw new Error(`Expected Result: 4, got: ${out}`);
}

// -- Stdin smoke (TestRunner) --------------------------------------------------

console.log("Stdin smoke (TestRunner)...");
{
  const src = `test("two plus two", () => { expect(2 + 2).toBe(4); });\n`;

  // No path arg -> stdin
  const out = await $`echo ${src} | ${TESTRUNNER} --no-progress`.text();
  if (!out.includes("Passed: 1")) throw new Error(`TestRunner stdin (no arg) expected Passed: 1, got: ${out}`);

  // Sole "-" arg -> stdin
  const outDash = await $`echo ${src} | ${TESTRUNNER} - --no-progress`.text();
  if (!outDash.includes("Passed: 1")) throw new Error(`TestRunner stdin ("-" arg) expected Passed: 1, got: ${outDash}`);
}

// -- Stdin smoke (BenchmarkRunner) ---------------------------------------------

console.log("Stdin smoke (BenchmarkRunner)...");
{
  const src = `suite("stdin", () => { bench("sum", { run: () => 1 + 1 }); });\n`;
  const out = await $`echo ${src} | ${BENCHRUNNER} --no-progress 2>&1`.text();
  if (!out.includes("sum")) throw new Error(`BenchmarkRunner stdin expected "sum" benchmark, got: ${out}`);

  const outDash = await $`echo ${src} | ${BENCHRUNNER} - --no-progress 2>&1`.text();
  if (!outDash.includes("sum")) throw new Error(`BenchmarkRunner stdin ("-" arg) expected "sum" benchmark, got: ${outDash}`);
}

// -- Stdin mixed-with-paths rejection (all three runners) ----------------------

console.log("Stdin mixed-with-paths rejection (Loader, TestRunner, BenchmarkRunner)...");
{
  const tmp = mkdtempSync(join(tmpdir(), "goccia-stdin-mix-"));
  try {
    const f = join(tmp, "x.js");
    writeFileSync(f, "1;\n");

    for (const [bin, label] of [[LOADER, "Loader"], [TESTRUNNER, "TestRunner"], [BENCHRUNNER, "BenchmarkRunner"]] as const) {
      const proc = await $`${bin} - ${f} 2>&1`.nothrow();
      if (proc.exitCode === 0) throw new Error(`${label} should reject "-" mixed with file paths`);
      if (!proc.text().includes("stdin supports only as the sole input path"))
        throw new Error(`${label} mixed-path error missing unified message, got: ${proc.text()}`);
    }
  } finally {
    rmSync(tmp, { recursive: true, force: true });
  }
}

// -- --help (all 6 apps) -------------------------------------------------------

console.log("--help (all 6 apps)...");
for (const bin of [LOADER, BARE, REPL, TESTRUNNER, BUNDLER, BENCHRUNNER]) {
  const help = await $`${bin} --help 2>&1`.text();
  if (!help.includes("--")) throw new Error(`${bin} --help missing options`);
}

// -- --unsafe-ffi gating --------------------------------------------------------

console.log("--unsafe-ffi gating...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json"], {
    stdin: new TextEncoder().encode("typeof FFI;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
  if (json.files?.[0]?.result !== "undefined") throw new Error(`FFI without flag should be "undefined", got ${json.files?.[0]?.result}`);

  const procOn = Bun.spawnSync([LOADER, "--unsafe-ffi", "--output=json"], {
    stdin: new TextEncoder().encode("typeof FFI;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const jsonOn = JSON.parse(procOn.stdout.toString());
  if (jsonOn.files?.[0]?.result !== "object") throw new Error(`FFI with flag should be "object", got ${jsonOn.files?.[0]?.result}`);
}

// -- --asi (Loader + Bundler) ---------------------------------------------------

console.log("--asi (Loader + Bundler)...");
{
  const tmp = mkdtempSync(join(tmpdir(), "goccia-asi-"));
  try {
    const src = join(tmp, "no-semi.js");
    writeFileSync(src, "const x = 42\nx\n");

    // Loader without --asi should fail
    const noAsi = await $`${LOADER} ${src} 2>&1`.nothrow();
    if (noAsi.exitCode === 0) throw new Error("Loader should reject without --asi");
    if (!noAsi.text().includes("SyntaxError")) throw new Error("Expected SyntaxError without --asi");

    // Loader with --asi should succeed
    const withAsi = await $`${LOADER} ${src} --asi 2>&1`.text();
    if (!withAsi.includes("Result: 42")) throw new Error(`Expected Result: 42 with --asi, got: ${withAsi}`);

    // Bundler without --asi should fail
    const bundleNoAsi = await $`${BUNDLER} ${src} 2>&1`.nothrow();
    if (bundleNoAsi.exitCode === 0) throw new Error("Bundler should reject without --asi");

    // Bundler with --asi should succeed
    await $`${BUNDLER} ${src} --asi`.quiet();
    if (!existsSync(join(tmp, "no-semi.gbc"))) throw new Error("Bundler --asi should produce .gbc");
  } finally {
    rmSync(tmp, { recursive: true, force: true });
  }
}

// -- --compat-var (Loader + Bundler + TestRunner) --------------------------------

console.log("--compat-var (Loader + Bundler + TestRunner)...");
{
  const tmp = mkdtempSync(join(tmpdir(), "goccia-var-"));
  try {
    const src = join(tmp, "use-var.js");
    writeFileSync(src, "var x = 10;\nx;\n");

    // Loader with --compat-var
    const loaderOut = await $`${LOADER} ${src} --compat-var 2>&1`.text();
    if (!loaderOut.includes("Result: 10")) throw new Error(`Loader --compat-var expected Result: 10, got: ${loaderOut}`);

    // Bundler with --compat-var
    await $`${BUNDLER} ${src} --compat-var`.quiet();
    if (!existsSync(join(tmp, "use-var.gbc"))) throw new Error("Bundler --compat-var should produce .gbc");

    // TestRunner with --compat-var
    const testSrc = join(tmp, "test-var.js");
    writeFileSync(
      testSrc,
      [
        "var y = 20;",
        'describe("var", () => {',
        '  test("works", () => {',
        "    expect(y).toBe(20);",
        "  });",
        "});",
      ].join("\n") + "\n",
    );
    const trOut = await $`${TESTRUNNER} ${testSrc} --compat-var --no-progress 2>&1`.text();
    if (!trOut.includes("Passed: 1")) throw new Error(`TestRunner --compat-var expected Passed: 1, got: ${trOut}`);
  } finally {
    rmSync(tmp, { recursive: true, force: true });
  }
}

// -- --mode=bytecode (Loader: both modes produce Result: 4) ---------------------

console.log("--mode=bytecode...");
{
  const interpOut = await $`echo 'const x = 2 + 2; x;' | ${LOADER}`.text();
  if (!interpOut.includes("Result: 4")) throw new Error(`Interpreted expected Result: 4, got: ${interpOut}`);
  if (!interpOut.includes("(interpreted)")) throw new Error(`Expected (interpreted) in output`);

  const bcOut = await $`echo 'const x = 2 + 2; x;' | ${LOADER} - --mode=bytecode`.text();
  if (!bcOut.includes("Result: 4")) throw new Error(`Bytecode expected Result: 4, got: ${bcOut}`);
  if (!bcOut.includes("(bytecode)")) throw new Error(`Expected (bytecode) in output`);
}

// -- --timeout (Loader: infinite loop, both modes) ------------------------------

console.log("--timeout (interpreted)...");
{
  const loop = "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
  const proc = Bun.spawnSync([LOADER, "--timeout=50", "--output=json"], {
    stdin: new TextEncoder().encode(loop),
    stdout: "pipe",
    stderr: "pipe",
    timeout: 10_000,
  });
  if (proc.exitCode !== 1) throw new Error(`Timeout exit code should be 1, got ${proc.exitCode}`);
  const json = JSON.parse(proc.stdout.toString());
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

console.log("--timeout (bytecode)...");
{
  const loop = "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
  const proc = Bun.spawnSync([LOADER, "--timeout=50", "--output=json", "--mode=bytecode"], {
    stdin: new TextEncoder().encode(loop),
    stdout: "pipe",
    stderr: "pipe",
    timeout: 10_000,
  });
  if (proc.exitCode !== 1) throw new Error(`Bytecode timeout exit code should be 1, got ${proc.exitCode}`);
  const json = JSON.parse(proc.stdout.toString());
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

// -- --max-instructions (Loader: infinite loop, both modes) ---------------------

console.log("--max-instructions (interpreted)...");
{
  const loop = "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
  const proc = Bun.spawnSync([LOADER, "--max-instructions=500", "--output=json"], {
    stdin: new TextEncoder().encode(loop),
    stdout: "pipe",
    stderr: "pipe",
    timeout: 10_000,
  });
  if (proc.exitCode !== 1) throw new Error(`Instruction limit exit code should be 1, got ${proc.exitCode}`);
  const json = JSON.parse(proc.stdout.toString());
  if (json.error?.type !== "InstructionLimitError") throw new Error(`Expected InstructionLimitError, got ${json.error?.type}`);
}

console.log("--max-instructions (bytecode)...");
{
  const loop = "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
  const proc = Bun.spawnSync([LOADER, "--max-instructions=500", "--output=json", "--mode=bytecode"], {
    stdin: new TextEncoder().encode(loop),
    stdout: "pipe",
    stderr: "pipe",
    timeout: 10_000,
  });
  if (proc.exitCode !== 1) throw new Error(`Bytecode instruction limit exit code should be 1, got ${proc.exitCode}`);
  const json = JSON.parse(proc.stdout.toString());
  if (json.error?.type !== "InstructionLimitError") throw new Error(`Expected InstructionLimitError, got ${json.error?.type}`);
}

// -- --max-memory (Loader) ------------------------------------------------------

console.log("--max-memory (default positive)...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json", "--asi"], {
    stdin: new TextEncoder().encode("Goccia.gc.maxBytes\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
  if (typeof json.files?.[0]?.result !== "number" || json.files[0].result <= 0) throw new Error(`Default maxBytes should be positive, got ${json.files?.[0]?.result}`);
}

console.log("--max-memory (override)...");
{
  const proc = Bun.spawnSync([LOADER, "--max-memory=5000000", "--output=json", "--asi"], {
    stdin: new TextEncoder().encode("Goccia.gc.maxBytes\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
  if (json.files?.[0]?.result !== 5000000) throw new Error(`Override maxBytes should be 5000000, got ${json.files?.[0]?.result}`);
}

console.log("--max-memory (OOM triggers RangeError)...");
{
  const res = await $`echo 'Array.from({length:5000},(_,i)=>({x:i}));' | ${LOADER} --max-memory=200000 --asi 2>&1`.nothrow();
  const out = res.text();
  if (res.exitCode !== 1) throw new Error(`OOM exit code should be 1, got ${res.exitCode}`);
  if (!out.includes("RangeError")) throw new Error(`OOM output should contain RangeError`);
}

console.log("--max-memory (maxBytes readonly)...");
{
  const res = await $`echo 'Goccia.gc.maxBytes = 999' | ${LOADER} --asi 2>&1`.nothrow();
  if (res.exitCode !== 1) throw new Error(`Read-only exit code should be 1, got ${res.exitCode}`);
  if (!res.text().includes("TypeError")) throw new Error(`Read-only should mention TypeError`);
}

// -- --stack-size (Loader) ------------------------------------------------------

console.log("--stack-size (default overflow)...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json"], {
    stdin: new TextEncoder().encode("const f = () => f(); f();\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 1) throw new Error(`Default overflow should exit 1, got ${proc.exitCode}`);
  const json = JSON.parse(proc.stdout.toString());
  if (json.error?.type !== "RangeError") throw new Error(`Expected RangeError, got ${json.error?.type}`);
}

console.log("--stack-size (custom limit)...");
{
  const out = await $`echo 'let n=0; const f=()=>{n++;f()}; try{f()}catch(e){console.log(n)};' | ${LOADER} --stack-size=100`.text();
  if (!out.includes("100")) throw new Error(`Custom stack-size output should contain 100, got: ${out}`);
}

console.log("--stack-size (bytecode trampoline)...");
{
  const src = "let n = 0; const f = () => { n++; if (n < 20000) f(); }; f(); console.log(n);";
  const out = await $`echo ${src} | ${LOADER} --mode=bytecode --stack-size=0`.text();
  if (!out.includes("20000")) throw new Error(`Trampoline should reach 20000, got: ${out}`);
}

// -- --log flag (Loader) --------------------------------------------------------

console.log("--log flag...");
{
  const tmp = mkdtempSync(join(tmpdir(), "goccia-log-"));
  try {
    const logPath = join(tmp, "output.log");
    await $`echo "console.log('hello-log'); console.warn('hello-warn');" | ${LOADER} --log=${logPath}`.quiet();
    if (!existsSync(logPath)) throw new Error(`Log file should exist at ${logPath}`);
    const content = readFileSync(logPath, "utf-8");
    if (!content.includes("[log]")) throw new Error(`Log file should contain [log]`);
    if (!content.includes("[warn]")) throw new Error(`Log file should contain [warn]`);
  } finally {
    rmSync(tmp, { recursive: true, force: true });
  }
}

// -- Example scripts (Loader) ---------------------------------------------------

console.log("Example scripts...");
await $`${LOADER} examples`.quiet();

console.log("\nAll test-cli.ts tests passed.");
