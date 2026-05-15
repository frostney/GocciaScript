#!/usr/bin/env bun
/**
 * test-cli.ts
 *
 * Common CLI flags tested across all apps: stdin smoke, --help, --unsafe-ffi,
 * --asi, --compat-var, --compat-loose-equality, --compat-non-strict-mode,
 * --mode, --timeout, --max-instructions, --max-memory, --stack-size, --log,
 * example scripts.
 */

import { $ } from "bun";
import { writeFileSync, readFileSync, existsSync } from "fs";
import { join } from "path";
import {
  LOADER,
  BARE,
  REPL,
  TESTRUNNER,
  BUNDLER,
  BENCHRUNNER,
} from "./test-cli/binaries";
import { containsLine, runLoaderJson } from "./test-cli/assertions";
import { mkdtemp, clean } from "./test-cli/tmpdir";

// -- Stdin smoke (Loader interpreted + bytecode) --------------------------------

console.log("Stdin smoke (interpreted)...");
{
  const out = await $`echo 'const x = 2 + 2; x;' | ${LOADER} --print`.text();
  if (!containsLine(out, "4")) throw new Error(`Expected 4 on its own line, got: ${out}`);
}

console.log("Stdin smoke (bytecode)...");
{
  const out = await $`echo 'const x = 2 + 2; x;' | ${LOADER} --print - --mode=bytecode`.text();
  if (!containsLine(out, "4")) throw new Error(`Expected 4 on its own line, got: ${out}`);
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
  const tmp = mkdtemp("goccia-stdin-mix-");
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
    clean(tmp);
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
  const { json } = runLoaderJson("typeof FFI;\n");
  if (json.files?.[0]?.result !== "undefined") throw new Error(`FFI without flag should be "undefined", got ${json.files?.[0]?.result}`);

  const { json: jsonOn } = runLoaderJson("typeof FFI;\n", ["--unsafe-ffi"]);
  if (jsonOn.files?.[0]?.result !== "object") throw new Error(`FFI with flag should be "object", got ${jsonOn.files?.[0]?.result}`);
}

// -- --asi (Loader + Bundler) ---------------------------------------------------

console.log("--asi (Loader + Bundler)...");
{
  const tmp = mkdtemp("goccia-asi-");
  try {
    const src = join(tmp, "no-semi.js");
    writeFileSync(src, "const x = 42\nx\n");

    // Loader without --asi should fail
    const noAsi = await $`${LOADER} ${src} 2>&1`.nothrow();
    if (noAsi.exitCode === 0) throw new Error("Loader should reject without --asi");
    if (!noAsi.text().includes("SyntaxError")) throw new Error("Expected SyntaxError without --asi");

    // Loader with --asi should succeed
    const withAsi = await $`${LOADER} --print ${src} --asi 2>&1`.text();
    if (!containsLine(withAsi, "42")) throw new Error(`Expected 42 with --asi, got: ${withAsi}`);

    // Bundler without --asi should fail
    const bundleNoAsi = await $`${BUNDLER} ${src} 2>&1`.nothrow();
    if (bundleNoAsi.exitCode === 0) throw new Error("Bundler should reject without --asi");

    // Bundler with --asi should succeed
    await $`${BUNDLER} ${src} --asi`.quiet();
    if (!existsSync(join(tmp, "no-semi.gbc"))) throw new Error("Bundler --asi should produce .gbc");
  } finally {
    clean(tmp);
  }
}

// -- --compat-var (Loader + Bundler + TestRunner) --------------------------------

console.log("--compat-var (Loader + Bundler + TestRunner)...");
{
  const tmp = mkdtemp("goccia-var-");
  try {
    const src = join(tmp, "use-var.js");
    writeFileSync(src, "var x = 10;\nx;\n");

    // Loader with --compat-var
    const loaderOut = await $`${LOADER} --print ${src} --compat-var 2>&1`.text();
    if (!containsLine(loaderOut, "10")) throw new Error(`Loader --compat-var expected 10, got: ${loaderOut}`);

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
    clean(tmp);
  }
}

// -- --compat-function + Bare loader compat parsing ----------------------------

console.log("--compat-function (Loader) + Bare loader compat parsing...");
{
  const tmp = mkdtemp("goccia-func-");
  try {
    const fnSrc = join(tmp, "use-fn.js");
    writeFileSync(fnSrc, "function f() { return 7; }\nf();\n");
    const loaderOut = await $`${LOADER} --print ${fnSrc} --compat-function 2>&1`.text();
    if (!containsLine(loaderOut, "7")) throw new Error(`Loader --compat-function expected 7, got: ${loaderOut}`);

    // Bare loader argv path — covered here so the full test262 suite isn't the
    // only thing exercising it.  Flag combo mirrors run_test262_suite.ts.
    const bothSrc = join(tmp, "use-both.js");
    writeFileSync(bothSrc, "var x = 22;\nfunction f() { return x; }\nf();\n");
    const bareOut = await $`${BARE} --print ${bothSrc} --compat-var --compat-function 2>&1`.text();
    if (bareOut.trim() !== "22") throw new Error(`Bare --compat-var --compat-function expected 22, got: ${bareOut}`);
    const bareNoFlag = await $`${BARE} ${bothSrc} 2>&1`.nothrow();
    if (bareNoFlag.exitCode === 0) throw new Error("Bare without compat flags should reject var/function");

    // Stdin path — the exact shape run_test262_suite.ts uses (source piped
    // into a `-` argument).  Without this, file-path is the only invocation
    // mode covered.
    const bareStdin = await $`cat ${bothSrc} | ${BARE} --print - --compat-var --compat-function 2>&1`.text();
    if (bareStdin.trim() !== "22") throw new Error(`Bare stdin --compat-var --compat-function expected 22, got: ${bareStdin}`);

    // --compat-all regression guard: the flag was removed and must now be
    // rejected as an unknown option.
    const bareCompatAll = await $`echo 'x;' | ${BARE} --compat-all - 2>&1`.nothrow();
    const compatAllOut = bareCompatAll.stdout.toString();
    if (bareCompatAll.exitCode === 0 || !compatAllOut.includes("--compat-all"))
      throw new Error(`Bare must reject --compat-all, got exit ${bareCompatAll.exitCode}: ${compatAllOut}`);

    const forSrc = join(tmp, "use-for.js");
    writeFileSync(forSrc, "let s = 0;\nfor (let i = 1; i <= 5; i++) { s = s + i; }\ns;\n");
    const forOut = await $`${BARE} --print ${forSrc} --compat-traditional-for-loop 2>&1`.text();
    if (forOut.trim() !== "15") throw new Error(`Bare --compat-traditional-for-loop expected 15, got: ${forOut}`);

    const looseSrc = join(tmp, "use-loose.js");
    writeFileSync(looseSrc, '"1" == 1;\n');
    const loaderLoose = await $`${LOADER} --print ${looseSrc} --compat-loose-equality 2>&1`.text();
    if (!containsLine(loaderLoose, "true")) throw new Error(`Loader --compat-loose-equality expected true, got: ${loaderLoose}`);

    const looseTest = join(tmp, "use-loose-test.js");
    writeFileSync(looseTest, 'test("loose equality", () => { expect("1" == 1).toBe(true); });\n');
    const trLoose = await $`${TESTRUNNER} ${looseTest} --compat-loose-equality --no-progress 2>&1`.text();
    if (!trLoose.includes("Passed: 1")) throw new Error(`TestRunner --compat-loose-equality expected Passed: 1, got: ${trLoose}`);

    const looseOut = join(tmp, "loose.gbc");
    await $`${BUNDLER} ${looseSrc} --compat-loose-equality --output=${looseOut}`.quiet();
    if (!existsSync(looseOut)) throw new Error("Bundler --compat-loose-equality should compile");

    const bareLoose = await $`${BARE} --print ${looseSrc} --compat-loose-equality 2>&1`.text();
    if (bareLoose.trim() !== "true") throw new Error(`Bare --compat-loose-equality expected true, got: ${bareLoose}`);
  } finally {
    clean(tmp);
  }
}

// -- --compat-non-strict-mode (Loader + Bundler + TestRunner + Bare) -----------

console.log("--compat-non-strict-mode (Loader + Bundler + TestRunner + Bare)...");
{
  const tmp = mkdtemp("goccia-nonstrict-");
  try {
    const src = join(tmp, "use-nonstrict.js");
    writeFileSync(
      src,
      [
        "function f(a) {",
        "  if (this !== globalThis) return -1;",
        "  with ({ extra: 5 }) {",
        "    return arguments.length + extra;",
        "  }",
        "}",
        "f(1, 2);",
      ].join("\n") + "\n",
    );

    const loaderOut = await $`${LOADER} --print ${src} --compat-function --compat-non-strict-mode 2>&1`.text();
    if (!containsLine(loaderOut, "7")) throw new Error(`Loader --compat-non-strict-mode expected 7, got: ${loaderOut}`);

    const loaderBcOut = await $`${LOADER} --print ${src} --mode=bytecode --compat-function --compat-non-strict-mode 2>&1`.text();
    if (!containsLine(loaderBcOut, "7")) throw new Error(`Loader bytecode --compat-non-strict-mode expected 7, got: ${loaderBcOut}`);

    const outPath = join(tmp, "use-nonstrict.gbc");
    await $`${BUNDLER} ${src} --output=${outPath} --compat-function --compat-non-strict-mode`.quiet();
    if (!existsSync(outPath)) throw new Error("Bundler --compat-non-strict-mode should compile");

    const deleteSrc = join(tmp, "delete-nonstrict.js");
    writeFileSync(
      deleteSrc,
      [
        "let binding = 1;",
        "globalThis.tempDeleteName = 1;",
        "const obj = {};",
        'Object.defineProperty(obj, "fixed", { value: 1, configurable: false });',
        "(delete binding ? 1 : 0) + (delete obj.fixed ? 2 : 0) + (delete missingName ? 4 : 0) + (delete tempDeleteName ? 8 : 0);",
      ].join("\n") + "\n",
    );
    const deleteOutPath = join(tmp, "delete-nonstrict.gbc");
    await $`${BUNDLER} ${deleteSrc} --output=${deleteOutPath} --compat-non-strict-mode`.quiet();
    const bundledDeleteOut = await $`${LOADER} --print ${deleteOutPath} 2>&1`.text();
    if (!containsLine(bundledDeleteOut, "12")) throw new Error(`Bundled non-strict delete expected 12, got: ${bundledDeleteOut}`);

    const assignmentSrc = join(tmp, "assignment-nonstrict.js");
    writeFileSync(
      assignmentSrc,
      [
        "const obj = {};",
        'Object.defineProperty(obj, "fixed", { value: 1, writable: false });',
        "obj.fixed = 2;",
        "const withObj = {};",
        'Object.defineProperty(withObj, "value", { value: 1, writable: false });',
        "with (withObj) {",
        "  value = 2;",
        "}",
        "obj.fixed + withObj.value;",
      ].join("\n") + "\n",
    );
    const assignmentOutPath = join(tmp, "assignment-nonstrict.gbc");
    await $`${BUNDLER} ${assignmentSrc} --output=${assignmentOutPath} --compat-non-strict-mode`.quiet();
    const bundledAssignmentOut = await $`${LOADER} --print ${assignmentOutPath} 2>&1`.text();
    if (!containsLine(bundledAssignmentOut, "2")) throw new Error(`Bundled non-strict assignment expected 2, got: ${bundledAssignmentOut}`);

    const testSrc = join(tmp, "test-nonstrict.js");
    writeFileSync(
      testSrc,
      [
        "function f(a) {",
        "  if (this !== globalThis) return -1;",
        "  with ({ extra: 5 }) {",
        "    return arguments.length + extra;",
        "  }",
        "}",
        'test("non-strict mode compat", () => { expect(f(1, 2)).toBe(7); });',
      ].join("\n") + "\n",
    );
    const trOut = await $`${TESTRUNNER} ${testSrc} --no-progress --compat-function --compat-non-strict-mode 2>&1`.text();
    if (!trOut.includes("Passed: 1")) throw new Error(`TestRunner --compat-non-strict-mode expected Passed: 1, got: ${trOut}`);

    const bareOut = await $`${BARE} --print ${src} --compat-function --compat-non-strict-mode 2>&1`.text();
    if (bareOut.trim() !== "7") throw new Error(`Bare --compat-non-strict-mode expected 7, got: ${bareOut}`);

    const bundleNoFlag = await $`${BUNDLER} ${src} --compat-function --output=${join(tmp, "no-flag.gbc")} 2>&1`.nothrow();
    if (bundleNoFlag.exitCode !== 0) throw new Error(`Bundler without --compat-non-strict-mode should skip unsupported with and still compile, got: ${bundleNoFlag.stderr.toString()}`);
  } finally {
    clean(tmp);
  }
}

// -- --mode=bytecode (Loader: both modes produce 4) ----------------------------

console.log("--mode=bytecode...");
{
  const interpOut = await $`echo 'const x = 2 + 2; x;' | ${LOADER} --print`.text();
  if (!containsLine(interpOut, "4")) throw new Error(`Interpreted expected 4 on its own line, got: ${interpOut}`);
  if (!interpOut.includes("(interpreted)")) throw new Error(`Expected (interpreted) in output`);

  const bcOut = await $`echo 'const x = 2 + 2; x;' | ${LOADER} --print - --mode=bytecode`.text();
  if (!containsLine(bcOut, "4")) throw new Error(`Bytecode expected 4 on its own line, got: ${bcOut}`);
  if (!bcOut.includes("(bytecode)")) throw new Error(`Expected (bytecode) in output`);
}

// -- --timeout (Loader: infinite loop, both modes) ------------------------------

console.log("--timeout (interpreted)...");
{
  const loop = "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
  const { exitCode, json } = runLoaderJson(loop, ["--timeout=50"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Timeout exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

console.log("--timeout (bytecode)...");
{
  const loop = "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
  const { exitCode, json } = runLoaderJson(loop, ["--timeout=50", "--mode=bytecode"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Bytecode timeout exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

// -- --max-instructions (Loader: infinite loop, both modes) ---------------------

console.log("--max-instructions (interpreted)...");
{
  const loop = "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
  const { exitCode, json } = runLoaderJson(loop, ["--max-instructions=500"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Instruction limit exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "InstructionLimitError") throw new Error(`Expected InstructionLimitError, got ${json.error?.type}`);
}

console.log("--max-instructions (bytecode)...");
{
  const loop = "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
  const { exitCode, json } = runLoaderJson(loop, ["--max-instructions=500", "--mode=bytecode"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Bytecode instruction limit exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "InstructionLimitError") throw new Error(`Expected InstructionLimitError, got ${json.error?.type}`);
}

// -- --max-memory (Loader) ------------------------------------------------------

console.log("--max-memory (default positive)...");
{
  const { json } = runLoaderJson("Goccia.gc.maxBytes\n", ["--asi"]);
  if (typeof json.files?.[0]?.result !== "number" || json.files[0].result <= 0) throw new Error(`Default maxBytes should be positive, got ${json.files?.[0]?.result}`);
}

console.log("--max-memory (override)...");
{
  const { json } = runLoaderJson("Goccia.gc.maxBytes\n", ["--max-memory=5000000", "--asi"]);
  if (json.files?.[0]?.result !== 5000000) throw new Error(`Override maxBytes should be 5000000, got ${json.files?.[0]?.result}`);
}

console.log("--max-memory (OOM triggers RangeError)...");
{
  const res = await $`echo 'Array.from({length:5000},(_,i)=>({x:i}));' | ${LOADER} --max-memory=200000 --asi 2>&1`.nothrow();
  const out = res.text();
  if (res.exitCode !== 1) throw new Error(`OOM exit code should be 1, got ${res.exitCode}`);
  if (!out.includes("RangeError")) throw new Error(`OOM output should contain RangeError`);
}

console.log("--max-memory (manual gc reclaims inside active calls)...");
{
  const src = [
    "const churn = (remaining) => remaining <= 0 ? Goccia.gc.bytesAllocated : (() => {",
    "  let junk = Array.from({ length: 300 }, (_, i) => ({ remaining, i }));",
    "  junk = null;",
    "  Goccia.gc();",
    "  return churn(remaining - 1);",
    "})();",
    "churn(30);",
    "",
  ].join("\n");

  for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
    const label = modeArgs.length > 0 ? modeArgs.join(" ") : "interpreter";
    const { exitCode, json, stderr } = runLoaderJson(src, ["--max-memory=500000", "--asi", ...modeArgs], { timeout: 30_000 });
    if (exitCode !== 0) throw new Error(`Manual GC active-call ${label} exit code should be 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
    if (typeof json.files?.[0]?.result !== "number" || json.files[0].result <= 0) throw new Error(`Manual GC active-call ${label} should return positive bytesAllocated`);
    if ((json.memory?.gc?.collections ?? 0) < 30) throw new Error(`Manual GC active-call ${label} should report at least 30 collections, got ${json.memory?.gc?.collections}`);
  }
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
  const { exitCode, json } = runLoaderJson("const f = () => f(); f();\n");
  if (exitCode !== 1) throw new Error(`Default overflow should exit 1, got ${exitCode}`);
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
  const tmp = mkdtemp("goccia-log-");
  try {
    const logPath = join(tmp, "output.log");
    await $`echo "console.log('hello-log'); console.warn('hello-warn');" | ${LOADER} --log=${logPath}`.quiet();
    if (!existsSync(logPath)) throw new Error(`Log file should exist at ${logPath}`);
    const content = readFileSync(logPath, "utf-8");
    if (!content.includes("[log]")) throw new Error(`Log file should contain [log]`);
    if (!content.includes("[warn]")) throw new Error(`Log file should contain [warn]`);
  } finally {
    clean(tmp);
  }
}

// -- Example scripts (Loader) ---------------------------------------------------

console.log("Example scripts...");
await $`${LOADER} examples`.quiet();

console.log("\nAll test-cli.ts tests passed.");
