#!/usr/bin/env bun
/**
 * test-cli.ts
 *
 * Common CLI options tested across all apps: stdin smoke, --help, --unsafe-ffi,
 * --compat-asi, --source-type, .mjs source-type inference, --compat-var, --compat-loose-equality, --compat-non-strict-mode,
 * --compat-for-in-loop, --compat-while-loops, --warning-unsupported-features,
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
import { containsLine, normalizeLineEndings, runLoaderJson } from "./test-cli/assertions";
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

console.log("REPL stdin decodes UTF-8...");
{
  const repl = Bun.spawnSync([REPL], {
    stdin: new TextEncoder().encode('"😀".length;\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  const output = repl.stdout.toString() + repl.stderr.toString();
  const hasExpectedResult = normalizeLineEndings(output)
    .split("\n")
    .some((line) => line === "2" || line === "> 2");
  if (repl.exitCode !== 0 || !hasExpectedResult)
    throw new Error(`REPL UTF-8 stdin expected UTF-16 length 2, got: ${output}`);
}

console.log("Bytecode top-level lexical TDZ...");
for (const [label, source] of [
  ["let", "let x = x;\n"],
  ["const", "const x = x;\n"],
] as const) {
  const { exitCode, json } = runLoaderJson(source, ["--mode=bytecode"]);
  if (exitCode === 0) throw new Error(`Top-level ${label} self-reference should fail in bytecode`);
  if (json.error?.type !== "ReferenceError")
    throw new Error(`Top-level ${label} self-reference should throw ReferenceError, got ${json.error?.type}`);
}

// -- Stdin smoke (TestRunner) --------------------------------------------------

console.log("Stdin smoke (TestRunner)...");
{
  const src = `test("two plus two", () => { expect(2 + 2).toBe(4); });\n`;

  // No positional argument -> stdin
  const out = await $`echo ${src} | ${TESTRUNNER} --no-progress`.text();
  if (!out.includes("Passed: 1")) throw new Error(`TestRunner stdin (no arg) expected Passed: 1, got: ${out}`);

  // Sole "-" arg -> stdin
  const outDash = await $`echo ${src} | ${TESTRUNNER} - --no-progress`.text();
  if (!outDash.includes("Passed: 1")) throw new Error(`TestRunner stdin ("-" arg) expected Passed: 1, got: ${outDash}`);
}

// -- Stdin smoke (BenchmarkRunner) ---------------------------------------------

console.log("Stdin smoke (BenchmarkRunner)...");
{
  const src = `import { bench, group } from "goccia:microbench"; group("stdin", () => { bench("sum", () => 1 + 1); });\n`;
  const out = await $`echo ${src} | ${BENCHRUNNER} --source-type=module --no-progress 2>&1`.text();
  if (!out.includes("sum")) throw new Error(`BenchmarkRunner stdin expected "sum" benchmark, got: ${out}`);

  const outDash = await $`echo ${src} | ${BENCHRUNNER} - --source-type=module --no-progress 2>&1`.text();
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
      if (!proc.text().includes("stdin is supported only as the sole input"))
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
  if (!help.includes("--warning-unsupported-features"))
    throw new Error(`${bin} --help missing --warning-unsupported-features`);
  if (!help.includes("--deterministic"))
    throw new Error(`${bin} --help missing --deterministic`);
}

// -- --deterministic -----------------------------------------------------------

console.log("--deterministic (Loader + Bare, interpreted + bytecode)...");
{
  const source = [
    "Math.random()",
    "Math.random()",
    "Date.now()",
    "new Date().getTime()",
    "new Date(0).getHours()",
    "Temporal.Now.instant().epochNanoseconds.toString()",
    "Temporal.Now.timeZoneId()",
    "performance.timeOrigin",
    "performance.now()",
    'new Intl.DateTimeFormat("en", { year: "numeric" }).format()',
    'new Intl.DateTimeFormat("en").resolvedOptions().timeZone',
  ].join(", ");
  const expected =
    "0.8833108082136426|0.43152799704850997|0|0|0|0|UTC|0|0|1970|UTC";

  for (const mode of ["interpreted", "bytecode"] as const) {
    for (let run = 0; run < 2; run++) {
      const { exitCode, json, stderr } = runLoaderJson(
        `[${source}].join("|");\n`,
        ["--deterministic", `--mode=${mode}`],
      );
      if (exitCode !== 0)
        throw new Error(`Loader deterministic ${mode} failed: ${stderr}`);
      if (json.files?.[0]?.result !== expected)
        throw new Error(
          `Loader deterministic ${mode} run ${run + 1} expected ${expected}, got ${json.files?.[0]?.result}`,
        );
    }

    const bareSource =
      '[Math.random(), Math.random(), Date.now(), Temporal.Now.instant().epochNanoseconds.toString(), Temporal.Now.timeZoneId()].join("|");\n';
    const bare = Bun.spawnSync(
      [BARE, "-", "--print", "--deterministic", `--mode=${mode}`],
      {
        stdin: new TextEncoder().encode(bareSource),
        stdout: "pipe",
        stderr: "pipe",
      },
    );
    const bareExpected =
      "0.8833108082136426|0.43152799704850997|0|0|UTC";
    if (bare.exitCode !== 0 || bare.stdout.toString().trim() !== bareExpected)
      throw new Error(
        `Bare deterministic ${mode} expected ${bareExpected}, got ${bare.stdout.toString()}${bare.stderr.toString()}`,
      );
  }
}

console.log("--deterministic child realms use stable distinct streams...");
for (const mode of ["interpreted", "bytecode"] as const) {
  const shadow = runLoaderJson(
    'const child = new ShadowRealm(); [Math.random(), child.evaluate("Math.random()")].join("|");\n',
    ["--deterministic", "--unsafe-shadowrealm", `--mode=${mode}`],
  );
  const shadowExpected = "0.8833108082136426|0.6524484863740322";
  if (shadow.exitCode !== 0 || shadow.json.files?.[0]?.result !== shadowExpected)
    throw new Error(
      `ShadowRealm deterministic ${mode} expected ${shadowExpected}, got ${shadow.json.files?.[0]?.result}${shadow.stderr}`,
    );

  const bareRealmSource = [
    "const realm = Goccia.test262.createRealm();",
    '[Math.random(), realm.evalScript("Math.random()"), realm.evalScript("Goccia.test262.createRealm().evalScript(\\"Math.random()\\")")].join("|");',
    "",
  ].join("\n");
  const bareRealm = Bun.spawnSync(
    [
      BARE,
      "-",
      "--print",
      "--test262-host",
      "--deterministic",
      `--mode=${mode}`,
    ],
    {
      stdin: new TextEncoder().encode(bareRealmSource),
      stdout: "pipe",
      stderr: "pipe",
    },
  );
  const bareRealmExpected =
    "0.8833108082136426|0.11260056966858045|0.40713339556004036";
  if (
    bareRealm.exitCode !== 0 ||
    bareRealm.stdout.toString().trim() !== bareRealmExpected
  )
    throw new Error(
      `Test262 realm deterministic ${mode} expected ${bareRealmExpected}, got ${bareRealm.stdout.toString()}${bareRealm.stderr.toString()}`,
    );
}

console.log("--deterministic keeps timeout clock live...");
{
  const proc = Bun.spawnSync(
    [LOADER, "--deterministic", "--compat-while-loops", "--timeout=20"],
    {
      stdin: new TextEncoder().encode("while (true) {}\n"),
      stdout: "pipe",
      stderr: "pipe",
      timeout: 5_000,
    },
  );
  const output = proc.stdout.toString() + proc.stderr.toString();
  if (proc.exitCode === 0 || !output.includes("timed out"))
    throw new Error(
      `Deterministic timeout should use the real infrastructure clock, got exit ${proc.exitCode}: ${output}`,
    );
}

// -- --unsafe-ffi gating --------------------------------------------------------

console.log("--unsafe-ffi gating...");
{
  const { json } = runLoaderJson("typeof FFI;\n");
  if (json.files?.[0]?.result !== "undefined") throw new Error(`FFI without flag should be "undefined", got ${json.files?.[0]?.result}`);

  const { json: jsonOn } = runLoaderJson("typeof FFI;\n", ["--unsafe-ffi"]);
  if (jsonOn.files?.[0]?.result !== "object") throw new Error(`FFI with flag should be "object", got ${jsonOn.files?.[0]?.result}`);
}

// -- --compat-asi (Loader + Bundler) ---------------------------------------------------

console.log("--compat-asi (Loader + Bundler)...");
{
  const tmp = mkdtemp("goccia-asi-");
  try {
    const src = join(tmp, "no-semi.js");
    writeFileSync(src, "const x = 42\nx\n");

    // Loader without --compat-asi should fail
    const noAsi = await $`${LOADER} ${src} 2>&1`.nothrow();
    if (noAsi.exitCode === 0) throw new Error("Loader should reject without --compat-asi");
    if (!noAsi.text().includes("SyntaxError")) throw new Error("Expected SyntaxError without --compat-asi");

    const oldAsi = await $`${LOADER} ${src} --asi 2>&1`.nothrow();
    if (oldAsi.exitCode === 0) throw new Error("Loader should reject removed --asi alias");
    if (!oldAsi.text().includes("Unknown option: --asi")) throw new Error(`Expected unknown --asi, got: ${oldAsi.text()}`);

    // Loader with --compat-asi should succeed
    const withAsi = await $`${LOADER} --print ${src} --compat-asi 2>&1`.text();
    if (!containsLine(withAsi, "42")) throw new Error(`Expected 42 with --compat-asi, got: ${withAsi}`);

    // Bundler without --compat-asi should fail
    const bundleNoAsi = await $`${BUNDLER} ${src} 2>&1`.nothrow();
    if (bundleNoAsi.exitCode === 0) throw new Error("Bundler should reject without --compat-asi");

    const bundleOldAsi = await $`${BUNDLER} ${src} --asi 2>&1`.nothrow();
    if (bundleOldAsi.exitCode === 0) throw new Error("Bundler should reject removed --asi alias");
    if (!bundleOldAsi.text().includes("Unknown option: --asi")) throw new Error(`Expected unknown --asi, got: ${bundleOldAsi.text()}`);

    // Bundler with --compat-asi should succeed
    await $`${BUNDLER} ${src} --compat-asi`.quiet();
    if (!existsSync(join(tmp, "no-semi.gbc"))) throw new Error("Bundler --compat-asi should produce .gbc");
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
    if (!bareNoFlag.text().includes("SyntaxError"))
      throw new Error(`Bare without compat flags should report SyntaxError, got: ${bareNoFlag.text()}`);

    // Stdin path — the exact shape run_test262_suite.ts uses (source piped
    // into a `-` argument).  Without this, file-path is the only invocation
    // mode covered.
    const bareStdin = await $`cat ${bothSrc} | ${BARE} --print - --compat-var --compat-function 2>&1`.text();
    if (bareStdin.trim() !== "22") throw new Error(`Bare stdin --compat-var --compat-function expected 22, got: ${bareStdin}`);

    const largeGlobalSrc = join(tmp, "large-global-declarations.js");
    writeFileSync(
      largeGlobalSrc,
      [
        `var ${Array.from({ length: 260 }, (_, i) => `globalName${i}`).join(", ")};`,
        "var lateVarBinding = 4;",
        "const lateConstBinding = 1;",
        "let lateLetBinding = 2;",
        "function lateFunctionBinding() { return 3; }",
        "lateVarBinding + lateConstBinding + lateLetBinding + lateFunctionBinding();",
        "",
      ].join("\n"),
    );
    const largeGlobalOut = await $`${BARE} --print ${largeGlobalSrc} --mode=bytecode --compat-var --compat-function 2>&1`.text();
    if (largeGlobalOut.trim() !== "10")
      throw new Error(`Bare bytecode large global declarations expected 10, got: ${largeGlobalOut}`);

    const wideNames = Array.from({ length: 260 }, (_, i) => `wideName${i}`);
    const wideDirectEvalSrc = join(tmp, "wide-direct-eval.js");
    writeFileSync(
      wideDirectEvalSrc,
      [
        "function readWideLocal() {",
        ...wideNames.map((name, index) => `  let ${name} = ${index};`),
        '  return eval("wideName259");',
        "}",
        "readWideLocal();",
        "",
      ].join("\n"),
    );
    const wideDirectEvalOut = await $`${BARE} --print ${wideDirectEvalSrc} --mode=bytecode --compat-function --test262-host 2>&1`.text();
    if (wideDirectEvalOut.trim() !== "259")
      throw new Error(`Bare bytecode wide direct eval expected 259, got: ${wideDirectEvalOut}`);

    const wideCapturedBlockSrc = join(tmp, "wide-captured-block.js");
    writeFileSync(
      wideCapturedBlockSrc,
      [
        "let readWideBlock;",
        "{",
        ...wideNames.map((name, index) => `  let ${name} = ${index};`),
        `  readWideBlock = () => [${wideNames.join(", ")}];`,
        "}",
        "const wideValues = readWideBlock();",
        "wideValues[0] + wideValues[259];",
        "",
      ].join("\n"),
    );
    const wideCapturedBlockOut = await $`${BARE} --print ${wideCapturedBlockSrc} --mode=bytecode 2>&1`.text();
    if (wideCapturedBlockOut.trim() !== "259")
      throw new Error(`Bare bytecode wide captured block expected 259, got: ${wideCapturedBlockOut}`);

    const thrownObjectSrc = join(tmp, "throw-test262-object.js");
    writeFileSync(
      thrownObjectSrc,
      [
        "function Test262Error(message) {",
        "  this.message = message || '';",
        "}",
        "Test262Error.prototype.toString = function () {",
        "  return 'Test262Error: ' + this.message;",
        "};",
        "throw new Test262Error('issue 830 readable failure message');",
        "",
      ].join("\n"),
    );
    const thrownObject = await $`cat ${thrownObjectSrc} | ${BARE} --mode=bytecode - --compat-function 2>&1`.nothrow();
    const thrownObjectOut = thrownObject.text();
    if (thrownObject.exitCode === 0)
      throw new Error("Bare thrown Test262Error-style object should fail");
    if (!thrownObjectOut.includes("issue 830 readable failure message"))
      throw new Error(`Bare thrown Test262Error-style object should report its message, got: ${thrownObjectOut}`);
    if (thrownObjectOut.trim() === "[object Object]")
      throw new Error("Bare thrown Test262Error-style object should not collapse to [object Object]");

    // --compat-all regression guard: the flag was removed and must now be
    // rejected as an unknown option.
    const bareCompatAll = await $`echo 'x;' | ${BARE} --compat-all - 2>&1`.nothrow();
    const compatAllOut = bareCompatAll.stdout.toString();
    if (bareCompatAll.exitCode === 0 || !compatAllOut.includes("--compat-all"))
      throw new Error(`Bare must reject --compat-all, got exit ${bareCompatAll.exitCode}: ${compatAllOut}`);

    const bareWarningSrc = join(tmp, "warning-unsupported.js");
    writeFileSync(bareWarningSrc, "while (false) {}\n23;\n");
    const bareWarningDefault = await $`${BARE} --print ${bareWarningSrc} 2>&1`.nothrow();
    if (bareWarningDefault.exitCode === 0)
      throw new Error("Bare without warning flag should reject unsupported while");
    if (!bareWarningDefault.text().includes("SyntaxError") ||
        !bareWarningDefault.text().includes("'while' loops are not supported by default"))
      throw new Error(`Bare without warning flag should report while SyntaxError, got: ${bareWarningDefault.text()}`);

    for (const args of [[] as string[], ["--mode=bytecode"]]) {
      const bareWarningFile = await $`${BARE} --print ${bareWarningSrc} --warning-unsupported-features ${args} 2>&1`.text();
      if (!bareWarningFile.includes("Warning: 'while' loops are not supported by default") ||
          !bareWarningFile.includes("23"))
        throw new Error(`Bare warning mode file path should warn and print 23, got: ${bareWarningFile}`);

      const bareWarningStdin = await $`cat ${bareWarningSrc} | ${BARE} --print - --warning-unsupported-features ${args} 2>&1`.text();
      if (!bareWarningStdin.includes("Warning: 'while' loops are not supported by default") ||
          !bareWarningStdin.includes("23"))
        throw new Error(`Bare warning mode stdin should warn and print 23, got: ${bareWarningStdin}`);
    }

    const shadowWarningSrc = join(tmp, "shadow-warning.js");
    writeFileSync(
      shadowWarningSrc,
      [
        "const realm = new ShadowRealm();",
        "const fromEvaluate = realm.evaluate('while (false) {} 23;');",
        "const fromEval = realm.evaluate(\"eval('while (false) {} 24;')\");",
        "fromEvaluate + fromEval;",
        "",
      ].join("\n"),
    );
    const shadowWarningProc = await $`${BARE} --print ${shadowWarningSrc} --unsafe-shadowrealm --test262-host --warning-unsupported-features 2>&1`.nothrow();
    const shadowWarningOut = shadowWarningProc.text();
    if (shadowWarningProc.exitCode !== 0 ||
        !containsLine(shadowWarningOut, "47"))
      throw new Error(`ShadowRealm child realm should inherit warning-unsupported-features, got: ${shadowWarningOut}`);

    const forSrc = join(tmp, "use-for.js");
    writeFileSync(forSrc, "let s = 0;\nfor (let i = 1; i <= 5; i++) { s = s + i; }\ns;\n");
    const forOut = await $`${BARE} --print ${forSrc} --compat-traditional-for-loop 2>&1`.text();
    if (forOut.trim() !== "15") throw new Error(`Bare --compat-traditional-for-loop expected 15, got: ${forOut}`);

    const forInSrc = join(tmp, "use-for-in.js");
    writeFileSync(forInSrc, "const obj = { a: 1, b: 2 };\nlet out = '';\nfor (const k in obj) { out = out + k; }\nout;\n");
    const forInOut = await $`${BARE} --print ${forInSrc} --compat-for-in-loop 2>&1`.text();
    if (forInOut.trim() !== "ab") throw new Error(`Bare --compat-for-in-loop expected ab, got: ${forInOut}`);

    const whileSrc = join(tmp, "use-while.js");
    writeFileSync(whileSrc, "let s = 0;\nlet i = 1;\nwhile (i <= 5) { s = s + i; i++; }\ns;\n");
    const whileOut = await $`${BARE} --print ${whileSrc} --compat-while-loops 2>&1`.text();
    if (whileOut.trim() !== "15") throw new Error(`Bare --compat-while-loops expected 15, got: ${whileOut}`);

    const whileBcOut = await $`${LOADER} --print ${whileSrc} --mode=bytecode --compat-while-loops 2>&1`.text();
    if (!containsLine(whileBcOut, "15")) throw new Error(`Loader bytecode --compat-while-loops expected 15, got: ${whileBcOut}`);

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

    const loaderOut = await $`${LOADER} --print ${src} --compat-function --compat-non-strict-mode --compat-arguments-object 2>&1`.text();
    if (!containsLine(loaderOut, "7")) throw new Error(`Loader --compat-non-strict-mode expected 7, got: ${loaderOut}`);

    const loaderBcOut = await $`${LOADER} --print ${src} --mode=bytecode --compat-function --compat-non-strict-mode --compat-arguments-object 2>&1`.text();
    if (!containsLine(loaderBcOut, "7")) throw new Error(`Loader bytecode --compat-non-strict-mode expected 7, got: ${loaderBcOut}`);

    const outPath = join(tmp, "use-nonstrict.gbc");
    await $`${BUNDLER} ${src} --output=${outPath} --compat-function --compat-non-strict-mode --compat-arguments-object`.quiet();
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
        "looseCreated = 3;",
        "let calledThis;",
        "const receiverObj = { x: 4, tag(strings) { return this.x; } };",
        "with (receiverObj) {",
        "  calledThis = tag``;",
        "}",
        "obj.fixed + withObj.value + looseCreated + calledThis;",
      ].join("\n") + "\n",
    );
    const assignmentOutPath = join(tmp, "assignment-nonstrict.gbc");
    await $`${BUNDLER} ${assignmentSrc} --output=${assignmentOutPath} --compat-non-strict-mode`.quiet();
    const bundledAssignmentOut = await $`${LOADER} --print ${assignmentOutPath} 2>&1`.text();
    if (!containsLine(bundledAssignmentOut, "9")) throw new Error(`Bundled non-strict assignment expected 9, got: ${bundledAssignmentOut}`);

    const moduleWithSrc = join(tmp, "module-with.js");
    writeFileSync(moduleWithSrc, "with ({ x: 1 }) { x; }\n");
    const moduleWithInterp = await $`${LOADER} ${moduleWithSrc} --source-type=module --compat-non-strict-mode 2>&1`.nothrow();
    const moduleWithInterpOutput = moduleWithInterp.text();
    if (moduleWithInterp.exitCode === 0 || !moduleWithInterpOutput.includes("'with' statements are not allowed in strict mode"))
      throw new Error(`Module with should fail as strict code in interpreter mode, got: ${moduleWithInterpOutput}`);
    const moduleWithBytecode = await $`${LOADER} ${moduleWithSrc} --source-type=module --mode=bytecode --compat-non-strict-mode 2>&1`.nothrow();
    const moduleWithBytecodeOutput = moduleWithBytecode.text();
    if (moduleWithBytecode.exitCode === 0 || !moduleWithBytecodeOutput.includes("'with' statements are not allowed in strict mode"))
      throw new Error(`Module with should fail as strict code in bytecode mode, got: ${moduleWithBytecodeOutput}`);
    const moduleWithWarning = await $`${LOADER} ${moduleWithSrc} --source-type=module --compat-non-strict-mode --warning-unsupported-features 2>&1`.nothrow();
    const moduleWithWarningOutput = moduleWithWarning.text();
    if (moduleWithWarning.exitCode === 0 || !moduleWithWarningOutput.includes("'with' statements are not allowed in strict mode"))
      throw new Error(`Module with should remain strict even in warning mode, got: ${moduleWithWarningOutput}`);

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
    const trOut = await $`${TESTRUNNER} ${testSrc} --no-progress --compat-function --compat-non-strict-mode --compat-arguments-object 2>&1`.text();
    if (!trOut.includes("Passed: 1")) throw new Error(`TestRunner --compat-non-strict-mode expected Passed: 1, got: ${trOut}`);

    const bareOut = await $`${BARE} --print ${src} --compat-function --compat-non-strict-mode --compat-arguments-object 2>&1`.text();
    if (bareOut.trim() !== "7") throw new Error(`Bare --compat-non-strict-mode expected 7, got: ${bareOut}`);

    const staticSetterSrc = join(tmp, "static-setter-nonstrict.js");
    writeFileSync(
      staticSetterSrc,
      [
        "let captured = 0;",
        'const computed = "computed";',
        'const symbolKey = Symbol("staticSetter");',
        "class C {",
        "  static set eval(value) { captured = captured + value; }",
        "  static set arguments(value) { captured = captured + value * 10; }",
        "  static set [computed](value) { captured = captured + value * 100; }",
        "  static set [symbolKey](value) { captured = captured + value * 1000; }",
        "}",
        "C.eval = 1;",
        "C.arguments = 2;",
        "C[computed] = 3;",
        "C[symbolKey] = 4;",
        "captured;",
      ].join("\n") + "\n",
    );
    const staticSetterBareOut = await $`${BARE} --print ${staticSetterSrc} --compat-non-strict-mode 2>&1`.text();
    if (staticSetterBareOut.trim() !== "4321")
      throw new Error(`Bare static setters in non-strict mode expected 4321, got: ${staticSetterBareOut}`);

    const noFlagOut = join(tmp, "no-flag.gbc");
    const bundleNoFlag = await $`${BUNDLER} ${src} --compat-function --output=${noFlagOut} 2>&1`.nothrow();
    if (bundleNoFlag.exitCode === 0)
      throw new Error(`Bundler without --compat-non-strict-mode should fail unsupported with by default`);
    if (!bundleNoFlag.text().includes("SyntaxError") ||
        !bundleNoFlag.text().includes("'with' statements require --compat-non-strict-mode"))
      throw new Error(`Bundler without --compat-non-strict-mode should report SyntaxError, got: ${bundleNoFlag.text()}`);
    if (existsSync(noFlagOut)) throw new Error("Bundler default failure should not write bytecode output");

    const warningOut = join(tmp, "warning-with.gbc");
    await $`${BUNDLER} ${src} --compat-function --warning-unsupported-features --output=${warningOut}`.quiet();
    if (!existsSync(warningOut)) throw new Error("Bundler --warning-unsupported-features should preserve warning recovery mode");
  } finally {
    clean(tmp);
  }
}

// -- --mode=bytecode (Loader: both execution modes produce 4) -------------------

console.log("--mode=bytecode...");
{
  const interpOut = await $`echo 'const x = 2 + 2; x;' | ${LOADER} --print`.text();
  if (!containsLine(interpOut, "4")) throw new Error(`Interpreted expected 4 on its own line, got: ${interpOut}`);
  if (!interpOut.includes("(interpreted)")) throw new Error(`Expected (interpreted) in output`);

  const bcOut = await $`echo 'const x = 2 + 2; x;' | ${LOADER} --print - --mode=bytecode`.text();
  if (!containsLine(bcOut, "4")) throw new Error(`Bytecode expected 4 on its own line, got: ${bcOut}`);
  if (!bcOut.includes("(bytecode)")) throw new Error(`Expected (bytecode) in output`);

  const bcSplitOut = await $`echo 'const x = 2 + 2; x;' | ${LOADER} --print - --mode bytecode`.text();
  if (!containsLine(bcSplitOut, "4")) throw new Error(`Bytecode split option expected 4 on its own line, got: ${bcSplitOut}`);
  if (!bcSplitOut.includes("(bytecode)")) throw new Error(`Expected (bytecode) in split option output`);
}

// -- --source-type and .mjs module inference -----------------------------------

console.log("--source-type and .mjs module inference (Loader + TestRunner + Bundler)...");
{
  const tmp = mkdtemp("goccia-source-type-");
  try {
    const moduleEntry = join(tmp, "entry.mjs");
    writeFileSync(moduleEntry, "this === undefined;\n");

    const loaderMjs = await $`${LOADER} --print ${moduleEntry} 2>&1`.text();
    if (!containsLine(loaderMjs, "true")) throw new Error(`Loader .mjs should infer module source, got: ${loaderMjs}`);

    const loaderMjsBytecode = await $`${LOADER} --print ${moduleEntry} --mode=bytecode 2>&1`.text();
    if (!containsLine(loaderMjsBytecode, "true"))
      throw new Error(`Loader .mjs bytecode should infer module source, got: ${loaderMjsBytecode}`);

    const loaderScriptOverride = await $`${LOADER} --print ${moduleEntry} --source-type=script 2>&1`.text();
    if (!containsLine(loaderScriptOverride, "false"))
      throw new Error(`Loader --source-type=script should override .mjs inference, got: ${loaderScriptOverride}`);

    const loaderScriptOverrideBytecode = await $`${LOADER} --print ${moduleEntry} --mode=bytecode --source-type=script 2>&1`.text();
    if (!containsLine(loaderScriptOverrideBytecode, "false"))
      throw new Error(`Loader bytecode --source-type=script should override .mjs inference, got: ${loaderScriptOverrideBytecode}`);

    const testModuleEntry = join(tmp, "entry-test.mjs");
    writeFileSync(
      testModuleEntry,
      [
        "const topLevelThis = this;",
        "const metaUrl = import.meta.url;",
        'test(".mjs top-level this", () => { expect(topLevelThis).toBeUndefined(); });',
        'test(".mjs import.meta", () => { expect(metaUrl.endsWith("entry-test.mjs")).toBe(true); });',
      ].join("\n") + "\n",
    );
    const testRunnerMjs = await $`${TESTRUNNER} ${testModuleEntry} --no-progress 2>&1`.text();
    if (!testRunnerMjs.includes("Passed: 2")) throw new Error(`TestRunner .mjs expected Passed: 2, got: ${testRunnerMjs}`);

    const testRunnerMjsBytecode = await $`${TESTRUNNER} ${testModuleEntry} --mode=bytecode --no-progress 2>&1`.text();
    if (!testRunnerMjsBytecode.includes("Passed: 2"))
      throw new Error(`TestRunner .mjs bytecode expected Passed: 2, got: ${testRunnerMjsBytecode}`);

    const testScriptOverride = join(tmp, "entry-test-script.mjs");
    writeFileSync(
      testScriptOverride,
      [
        "const topLevelThis = this;",
        'test(".mjs script override", () => { expect(topLevelThis === undefined).toBe(false); });',
      ].join("\n") + "\n",
    );
    const testRunnerScript = await $`${TESTRUNNER} ${testScriptOverride} --source-type=script --no-progress 2>&1`.text();
    if (!testRunnerScript.includes("Passed: 1"))
      throw new Error(`TestRunner --source-type=script expected Passed: 1, got: ${testRunnerScript}`);

    const testRunnerScriptBytecode = await $`${TESTRUNNER} ${testScriptOverride} --mode=bytecode --source-type=script --no-progress 2>&1`.text();
    if (!testRunnerScriptBytecode.includes("Passed: 1"))
      throw new Error(`TestRunner bytecode --source-type=script expected Passed: 1, got: ${testRunnerScriptBytecode}`);

    const strictModule = join(tmp, "strict-module.mjs");
    writeFileSync(strictModule, "with ({ x: 1 }) { x; }\n");
    const bundleModule = await $`${BUNDLER} ${strictModule} --compat-non-strict-mode 2>&1`.nothrow();
    const bundleModuleOutput = bundleModule.text();
    if (bundleModule.exitCode === 0 || !bundleModuleOutput.includes("'with' statements are not allowed in strict mode"))
      throw new Error(`Bundler .mjs should infer strict module source, got: ${bundleModuleOutput}`);

    const bundledScriptOut = join(tmp, "strict-module-script.gbc");
    await $`${BUNDLER} ${strictModule} --source-type=script --compat-non-strict-mode --output=${bundledScriptOut}`.quiet();
    if (!existsSync(bundledScriptOut)) throw new Error("Bundler --source-type=script should override .mjs inference and write bytecode");
  } finally {
    clean(tmp);
  }
}

// -- --timeout (Loader: infinite loop, both execution modes) --------------------

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

// -- --timeout inside long-running NATIVE operations ----------------------------
//
// A single JS statement can stall inside one native call (dense hole-fill for
// a huge array index, or a backtracking regex scan over a long subject).  The
// deadline must interrupt those loops too, not just the dispatch loop between
// JS-level steps.  Keep the regex fixture away from raw negative-scan
// prefilters: those are supposed to complete quickly.

const regexTimeoutScan = 'const s = "a".repeat(2000000); /a*b/.test(s);\n';

console.log("--timeout (native sparse-array fill, interpreted)...");
{
  // Far single-index writes route to sparse storage and complete instantly;
  // the Array constructor still materializes dense holes, so it stalls.
  // --max-memory=0 lifts the budget: 2**30 pointers is exactly the 8 GiB
  // default cap, so the allocation gate would otherwise refuse the request
  // up front and this would assert the memory limit instead of the deadline.
  // The stall is still bounded — hole extension polls the deadline as it
  // grows, so only the fraction allocated within 50ms is ever committed.
  const fill = "const x = new Array(2 ** 30); x.length;\n";
  const { exitCode, json } = runLoaderJson(fill, ["--timeout=50", "--max-memory=0"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Array-fill timeout exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

console.log("--timeout (native sparse-array fill, bytecode)...");
{
  // Far single-index writes route to sparse storage and complete instantly;
  // the Array constructor still materializes dense holes, so it stalls.
  // --max-memory=0 for the same reason as the interpreted case above.
  const fill = "const x = new Array(2 ** 30); x.length;\n";
  const { exitCode, json } = runLoaderJson(fill, ["--timeout=50", "--max-memory=0", "--mode=bytecode"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Bytecode array-fill timeout exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

console.log("--timeout (native regex scan, interpreted)...");
{
  const { exitCode, json } = runLoaderJson(regexTimeoutScan, ["--timeout=50"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Regex-scan timeout exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

console.log("--timeout (native regex scan, bytecode)...");
{
  const { exitCode, json } = runLoaderJson(regexTimeoutScan, ["--timeout=50", "--mode=bytecode"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Bytecode regex-scan timeout exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

// -- --timeout must escape promise boundaries (dynamic import) ------------------
//
// The deadline abort must not be convertible into a JS-catchable rejection:
// a module that stalls inside import() previously surfaced as a caught
// rejection and the script "succeeded" with exit 0, defeating --timeout.

for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
  const label = modeArgs.length ? "bytecode" : "interpreted";
  console.log(`--timeout (dynamic import stall, ${label})...`);
  const tmp = mkdtemp("goccia-timeout-import-");
  try {
    const dep = join(tmp, "dep.js");
    writeFileSync(dep, `${regexTimeoutScan}export const ready = true;\n`);
    const main = join(tmp, "main.js");
    writeFileSync(main, 'import("./dep.js").then(() => console.log("LOADED")).catch((e) => console.log("CAUGHT: " + e));\n');
    const proc = Bun.spawnSync(
      [LOADER, main, "--output=json", "--timeout=100", ...modeArgs],
      { stdout: "pipe", stderr: "pipe", timeout: 10_000 },
    );
    const json = JSON.parse(proc.stdout.toString());
    if (proc.exitCode !== 1) throw new Error(`Dynamic-import timeout (${label}) exit code should be 1, got ${proc.exitCode}`);
    if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError (${label}), got ${json.error?.type}`);
    const printed = (json.stdout ?? "") + proc.stdout.toString();
    if (printed.includes("CAUGHT")) throw new Error(`Timeout was swallowed by JS .catch() (${label})`);
  } finally {
    clean(tmp);
  }
}

// -- --max-instructions (Loader: infinite loop, both execution modes) -----------

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
  const { json } = runLoaderJson("Goccia.gc.maxBytes\n", ["--compat-asi"]);
  if (typeof json.files?.[0]?.result !== "number" || json.files[0].result <= 0) throw new Error(`Default maxBytes should be positive, got ${json.files?.[0]?.result}`);
}

console.log("--max-memory (override)...");
{
  const { json } = runLoaderJson("Goccia.gc.maxBytes\n", ["--max-memory=5000000", "--compat-asi"]);
  if (json.files?.[0]?.result !== 5000000) throw new Error(`Override maxBytes should be 5000000, got ${json.files?.[0]?.result}`);
}

console.log("--max-memory (OOM triggers RangeError)...");
{
  const res = await $`echo 'Array.from({length:5000},(_,i)=>({x:i}));' | ${LOADER} --max-memory=200000 --compat-asi 2>&1`.nothrow();
  const out = res.text();
  if (res.exitCode !== 1) throw new Error(`OOM exit code should be 1, got ${res.exitCode}`);
  if (!out.includes("RangeError")) throw new Error(`OOM output should contain RangeError`);
}

console.log("--max-memory (own-key enumeration survives a mid-loop collection)...");
{
  // Enumerating a large property map allocates one string per key, and every
  // string allocation is charged against the ceiling — so it is a GC safe
  // point. The half-built result array must stay rooted across the loop or the
  // collection sweeps it and the builtin writes through a dangling pointer
  // (bus error / nil dereference, depending on build flags).
  const src = [
    "const outer = Array.from({ length: 30 }, (_, i) => i);",
    "const inner = Array.from({ length: 1000 }, (_, i) => i);",
    "const o = {};",
    "for (const a of outer) { for (const b of inner) { o['k' + (a * 1000 + b)] = b; } }",
    "const total = Object.keys(o).length + Object.values(o).length + Object.entries(o).length +",
    "  Object.getOwnPropertyNames(o).length + Reflect.ownKeys(o).length;",
    "console.log('total', total);",
    "total;",
    "",
  ].join("\n");

  // The window in which a collection lands mid-enumeration moves with the
  // build's object sizes, so sweep limits rather than pinning one value.
  for (const maxMemory of [1_048_576, 1_572_864, 2_097_152, 3_145_728, 8_388_608]) {
    const proc = Bun.spawnSync([LOADER, `--max-memory=${maxMemory}`, "--compat-asi"], {
      stdin: new TextEncoder().encode(src),
      stdout: "pipe",
      stderr: "pipe",
      timeout: 60_000,
    });
    const out = proc.stdout.toString() + proc.stderr.toString();
    // The engine has two memory limiters and they surface differently, which is
    // why the clauses below excuse one text and reject every other fatal. A
    // charged allocation (string payload, buffer) raises a script-catchable
    // RangeError. The growth gate (RequireNativeBytes, Goccia.MemoryLimit.pas)
    // raises TGocciaMemoryLimitError, which is deliberately opaque to the guest:
    // every boundary re-raises it, it escapes to the host, and the loader reports
    // it as "Fatal error: ... would exceed the memory budget" with exit 1. That
    // text is a refusal by design, not a crash — do not tighten these clauses
    // into failures. Any other fatal — bus error, access violation, nil object
    // check — is the heap corruption this test guards against.
    if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
      throw new Error(`Own-key enumeration at --max-memory=${maxMemory} crashed: ${out}`);
    if (proc.exitCode !== 0 && !out.includes("RangeError") && !out.includes("would exceed the memory budget"))
      throw new Error(`Own-key enumeration at --max-memory=${maxMemory} failed without a clean refusal (exitCode=${proc.exitCode}): ${out}`);
    // A run that completes under pressure must also have enumerated
    // correctly — a wrong count here is silent heap corruption.
    if (proc.exitCode === 0 && !out.includes("total 150000"))
      throw new Error(`Own-key enumeration at --max-memory=${maxMemory} completed with wrong total: ${out}`);
  }

  // With headroom the enumeration must complete and return every key.
  const { exitCode, json, stderr } = runLoaderJson(src, ["--max-memory=134217728", "--compat-asi"], { timeout: 60_000 });
  if (exitCode !== 0) throw new Error(`Own-key enumeration exit code should be 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
  if (json.files?.[0]?.result !== 150000) throw new Error(`Own-key enumeration should return 150000, got ${json.files?.[0]?.result}`);
}

// Parking is measured, never assumed. A fixed ballast cap fails silently in
// both directions: at a tight ceiling the loop stops while the budget still has
// room to spare, and at a wide one the cap runs out long before the ceiling is
// in reach — either way the probe runs unparked and passes without ever entering
// the window it exists to test. This grows ballast in bounded passes, collecting
// between them so each measurement counts live bytes only, and reports the
// outcome for the assertions to insist on.
//
// Shared by every parked-heap block below (parser probes, parse ceiling, parse
// gate, stringify gate) so the calibration is defined once: they differ only in
// the slack they park at, which is measured per block and passed in here.
const parkingPreamble = (slackTarget: number): string[] => [
  `const SLACK = ${slackTarget};`,
  // Sized from the ceiling so it cannot run out, and materialised before the
  // first measurement so it counts as baseline live set instead of quietly
  // defeating the parking it is driving.
  "const iters = Array.from({ length: Math.ceil(Goccia.gc.maxBytes / 4096) + 64 }, (_, j) => j);",
  "const ballast = [];",
  "let slack = Goccia.gc.maxBytes;",
  "Goccia.gc();",
  // Each pass adds only live ballast and then collects, so the post-collection
  // slack falls monotonically and a handful of passes converges. Measuring
  // before the collection is what let the old loop stop early: it was reading
  // garbage the next collection would hand straight back.
  //
  // The push threshold sits one ballast chunk below the target because each
  // collection hands back a little transient garbage: pushing to exactly SLACK
  // lets the post-collection measurement bounce back just above it and stall
  // there for every remaining pass (CI stalled 116 bytes short of a 600000
  // target this way — the baseline live set differs per platform, so the
  // convergence point does too). The pass cap is a generous termination bound,
  // not a calibration: parking breaks out early on the first pass that lands.
  "for (const pass of Array.from({ length: 32 }, (_, p) => p)) {",
  "  for (const i of iters) {",
  "    if (Goccia.gc.maxBytes - Goccia.gc.bytesAllocated <= SLACK - 8192) break;",
  '    ballast.push("x".repeat(4096));',
  "  }",
  "  Goccia.gc();",
  "  slack = Goccia.gc.maxBytes - Goccia.gc.bytesAllocated;",
  "  if (slack <= SLACK) break;",
  "}",
  // Every caller asserts on "parked true"; the trailing numbers are diagnostics
  // for when a calibration drifts and the assertion starts failing.
  'console.log("parked", slack <= SLACK, "slack", slack, "ballast", ballast.length);',
];

console.log("--max-memory (builtin result builders survive mid-build collections)...");
{
  // Same defect class as own-key enumeration: any builtin that fills a result
  // container across string allocations (each charged against the ceiling and
  // therefore a GC safe point) must keep that container rooted. This exercises
  // the CSV/TSV parsers and revivers, URLSearchParams, RegExp match arrays,
  // and the Intl resolved-options/parts builders under a sweep of limits.
  const src = [
    'import * as CSVNS from "goccia:csv"; const CSV = CSVNS.CSV ?? CSVNS;',
    'import * as TSVNS from "goccia:tsv"; const TSV = TSVNS.TSV ?? TSVNS;',
    'const rows = Array.from({ length: 3000 }, (_, i) => "a" + i + ",b" + i + ",c" + i).join("\\n");',
    'const parsedCsv = CSV.parse("h1,h2,h3\\n" + rows);',
    'const trows = Array.from({ length: 3000 }, (_, i) => "a" + i + "\\tb" + i).join("\\n");',
    'const params = new URLSearchParams(Array.from({ length: 2000 }, (_, i) => "k=v" + i).join("&"));',
    'const m = "x123y456z".match(/(?<a>\\d+)y(?<b>\\d+)/d);',
    "const total = parsedCsv.length +",
    '  CSV.parse("h1,h2,h3\\n" + rows, {}, (k, v) => v).length +',
    '  CSV.parseChunk("h1,h2,h3\\n" + rows, {}, 0, -1).values.length +',
    '  TSV.parse("h1\\th2\\n" + trows).length +',
    '  params.getAll("k").length + [...params.entries()].length +',
    "  m.indices.groups.b[0] +",
    '  Intl.getCanonicalLocales(["en-US", "de-DE"]).length +',
    '  new Intl.PluralRules("en").resolvedOptions().pluralCategories.length +',
    '  new Intl.ListFormat("en").formatToParts(["a", "b", "c"]).length;',
    "console.log('total', total);",
    "",
  ].join("\n");
  const tmp = mkdtemp("goccia-memroot-");
  const srcPath = join(tmp, "sweep.mjs");
  writeFileSync(srcPath, src);
  try {
    for (const maxMemory of [1_048_576, 1_572_864, 2_097_152, 3_145_728, 8_388_608, 67_108_864]) {
      const proc = Bun.spawnSync([LOADER, `--max-memory=${maxMemory}`, srcPath], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 120_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      // Budget-text fatal = the uncatchable growth gate refusing; see the
      // two-limiter note on the own-key enumeration sweep above.
      if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
        throw new Error(`Builder sweep at --max-memory=${maxMemory} crashed: ${out}`);
      if (proc.exitCode !== 0 && !out.includes("RangeError") && !out.includes("would exceed the memory budget"))
        throw new Error(`Builder sweep at --max-memory=${maxMemory} failed without a clean refusal (exitCode=${proc.exitCode}): ${out}`);
      if (proc.exitCode === 0 && !out.includes("total 16014"))
        throw new Error(`Builder sweep at --max-memory=${maxMemory} completed with wrong total: ${out}`);
    }

    // With headroom the builders must complete and produce the exact total —
    // an all-RangeError sweep would otherwise verify nothing.
    {
      const proc = Bun.spawnSync([LOADER, "--max-memory=134217728", srcPath], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 120_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode !== 0)
        throw new Error(`Builder sweep headroom run should exit 0, got ${proc.exitCode}: ${out}`);
      if (!out.includes("total 16014"))
        throw new Error(`Builder sweep headroom run should report total 16014: ${out}`);
    }

    // Second wave of the same defect class, in builders the first sweep does not
    // reach: the JSONL chunk-result object and its error object, the JSON5
    // reviver context/holder plus the replacer's partially built copies, and the
    // Promise.allSettled entries — each is filled across a string allocation or
    // a property write, and each of those is charged against the ceiling and so
    // is a collecting safe point.
    const wave2Src = [
      'import * as JSONLNS from "goccia:jsonl"; const JSONL = JSONLNS.JSONL ?? JSONLNS;',
      'import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;',
      'const lines = Array.from({ length: 800 }, (_, i) => \'{"k":"v\' + i + \'","n":\' + i + \'}\').join("\\n");',
      'const chunk = JSONL.parseChunk(lines + "\\n");',
      // An unterminated record makes parseChunk build the SyntaxError object too.
      "const badChunk = JSONL.parseChunk('{\"k\":1}\\n{\"k\":\\n');",
      'const json5Text = "{" + Array.from({ length: 500 }, (_, i) => "k" + i + ": \'v" + i + "\'").join(", ") + "}";',
      "const revived = JSON5.parse(json5Text, (k, v) => v);",
      'const nested = JSON5.parse("[" + Array.from({ length: 400 }, (_, i) => "{a: " + i + "}").join(", ") + "]", (k, v) => v);',
      "const replaced = JSON5.stringify(nested, (k, v) => v);",
      "const sync = chunk.values.length + badChunk.values.length +",
      "  (badChunk.error === null ? 0 : 1) + Object.keys(revived).length +",
      "  nested.length + replaced.length;",
      "Promise.allSettled(Array.from({ length: 500 }, (_, i) =>",
      '  i % 2 === 0 ? Promise.resolve("v" + i) : Promise.reject(new Error("e" + i)),',
      ")).then((rs) => {",
      "  console.log('total', sync + rs.length +",
      '    rs.filter((r) => r.status === "fulfilled").length +',
      '    rs.filter((r) => r.status === "rejected").length);',
      "});",
      "",
    ].join("\n");
    const wave2Path = join(tmp, "sweep-wave2.mjs");
    writeFileSync(wave2Path, wave2Src);
    for (const maxMemory of [1_048_576, 1_572_864, 2_097_152, 3_145_728, 8_388_608, 67_108_864]) {
      const proc = Bun.spawnSync([LOADER, `--max-memory=${maxMemory}`, wave2Path], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 120_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      // Budget-text fatal = the uncatchable growth gate refusing, as above.
      if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
        throw new Error(`Wave-2 builder sweep at --max-memory=${maxMemory} crashed: ${out}`);
      if (proc.exitCode !== 0 && !out.includes("RangeError") && !out.includes("would exceed the memory budget"))
        throw new Error(`Wave-2 builder sweep at --max-memory=${maxMemory} failed without a clean refusal (exitCode=${proc.exitCode}): ${out}`);
      if (proc.exitCode === 0 && !out.includes("total 5793"))
        throw new Error(`Wave-2 builder sweep at --max-memory=${maxMemory} completed with wrong total: ${out}`);
    }

    // As above, one guaranteed-success run so an all-refusals sweep cannot pass
    // vacuously.
    {
      const proc = Bun.spawnSync([LOADER, "--max-memory=134217728", wave2Path], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 120_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode !== 0)
        throw new Error(`Wave-2 builder sweep headroom run should exit 0, got ${proc.exitCode}: ${out}`);
      if (!out.includes("total 5793"))
        throw new Error(`Wave-2 builder sweep headroom run should report total 5793: ${out}`);
    }

    // A ceiling sweep only lands in the collecting window by luck: the pressure
    // collection runs inside the allocation that would cross the ceiling, so the
    // heap has to already sit right below it. This shape parks it there on
    // purpose — ballast is grown until the remaining budget is a fixed slack,
    // after which nearly every builder allocation collects — and then keeps the
    // reviver/replacer results alive so a swept container's slot is reused (that
    // reuse is what turns the dangling pointer into an observable fault). With
    // the JSON5 holder/context/copy roots removed this faults within seconds
    // ("Object reference is Nil", "Invalid type cast"); with them it either
    // completes or refuses cleanly.
    const parkedSrc = [
      'import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;',
      "const ballast = [];",
      "for (const i of Array.from({ length: 20000 }, (_, j) => j)) {",
      "  if (Goccia.gc.maxBytes - Goccia.gc.bytesAllocated <= 262144) break;",
      '  ballast.push("x".repeat(2048));',
      "}",
      "const kept = [];",
      "let n = 0;",
      "for (const i of Array.from({ length: 600 }, (_, j) => j)) {",
      "  const revived = JSON5.parse(\"{a: 1, b: 'two'}\", (k, v) => v);",
      "  const text = JSON5.stringify({ a: 1, b: [2, 3] }, (k, v) => v);",
      "  kept.push(revived, text);",
      "  n += Object.keys(revived).length + text.length;",
      "}",
      "console.log('parked', n, ballast.length > 0, kept.length);",
      "",
    ].join("\n");
    const parkedPath = join(tmp, "sweep-parked.mjs");
    writeFileSync(parkedPath, parkedSrc);
    for (const maxMemory of [2_097_152, 3_145_728, 4_194_304]) {
      const proc = Bun.spawnSync([LOADER, `--max-memory=${maxMemory}`, parkedPath], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 180_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      // A parked heap makes the growth gate far more likely to be the limiter
      // that fires, and its refusal is uncatchable by design: the budget text is
      // a legitimate outcome here, anything else fatal is not. See the
      // two-limiter note on the own-key enumeration sweep above.
      if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
        throw new Error(`Parked-heap builder run at --max-memory=${maxMemory} crashed: ${out}`);
      if (proc.exitCode !== 0 && !out.includes("RangeError") && !out.includes("would exceed the memory budget"))
        throw new Error(`Parked-heap builder run at --max-memory=${maxMemory} failed without a clean refusal (exitCode=${proc.exitCode}): ${out}`);
      if (proc.exitCode === 0 && !out.includes("parked 9000 true 1200"))
        throw new Error(`Parked-heap builder run at --max-memory=${maxMemory} completed with wrong result: ${out}`);
    }

    // The same defect class in the recursive parsers, which build their trees in
    // plain Pascal fields and locals (JSON's visitor stack, YAML's key/value
    // locals and anchor map, JSONL's record accumulator, TOML's node tree) that
    // the collector cannot see. The shape below parks the heap right under the
    // ceiling with no other garbage available, so the parse crosses the ceiling
    // partway through and the only thing the pressure collection can free is the
    // in-progress tree itself — after which the parse keeps writing into swept
    // containers and the remaining values reuse their memory.
    //
    // The parse is wrapped in try/catch because a refused allocation surfaces as a
    // catchable JS RangeError. A crash used to be indistinguishable from a clean
    // refusal — both arrived as a caught SyntaxError, because the builtins
    // converted every exception out of the parser into one — so the assertions
    // read the whole line rather than just its shape.
    const USE_AFTER_FREE_SIGNATURES = [
      "Access violation",
      "Invalid type cast",
      "Object reference is Nil",
      "SIGSEGV",
      "Segmentation fault",
      "EAccessViolation",
      // FPC runtime errors for a nil-object call and an invalid typecast, as
      // reported when no handler formats them.
      "Runtime error 210",
      "Runtime error 216",
      "Runtime error 219",
      "Runtime error 204",
    ];

    // A refusal whose message is empty: the shape a blanket `on E: Exception`
    // handler produces when it relabels an engine failure, since the Pascal
    // Message of a thrown JS value is empty by construction. `SyntaxError: ` with
    // nothing after it is a resource ceiling the guest can mistake for bad input,
    // so it is a failure, not a refusal.
    const EMPTY_REFUSAL = /^refused \S+ ::\s*$/m;

    // `expect` is the exact line the parse must produce when it completes.
    // "ok " on its own proves nothing: a container that was swept and had its
    // memory reused almost always yields silently wrong data rather than a crash,
    // so every length and count is read back and compared.
    //
    // `windowCeiling` / `windowSlack` override the collecting-window run for
    // probes whose parse cannot finish in the default window. Every probe must
    // have one configuration that completes while parked, or the read-back check
    // only ever runs unballasted and the window run proves nothing; the values
    // are measured per probe, not guessed. Slack alone was enough for all nine
    // here — no probe needs a different ceiling — but both knobs are exposed
    // because which one moves a stuck probe into its window is a measurement.
    // See the window run below.
    const parserProbes: Array<{
      name: string;
      setup: string[];
      parse: string;
      check: string;
      expect: string;
      windowCeiling?: number;
      windowSlack?: number;
    }> = [
      {
        name: "json-flat",
        setup: [
          'const doc = "[" + Array.from({ length: 1200 }, (_, i) => \'"\' + S + i + \'"\').join(",") + "]";',
        ],
        parse: "JSON.parse(doc)",
        check: "'ok', b.length, b[0].length, b[600].length, b[1199].length",
        expect: "ok 1200 201 203 204",
      },
      {
        name: "json-nested",
        setup: [
          'const doc = "[" + Array.from({ length: 400 }, (_, i) => \'{"a":"\' + S + i + \'","b":["\' + S + \'","\' + S + \'"]}\').join(",") + "]";',
        ],
        parse: "JSON.parse(doc)",
        check: "'ok', b.length, b[0].a.length, b[399].b[1].length",
        expect: "ok 400 201 200",
        // Measured: at the default 600000 the parse only ever refuses; it first
        // completes at 650000 and 800000 leaves margin over that threshold.
        windowSlack: 800_000,
      },
      {
        name: "json5",
        setup: [
          'import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;',
          'const doc = "[" + Array.from({ length: 1200 }, (_, i) => "\'" + S + i + "\'").join(",") + "]";',
        ],
        parse: "JSON5.parse(doc)",
        check: "'ok', b.length, b[0].length, b[600].length, b[1199].length",
        expect: "ok 1200 201 203 204",
      },
      {
        // The JSONL accumulator: one array collecting every parsed record while
        // each line's parse builds strings that can collect. Only the caller's
        // hand-off window was rooted, not the loop that fills it.
        name: "jsonl-chunk",
        setup: [
          'import * as JSONLNS from "goccia:jsonl"; const JSONL = JSONLNS.JSONL ?? JSONLNS;',
          'const doc = Array.from({ length: 400 }, (_, i) => \'{"a":"\' + S + \'","b":"\' + S + i + \'"}\').join("\\n") + "\\n";',
        ],
        parse: "JSONL.parseChunk(doc)",
        check: "'ok', b.values.length, b.values[0].a.length, b.values[399].b.length, b.done, b.error === null",
        expect: "ok 400 200 203 true true",
      },
      {
        // The same accumulator reached through the whole-input entry point, which
        // returns the array directly instead of a chunk record.
        name: "jsonl-parse",
        setup: [
          'import * as JSONLNS from "goccia:jsonl"; const JSONL = JSONLNS.JSONL ?? JSONLNS;',
          'const doc = Array.from({ length: 400 }, (_, i) => \'{"a":"\' + S + \'","b":"\' + S + i + \'"}\').join("\\n") + "\\n";',
        ],
        parse: "JSONL.parse(doc)",
        check: "'ok', b.length, b[0].a.length, b[399].b.length",
        expect: "ok 400 200 203",
      },
      {
        name: "yaml-block-and-flow",
        setup: [
          'import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;',
          'const doc = Array.from({ length: 300 }, (_, i) => "k" + i + ":\\n  a: \'" + S + i + "\'\\n  b: [\'" + S + "\', \'" + S + "\']\\n  c:\\n    - \'" + S + "\'\\n    - \'" + S + "\'").join("\\n") + "\\n";',
        ],
        parse: "YAML.parse(doc)",
        check: "'ok', Object.keys(b).length, b.k0.a.length, b.k299.b[1].length, b.k299.c[1].length",
        expect: "ok 300 201 200 200",
        // Measured: refuses through 750000, first completes at 800000.
        windowSlack: 900_000,
      },
      {
        // Explicit `? key` / `: value` entries: the key survives a full nested
        // node parse before it is canonicalised, and the value survives the
        // canonicalisation. Long keys make both windows wide.
        name: "yaml-explicit-keys",
        setup: [
          'import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;',
          'const K = "k".repeat(200);',
          'const doc = Array.from({ length: 200 }, (_, i) => "? " + K + i + "\\n:\\n  - \'" + S + "\'\\n  - \'" + S + "\'").join("\\n") + "\\n";',
        ],
        parse: "YAML.parse(doc)",
        check: "'ok', Object.keys(b).length, b[K + 0].length, b[K + 199][1].length",
        expect: "ok 200 2 200",
      },
      {
        // Anchors and aliases specifically: a value referenced only by the anchor
        // map is invisible to the collector without a root over that map.
        name: "yaml-anchors",
        setup: [
          'import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;',
          'const doc = Array.from({ length: 250 }, (_, i) => "a" + i + ": &anc" + i + "\\n  x: \'" + S + i + "\'\\n  y: [\'" + S + "\', \'" + S + "\']\\nb" + i + ": *anc" + i + "\\nc" + i + ":\\n  <<: *anc" + i + "\\n  z: \'" + S + i + "\'").join("\\n") + "\\n";',
        ],
        parse: "YAML.parse(doc)",
        check: "'ok', Object.keys(b).length, b.a0.x.length, b.b249.y[1].length, b.c249.z.length, b.c249.x.length",
        expect: "ok 750 201 200 203 203",
      },
      {
        name: "toml",
        setup: [
          'import * as TOMLNS from "goccia:toml"; const TOML = TOMLNS.TOML ?? TOMLNS;',
          'const doc = Array.from({ length: 300 }, (_, i) => "k" + i + \' = "\' + S + i + \'"\\narr\' + i + \' = ["\' + S + \'", "\' + S + \'"]\\ninl\' + i + \' = { x = "\' + S + \'", y = "\' + S + \'" }\').join("\\n") + "\\n";',
        ],
        parse: "TOML.parse(doc)",
        check: "'ok', Object.keys(b).length, b.k0.length, b.arr299[1].length, b.inl299.y.length",
        expect: "ok 900 201 200 200",
        // Measured: refuses through 750000, first completes at 800000.
        windowSlack: 900_000,
      },
    ];

    // Shared verdict for every parked-parse run. `requireParked` is off only for
    // the headroom run, which deliberately carries no ballast. `requireExpect`
    // is on for the calibrated window run, where a refusal is not an acceptable
    // outcome — see there.
    const assertProbeRun = (
      label: string,
      out: string,
      exitCode: number | null,
      expect: string,
      requireParked: boolean,
      requireExpect: boolean,
    ): void => {
      const signature = USE_AFTER_FREE_SIGNATURES.find((text) => out.includes(text));
      if (signature) throw new Error(`${label} hit a use-after-free (${signature}): ${out}`);
      if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
        throw new Error(`${label} crashed: ${out}`);
      if (requireParked && !out.includes("parked true"))
        throw new Error(
          `${label} never got the heap under the ceiling, so it exercised no collecting window: ${out}`,
        );
      if (EMPTY_REFUSAL.test(out))
        throw new Error(
          `${label} refused with an empty message — an engine failure relabelled as a syntax error: ${out}`,
        );
      if (out.includes("refused ") && !out.includes("refused RangeError"))
        throw new Error(
          `${label} refused with something other than the RangeError a memory ceiling raises: ${out}`,
        );
      if (requireExpect) {
        // No `|| refused` escape: this run is calibrated to finish, and a
        // refusal means the calibration drifted rather than that the ceiling
        // did its job. Accepting one here would silently retire the only check
        // that reads the parsed values back under collection pressure.
        if (exitCode !== 0 || !out.includes(expect))
          throw new Error(
            `${label} did not complete with "${expect}" while parked (exitCode=${exitCode}): ${out}`,
          );
        return;
      }
      if (exitCode !== 0 && !out.includes("RangeError") && !out.includes("would exceed the memory budget"))
        throw new Error(`${label} failed without a clean refusal (exitCode=${exitCode}): ${out}`);
      if (exitCode === 0 && !out.includes(expect) && !out.includes("refused RangeError"))
        throw new Error(`${label} produced neither "${expect}" nor a clean refusal: ${out}`);
    };

    for (const probe of parserProbes) {
      const buildSrc = (preamble: string[]): string =>
        [
          ...probe.setup.filter((line) => line.startsWith("import ")),
          'const S = "s".repeat(200);',
          ...probe.setup.filter((line) => !line.startsWith("import ")),
          ...preamble,
          "try {",
          `  const b = ${probe.parse};`,
          `  console.log(${probe.check});`,
          "} catch (e) {",
          "  console.log('refused', e.name, '::', e.message);",
          "}",
          "",
        ].join("\n");

      // A tight window puts the crossing early in the parse, where the most
      // allocation still follows to reuse whatever the sweep freed — 100 KiB is
      // where the YAML explicit-key and JSONL accumulator defects reproduce, and
      // a wider one lets several of them through. 2 MiB is not in the ceiling
      // set: the wider documents cannot even be built at that ceiling, so the run
      // refuses before it has a heap to park and proves nothing.
      const tightPath = join(tmp, `parser-parked-${probe.name}.mjs`);
      writeFileSync(tightPath, buildSrc(parkingPreamble(100_000)));
      for (const maxMemory of [3_145_728, 4_194_304]) {
        const proc = Bun.spawnSync([LOADER, `--max-memory=${maxMemory}`, tightPath], {
          stdout: "pipe",
          stderr: "pipe",
          timeout: 180_000,
        });
        assertProbeRun(
          `Parked-heap ${probe.name} parse at --max-memory=${maxMemory}`,
          proc.stdout.toString() + proc.stderr.toString(),
          proc.exitCode,
          probe.expect,
          true,
          false,
        );
      }

      // A wider window at a wider ceiling: parked enough that the parse collects
      // repeatedly, with enough left over that it can still finish. This is the
      // only shape that can observe a swept container being written into and then
      // read back, which is how a missing root shows up as wrong data rather than
      // as a fault — so this run has to complete, and a refusal fails it.
      //
      // The window is per probe because the documents differ by an order of
      // magnitude in what they need to finish: the default pair completes for six
      // of the nine, while json-nested, yaml-block-and-flow and toml only refuse
      // there and carry a measured `windowSlack` instead. Each override sits above
      // the smallest slack at which that probe was observed to complete, and the
      // slack the preamble converges to is deterministic for a given build, so
      // "completes while parked" is a property of the calibration, not luck.
      {
        const windowCeiling = probe.windowCeiling ?? 6_291_456;
        const windowPath = join(tmp, `parser-window-${probe.name}.mjs`);
        writeFileSync(windowPath, buildSrc(parkingPreamble(probe.windowSlack ?? 600_000)));
        const proc = Bun.spawnSync([LOADER, `--max-memory=${windowCeiling}`, windowPath], {
          stdout: "pipe",
          stderr: "pipe",
          timeout: 180_000,
        });
        assertProbeRun(
          `Collecting-window ${probe.name} parse at --max-memory=${windowCeiling}`,
          proc.stdout.toString() + proc.stderr.toString(),
          proc.exitCode,
          probe.expect,
          true,
          true,
        );
      }

      // One run with room to spare and no ballast at all, so a probe that only
      // ever refuses cannot pass vacuously: the parse has to complete and every
      // value has to read back exactly.
      {
        const headroomPath = join(tmp, `parser-headroom-${probe.name}.mjs`);
        writeFileSync(headroomPath, buildSrc([]));
        const proc = Bun.spawnSync([LOADER, "--max-memory=134217728", headroomPath], {
          stdout: "pipe",
          stderr: "pipe",
          timeout: 180_000,
        });
        const out = proc.stdout.toString() + proc.stderr.toString();
        if (proc.exitCode !== 0)
          throw new Error(`Headroom ${probe.name} run should exit 0, got ${proc.exitCode}: ${out}`);
        if (!out.includes(probe.expect))
          throw new Error(`Headroom ${probe.name} run should report "${probe.expect}": ${out}`);
      }
    }
  } finally {
    clean(tmp);
  }
}

// A refused allocation is a resource ceiling, not an in-language error: a
// ceiling the guest can catch is a ceiling it can ignore in a loop. Every
// shape below wraps the refusal in the handler that used to swallow it, so
// each one fails if a re-raise allowlist ever drops the memory limit again.
// Both execution modes must agree — the interpreter used to let the script
// catch and continue while the VM treated the same refusal as fatal.
//
// The Promise.all/race shapes additionally guard the combinator error path:
// a refusal escaping iteration was re-raised by name from inside the active
// handler (PromiseRejectionReasonFromException), which dereferenced the
// exception object the handler had already freed and surfaced as a spurious
// "Access violation" (exit 1 with the wrong message) instead of the ceiling.
{
  const combinatorIterator =
    "{ [Symbol.iterator]: () => ({ next: () => { const a = new Array(100000000); return { done: true, value: a.length }; } }) }";
  const shapes: Array<[string, string]> = [
    ["sync try/catch", "try { const a = new Array(100000000); a.length; } catch (e) {}\n"],
    [
      "async function body",
      [
        "const grow = async () => { const a = new Array(100000000); return a.length; };",
        "const main = async () => { try { await grow(); } catch (e) {} };",
        "main();",
        "",
      ].join("\n"),
    ],
    [
      "promise executor",
      "try { new Promise((r) => { const a = new Array(100000000); r(a.length); }).catch(() => {}); } catch (e) {}\n",
    ],
    [
      "Promise.all iterator allocation",
      `Promise.all(${combinatorIterator}).then(() => {}, () => {});\n`,
    ],
    [
      "Promise.race iterator allocation",
      `Promise.race(${combinatorIterator}).then(() => {}, () => {});\n`,
    ],
    // `for await ... break` runs the async generator's .return(), executing the
    // body's finally as guest code. A refusal there was folded into a catchable
    // rejection by the interpreter (guest caught it, kept running) while the VM
    // was fatal — a swallow and a mode divergence. Both must be fatal now.
    [
      "async generator return/finally",
      [
        "const obj = { async *g() { try { yield 1; } finally { const a = new Array(100000000); a.length; } } };",
        "const main = async () => { try { for await (const v of obj.g()) { break; } } catch (e) {} };",
        "main();",
        "",
      ].join("\n"),
    ],
  ];

  for (const [shape, src] of shapes) {
    for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
      const label = `${shape} ${modeArgs.length > 0 ? "bytecode" : "interpreted"}`;
      console.log(`--max-memory (refusal is not catchable: ${label})...`);
      const { exitCode, json } = runLoaderJson(src, ["--max-memory=67108864", ...modeArgs], { timeout: 30_000 });
      if (exitCode !== 1) throw new Error(`Memory limit refusal (${label}) should exit 1, got ${exitCode}: ${JSON.stringify(json)}`);
      if (json.error?.type !== "MemoryLimitError") throw new Error(`Memory limit refusal (${label}) should report MemoryLimitError, got ${json.error?.type}`);
    }
  }
}

// The data-format parse builtins are the other half of that convention. Their
// handlers used to catch every Pascal exception out of the parser and re-throw it
// as a SyntaxError, which is the same swallow in a different disguise: the
// ceiling still reached the guest, but wearing a name that says "your input is
// malformed" and carrying no message at all (a thrown JS value's Pascal Message
// is empty by construction, so the conversion produced `SyntaxError: ` and
// nothing else). Each handler now names only its own parse-error class, so a
// refusal keeps its RangeError identity and its message.
//
// TOML is included even though its handler was already narrow — it is the
// reference shape the others were brought in line with, and it should stay that
// way. Both execution modes run, because a handler that only the VM path reaches
// is a handler that can diverge.
{
  const ceilingTmp = mkdtemp("goccia-parse-ceiling-");
  try {
    const parkedParse: Array<[string, string[], string]> = [
      ["JSON.parse", [], "JSON.parse(doc)"],
      [
        "JSON5.parse",
        ['import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;'],
        "JSON5.parse(doc)",
      ],
      [
        "YAML.parse",
        ['import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;'],
        'YAML.parse(Array.from({ length: 400 }, (_, i) => "k" + i + ": \'" + S + i + "\'").join("\\n") + "\\n")',
      ],
      [
        "JSONL.parse",
        ['import * as JSONLNS from "goccia:jsonl"; const JSONL = JSONLNS.JSONL ?? JSONLNS;'],
        'JSONL.parse(Array.from({ length: 400 }, (_, i) => \'{"a":"\' + S + i + \'"}\').join("\\n") + "\\n")',
      ],
      [
        "TOML.parse",
        ['import * as TOMLNS from "goccia:toml"; const TOML = TOMLNS.TOML ?? TOMLNS;'],
        'TOML.parse(Array.from({ length: 400 }, (_, i) => "k" + i + \' = "\' + S + i + \'"\').join("\\n") + "\\n")',
      ],
    ];

    for (const [label, imports, parse] of parkedParse) {
      for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
        const modeLabel = modeArgs.length > 0 ? "bytecode" : "interpreted";
        console.log(`--max-memory (parse refusal keeps its RangeError: ${label} ${modeLabel})...`);
        const src = [
          ...imports,
          'const S = "s".repeat(200);',
          'const doc = "[" + Array.from({ length: 1200 }, (_, i) => \'"\' + S + i + \'"\').join(",") + "]";',
          // Same measured parking as the parser probes above, at the slack this
          // block was calibrated against.
          ...parkingPreamble(300_000),
          "try {",
          `  const value = ${parse};`,
          '  console.log("completed", value === null || value === undefined ? "empty" : "value");',
          "} catch (e) {",
          "  console.log('caught', e.name, '::', e.message);",
          "}",
          "",
        ].join("\n");
        const srcPath = join(ceilingTmp, `parse-ceiling-${label.replace(".", "-")}-${modeLabel}.mjs`);
        writeFileSync(srcPath, src);
        const proc = Bun.spawnSync([LOADER, "--max-memory=4194304", ...modeArgs, srcPath], {
          stdout: "pipe",
          stderr: "pipe",
          timeout: 180_000,
        });
        const out = proc.stdout.toString() + proc.stderr.toString();
        if (!out.includes("parked true"))
          throw new Error(`Parse-ceiling ${label} (${modeLabel}) never parked the heap: ${out}`);
        if (/^caught \S+ ::\s*$/m.test(out))
          throw new Error(
            `Parse-ceiling ${label} (${modeLabel}) surfaced a refusal with an empty message: ${out}`,
          );
        if (out.includes("caught SyntaxError"))
          throw new Error(
            `Parse-ceiling ${label} (${modeLabel}) relabelled a memory ceiling as a SyntaxError: ${out}`,
          );
        if (!out.includes("caught RangeError") && !out.includes("completed"))
          throw new Error(
            `Parse-ceiling ${label} (${modeLabel}) produced neither a RangeError refusal nor a completed parse: ${out}`,
          );
      }
    }
  } finally {
    clean(ceilingTmp);
  }
}

// The narrowing above must not have made the OTHER limiter guest-visible. A
// charged allocation inside a parse raises the script-catchable RangeError the
// block above pins; the growth gate (RequireNativeBytes, Goccia.MemoryLimit.pas)
// raises TGocciaMemoryLimitError, which is opaque to the guest by design — the
// narrowed handlers name only their own parse-error class, so the gate passes
// straight through them, escapes to the host, and the loader reports it as
// "Fatal error: ... would exceed the memory budget" with a nonzero exit. A guest
// catch marker printing here would mean a parse builtin had converted the
// ceiling into something script code can absorb and retry in a loop.
//
// Goccia.MemoryLimit.Test.pas guards the same convention at the executor
// boundaries; this lives here because the gate has to be reached mid-parse,
// which needs a measured park against a CLI-set ceiling, and because the
// assertion is host-level (exit code plus the loader's report).
//
// The shape: a wide object of cheap numeric values, so the parse charges almost
// nothing per property (~24 bytes measured) while the property map's storage
// doubling asks for a single block far larger than the parked slack. The
// document is built before parking, or building it — not parsing it — is what
// would cross the ceiling.
{
  const gateTmp = mkdtemp("goccia-parse-gate-");
  try {
    const gatedParses: Array<[string, string[], string, string]> = [
      [
        "JSON.parse",
        [],
        'const doc = "{" + Array.from({ length: 4000 }, (_, i) => \'"k\' + i + \'":\' + i).join(",") + "}";',
        "JSON.parse(doc)",
      ],
      [
        "JSON5.parse",
        ['import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;'],
        'const doc = "{" + Array.from({ length: 4000 }, (_, i) => "k" + i + ": " + i).join(",") + "}";',
        "JSON5.parse(doc)",
      ],
      [
        "YAML.parse",
        ['import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;'],
        'const doc = Array.from({ length: 4000 }, (_, i) => "k" + i + ": " + i).join("\\n") + "\\n";',
        "YAML.parse(doc)",
      ],
    ];

    for (const [label, imports, setup, parse] of gatedParses) {
      for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
        const modeLabel = modeArgs.length > 0 ? "bytecode" : "interpreted";
        console.log(`--max-memory (growth gate inside ${label} stays opaque to the guest: ${modeLabel})...`);
        const src = [
          ...imports,
          setup,
          // 120000 sits in the middle of the measured window: the parse survives
          // its own charges (~96 KB for 4000 properties) but the storage doubling
          // asks for 73632 or 147360 bytes in one block and is refused.
          ...parkingPreamble(120_000),
          "try {",
          `  const value = ${parse};`,
          '  console.log("guest-completed", Object.keys(value).length);',
          "} catch (e) {",
          // The marker the gate must never let the guest print.
          "  console.log('guest-caught', e.name, '::', e.message);",
          "}",
          "",
        ].join("\n");
        const srcPath = join(gateTmp, `parse-gate-${label.replace(".", "-")}-${modeLabel}.mjs`);
        writeFileSync(srcPath, src);
        const proc = Bun.spawnSync([LOADER, "--max-memory=4194304", ...modeArgs, srcPath], {
          stdout: "pipe",
          stderr: "pipe",
          timeout: 180_000,
        });
        const out = proc.stdout.toString() + proc.stderr.toString();
        if (!out.includes("parked true"))
          throw new Error(`Parse gate ${label} (${modeLabel}) never parked the heap: ${out}`);
        if (out.includes("guest-caught"))
          throw new Error(
            `Parse gate ${label} (${modeLabel}) let the guest catch a growth-gate refusal: ${out}`,
          );
        if (out.includes("guest-completed"))
          throw new Error(
            `Parse gate ${label} (${modeLabel}) never reached the growth gate, so it proved nothing: ${out}`,
          );
        if (proc.exitCode === 0)
          throw new Error(`Parse gate ${label} (${modeLabel}) should exit nonzero, got 0: ${out}`);
        if (!out.includes("would exceed the memory budget"))
          throw new Error(
            `Parse gate ${label} (${modeLabel}) failed without the budget refusal the host reports: ${out}`,
          );
      }
    }
  } finally {
    clean(gateTmp);
  }
}

// The stringify half of the same convention. JSON.stringify and JSON5.stringify
// wrap their whole body in a handler that converts a Pascal exception into a
// script-visible TypeError ("JSON.stringify error: ..."), and that handler had
// no re-raise allowlist: a growth-gate refusal arrived at the guest as a
// catchable TypeError carrying the budget text, which is the ceiling-you-can-
// ignore-in-a-loop this whole convention exists to prevent. Both handlers now
// name the limit family (timeout, instruction limit, memory limit) ahead of the
// generic arm, as Goccia.Builtins.GlobalFetch.pas and Goccia.Interpreter.pas do.
//
// Reaching the gate from a stringify needs a replacer: the plain serializer
// writes into a native buffer and only the result string is charged, while the
// replacer walk rebuilds every object property-by-property, so the property
// map's storage doubling is what asks for one block larger than the parked
// slack. The same 4000-key cheap-value object and the same measured slack as
// the parse block above land inside the window (measured: the gate is the
// crossing limiter for a slack of roughly 10000-150000; at 160000 and above the
// stringify simply completes).
{
  const stringifyGateTmp = mkdtemp("goccia-stringify-gate-");
  const parkedStringifySource = (
    imports: string[],
    setup: string,
    call: string,
    slackTarget: number,
  ): string =>
    [
      ...imports,
      setup,
      ...parkingPreamble(slackTarget),
      "try {",
      `  const value = ${call};`,
      '  console.log("guest-completed", value.length);',
      "} catch (e) {",
      // The marker the gate must never let the guest print.
      "  console.log('guest-caught', e.name, '::', e.message);",
      "}",
      "",
    ].join("\n");

  try {
    // The object is built before parking, or building it — not stringifying it —
    // is what would cross the ceiling.
    const wideObject =
      'const obj = Object.fromEntries(Array.from({ length: 4000 }, (_, i) => ["k" + i, i]));';
    const gatedStringifies: Array<[string, string[], string, string]> = [
      ["JSON.stringify", [], wideObject, "JSON.stringify(obj, (k, v) => v)"],
      [
        "JSON5.stringify",
        ['import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;'],
        wideObject,
        "JSON5.stringify(obj, (k, v) => v)",
      ],
    ];

    for (const [label, imports, setup, call] of gatedStringifies) {
      for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
        const modeLabel = modeArgs.length > 0 ? "bytecode" : "interpreted";
        console.log(`--max-memory (growth gate inside ${label} stays opaque to the guest: ${modeLabel})...`);
        const srcPath = join(stringifyGateTmp, `stringify-gate-${label.replace(".", "-")}-${modeLabel}.mjs`);
        writeFileSync(srcPath, parkedStringifySource(imports, setup, call, 120000));
        const proc = Bun.spawnSync([LOADER, "--max-memory=4194304", ...modeArgs, srcPath], {
          stdout: "pipe",
          stderr: "pipe",
          timeout: 180_000,
        });
        const out = proc.stdout.toString() + proc.stderr.toString();
        if (!out.includes("parked true"))
          throw new Error(`Stringify gate ${label} (${modeLabel}) never parked the heap: ${out}`);
        if (out.includes("guest-caught"))
          throw new Error(
            `Stringify gate ${label} (${modeLabel}) let the guest catch a growth-gate refusal: ${out}`,
          );
        if (out.includes("guest-completed"))
          throw new Error(
            `Stringify gate ${label} (${modeLabel}) never reached the growth gate, so it proved nothing: ${out}`,
          );
        if (proc.exitCode === 0)
          throw new Error(`Stringify gate ${label} (${modeLabel}) should exit nonzero, got 0: ${out}`);
        if (!out.includes("would exceed the memory budget"))
          throw new Error(
            `Stringify gate ${label} (${modeLabel}) failed without the budget refusal the host reports: ${out}`,
          );
      }
    }

    // Vacuity control for the "no guest-caught" assertion above. The other
    // limiter — a charged string allocation — is script-visible by design and
    // always was, so the same harness pointed at a shape that only ever crosses
    // the charge (no replacer, one oversized string) MUST print the marker the
    // gate cases forbid. If this stops catching, the assertions above are
    // passing because nothing reaches any limiter, not because the ceiling is
    // opaque.
    console.log("--max-memory (stringify gate vacuity control: a charged refusal is still catchable)...");
    {
      const controlPath = join(stringifyGateTmp, "stringify-gate-control.mjs");
      writeFileSync(
        controlPath,
        parkedStringifySource([], 'const big = "y".repeat(400000);', "JSON.stringify(big)", 120000),
      );
      const proc = Bun.spawnSync([LOADER, "--max-memory=4194304", controlPath], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 180_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      if (!out.includes("parked true"))
        throw new Error(`Stringify gate control never parked the heap: ${out}`);
      if (!out.includes("guest-caught RangeError"))
        throw new Error(
          `Stringify gate control should let the guest catch the charged RangeError, making the gate assertions non-vacuous: ${out}`,
        );
      if (proc.exitCode !== 0)
        throw new Error(`Stringify gate control should exit 0, got ${proc.exitCode}: ${out}`);
    }
  } finally {
    clean(stringifyGateTmp);
  }
}

console.log("goccia:yaml (deep flow nesting reports a named, non-empty error)...");
{
  // Depth refusals out of the data-format parsers do not agree on a class, and
  // that split is pre-existing and deliberate here: JSON, JSON5 and JSONL hit
  // their own parser-internal nesting cap and report SyntaxError, while YAML and
  // TOML hit the shared native depth guard (EnterNativeDataDepth) and report
  // RangeError "Maximum call stack size exceeded". Documented, not changed.
  //
  // For YAML this is a reclassification the except-narrowing brought about: the
  // blanket handler used to relabel the depth guard's RangeError as a SyntaxError
  // with an empty message, so the guest could not tell a resource ceiling from
  // malformed input. The message being non-empty is the half that regressed.
  const depthTmp = mkdtemp("goccia-parse-depth-");
  try {
    const src = [
      'import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;',
      'const deep = "[".repeat(5000) + "]".repeat(5000);',
      "const report = (label, parse) => {",
      "  try {",
      "    parse();",
      '    console.log(label, "no-throw", "::", "no-throw");',
      "  } catch (e) {",
      "    console.log(label, e.name, '::', e.message);",
      "  }",
      "};",
      'report("yaml", () => YAML.parse(deep));',
      'report("json", () => JSON.parse(deep));',
      "",
    ].join("\n");
    const srcPath = join(depthTmp, "parse-depth.mjs");
    writeFileSync(srcPath, src);
    const proc = Bun.spawnSync([LOADER, srcPath], { stdout: "pipe", stderr: "pipe", timeout: 60_000 });
    const out = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode !== 0) throw new Error(`Parse depth run should exit 0, got ${proc.exitCode}: ${out}`);
    if (!/^yaml RangeError :: .+$/m.test(out))
      throw new Error(`YAML deep flow nesting should report a named RangeError with a message: ${out}`);
    if (!out.includes("yaml RangeError :: Maximum call stack size exceeded"))
      throw new Error(`YAML deep flow nesting should report the native depth guard's message: ${out}`);
    if (!/^json SyntaxError :: .+$/m.test(out))
      throw new Error(`JSON deep nesting should report a SyntaxError with a message: ${out}`);
  } finally {
    clean(depthTmp);
  }
}

console.log("--max-memory (ordinary script errors stay catchable under a budget)...");
{
  // The counterweight: the same budget must not turn an in-language error
  // into a host-level failure.
  for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
    const label = modeArgs.length > 0 ? "bytecode" : "interpreted";
    const src = "let caught = false; try { null.property; } catch (e) { caught = true; } caught\n";
    const { exitCode, json } = runLoaderJson(src, ["--max-memory=67108864", "--compat-asi", ...modeArgs], { timeout: 30_000 });
    if (exitCode !== 0) throw new Error(`Catchable script error (${label}) should exit 0, got ${exitCode}: ${JSON.stringify(json)}`);
    if (json.files?.[0]?.result !== true) throw new Error(`Catchable script error (${label}) should be caught by the script, got ${json.files?.[0]?.result}`);
  }
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
    const { exitCode, json, stderr } = runLoaderJson(src, ["--max-memory=500000", "--compat-asi", ...modeArgs], { timeout: 30_000 });
    if (exitCode !== 0) throw new Error(`Manual GC active-call ${label} exit code should be 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
    if (typeof json.files?.[0]?.result !== "number" || json.files[0].result <= 0) throw new Error(`Manual GC active-call ${label} should return positive bytesAllocated`);
    if ((json.memory?.gc?.collections ?? 0) < 30) throw new Error(`Manual GC active-call ${label} should report at least 30 collections, got ${json.memory?.gc?.collections}`);
  }
}

console.log("--max-memory (interpreter recursive expression pressure reclaims)...");
{
  const src = [
    "const fib = (n) => n < 2 ? n : fib(n - 1) + fib(n - 2);",
    "fib(24);",
    "",
  ].join("\n");

  const { exitCode, json, stderr } = runLoaderJson(src, ["--max-memory=500000", "--compat-asi"], { timeout: 30_000 });
  if (exitCode !== 0) throw new Error(`Recursive expression pressure exit code should be 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
  if (json.files?.[0]?.result !== 46368) throw new Error(`Recursive expression pressure should return 46368, got ${json.files?.[0]?.result}`);
  if ((json.memory?.gc?.collections ?? 0) <= 0) throw new Error(`Recursive expression pressure should report collections, got ${json.memory?.gc?.collections}`);
}

console.log("--max-memory (interpreter lexical for-loop roots stay bounded)...");
{
  const src = [
    "for (let i = 0; i < 5000; i++) {",
    "  if (i % 100 === 0) Goccia.gc();",
    "}",
    '"completed";',
    "",
  ].join("\n");
  const { exitCode, json, stderr } = runLoaderJson(src, [
    "--max-memory=300000",
    "--compat-traditional-for-loop",
  ], { timeout: 30_000 });
  if (exitCode !== 0) throw new Error(`Interpreter lexical for-loop exit code should be 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
  if (json.files?.[0]?.result !== "completed") throw new Error(`Interpreter lexical for-loop should complete, got ${json.files?.[0]?.result}`);
}

console.log("--max-memory (bytecode loop pressure reclaims)...");
{
  const src = [
    "let total = 0;",
    "for (let round = 0; round < 1000; round++) {",
    "  let junk = Array.from({ length: 500 }, (_, i) => ({ round, i }));",
    "  total += junk.length;",
    "  junk = null;",
    "}",
    "total;",
    "",
  ].join("\n");
  const { exitCode, json, stderr } = runLoaderJson(src, [
    "--mode=bytecode",
    // Keep the pressure threshold below both 32-bit and 64-bit allocation
    // totals while leaving enough room for the loop's live object graph.
    "--max-memory=20000000",
    "--compat-asi",
    "--compat-traditional-for-loop",
  ], { timeout: 30_000 });
  if (exitCode !== 0) throw new Error(`Bytecode loop pressure exit code should be 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
  if (json.files?.[0]?.result !== 500000) throw new Error(`Bytecode loop pressure should return 500000, got ${json.files?.[0]?.result}`);
  if ((json.memory?.gc?.collections ?? 0) <= 0) throw new Error(`Bytecode loop pressure should report collections, got ${json.memory?.gc?.collections}`);
}

console.log("--max-memory (maxBytes readonly)...");
{
  const res = await $`echo 'Goccia.gc.maxBytes = 999' | ${LOADER} --compat-asi 2>&1`.nothrow();
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

// -- Console observable behavior (Loader, interpreted + bytecode) ---------------

console.log("Console observable behavior...");
{
  const source = [
    'console.assert(true, "hidden");',
    'console.assert(false, "failed", 7);',
    'console.log("log", 1);',
    'console.warn("warn");',
    'console.error("error");',
    'console.info("info");',
    'console.debug("debug");',
    "console.dir({ value: 1 });",
    "console.count();",
    "console.count();",
    "console.countReset();",
    "console.count();",
    "let coercions = 0;",
    'console.count({ toString() { coercions++; return "coerced"; } });',
    'console.log("coercions", coercions);',
    'console.group("outer");',
    'console.log("inside");',
    'console.group("inner");',
    'console.trace("trace");',
    "console.table([1, 2]);",
    "console.groupEnd();",
    'console.error("outer");',
    "console.groupEnd();",
    "console.groupEnd();",
    'console.log("after");',
    "",
  ].join("\n");
  const expectedOutput = [
    "Assertion failed: failed 7",
    "log 1",
    "Warning: warn",
    "Error: error",
    "Info: info",
    "Debug: debug",
    "{ value: 1 }",
    "default: 1",
    "default: 2",
    "default: 1",
    "coerced: 1",
    "coercions 1",
    "outer",
    "  inside",
    "  inner",
    "    Trace: trace",
    "    [ 1, 2 ]",
    "  Error: outer",
    "after",
  ];
  const expectedStdout = [
    "log 1",
    "info",
    "debug",
    "{ value: 1 }",
    "default: 1",
    "default: 2",
    "default: 1",
    "coerced: 1",
    "coercions 1",
    "outer",
    "  inside",
    "  inner",
    "    [ 1, 2 ]",
    "after",
    "",
  ].join("\n");
  const expectedStderr = [
    "Assertion failed: failed 7",
    "warn",
    "error",
    "    trace",
    "  outer",
    "",
  ].join("\n");

  for (const modeArgs of [[] as string[], ["--mode=bytecode"]]) {
    const label = modeArgs.length ? "bytecode" : "interpreted";
    const { exitCode, json, stderr } = runLoaderJson(source, modeArgs);
    if (exitCode !== 0)
      throw new Error(`Console behavior ${label} failed: ${JSON.stringify(json.error)}${stderr}`);
    if (stderr !== "")
      throw new Error(
        `Console behavior ${label} leaked process stderr: ${JSON.stringify(stderr)}`,
      );
    if (JSON.stringify(json.output) !== JSON.stringify(expectedOutput))
      throw new Error(
        `Console behavior ${label} output mismatch: expected ${JSON.stringify(expectedOutput)}, got ${JSON.stringify(json.output)}`,
      );
    if (normalizeLineEndings(json.stdout) !== expectedStdout)
      throw new Error(
        `Console behavior ${label} stdout mismatch: expected ${JSON.stringify(expectedStdout)}, got ${JSON.stringify(json.stdout)}`,
      );
    if (normalizeLineEndings(json.stderr) !== expectedStderr)
      throw new Error(
        `Console behavior ${label} stderr mismatch: expected ${JSON.stringify(expectedStderr)}, got ${JSON.stringify(json.stderr)}`,
      );
  }
}

console.log("Console timer lifecycle...");
{
  const source = [
    'console.timeEnd("missing");',
    'console.timeLog("missing-log");',
    "let coercions = 0;",
    'const label = { toString() { coercions++; return "timer"; } };',
    "console.time(label);",
    "console.timeLog(label);",
    "console.timeEnd(label);",
    "console.timeEnd(label);",
    'console.log("timer-label-coercions", coercions);',
    "console.time();",
    "console.timeEnd();",
    "console.timeEnd();",
    "",
  ].join("\n");

  for (const modeArgs of [[] as string[], ["--mode=bytecode"]]) {
    const label = modeArgs.length ? "bytecode" : "interpreted";
    const { exitCode, json, stderr } = runLoaderJson(source, modeArgs);
    if (exitCode !== 0)
      throw new Error(`Console timers ${label} failed: ${JSON.stringify(json.error)}${stderr}`);
    if (stderr !== "")
      throw new Error(
        `Console timers ${label} leaked process stderr: ${JSON.stringify(stderr)}`,
      );
    if (!Array.isArray(json.output) || json.output.length !== 8)
      throw new Error(`Console timers ${label} should emit 8 lines, got ${JSON.stringify(json.output)}`);

    const [missingEnd, missingLog, timeLog, timeEnd, removedEnd, coercions, defaultEnd, removedDefault] =
      json.output as string[];
    if (missingEnd !== "Timer 'missing' does not exist")
      throw new Error(`Console timers ${label} missing timeEnd output mismatch: ${missingEnd}`);
    if (missingLog !== "Timer 'missing-log' does not exist")
      throw new Error(`Console timers ${label} missing timeLog output mismatch: ${missingLog}`);
    if (!/^timer: \d+(?:\.\d{1,3})?ms$/.test(timeLog))
      throw new Error(`Console timers ${label} timeLog output mismatch: ${timeLog}`);
    if (!/^timer: \d+(?:\.\d{1,3})?ms$/.test(timeEnd))
      throw new Error(`Console timers ${label} timeEnd output mismatch: ${timeEnd}`);
    if (removedEnd !== "Timer 'timer' does not exist")
      throw new Error(`Console timers ${label} should remove an ended timer, got: ${removedEnd}`);
    if (coercions !== "timer-label-coercions 4")
      throw new Error(`Console timers ${label} should coerce each timer label once, got: ${coercions}`);
    if (!/^default: \d+(?:\.\d{1,3})?ms$/.test(defaultEnd))
      throw new Error(`Console timers ${label} default timer output mismatch: ${defaultEnd}`);
    if (removedDefault !== "Timer 'default' does not exist")
      throw new Error(`Console timers ${label} should remove the default timer, got: ${removedDefault}`);
    if (normalizeLineEndings(json.stderr) !== "")
      throw new Error(`Console timers ${label} should route output to stdout, got stderr: ${json.stderr}`);
  }
}

// -- --log option (Loader) ------------------------------------------------------

console.log("--log option...");
{
  const tmp = mkdtemp("goccia-log-");
  try {
    const source = [
      'console.log("log");',
      'console.warn("warn");',
      'console.error("error");',
      'console.info("info");',
      'console.debug("debug");',
      "console.dir({ value: 1 });",
      'console.assert(false, "assert");',
      'console.count("count");',
      'console.group("group");',
      'console.trace("trace");',
      "console.table([1, 2]);",
      "console.groupEnd();",
      'console.timeEnd("missing");',
      "",
    ].join("\n");
    const methods = [
      "log",
      "warn",
      "error",
      "info",
      "debug",
      "dir",
      "assert",
      "count",
      "group",
      "trace",
      "table",
      "timeEnd",
    ];

    for (const modeArgs of [[] as string[], ["--mode=bytecode"]]) {
      const label = modeArgs.length ? "bytecode" : "interpreted";
      const logPath = join(tmp, `${label}.log`);
      const { exitCode, json, stderr } = runLoaderJson(source, [`--log=${logPath}`, ...modeArgs]);
      if (exitCode !== 0)
        throw new Error(`Log file ${label} run failed: ${JSON.stringify(json.error)}${stderr}`);
      if (!existsSync(logPath)) throw new Error(`Log file should exist at ${logPath}`);
      const content = readFileSync(logPath, "utf-8");
      for (const method of methods) {
        if (!content.includes(`[${method}]`))
          throw new Error(`Log file ${label} should contain [${method}], got: ${content}`);
      }
    }
  } finally {
    clean(tmp);
  }
}

// -- Assertion failure text (TestRunner) ---------------------------------------

// A failed assertion is recorded rather than thrown, so its message cannot be
// observed from inside a test. These two properties are load-bearing enough to
// pin from the outside: toBeInstanceOf naming what it compared, and the vitest
// shim keeping a named, actionable error for every member it does not provide.
console.log("Assertion failure text...");
{
  const tmp = mkdtemp("goccia-assertion-text-");
  try {
    const instanceOfSrc = join(tmp, "instance-of.test.js");
    writeFileSync(
      instanceOfSrc,
      [
        "class BatchError extends Error {}",
        "class NamedError extends Error {}",
        "class Plain {}",
        "class Sibling {}",
        'describe("rendering", () => {',
        '  test("error subject", () => {',
        '    expect(new BatchError("boom")).toBeInstanceOf(NamedError);',
        "  });",
        '  test("plain subject", () => {',
        "    expect(new Plain()).toBeInstanceOf(Sibling);",
        "  });",
        "});",
        "",
      ].join("\n"),
    );
    const instanceOf = await $`${TESTRUNNER} ${instanceOfSrc} --no-progress 2>&1`.nothrow();
    const instanceOfOut = instanceOf.text();
    // The class object serializes as "{}", so a message built from the values
    // alone said "Expected {} to be an instance of {}" and named neither side.
    for (const expected of [
      "Expected Error: boom to be an instance of NamedError",
      "Expected Plain{} to be an instance of Sibling",
    ]) {
      if (!instanceOfOut.includes(expected))
        throw new Error(`toBeInstanceOf should report ${expected}, got: ${instanceOfOut}`);
    }

    // Every member the shim does not implement must keep throwing by name. The
    // contract is what tells a suite author which member to work around, so
    // silently degrading one to a no-op is worse than not having it.
    const unsupported: Array<[string, string]> = [
      ["vi.hoisted(() => ({}))", "vi.hoisted is not supported"],
      ["vi.importMock('./x.js')", "vi.importMock is not supported"],
      ["vi.setConfig({})", "vi.setConfig is not supported"],
      ["vi.useFakeTimers()", "vi.useFakeTimers is not supported"],
      ["vi.advanceTimersByTime(1)", "vi.advanceTimersByTime is not supported"],
      ["vi.importActual('./x.js')", "vi.importActual is not supported"],
      ["vi.resetModules()", "vi.resetModules is not supported"],
      ["vi.doMock('./x.js')", "vi.doMock is not supported"],
    ];
    const shimSrc = join(tmp, "shim.test.js");
    writeFileSync(
      shimSrc,
      [
        'import { vi } from "vitest";',
        'describe("unsupported", () => {',
        ...unsupported.map(
          ([call], index) =>
            `  test("case ${index}", () => { try { ${call}; console.log("NO-THROW ${index}"); } catch (e) { console.log("THREW ${index}: " + e.message); } });`,
        ),
        "});",
        "",
      ].join("\n"),
    );
    const shim = await $`${TESTRUNNER} ${shimSrc} --source-type=module --no-progress 2>&1`.nothrow();
    const shimOut = shim.text();
    unsupported.forEach(([call, message], index) => {
      if (shimOut.includes(`NO-THROW ${index}`))
        throw new Error(`${call} must keep throwing its named error, got: ${shimOut}`);
      // The whole capture would pass the docs-link check as soon as any one
      // member carried the link, so each case is matched on its own line.
      const line = shimOut
        .split("\n")
        .find((entry) => entry.includes(`THREW ${index}: `));
      if (line === undefined || !line.includes(`THREW ${index}: ${message}`))
        throw new Error(`${call} should report "${message}", got: ${shimOut}`);
      if (!line.includes("docs/testing-api.md"))
        throw new Error(`${call} should point at the docs, got: ${line}`);
    });
  } finally {
    clean(tmp);
  }
}

// -- Global injection (TestRunner) ---------------------------------------------

// The runner grew --global/--globals so a suite can be handed a host global it
// needs. `process` is the case that forced it: GocciaScript has none, and
// vi.stubEnv has nowhere to write without one.
console.log("Global injection (TestRunner)...");
{
  const tmp = mkdtemp("goccia-testrunner-globals-");
  try {
    const suite = join(tmp, "globals.test.js");
    writeFileSync(
      suite,
      [
        'describe("injected", () => {',
        '  test("reads the injected global", () => {',
        '    expect(process.env.PRESET).toBe("from-host");',
        "  });",
        "});",
        "",
      ].join("\n"),
    );

    const inline = await $`${TESTRUNNER} ${suite} --no-progress --global ${'process={"env":{"PRESET":"from-host"}}'} 2>&1`.nothrow();
    if (inline.exitCode !== 0)
      throw new Error(`--global should inject process, got: ${inline.text()}`);

    const globalsFile = join(tmp, "env.json");
    writeFileSync(globalsFile, JSON.stringify({ process: { env: { PRESET: "from-host" } } }));
    const fromFile = await $`${TESTRUNNER} ${suite} --no-progress --globals=${globalsFile} 2>&1`.nothrow();
    if (fromFile.exitCode !== 0)
      throw new Error(`--globals should inject process, got: ${fromFile.text()}`);

    // Without the injection the suite must fail on the missing global rather
    // than quietly reading undefined.
    const without = await $`${TESTRUNNER} ${suite} --no-progress 2>&1`.nothrow();
    if (without.exitCode === 0)
      throw new Error("Suite should fail when process is not injected");
    if (!without.text().includes("process"))
      throw new Error(`Missing-global failure should name process, got: ${without.text()}`);
  } finally {
    clean(tmp);
  }
}

// -- Example scripts (Loader) ---------------------------------------------------

console.log("Example scripts...");
const stableExamples = [...new Bun.Glob("**/*.js").scanSync({ cwd: "examples" })]
  .filter((path) => !path.split("/").some((segment) => segment.startsWith("_")))
  .sort()
  .map((path) => join("examples", path));
await $`${LOADER} ${stableExamples}`.quiet();

console.log("\nAll test-cli.ts tests passed.");
