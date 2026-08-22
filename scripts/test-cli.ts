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

// -- super-constructor resolution terminates without the sandbox limits --------
//
// ES2026 §13.3.7.3 GetSuperConstructor resolves super() through the
// constructor's [[Prototype]]. A resolver that fell back to the declared
// superclass when that hop landed on a non-constructor walked the union of two
// relations: each is acyclic on its own, their union is not. The union spun
// forever inside one native call, so neither --timeout nor --max-instructions
// could interrupt it. It has to terminate on its own.

const retargetCycle =
  "class Base { b = 1; }\n" +
  "class Middle extends Base { m = 2; }\n" +
  "class Leaf extends Middle { l = 3; }\n" +
  "Object.setPrototypeOf(Leaf, {});\n" +
  "Object.setPrototypeOf(Middle, Leaf);\n" +
  "new Leaf();\n";

for (const mode of ["interpreted", "bytecode"] as const) {
  console.log(`super-constructor cycle terminates (${mode})...`);
  const modeArgs = mode === "bytecode" ? ["--mode=bytecode"] : [];
  const { exitCode, json } = runLoaderJson(retargetCycle, modeArgs, { timeout: 20_000 });
  if (exitCode !== 1) throw new Error(`Retarget cycle exit code should be 1, got ${exitCode} (${mode})`);
  if (json.error?.type !== "TypeError") {
    throw new Error(`Expected TypeError for the retarget cycle, got ${json.error?.type} (${mode})`);
  }
  // And the sandbox limits stay effective for code that really is unbounded.
  const bounded = runLoaderJson(retargetCycle, [...modeArgs, "--max-instructions=5000000"], { timeout: 20_000 });
  if (bounded.json.error?.type !== "TypeError") {
    throw new Error(`Retarget cycle should still be a TypeError under a limit, got ${bounded.json.error?.type} (${mode})`);
  }
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
      // a wider one lets several of them through. The window is that slack, not
      // the ceiling: the ceiling only has to be wide enough that building the
      // document cannot refuse before there is a heap to park at all.
      //
      // That floor is higher than it looks. Every setup ends in one `join` that
      // asks for the whole document as a single charged string — 617 KiB for
      // yaml-block-and-flow, 620 KiB for toml — on top of whatever transient
      // garbage the setup is holding when it lands.
      //
      // It used to be worse than high: it was not even monotonic. A reservation
      // R was refused *without* any collection whenever the heap sat in
      // (maxBytes - R, maxBytes - maxBytes/8), so which ceilings survived
      // depended on where the setup's garbage happened to sit and the refusals
      // came in bands — the old 3 MiB first ceiling sat 54 KB below one on this
      // machine and inside one on i386-win32, which is how a document that never
      // got parsed failed as "exercised no collecting window".
      // TryReserveExternalBytes now forces the collection and re-tests before
      // refusing, so the bands are gone (see the reserve-band probe below).
      //
      // The 6 and 8 MiB ceilings are kept, and for a structural reason rather
      // than a historical one: these probes must not depend on a last-resort
      // collection for their own setup, or the state they park from is the state
      // that collection happened to leave. That interval was non-empty only when
      // R > maxBytes/8, and the largest setup reservation is toml's 634,520 B —
      // below maxBytes/8 at 6 MiB (786,432) and 8 MiB (1,048,576) — so at these
      // ceilings the document is built without a forced collection ever being
      // load-bearing for it. The last-resort branch is still entered if a
      // reservation misses; what these ceilings buy is that the setup never
      // depends on it. R is pure charged string payload (length * SizeOf(Char), 2
      // bytes/char on every target under delphiunicode), so that holds on i386
      // unchanged; per-object InstanceSize is the only pointer-size-sensitive
      // heap term and it only makes i386 smaller. Which of the two accepted
      // terminal outcomes a probe lands on (refused RangeError vs the tolerated
      // growth-gate fatal) can still shift with the ceiling — yaml-anchors takes
      // the RangeError path at 4 MiB but the growth gate here — and
      // assertProbeRun accepts both.
      const tightPath = join(tmp, `parser-parked-${probe.name}.mjs`);
      writeFileSync(tightPath, buildSrc(parkingPreamble(100_000)));
      for (const maxMemory of [6_291_456, 8_388_608]) {
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

      // A wider window: parked enough that the parse collects repeatedly, with
      // enough left over that it can still finish. This is the
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

// --- Growth-gate family -----------------------------------------------------
//
// The convention these blocks pin: the growth gate (RequireNativeBytes,
// Goccia.MemoryLimit.pas) raises TGocciaMemoryLimitError, which is opaque to
// the guest by design — it passes straight through every builtin's handler,
// escapes to the host, and the loader reports it as "Fatal error: ... would
// exceed the memory budget" with a nonzero exit. A guest catch marker carrying
// that text would mean a builtin had converted the ceiling into something
// script code can absorb and retry in a loop.
//
// Goccia.MemoryLimit.Test.pas guards the same convention at the executor
// boundaries; this lives here because the gate has to be reached inside a
// builtin, and because the assertion is host-level (exit code plus the
// loader's report).
//
// WHY THIS IS NOT A CALIBRATED PROBE ANY MORE.
//
// The engine has two memory limiters with two different contracts, and for a
// fixed script WHICH one refuses first is a race:
//
//   * The gated request is one storage doubling of a property map.
//     TOrderedStringMap grows the entry array C -> 2C + 2 and gates the
//     transient old + new = (3C + 2) * SizeOf(TEntry) — 24 bytes per entry on a
//     64-bit target, 16 on i386, so every request there is two thirds the size.
//     The bucket array is gated too, at 12 * B bytes, and that one is Int32 on
//     every width.
//   * The charged side — string payloads and GC-registered values — reserves
//     through the garbage collector, which collects and re-tests before it
//     refuses, and then raises the script-CATCHABLE RangeError.
//
// The gate refuses at the FIRST doubling whose request exceeds what is left of
// the budget, so the loser of the race is decided by the charge the builtin has
// piled up by the time that doubling is reached — a per-parser trajectory, not
// a constant. Measured over a 4000-key document of `true` values: JSON, JSON5
// and TOML charge 104 bytes for the whole parse (0.026 B per property), while
// YAML holds 165,884 bytes of live intermediates mid-parse (41.5 B per
// property). Cross that spread with the two-thirds width factor on the request
// side and the crossing point moves by parser AND by pointer width. The i386
// CI failure that produced this rework was exactly that: JSON.parse and
// JSON5.parse refused through the gate on i386 as intended, and only
// YAML.parse lost the race — by about 1% of the parked slack — and surfaced
// the catchable RangeError instead.
//
// One further source of drift, recorded because it was a live defect here: an
// assertion that itself allocates inside the region under test (this block used
// to call Object.keys there) can be the allocation that decides the outcome.
//
// A second one has been REMOVED rather than worked around, and the difference
// matters to anyone re-tuning these constants. The gate used to compare against
// instantaneous BytesAllocated without collecting first, so a doubling was
// refused or permitted depending on how much collectable garbage happened to be
// on the heap at that instant — which made every rung here sensitive to
// transient allocation the probe does not control. It now forces a collection
// and re-tests before refusing (ADR 0110), so the crossing a parked run reaches
// is a property of its LIVE set. The parking loop already parks with live
// ballast and collects before measuring, so the slack it reports is what the
// gate now sees; that is why the rungs below did not have to move. What did
// move is which doubling is reached: a probe whose transients used to push it
// over the line now gets one or more doublings further, so a rung that stops
// reaching a limiter is re-tuned by walking the ladder, not by adding garbage.
//
// So no constant can make "the gate refuses first" true on every width for
// every parser, and picking one by margin arithmetic is how this block broke.
// The rework instead:
//
//   1. carries the contract assertion in a probe that needs no parking at all
//      (below): a single doubling whose request exceeds the WHOLE ceiling is
//      refused by CanAllocateNativeBytes' own arithmetic, independent of the
//      heap, the collector, the parser and the pointer width;
//   2. derives every parked probe's slack from what THIS build on THIS
//      architecture actually refuses, instead of from arithmetic written down
//      here;
//   3. measures each builtin's charged footprint in-guest — as a peak, with a
//      collection canary that says whether the number is a peak at all — and
//      lets that MEASUREMENT decide which assertion applies, so a width where
//      the race goes the other way is classified rather than failed, and a
//      measurement that cannot support the strong claim does not get to make it;
//   4. checks that a refusal really came from property-map growth, so a probe
//      cannot pass on a refusal from some unrelated allocation.

// The ceiling the parked probes run under. The constructed probe below sets its
// own, because its whole point is a request that outgrows the ceiling.
const GATE_CEILING = 4_194_304;
// The charge measurements run under their own, far larger ceiling, so neither
// the workload nor the collection canary they carry can come near it.
const CHARGE_MEASURE_CEILING = 134_217_728;

// The byte counts the two ENUMERABLE property-map growth shapes can ask the gate
// for, across every plausible entry size (source/shared/OrderedStringMap.pas):
//   entry array   C -> 2C + 2, transient (3C + 2) * SizeOf(TEntry)
//   bucket array  B -> 2B,     transient 12 * B (Int32 on every width)
// SizeOf(TEntry) is 24 on a 64-bit target and 16 on i386 today; it is swept
// rather than pinned because the assertion this set backs is "the refusal came
// from property-map growth", which must not need re-tuning when a field is
// added to TEntry.
//
// Compact is the third gated shape and is deliberately absent: it reports
// (FEntryCount + FCount) * SizeOf(TEntry), a pair that depends on how many
// entries have been deleted, so it cannot be enumerated. It also cannot run
// here — Compact is reached only through a delete, or through a load factor a
// delete produced, and no probe in this family deletes a property. A refusal
// carrying a Compact-shaped size is therefore a probe that has drifted into a
// shape it was never meant to test, which is what this set exists to catch.
const gatedStorageRequests: Set<number> = (() => {
  const sizes = new Set<number>();
  for (let entrySize = 8; entrySize <= 64; entrySize += 4) {
    let capacity = 0;
    for (let step = 0; step < 32; step += 1) {
      sizes.add((3 * capacity + 2) * entrySize);
      capacity = capacity * 2 + 2;
    }
  }
  let buckets = 16;
  for (let step = 0; step < 24; step += 1) {
    sizes.add(12 * buckets);
    buckets *= 2;
  }
  return sizes;
})();

const refusedRequestBytes = (out: string): number | null => {
  const m = out.match(/Allocation of (\d+) bytes would exceed the memory budget/);
  return m === null ? null : Number(m[1]);
};

type GateRun = {
  outcome: "gate" | "charged" | "completed" | "unparked" | "other";
  refused: number | null;
  parkedSlack: number | null;
  charge: number | null;
  caught: string | null;
  exitCode: number | null;
  out: string;
};

// Classification is deliberately mechanical: every later assertion reads these
// fields rather than re-matching the output, so "which limiter refused" is
// decided in one place.
const runGateCase = (
  srcPath: string,
  src: string,
  modeArgs: readonly string[],
  ceiling: number = GATE_CEILING,
): GateRun => {
  writeFileSync(srcPath, src);
  const proc = Bun.spawnSync([LOADER, `--max-memory=${ceiling}`, ...modeArgs, srcPath], {
    stdout: "pipe",
    stderr: "pipe",
    timeout: 180_000,
  });
  const out = proc.stdout.toString() + proc.stderr.toString();
  const parked = out.match(/parked (?:true|false) slack (\d+)/);
  // Signed: a negative delta is not noise, it is the canary telling us a
  // collection ran inside the call (see measureCallCharge).
  const charge = out.match(/^charge (-?\d+)/m);
  const caught = out.match(/^guest-caught .*$/m);
  const refused = refusedRequestBytes(out);
  let outcome: GateRun["outcome"];
  if (src.includes("parked") && !out.includes("parked true")) outcome = "unparked";
  else if (caught !== null) outcome = "charged";
  else if (out.includes("guest-completed")) outcome = "completed";
  else if (refused !== null && proc.exitCode !== 0) outcome = "gate";
  else outcome = "other";
  return {
    outcome,
    refused,
    parkedSlack: parked === null ? null : Number(parked[1]),
    charge: charge === null ? null : Number(charge[1]),
    caught: caught === null ? null : caught[0],
    exitCode: proc.exitCode,
    out,
  };
};

// The assertions every gate probe shares, whichever limiter it ends up
// reaching. None of them depends on a slack, a width or a parser: they say
// that a gate refusal never becomes guest-visible, that a refusal is never
// relabelled or emptied on its way out, and that a "budget" fatal really is
// one — not a crash wearing the same exit code.
const assertGateContract = (what: string, run: GateRun): void => {
  // Most specific first, so the message names the actual failure mode: a relabel
  // is a SyntaxError or TypeError that still carries the ceiling's text. A
  // genuine parse or serializer error must not abort this family as a
  // misdiagnosis — and the historical relabel that carried NO message at all is
  // caught by the empty-message check below, not by this one.
  if (/^guest-caught (?:SyntaxError|TypeError) ::.*would exceed the memory budget/m.test(run.out))
    throw new Error(`${what} relabelled a memory ceiling as a parse or serializer error: ${run.out}`);
  // Every caught line, not just the first: the guarantee must not depend on
  // which refusal the guest happened to print first.
  if (/^guest-caught .*would exceed the memory budget/m.test(run.out))
    throw new Error(`${what} let the guest catch a growth-gate refusal: ${run.out}`);
  if (/^guest-caught \S+ ::\s*$/m.test(run.out))
    throw new Error(`${what} surfaced a refusal with an empty message: ${run.out}`);
  // Line-wise, not whole-output: a crash fatal alongside a budget refusal is
  // exactly the case a substring test would mask.
  const foreignFatal = run.out
    .split("\n")
    .find((line) => line.includes("Fatal error") && !line.includes("would exceed the memory budget"));
  if (foreignFatal !== undefined)
    throw new Error(`${what} produced a fatal that is not a budget refusal (${foreignFatal.trim()}): ${run.out}`);
  if (run.outcome === "gate") {
    if (run.exitCode === 0)
      throw new Error(`${what} reported a budget refusal but exited 0: ${run.out}`);
    if (run.refused === null || !gatedStorageRequests.has(run.refused))
      throw new Error(
        `${what} was refused ${run.refused} bytes, which is not a property-map storage growth ` +
          `((3C + 2) * SizeOf(TEntry), or 12 * B buckets) — the probe is passing on a refusal from ` +
          `somewhere else and is no longer testing what it claims: ${run.out}`,
      );
  }
};

// The contract assertion, with no slack to calibrate — but one sizing decision,
// stated with its margin rather than called free.
//
// CanAllocateNativeBytes refuses whenever the request alone exceeds MaxBytes,
// whatever BytesAllocated happens to be — so a run that reaches a doubling
// bigger than the entire ceiling is refused on every pointer width, at every
// heap position, with no parking, no ballast and no slack. The live set is kept
// near zero (the keys are Pascal strings inside the map and the values are the
// boolean singletons, neither of which is charged; the transient key strings are
// collected each pass), so the whole ceiling is available and the crossing is
// decided by the request size alone.
//
// This is also the one probe the collecting gate (ADR 0110) leaves exactly as
// it was, and for a reason worth stating rather than assuming: a request larger
// than the WHOLE budget is the first of the three shapes
// ShouldForceLimitCollection refuses without walking the heap, so this probe
// still measures pure arithmetic. Its periodic Goccia.gc() is now belt and
// braces — the gate would collect for itself at any earlier doubling — and it
// is kept because the sizing argument above is stated in terms of a near-zero
// live set, and a probe whose premise is maintained by the code under test is a
// probe that stops being independent of it.
//
// The sizing: under a 1 MiB ceiling the entry array's transient (3C + 2) * E
// first exceeds the ceiling at
//   E = 24 (64-bit today)  C = 16,382, 1,179,552 B, at property 16,383
//   E = 16 (i386 today)    C = 32,766, 1,572,800 B, at property 32,767
//   E = 12                 C = 32,766, 1,179,600 B, at property 32,767
//   E =  8                 C = 65,534, 1,572,832 B, at property 65,535
// so 300 * 300 = 90,000 properties crosses for any entry size down to 8 bytes —
// 1.4x the properties the narrowest of those needs, and 5.5x what the current
// 64-bit build needs. Below 8 bytes a TEntry cannot hold a string reference and
// a pointer on any target this builds for. The two widths refuse at different
// points, which is the part that cannot be made width-independent; the contract
// they refuse under is identical, which is the part that must be.
const CONSTRUCTED_GATE_CEILING = 1_048_576;
{
  const constructedTmp = mkdtemp("goccia-gate-constructed-");
  try {
    const src = [
      // Both loop bounds are materialised before the region under test, so the
      // only growth left inside it is the property map's.
      "const outer = Array.from({ length: 300 }, (_, i) => i);",
      "const inner = Array.from({ length: 300 }, (_, j) => j);",
      "let built = false;",
      "try {",
      "  const o = {};",
      "  for (const i of outer) {",
      '    for (const j of inner) o["k" + i + "_" + j] = true;',
      // Keeps the transient key strings from inflating BytesAllocated, so the
      // near-zero live set the sizing above assumes is maintained by the probe
      // rather than by the gate's own collection.
      "    Goccia.gc();",
      "  }",
      "  built = true;",
      "} catch (e) {",
      "  console.log('guest-caught', e.name, '::', e.message);",
      "}",
      // Outside the try and allocating nothing beyond the call itself: an
      // assertion that allocates inside the region under test can be the
      // allocation that decides the outcome.
      "if (built) console.log('guest-completed');",
      "",
    ].join("\n");

    for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
      const modeLabel = modeArgs.length > 0 ? "bytecode" : "interpreted";
      console.log(`--max-memory (growth gate refuses a request larger than the whole ceiling: ${modeLabel})...`);
      const what = `Constructed gate probe (${modeLabel})`;
      const run = runGateCase(
        join(constructedTmp, `gate-constructed-${modeLabel}.mjs`),
        src,
        modeArgs,
        CONSTRUCTED_GATE_CEILING,
      );
      assertGateContract(what, run);
      if (run.outcome !== "gate")
        throw new Error(
          `${what} did not reach the growth gate (outcome: ${run.outcome}). This probe cannot be ` +
            `raced by the charged path — it allocates nothing charged — so this means the map never ` +
            `reached a doubling larger than the ceiling: ${run.out}`,
        );
      if (run.refused === null || run.refused <= CONSTRUCTED_GATE_CEILING)
        throw new Error(
          `${what} was refused ${run.refused} bytes against a ${CONSTRUCTED_GATE_CEILING}-byte ceiling. The point ` +
            `of this probe is that the request alone exceeds the budget, so the refusal cannot depend ` +
            `on the heap: a smaller request means it does: ${run.out}`,
        );
    }
  } finally {
    clean(constructedTmp);
  }
}

// The capacity result — the reason ADR 0110 exists, asserted as a differential
// rather than as a number.
//
// The gate used to answer from instantaneous BytesAllocated without collecting,
// so which doubling was refused depended on how much collectable garbage
// happened to be resident. That made the ceiling a measure of the collector's
// recent luck rather than of the program, and it made the two execution modes
// disagree — automatic collection is disabled during bytecode execution, so the
// compiled run reached the gate with a dirtier heap and was refused a doubling
// EARLIER. Measured on this workload at a 4 MiB ceiling, before the change:
//
//              plain loop                     with a periodic Goccia.gc()
//   interp.    2,359,200 B  (C = 32,766)      4,718,496 B  (C = 65,534)
//   bytecode   1,179,552 B  (C = 16,382)      4,718,496 B  (C = 65,534)
//
// The assertion is that the two columns are now the SAME: the same workload,
// with and without the guest collecting for itself, must be refused the same
// byte count. That is sharper than pinning either number and it needs no
// re-tuning per pointer width — the equality holds for any SizeOf(TEntry),
// while 4,718,496 is a 64-bit fact. Both modes are asserted because the
// pre-change gap differed per mode, so a fix that only reached one of them
// would pass a single-mode test.
//
// Two supporting checks, each closing a way this could pass while proving
// nothing. Both runs must actually reach the gate — a workload that completed,
// or that lost to the charged limiter, would make the equality vacuous. And the
// refused request must exceed the whole ceiling, which is what pins it as the
// ARITHMETIC crossing rather than merely a shared one: consecutive doublings
// roughly double, so the first request that does not fit beside a
// freshly-collected live set is above the ceiling, while any earlier doubling
// (the pre-change answers above, both under 4 MiB) is not.
//
// Sizing: 400 * 400 = 160,000 properties. Reaching the crossing under a 4 MiB
// ceiling needs 65,535 properties at SizeOf(TEntry) = 24 (64-bit today) and
// 131,071 at 16 (i386 today) or 12, so this clears the narrowest real width by
// 1.2x. It does not cover a hypothetical 8-byte TEntry, which would need
// 262,143 — an entry cannot hold a string reference and a pointer in 8 bytes on
// any 32-bit target this builds for, and the non-vacuity check below turns that
// case into a clear failure rather than a silent pass.
const CAPACITY_GATE_CEILING = 4_194_304;
{
  const capacityTmp = mkdtemp("goccia-gate-capacity-");
  const capacityWorkload = (collectPerPass: boolean): string =>
    [
      // Both loop bounds are materialised before the region under test, so the
      // only growth left inside it is the property map's.
      "const outer = Array.from({ length: 400 }, (_, i) => i);",
      "const inner = Array.from({ length: 400 }, (_, j) => j);",
      "let built = false;",
      "try {",
      "  const o = {};",
      "  for (const i of outer) {",
      '    for (const j of inner) o["k" + i + "_" + j] = true;',
      ...(collectPerPass ? ["    Goccia.gc();"] : []),
      "  }",
      "  built = true;",
      "} catch (e) {",
      "  console.log('guest-caught', e.name, '::', e.message);",
      "}",
      // Outside the try and allocating nothing beyond the call itself.
      "if (built) console.log('guest-completed');",
      "",
    ].join("\n");
  try {
    for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
      const modeLabel = modeArgs.length > 0 ? "bytecode" : "interpreted";
      console.log(
        `--max-memory (a gated growth is refused at the same doubling with or without guest collection: ${modeLabel})...`,
      );
      const what = `Gate capacity probe (${modeLabel})`;
      const runs = (["plain", "collected"] as const).map((variant) => ({
        variant,
        run: runGateCase(
          join(capacityTmp, `gate-capacity-${variant}-${modeLabel}.mjs`),
          capacityWorkload(variant === "collected"),
          modeArgs,
          CAPACITY_GATE_CEILING,
        ),
      }));

      for (const { variant, run } of runs) {
        assertGateContract(`${what} [${variant}]`, run);
        if (run.outcome !== "gate")
          throw new Error(
            `${what} [${variant}] did not reach the growth gate (outcome: ${run.outcome}). The ` +
              `equality below is vacuous unless both runs are refused by the gate — either the ` +
              `workload no longer reaches a doubling this ceiling cannot fit, or the charged ` +
              `limiter won the race: ${run.out}`,
          );
        if (run.refused === null || run.refused <= CAPACITY_GATE_CEILING)
          throw new Error(
            `${what} [${variant}] was refused ${run.refused} bytes against a ${CAPACITY_GATE_CEILING}-byte ` +
              `ceiling. A refusal BELOW the ceiling is a doubling that only failed because the heap ` +
              `was dirty, which is exactly what the gate's forced collection is supposed to have ` +
              `stopped happening: ${run.out}`,
          );
      }

      const [plain, collected] = runs;
      if (plain.run.refused !== collected.run.refused)
        throw new Error(
          `${what} refused ${plain.run.refused} B without a periodic Goccia.gc() and ` +
            `${collected.run.refused} B with one. The gate is meant to force a collection and ` +
            `re-test before refusing, so a guest that collects for itself must not get more ` +
            `capacity out of the same budget (ADR 0110):\n${plain.run.out}\n${collected.run.out}`,
        );
      console.log(`  (${what}: both variants refused ${plain.run.refused} B)`);
    }
  } finally {
    clean(capacityTmp);
  }
}

// The parked half: per-builtin coverage. The probe above proves the gate stays
// opaque; these prove it for each builtin's own handler, which is where the
// leak this family was written for actually lived (a handler that catches every
// Pascal exception and re-throws it as SyntaxError or TypeError swallows the
// ceiling too). Reaching a handler needs the gate to fire INSIDE the builtin,
// which needs a parked heap — so this half keeps the parking, and derives
// everything it would otherwise have had to assume.
type ParkedGateCase = {
  label: string;
  imports: string[];
  setup: string;
  call: string;
  // Printed once the call has returned, so nothing on the success path
  // allocates inside the region under test.
  completedGuard: string;
};

// Source shared by the parse and stringify halves. Nothing but the call itself
// runs inside the try — not even the success guard — because an assertion that
// allocates inside the region under test can be the allocation that decides
// which limiter refuses.
const parkedGateSource = (probe: ParkedGateCase, slackTarget: number): string =>
  [
    ...probe.imports,
    probe.setup,
    ...parkingPreamble(slackTarget),
    "let produced = undefined;",
    "let completed = false;",
    "try {",
    `  produced = ${probe.call};`,
    "  completed = true;",
    "} catch (e) {",
    "  console.log('guest-caught', e.name, '::', e.message);",
    "}",
    `if (completed && ${probe.completedGuard}) console.log('guest-completed');`,
    "",
  ].join("\n");

type ChargeMeasurement = {
  bytes: number;
  // True only when the number is a genuine PEAK bound: no collection ran inside
  // the call, so BytesAllocated rose monotonically and its end value is its
  // high-water mark. False means the number is a lower bound and nothing may be
  // inferred from it.
  peakKnown: boolean;
  note: string;
};

// What a call charges, measured as a peak instead of inferred from a delta.
//
// A post-call delta of BytesAllocated is not a peak: a collection inside the
// call reclaims transients, and the delta then understates the high-water mark
// by whatever it freed — which would put the architecture race back into the
// probe, relocated into an inference. The canary makes the difference
// observable from inside the guest. Unreferenced strings are allocated and
// dropped, still counted, immediately before the call; any collection during the
// call must reclaim them, because they are unreachable, so the delta falls by at
// least the canary and goes negative:
//
//   delta >= 0  =>  nothing was reclaimed  =>  BytesAllocated only rose
//                                          =>  the delta IS the peak increment
//   delta <  0  =>  a collection ran       =>  lower bound only
//
// `armed` closes the last hole: if something collected between dropping the
// canary and reading the baseline, the detector was disarmed before the call,
// and that result is reported as a lower bound too.
//
// The canary is far above any charge these workloads produce (the largest
// measured is ~1 MB) and far below both the measurement ceiling and
// EXTERNAL_MEMORY_PRESSURE_ALLOCATION_INTERVAL (256 MiB), so arming it cannot
// provoke the collection it exists to detect. The measurement runs in its own
// process at its own ceiling, and a peak measured with nothing reclaimed is an
// upper bound on what the parked run — which may collect — can hold at once, so
// using it below is conservative in the direction that matters.
const CHARGE_CANARY_BYTES = 4_194_304;

const measureCallCharge = (
  probe: ParkedGateCase,
  modeArgs: readonly string[],
  srcPath: string,
): ChargeMeasurement => {
  const src = [
    ...probe.imports,
    probe.setup,
    "Goccia.gc();",
    "const canaryBase = Goccia.gc.bytesAllocated;",
    `let canary = Array.from({ length: ${CHARGE_CANARY_BYTES / 8192} }, () => "x".repeat(4096));`,
    "canary = null;",
    "const before = Goccia.gc.bytesAllocated;",
    `const armed = before - canaryBase >= ${CHARGE_CANARY_BYTES - 65_536};`,
    `let produced = ${probe.call};`,
    "const delta = Goccia.gc.bytesAllocated - before;",
    "produced = null;",
    'console.log("charge", delta, "armed", armed);',
    "",
  ].join("\n");
  writeFileSync(srcPath, src);
  const proc = Bun.spawnSync([LOADER, `--max-memory=${CHARGE_MEASURE_CEILING}`, ...modeArgs, srcPath], {
    stdout: "pipe",
    stderr: "pipe",
    timeout: 180_000,
  });
  const out = proc.stdout.toString() + proc.stderr.toString();
  const m = out.match(/^charge (-?\d+) armed (true|false)/m);
  if (proc.exitCode !== 0 || m === null)
    throw new Error(
      `Charge measurement for ${probe.label} did not complete (exit ${proc.exitCode}). The parked ` +
        `probes below classify themselves from this number, so a missing measurement is a failure, ` +
        `not a default: ${out}`,
    );
  const bytes = Number(m[1]);
  const armed = m[2] === "true";
  if (!armed) return { bytes, peakKnown: false, note: "canary was already gone before the call" };
  if (bytes < 0) return { bytes, peakKnown: false, note: "a collection ran inside the call" };
  return { bytes, peakKnown: true, note: "no collection ran inside the call" };
};

// The parked slack is searched for, not chosen.
//
// A parked probe needs a slack tight enough that the builtin's own storage
// growth crosses it, and the tightest one that works is a property of the
// build, the architecture and the parser — SizeOf(TEntry) alone moves every
// request by a third. So the ladder is walked from tight to loose and the first
// rung at which THIS build refuses through the growth gate is the probe. The
// rungs are search positions, not calibrations: their only requirement is that
// the parking loop can converge to one of them, and the error below fires if
// none of them reaches a limiter at all.
//
// Walking upward matters: the tightest rung that works crosses at the earliest
// storage doubling, which is the point at which the builtin has charged the
// least, so it is also the rung least able to lose the race to the charged
// limiter.
const GATE_SLACK_LADDER = [16_384, 32_768, 65_536, 131_072] as const;

// One parked case, end to end. Everything that used to be a constant is either
// searched for (the slack) or measured (the charge), and the measurement
// decides which assertion applies rather than a comment claiming one holds.
const runParkedGateProbe = (
  kind: string,
  probe: ParkedGateCase,
  modeArgs: readonly string[],
  tmpDir: string,
): GateRun => {
  const modeLabel = modeArgs.length > 0 ? "bytecode" : "interpreted";
  const what = `${kind} ${probe.label} (${modeLabel})`;
  const srcPath = join(tmpDir, `${kind.replace(" ", "-")}-${probe.label.replace(".", "-")}-${modeLabel}.mjs`);
  const charge = measureCallCharge(probe, modeArgs, `${srcPath}.charge.mjs`);
  const attempts: string[] = [];
  // The FIRST — tightest — rung that reached the charged limiter instead. Kept
  // as the fallback so a width or a parser that genuinely cannot win the race is
  // still held to the contract assertions rather than failing for losing a race
  // no constant can win. First-wins, not last: the tightest racing rung is the
  // one the tightest-rung rationale above argues for, and last-wins would keep
  // the loosest one — the rung most likely to trip the hard throw below, which
  // is exactly backwards. YAML.parse is the live example: it holds ~166 KB of
  // intermediates for a 4000-key document, more than any rung here parks at.
  let raced: GateRun | null = null;
  let racedSeed: number | null = null;

  for (const seed of GATE_SLACK_LADDER) {
    const run = runGateCase(srcPath, parkedGateSource(probe, seed), modeArgs);
    // Every rung is held to the contract, whatever it reached.
    assertGateContract(`${what} at slack rung ${seed}`, run);
    attempts.push(
      `${seed} -> ${run.outcome}${run.refused === null ? "" : ` (${run.refused} B)`}` +
        `${run.parkedSlack === null ? "" : ` [parked ${run.parkedSlack}]`}`,
    );
    if (run.outcome === "unparked") continue; // tighter than this build's parking loop converges
    if (run.outcome === "completed") continue; // looser than this builtin's largest storage growth
    if (run.outcome === "gate") {
      // A tighter rung that lost the race is the under-reporting signal this
      // family is here to surface: the gate result below is real, but on a
      // narrower pointer width the tighter rung is the one that would be
      // reached. Log it rather than discarding it.
      if (racedSeed !== null)
        console.log(
          `  (${what}: slack rung ${racedSeed} refused through the charged limiter while rung ${seed} ` +
            `reached the gate — a width whose requests are smaller may see the charged path here)`,
        );
      return assertParkedGateOutcome(what, run, attempts, charge);
    }
    if (run.outcome === "charged" && raced === null) {
      raced = run;
      racedSeed = seed;
    }
  }

  if (raced !== null) return assertParkedGateOutcome(what, raced, attempts, charge);
  throw new Error(
    `${what} reached no limiter at any slack rung (${attempts.join("; ")}), so it proved nothing. ` +
      `Either the builtin no longer grows a property map, or the parking loop can no longer converge ` +
      `tightly enough to reach one.`,
  );
};

// The checked precondition, applied to whichever rung the search settled on.
//
// The hard form holds only when the measurement is a genuine peak bound: if the
// whole call's PEAK charge is below the parked slack then the charged limiter is
// unreachable for this run, "the gate refuses first" is a fact about measured
// bytes, and anything else is a regression. Two cases fall out of it, and both
// route to the same softer classification rather than to a throw:
//
//   * the peak is not known (a collection ran inside the measured call, so the
//     number is a lower bound) — inferring from it would be the architecture
//     race back again, wearing an inference;
//   * the peak is known and is at or above the parked slack — the two limiters
//     genuinely race, and which one wins moves with SizeOf(TEntry).
//
// A note on the second: it is a bound on the WHOLE call, so it says only that
// one-sidedness cannot be proven — the crossing may still happen long before the
// charge arrives, and on this build it does. It is reported either way so a
// width that flips is visible in the log instead of silent.
const assertParkedGateOutcome = (
  what: string,
  run: GateRun,
  attempts: string[],
  charge: ChargeMeasurement,
): GateRun => {
  if (run.parkedSlack === null)
    throw new Error(`${what} did not report its parked slack: ${run.out}`);
  if (charge.peakKnown && charge.bytes < run.parkedSlack) {
    if (run.outcome !== "gate")
      throw new Error(
        `${what} charges at most ${charge.bytes} B (measured peak, ${charge.note}) against ` +
          `${run.parkedSlack} B of parked slack, so the growth gate is the only limiter it can ` +
          `reach — but the run refused through "${run.outcome}" (rungs: ${attempts.join("; ")}): ${run.out}`,
      );
  } else {
    const why = charge.peakKnown
      ? `measured peak charge ${charge.bytes} B >= parked slack ${run.parkedSlack} B`
      : `charge is a lower bound only (${charge.note}), so ${charge.bytes} B proves nothing`;
    console.log(
      `  (${what}: ${why} — one-sidedness not provable, so only the contract assertions apply here; ` +
        `refused through "${run.outcome}")`,
    );
  }
  return run;
};

// A 4000-key document of `true` values: the boolean singletons are not charged
// and the keys are Pascal strings inside the map, so for every parser except
// YAML the parse charges essentially nothing and the one-sidedness above is
// measured to hold. The document is built before the charge measurement and
// before parking, or building it — not parsing it — is what would cross the
// ceiling.
{
  const gateTmp = mkdtemp("goccia-parse-gate-");
  // Counted per execution mode, not per family: a mode whose gate is never
  // reached is a mode whose contract assertions are all running against
  // refusals that never involve the gate, and a single gate hit anywhere would
  // hide that.
  const reachedGate: Record<string, number> = { interpreted: 0, bytecode: 0 };
  try {
    const gatedParses: ParkedGateCase[] = [
      {
        label: "JSON.parse",
        imports: [],
        setup:
          'const doc = "{" + Array.from({ length: 4000 }, (_, i) => \'"k\' + i + \'":true\').join(",") + "}";',
        call: "JSON.parse(doc)",
        completedGuard: "produced !== undefined",
      },
      {
        label: "JSON5.parse",
        imports: ['import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;'],
        setup: 'const doc = "{" + Array.from({ length: 4000 }, (_, i) => "k" + i + ": true").join(",") + "}";',
        call: "JSON5.parse(doc)",
        completedGuard: "produced !== undefined",
      },
      {
        label: "YAML.parse",
        imports: ['import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;'],
        setup: 'const doc = Array.from({ length: 4000 }, (_, i) => "k" + i + ": true").join("\\n") + "\\n";',
        call: "YAML.parse(doc)",
        completedGuard: "produced !== undefined",
      },
      {
        label: "JSONL.parse",
        imports: ['import * as JSONLNS from "goccia:jsonl"; const JSONL = JSONLNS.JSONL ?? JSONLNS;'],
        // One line holding the whole wide object: the map that has to double is
        // per record, so splitting it across lines would only build 4000 small
        // maps that never reach the gate.
        setup:
          'const doc = "{" + Array.from({ length: 4000 }, (_, i) => \'"k\' + i + \'":true\').join(",") + "}\\n";',
        call: "JSONL.parse(doc)",
        completedGuard: "produced !== undefined",
      },
      {
        label: "TOML.parse",
        imports: ['import * as TOMLNS from "goccia:toml"; const TOML = TOMLNS.TOML ?? TOMLNS;'],
        setup: 'const doc = Array.from({ length: 4000 }, (_, i) => "k" + i + " = true").join("\\n") + "\\n";',
        call: "TOML.parse(doc)",
        completedGuard: "produced !== undefined",
      },
    ];

    for (const probe of gatedParses) {
      for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
        const modeLabel = modeArgs.length > 0 ? "bytecode" : "interpreted";
        console.log(`--max-memory (growth gate inside ${probe.label} stays opaque to the guest: ${modeLabel})...`);
        if (runParkedGateProbe("Parse gate", probe, modeArgs, gateTmp).outcome === "gate")
          reachedGate[modeLabel] += 1;
      }
    }

    // Non-vacuity, per mode: the contract assertions accept a charged refusal
    // wherever the race is real, so a mode in which no parse reached the gate is
    // a mode asserting the contract against refusals that never involve the gate.
    for (const [modeLabel, hits] of Object.entries(reachedGate))
      if (hits === 0)
        throw new Error(
          `No parse gate case reached the growth gate in ${modeLabel} mode on this build, so the ` +
            `parse half of this family is asserting the contract against refusals that never ` +
            `involve the gate there.`,
        );
  } finally {
    clean(gateTmp);
  }
}

// The stringify half of the same convention, and the same treatment. JSON.stringify
// and JSON5.stringify wrap their whole body in a handler that converts a Pascal
// exception into a script-visible TypeError ("JSON.stringify error: ..."), and that
// handler had no re-raise allowlist: a growth-gate refusal arrived at the guest as a
// catchable TypeError carrying the budget text, which is the ceiling-you-can-ignore-
// in-a-loop this whole convention exists to prevent. Both handlers now name the limit
// family (timeout, instruction limit, memory limit) ahead of the generic arm, as
// Goccia.Builtins.GlobalFetch.pas and Goccia.Interpreter.pas do.
//
// Reaching the gate from a stringify needs a replacer: the plain serializer writes
// into a native buffer and only the result string is charged, while the replacer walk
// rebuilds every object property-by-property, so the property map's storage doubling
// is what asks for a block larger than the parked slack.
//
// What the measurement can and cannot say here. It measures the peak charge of the
// WHOLE call, and that necessarily includes the result string — around a megabyte for
// a 4000-key object, far above any slack rung. So these cases can never satisfy the
// one-sidedness precondition and always classify as "not provable", on every
// architecture; a comment arguing that the result string is reserved too late to beat
// the gate would be claiming something no assertion here checks. What IS checked is
// the contract on every rung, plus the per-mode requirement below that a stringify
// actually reaches the gate — which is what keeps this half pointed at the thing it
// names.
{
  const stringifyGateTmp = mkdtemp("goccia-stringify-gate-");
  const reachedGate: Record<string, number> = { interpreted: 0, bytecode: 0 };
  try {
    // The object is built before the charge measurement and before parking, or
    // building it — not stringifying it — is what would cross the ceiling.
    const wideObject =
      'const obj = Object.fromEntries(Array.from({ length: 4000 }, (_, i) => ["k" + i, true]));';
    const gatedStringifies: ParkedGateCase[] = [
      {
        label: "JSON.stringify",
        imports: [],
        setup: wideObject,
        call: "JSON.stringify(obj, (k, v) => v)",
        completedGuard: "produced.length > 0",
      },
      {
        label: "JSON5.stringify",
        imports: ['import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;'],
        setup: wideObject,
        call: "JSON5.stringify(obj, (k, v) => v)",
        completedGuard: "produced.length > 0",
      },
    ];

    for (const probe of gatedStringifies) {
      for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
        const modeLabel = modeArgs.length > 0 ? "bytecode" : "interpreted";
        console.log(`--max-memory (growth gate inside ${probe.label} stays opaque to the guest: ${modeLabel})...`);
        if (runParkedGateProbe("Stringify gate", probe, modeArgs, stringifyGateTmp).outcome === "gate")
          reachedGate[modeLabel] += 1;
      }
    }

    // Per mode, and it carries more weight here than in the parse half: a
    // stringify's measured charge always includes the result string, so no
    // stringify case can ever prove one-sidedness from its measurement and this
    // is the only assertion that keeps the half pointed at the gate.
    for (const [modeLabel, hits] of Object.entries(reachedGate))
      if (hits === 0)
        throw new Error(
          `No stringify gate case reached the growth gate in ${modeLabel} mode on this build, so the ` +
            `stringify half of this family is asserting the contract against refusals that never ` +
            `involve the gate there.`,
        );

    // Vacuity control for the "no guest-caught gate text" assertion above. The
    // other limiter — a charged string allocation — is script-visible by design
    // and always was, so the same harness pointed at a shape that only ever
    // crosses the charge (no replacer, one oversized string) MUST print the
    // marker the gate cases forbid. If this stops catching, the assertions above
    // are passing because nothing reaches any limiter, not because the ceiling is
    // opaque.
    //
    // This half needs no calibration on any width: the result string reserves
    // 800,004 bytes (length * SizeOf(Char), 2 bytes per char under delphiunicode
    // everywhere), more than 20x the parked slack whichever seed is used, and no
    // property map is touched at all — so there is no gate request for it to
    // race.
    console.log("--max-memory (stringify gate vacuity control: a charged refusal is still catchable)...");
    {
      const control: ParkedGateCase = {
        label: "control",
        imports: [],
        setup: 'const big = "y".repeat(400000);',
        call: "JSON.stringify(big)",
        completedGuard: "produced.length > 0",
      };
      const run = runGateCase(
        join(stringifyGateTmp, "stringify-gate-control.mjs"),
        parkedGateSource(control, 32_768),
        [],
      );
      if (run.outcome === "unparked")
        throw new Error(`Stringify gate control never parked the heap: ${run.out}`);
      if (run.caught === null || !/^guest-caught RangeError/.test(run.caught))
        throw new Error(
          `Stringify gate control should let the guest catch the charged RangeError, making the gate assertions non-vacuous: ${run.out}`,
        );
      if (run.exitCode !== 0)
        throw new Error(`Stringify gate control should exit 0, got ${run.exitCode}: ${run.out}`);
    }
  } finally {
    clean(stringifyGateTmp);
  }
}

// The same growth gate reached from plain guest code rather than a builtin, and
// the reason this block exists at all: under a parked heap the tree-walking
// interpreter used to answer `Error :: Object reference is Nil` where the VM
// answered the uncatchable refusal. That was not a limiter disagreement — it was
// a use-after-free surfacing as a guest-catchable error. `o["k" + i] = V(i)`
// holds three GC-managed values in native Pascal locals (base, key, assigned
// value) while `V(i)` pushes a call frame; that allocation charges the ceiling,
// trips CollectForMemoryPressure, and the collector marks explicit roots only,
// so the key was swept and ToPropertyKeyForBase then dispatched a virtual call
// through it (EObjectCheck under `$OBJECTCHECKS ON`, a silent read into freed
// memory in a production build). Two defences now stand behind this probe: the
// temporaries are rooted (Goccia.AST.Expressions.pas) and integrity faults are
// re-raised ahead of every generic conversion arm (Goccia.EngineFault.pas).
//
// One-sided by construction, unlike the parse and stringify gates above. Those
// pin `true` values so nothing charged can refuse first, which is what lets them
// insist the gate fires. This shape cannot: `"k" + i` builds a charged string
// per iteration, and the GROWTH_GATE_SLACK note explains why a charged side that
// scales with property count turns which-limiter-first into a per-width race.
// So all three survivable outcomes are accepted — the loop completing, the host
// reporting the growth-gate refusal, or the guest catching the charged
// RangeError that has always been catchable by design — and only the outcomes
// that mean the engine ran on freed memory are rejected: any integrity-fault
// text, and any guest catch that is not that one charged RangeError. A
// `guest-caught Error :: Object reference is Nil` is exactly what the
// interpreter printed here before the fix.
//
// Parked wider than GROWTH_GATE_SLACK, and that is the calibration. At 48,000
// the charged string keys exhaust the slack before the property map doubles
// (measured: both modes end in the catchable RangeError), so the growth-gate
// path — the one that was faulting — is never entered and the probe proves
// nothing. 147,000 sits between the 64-bit C = 1022 doubling (73,632) and the
// C = 2046 one (147,360): measured, the run parks at ~133,800 of real slack and
// the gate refuses the 73,632-byte request, which is where the fault reproduced.
// On i386 the largest reachable doubling for a 4000-key map is 98,240, below
// this slack, so the gate cannot fire there at all and the run ends in one of
// the other two outcomes — which is the reason this probe is one-sided rather
// than a re-tuned two-sided one. `parked true` keeps it from passing without
// ever entering the window.
//
// RE-DERIVED when the gate learned to collect (ADR 0110), because a collecting
// gate is exactly the kind of change that silently moves a crossing. The
// constants did not have to move, and the reason is worth recording rather than
// leaving as a lucky pass. Measured on the same shape, before and after:
//
//   before   interpreted  parked slack 136,498   refused 73,632
//            bytecode     parked slack 136,804   refused 73,632
//   after    interpreted  parked slack 133,802   refused 73,632
//            bytecode     parked slack 134,124   refused 73,632
//
// The crossing is unmoved because almost nothing here is reclaimable at the
// moment the gate fires: the parked ballast is live by construction, and the
// 4000-element iterable the loop walks is live for the whole loop, so the
// forced collection finds little and the fit test lands where it landed
// before. That is the opposite of the capacity probe above — where the live set
// really is near zero and the collection moves the crossing by two doublings —
// and having both shapes in the suite is what distinguishes "the gate collects"
// from "the gate collects and that always buys capacity". The ~2.7 KB of slack
// difference between the columns is run-to-run baseline noise, not behaviour:
// it is well inside the 60 KB margin between the parked slack and the refused
// request.
const ASSIGNMENT_FAULT_SLACK = 147_000;
{
  const assignGateTmp = mkdtemp("goccia-assign-gate-");
  try {
    for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
      const modeLabel = modeArgs.length > 0 ? "bytecode" : "interpreted";
      console.log(
        `--max-memory (computed property assignment never faults into the guest: ${modeLabel})...`,
      );
      const src = [
        "const V = (i) => true;",
        ...parkingPreamble(ASSIGNMENT_FAULT_SLACK),
        "try {",
        "  const o = {};",
        "  for (const i of Array.from({ length: 4000 }, (_, j) => j)) o['k' + i] = V(i);",
        '  console.log("guest-completed", Object.keys(o).length);',
        "} catch (e) {",
        // The marker an internal fault must never reach.
        "  console.log('guest-caught', e.name, '::', e.message);",
        "}",
        "",
      ].join("\n");
      const srcPath = join(assignGateTmp, `assign-gate-${modeLabel}.mjs`);
      writeFileSync(srcPath, src);
      const proc = Bun.spawnSync([LOADER, "--max-memory=4194304", ...modeArgs, srcPath], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 180_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      if (!out.includes("parked true"))
        throw new Error(`Assignment gate (${modeLabel}) never parked the heap: ${out}`);
      // The fault text itself, wherever it lands — caught by the guest or
      // reported by the host. Either way the engine touched freed memory.
      if (/Object reference is Nil|Access violation|Bus error|Invalid pointer/i.test(out))
        throw new Error(
          `Assignment gate (${modeLabel}) hit an engine-integrity fault under memory pressure: ${out}`,
        );
      if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
        throw new Error(`Assignment gate (${modeLabel}) crashed: ${out}`);
      if (out.includes("guest-caught")) {
        // The charged limiter is script-visible by design and always was; every
        // other guest catch here is the engine handing script code a failure of
        // its own, which is what this block exists to forbid.
        if (!out.includes("guest-caught RangeError :: Allocation failed"))
          throw new Error(
            `Assignment gate (${modeLabel}) let the guest catch something other than the charged RangeError: ${out}`,
          );
        if (proc.exitCode !== 0)
          throw new Error(
            `Assignment gate (${modeLabel}) caught the charged RangeError but exited ${proc.exitCode}: ${out}`,
          );
      } else if (out.includes("guest-completed")) {
        if (!out.includes("guest-completed 4000"))
          throw new Error(
            `Assignment gate (${modeLabel}) completed with the wrong key count, which is silent corruption: ${out}`,
          );
        if (proc.exitCode !== 0)
          throw new Error(
            `Assignment gate (${modeLabel}) completed but exited ${proc.exitCode}: ${out}`,
          );
      } else if (out.includes("would exceed the memory budget")) {
        if (proc.exitCode === 0)
          throw new Error(
            `Assignment gate (${modeLabel}) reported the budget refusal but exited 0: ${out}`,
          );
      } else {
        throw new Error(
          `Assignment gate (${modeLabel}) produced none of the permitted outcomes: ${out}`,
        );
      }
    }
  } finally {
    clean(assignGateTmp);
  }
}

// An ordinary failing test file is still an ordinary failing test file. The
// runner now aborts the whole run — exit 70, no summary — when an engine
// integrity fault reaches one of its per-file arms (ADR 0109, "Host tier: the
// test runner"), and the failure mode worth guarding against is that policy
// widening to catch things it was never meant to. A failed assertion is a
// verdict on one file delivered by a sound heap: it stays exit 1, the passing
// file beside it still runs and still counts, and the summary is unchanged.
//
// The abort path itself is not reachable from here. Raising a genuine
// EObjectCheck inside a test file would take an injection hook in the runner —
// a switch whose only purpose is to corrupt a production binary on request —
// and that is not worth shipping for a test; the abort was verified by hand
// against a temporary build instead. What this block locks is the contract the
// abort must not disturb, in both execution modes and both --jobs shapes,
// since the sequential and parallel paths reach the arms differently.
console.log("TestRunner (a failing file stays exit 1, not an integrity abort)...");
{
  const failTmp = mkdtemp("goccia-runner-fail-");
  try {
    writeFileSync(
      join(failTmp, "failing.test.js"),
      'describe("d", () => {\n  test("fails", () => {\n    expect(1).toBe(2);\n  });\n});\n',
    );
    // The console.log marker proves the passing FILE executed — the count
    // assertions below prove one test passed, but only the marker ties that
    // pass to this file rather than to some shape change in the failing one.
    writeFileSync(
      join(failTmp, "passing.test.js"),
      'console.log("RAN: passing.test.js");\ndescribe("d", () => {\n  test("passes", () => {\n    expect(1).toBe(1);\n  });\n});\n',
    );
    for (const modeArgs of [[], ["--mode=bytecode"]] as const) {
      for (const jobsArg of ["--jobs=1", "--jobs=2"]) {
        const label = `${modeArgs.length > 0 ? "bytecode" : "interpreted"} ${jobsArg}`;
        const proc = Bun.spawnSync(
          [TESTRUNNER, failTmp, "--no-progress", jobsArg, ...modeArgs],
          { stdout: "pipe", stderr: "pipe" },
        );
        const out = proc.stdout.toString() + proc.stderr.toString();
        if (out.includes("Integrity fault:"))
          throw new Error(`TestRunner (${label}) treated a failed assertion as an integrity fault: ${out}`);
        if (proc.exitCode !== 1)
          throw new Error(`TestRunner (${label}) should exit 1 on a failed test, got ${proc.exitCode}: ${out}`);
        // The report shape the abort must leave alone: the failing file counted
        // once, the file beside it still executed, and the failure named.
        for (const expected of [
          "Test Results Test Files: 2",
          "Test Results Run Tests: 2",
          "Test Results Passed: 1 (50.00%)",
          "Test Results Failed: 1 (50.00%)",
          'Test "fails" in suite "d": Expected 1 to be 2',
          // The guest marker directly proves the passing FILE executed. The
          // parallel workers capture guest console output, so the marker is
          // only observable sequentially; the jobs=2 shape keeps the count
          // assertions, which pin the same fact arithmetically.
          ...(jobsArg === "--jobs=1" ? ["RAN: passing.test.js"] : []),
        ]) {
          if (!out.includes(expected))
            throw new Error(`TestRunner (${label}) report lost "${expected}": ${out}`);
        }
      }
    }
  } finally {
    clean(failTmp);
  }
}

console.log("--max-memory (a charged reservation collects before it refuses)...");
{
  // A charged reservation R used to be refused without collecting at all
  // whenever the live set sat below the pressure trigger, which fires only
  // once BytesAllocated reaches maxBytes - maxBytes/8 (clamped to
  // 16 KiB..16 MiB). Any R larger than that reserve therefore had a whole
  // interval of heap positions in which it was refused with megabytes of
  // reclaimable garbage still on the heap, and which positions those were
  // depended on where a script's transient garbage happened to sit — so the
  // refusals came in bands rather than at a floor. TryReserveExternalBytes now
  // forces the collection and re-tests once before refusing, through the shared
  // TryCollectForLimitedBytes that the uncharged growth gate also uses (ADR
  // 0110) — including the shared floor that keeps a retry at O(1). This probe
  // covers the charged half of that routine; the capacity probe above covers
  // the gated half.
  //
  // The shape is constructed rather than sampled, because a band's position is
  // a property of a particular machine's live set and would not survive CI.
  // Under a 64 MiB ceiling the pressure reserve is 8,388,608 B. Park to 22 MB
  // of slack, then drop ~10.5 MB of reclaimable garbage: the heap sits at
  // ~11.5 MB of slack, still clear of the trigger by ~3 MB, so the heuristic
  // declines. An 18 MB reservation is over that slack and over the reserve —
  // squarely in the old refusal interval — and fits comfortably once the
  // garbage goes. `band true` pins that the probe really is in the interval;
  // if the margins ever drift the diagnostics say which one.
  const reserveTmp = mkdtemp("goccia-reserve-collect-");
  try {
    const RESERVE_CEILING = 67_108_864;
    const parkedReservationSource = (repeatChars: number): string =>
      [
        ...parkingPreamble(22_000_000),
        "let garbage = [];",
        'for (const g of Array.from({ length: 320 }, (_, j) => j)) garbage.push("g".repeat(16384));',
        "garbage = null;",
        "const floor = Goccia.gc.maxBytes / 8;",
        "const slackBefore = Goccia.gc.maxBytes - Goccia.gc.bytesAllocated;",
        'console.log("band", slackBefore > floor, "slack", slackBefore, "floor", floor);',
        "try {",
        `  const big = "b".repeat(${repeatChars});`,
        '  console.log("reserved", big.length);',
        "} catch (e) {",
        "  console.log('refused', e.name, '::', e.message);",
        "}",
        "",
      ].join("\n");

    // 18,000,000 charged bytes: over the ~11.5 MB of slack the reservation
    // sees, over the 8,388,608 B reserve, and under the ~22 MB a collection
    // gives back. Before the fix this refused; it must now succeed.
    const bandPath = join(reserveTmp, "reserve-band.mjs");
    writeFileSync(bandPath, parkedReservationSource(9_000_000));
    const bandProc = Bun.spawnSync([LOADER, `--max-memory=${RESERVE_CEILING}`, bandPath], {
      stdout: "pipe",
      stderr: "pipe",
      timeout: 180_000,
    });
    const bandOut = bandProc.stdout.toString() + bandProc.stderr.toString();
    if (!bandOut.includes("parked true"))
      throw new Error(`Reserve-band probe never parked the heap: ${bandOut}`);
    if (!bandOut.includes("band true"))
      throw new Error(
        `Reserve-band probe did not land below the pressure trigger, so it proves nothing: ${bandOut}`,
      );
    if (bandProc.exitCode !== 0)
      throw new Error(`Reserve-band probe should exit 0, got ${bandProc.exitCode}: ${bandOut}`);
    if (!bandOut.includes("reserved 9000000"))
      throw new Error(
        `A reservation larger than the pressure reserve must succeed after the forced collection: ${bandOut}`,
      );

    // The floor is still a floor. 48,000,000 charged bytes fits the ceiling on
    // its own but not beside a live set the collection cannot reclaim, so the
    // collect-then-retry path has to end in the refusal it always did.
    const floorPath = join(reserveTmp, "reserve-floor.mjs");
    writeFileSync(floorPath, parkedReservationSource(24_000_000));
    const floorProc = Bun.spawnSync([LOADER, `--max-memory=${RESERVE_CEILING}`, floorPath], {
      stdout: "pipe",
      stderr: "pipe",
      timeout: 180_000,
    });
    const floorOut = floorProc.stdout.toString() + floorProc.stderr.toString();
    if (!floorOut.includes("parked true"))
      throw new Error(`Reserve-floor probe never parked the heap: ${floorOut}`);
    if (floorProc.exitCode !== 0)
      throw new Error(`Reserve-floor probe should exit 0, got ${floorProc.exitCode}: ${floorOut}`);
    if (!floorOut.includes("refused RangeError"))
      throw new Error(
        `A reservation a collection cannot make room for must still be refused: ${floorOut}`,
      );
  } finally {
    clean(reserveTmp);
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
// observed from inside a test. These properties are load-bearing enough to pin
// from the outside: toBeInstanceOf naming what it compared, a rejected returned
// Promise naming the error it rejected with, and the vitest shim keeping a
// named, actionable error for every member it does not provide.
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

    // The reason a returned Promise rejected with is the whole failure report,
    // and an Error keeps "name" on its prototype and "message" non-enumerable,
    // so serializing the value reported `new Error("boom")` as "{}" — the one
    // shape a debugging session most needs named. A class extending Error
    // inherits Error.prototype.name, so its identity is read off the
    // constructor, while an explicitly assigned name still wins.
    const rejectionSrc = join(tmp, "rejection.test.js");
    writeFileSync(
      rejectionSrc,
      [
        "class MyErr extends Error {}",
        "class NamedErr extends Error {",
        "  constructor(message) { super(message); this.name = 'ValidationFailure'; }",
        "}",
        "class ProtoNamed extends Error {}",
        "ProtoNamed.prototype.name = 'ProtoAssigned';",
        // An explicit prototype name that happens to spell "Error" is still
        // the author's answer, so the constructor name must not displace it.
        "class ProtoErrorNamed extends Error {}",
        "ProtoErrorNamed.prototype.name = 'Error';",
        'test("plain error", () => Promise.reject(new Error("boom")));',
        'test("subclass error", () => Promise.reject(new MyErr("boom")));',
        'test("named subclass error", () => Promise.reject(new NamedErr("boom")));',
        'test("prototype-named subclass error", () => Promise.reject(new ProtoNamed("boom")));',
        'test("prototype-named Error subclass", () => Promise.reject(new ProtoErrorNamed("boom")));',
        'test("native error", () => Promise.reject(new TypeError("bad")));',
        'test("plain object", () => Promise.reject({ code: 42 }));',
        'test("message only", () => Promise.reject({ message: "hi" }));',
        "",
      ].join("\n"),
    );
    for (const mode of ["--mode=interpreted", "--mode=bytecode"]) {
      const rejection = await $`${TESTRUNNER} ${rejectionSrc} ${mode} --no-progress 2>&1`.nothrow();
      const rejectionOut = rejection.text();
      for (const expected of [
        "Returned Promise rejected: Error: boom",
        "Returned Promise rejected: MyErr: boom",
        "Returned Promise rejected: ValidationFailure: boom",
        "Returned Promise rejected: ProtoAssigned: boom",
        "Returned Promise rejected: TypeError: bad",
        "Returned Promise rejected: { code: 42 }",
        "Returned Promise rejected: { message: 'hi' }",
      ]) {
        if (!rejectionOut.includes(expected))
          throw new Error(
            `TestRunner (${mode}) should report "${expected}", got: ${rejectionOut}`,
          );
      }
      // ProtoErrorNamed spells its prototype name "Error" on purpose, which
      // reads identically to the inherited default; only the absence of the
      // constructor name tells the two apart.
      if (rejectionOut.includes("ProtoErrorNamed: boom"))
        throw new Error(
          `TestRunner (${mode}) must keep an explicitly assigned "Error" prototype name, got: ${rejectionOut}`,
        );
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

// -- Runtime diagnostic parity (TestRunner) ------------------------------------

// An uncaught runtime fault is reported by the runner, not by the suite, so the
// rendered diagnostic is only observable from outside. It used to be a
// different diagnostic per execution mode: interpreted runs printed the named
// callee, a suggestion, a `--> file:line:column` header and a code frame, while
// bytecode runs printed a bare "Fatal error: TypeError: undefined is not a
// function". The two renderings are pinned together here — byte-for-byte
// equality is the contract, because any drift in either mode is the defect.
console.log("Runtime diagnostic parity...");
{
  const tmp = mkdtemp("goccia-diagnostic-parity-");
  try {
    const calleeSrc = join(tmp, "callee.test.js");
    writeFileSync(calleeSrc, ["const obj = {};", "obj.missingMethod();", ""].join("\n"));

    const renderings: Record<string, string> = {};
    for (const mode of ["--mode=interpreted", "--mode=bytecode"]) {
      const run = await $`${TESTRUNNER} ${calleeSrc} ${mode} --no-progress 2>&1`.nothrow();
      const out = run.text();
      for (const expected of [
        "TypeError: obj.missingMethod is not a function",
        "Suggestion: 'obj' is of type 'object' which does not have method 'missingMethod'",
        "callee.test.js:2:18",
        "2 | obj.missingMethod();",
      ]) {
        if (!out.includes(expected))
          throw new Error(
            `TestRunner (${mode}) should report "${expected}", got: ${out}`,
          );
      }
      if (out.includes("Fatal error"))
        throw new Error(
          `TestRunner (${mode}) must render a thrown value as a diagnostic, not a fatal error, got: ${out}`,
        );
      // The header and code frame are the part that must match across modes;
      // the results block below carries mode-specific timing lines.
      renderings[mode] = out.slice(0, out.indexOf("Test Results Test Files:"));
    }
    if (renderings["--mode=interpreted"] !== renderings["--mode=bytecode"])
      throw new Error(
        `Both modes must render an identical diagnostic.\ninterpreted:\n${renderings["--mode=interpreted"]}\nbytecode:\n${renderings["--mode=bytecode"]}`,
      );

    // The code frame is read from a file, and the file it was read from used to
    // be the entry every time: a module that threw while evaluating produced a
    // header naming the module and an excerpt quoting whatever the entry file
    // happened to have at that line number.
    const dep = join(tmp, "dep.js");
    writeFileSync(
      dep,
      [
        "// dep filler 1",
        "// dep filler 2",
        "// dep filler 3",
        "// dep filler 4",
        "export const boom = (() => { throw new Error('dep exploded'); })();",
        "",
      ].join("\n"),
    );
    const entry = join(tmp, "entry.test.js");
    writeFileSync(
      entry,
      [
        "// entry filler 1",
        "// entry filler 2",
        "// entry filler 3",
        "// entry filler 4",
        "import { boom } from './dep.js';",
        "console.log(boom);",
        "",
      ].join("\n"),
    );

    const frames: Record<string, string> = {};
    for (const mode of ["--mode=interpreted", "--mode=bytecode"]) {
      const run = await $`${TESTRUNNER} ${entry} ${mode} --no-progress 2>&1`.nothrow();
      const out = run.text();
      if (!out.includes("Error: dep exploded"))
        throw new Error(`TestRunner (${mode}) should report the module's error, got: ${out}`);
      if (!out.includes("dep.js:5:"))
        throw new Error(`TestRunner (${mode}) should locate the fault in dep.js, got: ${out}`);
      if (!out.includes("throw new Error('dep exploded')"))
        throw new Error(
          `TestRunner (${mode}) should quote dep.js in the code frame, got: ${out}`,
        );
      if (out.includes("entry filler") || out.includes("import { boom }"))
        throw new Error(
          `TestRunner (${mode}) must not quote the entry file for a fault in dep.js, got: ${out}`,
        );
      frames[mode] = out.slice(0, out.indexOf("Test Results Test Files:"));
    }
    if (frames["--mode=interpreted"] !== frames["--mode=bytecode"])
      throw new Error(
        `Both modes must render an identical module diagnostic.\ninterpreted:\n${frames["--mode=interpreted"]}\nbytecode:\n${frames["--mode=bytecode"]}`,
      );
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
