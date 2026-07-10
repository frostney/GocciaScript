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
        !shadowWarningOut.replace(/\r/g, "").split("\n").includes("47"))
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
  const fill = "const x = new Array(2 ** 30); x.length;\n";
  const { exitCode, json } = runLoaderJson(fill, ["--timeout=50"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Array-fill timeout exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

console.log("--timeout (native sparse-array fill, bytecode)...");
{
  // Far single-index writes route to sparse storage and complete instantly;
  // the Array constructor still materializes dense holes, so it stalls.
  const fill = "const x = new Array(2 ** 30); x.length;\n";
  const { exitCode, json } = runLoaderJson(fill, ["--timeout=50", "--mode=bytecode"], { timeout: 10_000 });
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

// -- --log option (Loader) ------------------------------------------------------

console.log("--log option...");
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
