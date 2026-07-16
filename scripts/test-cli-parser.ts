#!/usr/bin/env bun
/**
 * test-cli-parser.ts
 *
 * Parser-level CLI tests: error display (caret, suggestions, JSON fields).
 */

import { $ } from "bun";
import { writeFileSync } from "fs";
import { join } from "path";
import { LOADER } from "./test-cli/binaries";
import { assertSyntaxError, normalizeLineEndings, runLoaderJson } from "./test-cli/assertions";
import { clean, mkdtemp } from "./test-cli/tmpdir";

function assertSyntaxErrorInBothModes(
  source: string,
  desc: string,
  args: readonly string[] = [],
): void {
  assertSyntaxError(source, desc, [...args]);
  assertSyntaxError(source, `${desc} (bytecode)`, [...args, "--mode=bytecode"]);
}

// -- Error display (SyntaxError with caret and suggestion) ----------------------

console.log("Error display (SyntaxError with caret and suggestion)...");
{
  const res = await $`printf '%s\n' 'const x = 1' 'const y = x +' | ${LOADER} 2>&1`.nothrow();
  const out = res.text();
  if (res.exitCode === 0) throw new Error("Expected syntax error exit code");
  if (!out.includes("SyntaxError")) throw new Error(`Expected SyntaxError, got: ${out}`);
  if (!out.includes("^")) throw new Error(`Expected caret in error display, got: ${out}`);
  const lower = out.toLowerCase();
  if (!lower.includes("suggest") && !lower.includes("expect") && !lower.includes("unexpected")) {
    throw new Error(`Expected suggestion/expectation hint in error display`);
  }
}

// -- Error display (JSON error output with line/column) -------------------------

console.log("Error display (JSON error output)...");
{
  const { exitCode, json } = runLoaderJson("const x = ;\n");
  if (exitCode !== 1) throw new Error(`Syntax error exit code should be 1, got ${exitCode}`);
  if (json.ok !== false) throw new Error(`JSON error ok should be false`);
  if (json.error?.type !== "SyntaxError") throw new Error(`Expected SyntaxError, got ${json.error?.type}`);
  if (typeof json.error?.line !== "number") throw new Error(`JSON error should include numeric line, got ${json.error?.line}`);
  if (typeof json.error?.column !== "number") throw new Error(`JSON error should include numeric column, got ${json.error?.column}`);
}

// -- Error display (bytecode mode) ----------------------------------------------

console.log("Error display (bytecode SyntaxError)...");
{
  const { exitCode, json } = runLoaderJson("const x = ;\n", ["--mode=bytecode"]);
  if (exitCode !== 1) throw new Error(`Bytecode syntax error exit code should be 1, got ${exitCode}`);
  if (json.ok !== false) throw new Error(`Bytecode JSON error ok should be false`);
  if (json.error?.type !== "SyntaxError") throw new Error(`Expected SyntaxError in bytecode, got ${json.error?.type}`);
  if (typeof json.error?.line !== "number") throw new Error(`Bytecode JSON error should include numeric line, got ${json.error?.line}`);
  if (typeof json.error?.column !== "number") throw new Error(`Bytecode JSON error should include numeric column, got ${json.error?.column}`);
}

// -- Malformed optional private field access ------------------------------------

console.log("Malformed optional private field access...");
{
  const source = "class Box { #value = 1; read(obj) { return obj?.#; } }\n";
  assertSyntaxError(source, "optional private access without name");
  assertSyntaxError(source, "optional private access without name (bytecode)", ["--mode=bytecode"]);
}

// -- Malformed literal class accessors ------------------------------------------

console.log("Malformed literal class accessors...");
{
  for (const [source, desc] of [
    ['class C { get "dash-key" = 1; }\n', "string-named getter field"],
    ["class C { get 1 = 1; }\n", "numeric-named getter field"],
    ["class C { get class = 1; }\n", "keyword-named getter field"],
    ['class C { set "dash-key" = 1; }\n', "string-named setter field"],
    ["class C { set 1; }\n", "numeric-named setter declaration"],
    ["class C { set class = 1; }\n", "keyword-named setter field"],
  ] as const) {
    assertSyntaxError(source, desc);
    assertSyntaxError(source, `${desc} (bytecode)`, ["--mode=bytecode"]);
  }
}

// -- Accessor properties are invalid destructuring assignment targets -----------

console.log("Accessor properties are invalid destructuring assignment targets...");
{
  for (const [source, desc] of [
    ["let obj = {}; ({ get x() { return 1; } } = obj);\n", "getter destructuring target"],
    ["let obj = {}; ({ set x(value) {} } = obj);\n", "setter destructuring target"],
    ["let obj = {}; ({ get ['x']() { return 1; } } = obj);\n", "computed getter destructuring target"],
    ["let obj = {}; ({ set ['x'](value) {} } = obj);\n", "computed setter destructuring target"],
  ] as const) {
    assertSyntaxError(source, desc);
    assertSyntaxError(source, `${desc} (bytecode)`, ["--mode=bytecode"]);
  }
}

// -- Function parameter-list early errors ---------------------------------------

console.log("Function parameter-list early errors...");
{
  const cases = [
    {
      desc: "async function parameter default containing await",
      source: "async function f(a = await 1) {}\n",
      args: ["--compat-function"],
    },
    {
      desc: "async arrow parameter default containing await",
      source: "const f = async (a = await 1) => a;\n",
      args: [],
    },
    {
      desc: "async function parameter binding named await",
      source: "async function f(await) {}\n",
      args: ["--compat-function"],
    },
    {
      desc: "async arrow rest binding named await",
      source: "const f = async (...await) => {};\n",
      args: [],
    },
    {
      desc: "generator function rest binding named yield",
      source: "function* g(...yield) {}\n",
      args: ["--compat-function"],
    },
    {
      desc: "arrow duplicate parameter names",
      source: "const f = (a, a) => a;\n",
      args: [],
    },
    {
      desc: "object method duplicate parameter names",
      source: "const obj = { m(a, a) {} };\n",
      args: [],
    },
    {
      desc: "class method duplicate parameter names",
      source: "class C { m(a, a) {} }\n",
      args: [],
    },
    {
      desc: "function duplicate binding with rest parameter",
      source: "function f(a, ...a) {}\n",
      args: ["--compat-function"],
    },
    {
      desc: "function duplicate binding with default parameter",
      source: "function f(a = 0, a) {}\n",
      args: ["--compat-function"],
    },
  ] as const;

  for (const { desc, source, args } of cases) {
    assertSyntaxError(source, desc, [...args]);
    assertSyntaxError(source, `${desc} (bytecode)`, [...args, "--mode=bytecode"]);
  }
}

// -- Migrated language early-error coverage ------------------------------------

console.log("Language early errors use the CLI parser...");
{
  const cases = [
    {
      desc: "await without an operand in an async arrow body",
      source: "const fn = async () => { await; };\n",
      args: [],
    },
    {
      desc: "await in an async function expression parameter default",
      source: "const fn = async function(value = await 1) {};\n",
      args: ["--compat-function"],
    },
    {
      desc: "await in an async object method parameter default",
      source: "const value = { async method(value = await 1) {} };\n",
      args: [],
    },
    {
      desc: "await in an async class method parameter default",
      source: "class Value { async method(value = await 1) {} }\n",
      args: [],
    },
    {
      desc: "reserved word used as an object-pattern binding",
      source: "const { if } = { if: 1 };\n",
      args: [],
    },
    {
      desc: "unicode escape with an invalid identifier-start character",
      source: "const \\u0031 = 7;\n",
      args: [],
    },
    {
      desc: "escaped reserved word in a declaration binding",
      source: "const \\u0069f = 7;\n",
      args: [],
    },
    {
      desc: "escaped reserved word in an identifier reference",
      source: "const read = () => \\u0069f;\n",
      args: [],
    },
    {
      desc: "escaped reserved word in an arrow parameter",
      source: "const read = (\\u0069f) => 1;\n",
      args: [],
    },
    {
      desc: "escaped reserved word in a class binding",
      source: "class \\u0069f {}\n",
      args: [],
    },
    {
      desc: "escaped new in new.target",
      source: "const read = () => n\\u0065w.target;\n",
      args: [],
    },
    {
      desc: "escaped target in new.target",
      source: "const read = function() { return new.\\u0074arget; };\n",
      args: ["--compat-function"],
    },
    {
      desc: "escaped of in for-of",
      source: "for (let value o\\u0066 [1]) {}\n",
      args: [],
    },
    {
      desc: "escaped in in for-in",
      source: "for (let value i\\u006e [1]) {}\n",
      args: ["--compat-for-in-loop"],
    },
    {
      desc: "escaped get in an object accessor",
      source: "const value = ({ g\\u0065t property() { return 1; } });\n",
      args: [],
    },
    {
      desc: "escaped reserved word in a function-expression binding",
      source: "const value = function \\u0069f() {};\n",
      args: ["--compat-function"],
    },
    {
      desc: "escaped reserved word in a generator-expression binding",
      source: "const value = function* \\u0069f() {};\n",
      args: ["--compat-function"],
    },
    {
      desc: "escaped reserved word in a class-expression binding",
      source: "const value = class \\u0069f {};\n",
      args: [],
    },
    {
      desc: "unicode escape with an invalid identifier-continuation character",
      source: "const a\\u0020 = 7;\n",
      args: [],
    },
    {
      desc: "unicode escape with an invalid property identifier character",
      source: "const value = { a: 1 }; value.\\u0020;\n",
      args: [],
    },
    {
      desc: "continue to a non-iteration label",
      source: "block: { continue block; }\n",
      args: ["--compat-label"],
    },
    {
      desc: "label targeting a lexical declaration",
      source: "label: let value = 1;\n",
      args: ["--compat-label", "--compat-non-strict-mode"],
    },
    {
      desc: "label targeting a class declaration",
      source: "label: class Value {}\n",
      args: ["--compat-label", "--compat-non-strict-mode"],
    },
    {
      desc: "break crossing an arrow-function boundary",
      source: "outer: while (true) { const fn = () => { break outer; }; }\n",
      args: ["--compat-label", "--compat-while-loops"],
    },
    {
      desc: "same-line labeled lexical binding pattern",
      source: "label: let [value] = [1];\n",
      args: ["--compat-label", "--compat-non-strict-mode"],
    },
    {
      desc: "newline before a labeled lexical binding pattern",
      source: "if (false) { label: let\n[value] = [1]; }\n",
      args: ["--compat-label", "--compat-non-strict-mode", "--compat-asi"],
    },
    {
      desc: "strict let identifier expression",
      source: '"use strict"; let + 1;\n',
      args: ["--compat-non-strict-mode"],
    },
    {
      desc: "strict labeled let identifier expression",
      source: '"use strict"; if (false) { label: let\nvalue = 1; }\n',
      args: ["--compat-label", "--compat-non-strict-mode", "--compat-asi"],
    },
    {
      desc: "strict let identifier expression in a function",
      source: 'const fn = function() { "use strict"; let + 1; };\n',
      args: ["--compat-function", "--compat-non-strict-mode"],
    },
    {
      desc: "strict let identifier expression in an arrow",
      source: 'const fn = () => { "use strict"; let + 1; };\n',
      args: ["--compat-non-strict-mode"],
    },
    {
      desc: "let identifier expression in a class method",
      source: "class Value { method() { let + 1; } }\n",
      args: ["--compat-non-strict-mode"],
    },
    {
      desc: "let identifier expression in a static class method",
      source: "class Value { static method() { let + 1; } }\n",
      args: ["--compat-non-strict-mode"],
    },
    {
      desc: "let identifier expression in a class getter",
      source: "class Value { get property() { let + 1; } }\n",
      args: ["--compat-non-strict-mode"],
    },
    {
      desc: "let identifier expression in a class setter",
      source: "class Value { set property(value) { let + 1; } }\n",
      args: ["--compat-non-strict-mode"],
    },
    {
      desc: "let identifier expression in a static block",
      source: "class Value { static { let + 1; } }\n",
      args: ["--compat-non-strict-mode"],
    },
    {
      desc: "let identifier expression in a class field initializer",
      source: "class Value { property = let + 1; }\n",
      args: ["--compat-non-strict-mode"],
    },
    {
      desc: "type assertion without a type before is",
      source: "const value = 1; value as is 1;\n",
      args: [],
    },
    {
      desc: "pattern-filtered for-of without of",
      source: "for (const item is _ items) {}\n",
      args: [],
    },
    {
      desc: "catch annotation without a type",
      source: "try { throw 1; } catch (error:) {}\n",
      args: [],
    },
    {
      desc: "strict function parameter named arguments",
      source: '"use strict"; function echo(arguments) { return arguments; }\n',
      args: ["--compat-function"],
    },
    {
      desc: "strict string decimal escape",
      source: '"use strict"; "\\1";\n',
      args: ["--compat-non-strict-mode"],
    },
    {
      desc: "decimal escape before a later use strict directive",
      source: '"\\145"; "use strict";\n',
      args: ["--compat-non-strict-mode"],
    },
    {
      desc: "decimal escape before a later use strict directive in a nested function",
      source: 'function nested() { "\\8"; "use strict"; }\n',
      args: ["--compat-function", "--compat-non-strict-mode"],
    },
    {
      desc: "newline after throw",
      source: 'throw\nnew Error("unreachable");\n',
      args: ["--compat-asi"],
    },
  ] as const;

  for (const { source, desc, args } of cases)
    assertSyntaxErrorInBothModes(source, desc, args);
}

// -- Annex B labelled-function early errors -----------------------------------

console.log("Annex B labelled-function early errors...");
{
  const baseArgs = ["--compat-label", "--compat-function", "--compat-non-strict-mode"] as const;
  const cases = [
    {
      desc: "strict direct labelled function",
      source: '"use strict"; label: function value() {}\n',
      args: baseArgs,
    },
    {
      desc: "strict nested labelled function",
      source: '"use strict"; outer: inner: function value() {}\n',
      args: baseArgs,
    },
    {
      desc: "labelled function as an if consequent",
      source: "if (true) label: function value() {}\n",
      args: baseArgs,
    },
    {
      desc: "labelled function as an else consequent",
      source: "if (false) {} else label: function value() {}\n",
      args: baseArgs,
    },
    {
      desc: "labelled function as a with body",
      source: "with ({}) label: function value() {}\n",
      args: baseArgs,
    },
    {
      desc: "labelled function as a while body",
      source: "while (false) label: function value() {}\n",
      args: [...baseArgs, "--compat-while-loops"],
    },
    {
      desc: "labelled function as a do-while body",
      source: "do label: function value() {} while (false);\n",
      args: [...baseArgs, "--compat-while-loops"],
    },
    {
      desc: "labelled function as a traditional for body",
      source: "for (let index = 0; index < 0; index++) label: function value() {}\n",
      args: [...baseArgs, "--compat-traditional-for-loop"],
    },
    {
      desc: "labelled function as a for-in body with a declaration",
      source: "for (const key in {}) label: function value() {}\n",
      args: [...baseArgs, "--compat-for-in-loop"],
    },
    {
      desc: "labelled function as a for-in body with an assignment",
      source: "let key; for (key in {}) label: function value() {}\n",
      args: [...baseArgs, "--compat-for-in-loop"],
    },
    {
      desc: "labelled function as a for-of body",
      source: "for (const value of []) label: function nested() {}\n",
      args: baseArgs,
    },
  ] as const;

  for (const { source, desc, args } of cases)
    assertSyntaxErrorInBothModes(source, desc, args);
}

// -- Strict-mode legacy octal literal rejection ---------------------------------

console.log("Strict-mode legacy octal literal rejection...");
{
  const source = '"use strict";\n01;\n';
  assertSyntaxError(source, "legacy octal literal in strict code", ["--compat-non-strict-mode"]);
  assertSyntaxError(source, "legacy octal literal in strict bytecode", [
    "--compat-non-strict-mode",
    "--mode=bytecode",
  ]);
}

// -- ASI at EOF -----------------------------------------------------------------

console.log("ASI at EOF...");
{
  const asiRes = runLoaderJson("const value = 42", ["--compat-asi"]);
  if (asiRes.exitCode !== 0) throw new Error(`ASI should accept a final declaration at EOF without a semicolon`);
  if (asiRes.json.ok !== true)
    throw new Error(`ASI EOF declaration should succeed, got: ${JSON.stringify(asiRes.json)}`);

  const noAsiRes = runLoaderJson("const value = 42", []);
  if (noAsiRes.exitCode !== 0)
    throw new Error(`Non-ASI should accept a final declaration at EOF without a semicolon`);
  if (noAsiRes.json.ok !== true)
    throw new Error(`Non-ASI EOF declaration should succeed, got: ${JSON.stringify(noAsiRes.json)}`);
}

// -- ASI after do...while --------------------------------------------------------

console.log("ASI after do...while...");
{
  const sameLine = runLoaderJson(
    "do {} while (false) console.log('bad');\n",
    ["--compat-asi", "--compat-while-loops"],
  );
  if (sameLine.exitCode !== 0)
    throw new Error(`do...while same-line continuation should apply ASI, got: ${JSON.stringify(sameLine.json)}`);
  if (normalizeLineEndings(sameLine.json.output) !== "bad\n")
    throw new Error(`Expected do...while same-line ASI output bad, got: ${sameLine.json.output}`);

  const newline = runLoaderJson(
    "do {} while (false)\nconsole.log('after');\n",
    ["--compat-asi", "--compat-while-loops"],
  );
  if (newline.exitCode !== 0)
    throw new Error(`do...while followed by newline should apply ASI, got: ${JSON.stringify(newline.json)}`);
  if (normalizeLineEndings(newline.json.output) !== "after\n")
    throw new Error(`Expected do...while newline ASI output after, got: ${newline.json.output}`);
}

// -- Unsupported/default-disabled syntax errors by default ----------------------

console.log("Unsupported/default-disabled syntax errors by default...");
{
  const cases = [
    {
      desc: "var declaration",
      source: 'var skipped = 1;\nconsole.log("after");\n',
      message: "'var' declarations are not supported",
      expectedWarningOutput: "after\n",
    },
    {
      desc: "function declaration",
      source: 'function skipped() { return 1; }\nconsole.log("after");\n',
      message: "'function' declarations are not supported",
      expectedWarningOutput: "after\n",
    },
    {
      desc: "loose equality",
      source: "console.log(1 == 1);\n",
      message: "'==' (loose equality) is not supported",
      expectedWarningOutput: "undefined\n",
    },
    {
      desc: "loose inequality",
      source: "console.log(1 != 1);\n",
      message: "'!=' (loose inequality) is not supported",
      expectedWarningOutput: "undefined\n",
    },
    {
      desc: "traditional for-loop",
      source: 'for (let i = 0; i < 1; i++) { console.log("skip"); }\nconsole.log("after");\n',
      message: "Traditional 'for(;;)' loops are not supported",
      expectedWarningOutput: "after\n",
    },
    {
      desc: "for-in loop",
      source: 'for (const key in { a: 1 }) { console.log(key); }\nconsole.log("after");\n',
      message: "'for...in' loops are not supported",
      expectedWarningOutput: "after\n",
    },
    {
      desc: "while loop",
      source: 'while (false) { console.log("skip"); }\nconsole.log("after");\n',
      message: "'while' loops are not supported",
      expectedWarningOutput: "after\n",
    },
    {
      desc: "do-while loop",
      source: 'do { console.log("skip"); } while (false);\nconsole.log("after");\n',
      message: "'do...while' loops are not supported",
      expectedWarningOutput: "after\n",
    },
    {
      desc: "with statement",
      source: 'with ({ value: 1 }) { console.log(value); }\nconsole.log("after");\n',
      message: "'with' statements require --compat-non-strict-mode",
      expectedWarningOutput: "after\n",
    },
    {
      desc: "label statement",
      source: 'label: console.log("skip");\nconsole.log("after");\n',
      message: "Labeled statements are not supported",
      expectedWarningOutput: "skip\nafter\n",
    },
  ];

  for (const { desc, source, message, expectedWarningOutput } of cases) {
    for (const modeArgs of [[] as string[], ["--mode=bytecode"]]) {
      const label = modeArgs.length ? `${desc} (bytecode)` : desc;
      const defaultRes = runLoaderJson(source, modeArgs);
      if (defaultRes.exitCode === 0)
        throw new Error(`${label}: unsupported syntax should fail by default`);
      if (defaultRes.json.ok !== false || defaultRes.json.error?.type !== "SyntaxError")
        throw new Error(`${label}: expected SyntaxError JSON, got ${JSON.stringify(defaultRes.json.error)}`);
      if (!String(defaultRes.json.error?.message ?? "").includes(message))
        throw new Error(`${label}: expected message containing ${message}, got ${defaultRes.json.error?.message}`);

      const warningRes = runLoaderJson(source, ["--warning-unsupported-features", ...modeArgs]);
      if (warningRes.exitCode !== 0)
        throw new Error(`${label}: --warning-unsupported-features should recover, got exit ${warningRes.exitCode}`);
      if (warningRes.json.ok !== true)
        throw new Error(`${label}: warning recovery should succeed, got ${JSON.stringify(warningRes.json.error)}`);
      if (normalizeLineEndings(warningRes.json.output) !== expectedWarningOutput)
        throw new Error(
          `${label}: expected warning recovery output ${JSON.stringify(expectedWarningOutput)}, got ${JSON.stringify(warningRes.json.output)}`,
        );
    }
  }

  const sourceBeforeDeclaration = [
    "var skipped = 1",
    "const after = 2;",
    "console.log(after);",
    "",
  ].join("\n");
  const warningAsiRes = runLoaderJson(sourceBeforeDeclaration, ["--warning-unsupported-features", "--compat-asi"]);
  if (warningAsiRes.exitCode !== 0)
    throw new Error(`warning recovery with ASI should preserve the following declaration`);
  if (normalizeLineEndings(warningAsiRes.json.output) !== "2\n")
    throw new Error(`Expected warning ASI recovery output 2, got: ${warningAsiRes.json.output}`);

  const compatVarAsiRes = runLoaderJson(sourceBeforeDeclaration, ["--compat-asi", "--compat-var"]);
  if (compatVarAsiRes.exitCode !== 0)
    throw new Error(`compat-var with ASI should parse var without an explicit semicolon`);
  if (compatVarAsiRes.json.ok !== true)
    throw new Error(`compat-var with ASI should succeed, got: ${JSON.stringify(compatVarAsiRes.json)}`);
  if (normalizeLineEndings(compatVarAsiRes.json.output) !== "2\n")
    throw new Error(`Expected compat-var ASI output 2, got: ${compatVarAsiRes.json.output}`);

  const compatVarNoAsiRes = runLoaderJson(sourceBeforeDeclaration, ["--compat-var"]);
  if (compatVarNoAsiRes.exitCode === 0)
    throw new Error(`compat-var without ASI should require a semicolon before the following declaration`);
  if (compatVarNoAsiRes.json.ok !== false)
    throw new Error(`compat-var without ASI should fail, got: ${JSON.stringify(compatVarNoAsiRes.json)}`);
  if (compatVarNoAsiRes.json.error?.type !== "SyntaxError")
    throw new Error(`Expected SyntaxError without ASI, got: ${compatVarNoAsiRes.json.error?.type}`);
}

// -- Unsupported syntax in imported modules follows the entry policy ------------

console.log("Unsupported syntax in imported modules follows the entry policy...");
{
  const tmp = mkdtemp("goccia-parser-mod-");
  try {
    const dep = join(tmp, "dep.js");
    const entry = join(tmp, "entry.js");
    writeFileSync(dep, 'var dep = 1;\nconsole.log("dep");\n');
    writeFileSync(entry, 'import "./dep.js";\nconsole.log("entry");\n');

    for (const modeArgs of [[] as string[], ["--mode=bytecode"]]) {
      const label = modeArgs.length ? "module dependency (bytecode)" : "module dependency";
      const defaultProc = Bun.spawnSync([LOADER, "--source-type=module", ...modeArgs, entry], {
        stdout: "pipe",
        stderr: "pipe",
      });
      const defaultOut = `${defaultProc.stdout.toString()}${defaultProc.stderr.toString()}`;
      if (defaultProc.exitCode === 0)
        throw new Error(`${label}: imported unsupported syntax should fail by default`);
      if (!defaultOut.includes("SyntaxError") || !defaultOut.includes("'var' declarations are not supported"))
        throw new Error(`${label}: expected imported SyntaxError, got: ${defaultOut}`);

      const warningProc = Bun.spawnSync(
        [LOADER, "--source-type=module", "--warning-unsupported-features", ...modeArgs, entry],
        { stdout: "pipe", stderr: "pipe" },
      );
      const warningOut = `${warningProc.stdout.toString()}${warningProc.stderr.toString()}`;
      if (warningProc.exitCode !== 0)
        throw new Error(`${label}: warning mode should recover imported unsupported syntax, got: ${warningOut}`);
      if (!warningOut.includes("dep") || !warningOut.includes("entry"))
        throw new Error(`${label}: warning mode should execute dependency and entry, got: ${warningOut}`);
    }
  } finally {
    clean(tmp);
  }
}

// -- Disabled-feature diagnostics with interpolated template literals -----------
// Regression: recovery/error handling for a disabled construct must skip a
// `${ ... }` substitution as part of its template literal. Previously the
// substitution's closing brace was miscounted as the structural brace that ends
// the skipped region, and the trailing backtick was then re-scanned as a fresh,
// unterminated template literal. Default mode now fails with the unsupported
// syntax error; warning mode keeps the historical recovery path.

console.log("Disabled-feature diagnostics with interpolated template literals...");
{
  const recoveryCases = [
    {
      desc: "for-loop, interpolated template in block body",
      compatFlag: "--compat-traditional-for-loop",
      source: [
        "const obj = {};",
        "for (let level = 0; level < 2; level++) {",
        "  obj[`u${level}`] = level;",
        "}",
        "console.log(JSON.stringify(obj));",
        "",
      ].join("\n"),
      expected: "{}\n",
    },
    {
      desc: "for-loop, interpolated template in non-block body",
      compatFlag: "--compat-traditional-for-loop",
      source: [
        "const obj = {};",
        "for (let i = 0; i < 2; i++) obj[`u${i}`] = i;",
        'console.log("after");',
        "",
      ].join("\n"),
      expected: "after\n",
    },
    {
      desc: "for-loop, interpolated template in loop header",
      compatFlag: "--compat-traditional-for-loop",
      source: [
        "let x = 1;",
        "for (let i = `s${0}`.length; i < 2; i++) { x = 99; }",
        "console.log(x);",
        "",
      ].join("\n"),
      expected: "1\n",
    },
    {
      desc: "while-loop, interpolated template in condition",
      compatFlag: "--compat-while-loops",
      source: [
        "let n = 0;",
        "while (`v${n}`.length > 99) { n = 99; }",
        'console.log("while-after");',
        "",
      ].join("\n"),
      expected: "while-after\n",
    },
    {
      desc: "do...while loop, interpolated template in body",
      compatFlag: "--compat-while-loops",
      source: [
        "let n = 0;",
        "do { const s = `d${n}`; } while (n > 99);",
        'console.log("do-after");',
        "",
      ].join("\n"),
      expected: "do-after\n",
    },
  ];

  for (const { desc, compatFlag, source, expected } of recoveryCases) {
    for (const args of [[] as string[], ["--mode=bytecode"]]) {
      const label = args.length ? `${desc} (bytecode)` : desc;
      const defaultRes = runLoaderJson(source, args);
      if (defaultRes.exitCode === 0)
        throw new Error(`${label}: disabled syntax should fail by default`);
      if (defaultRes.json.error?.type !== "SyntaxError")
        throw new Error(`${label}: expected SyntaxError, got ${JSON.stringify(defaultRes.json.error)}`);
      if (defaultRes.json.error?.message === "Unterminated template literal")
        throw new Error(`${label}: regressed to unterminated-template syntax error`);

      const warningRes = runLoaderJson(source, ["--warning-unsupported-features", ...args]);
      if (warningRes.json.ok !== true) {
        if (warningRes.json.error?.message === "Unterminated template literal")
          throw new Error(
            `${label}: regressed -- warning recovery surfaced "Unterminated template literal"`,
          );
        throw new Error(`${label}: warning mode should recover, got ok=${warningRes.json.ok} error=${JSON.stringify(warningRes.json.error)}`);
      }
      if (warningRes.exitCode !== 0) throw new Error(`${label}: warning mode should exit 0, got ${warningRes.exitCode}`);
      if (normalizeLineEndings(warningRes.json.output) !== expected)
        throw new Error(`${label}: expected output ${JSON.stringify(expected)}, got ${JSON.stringify(warningRes.json.output)}`);
    }

    const diag = Bun.spawnSync([LOADER], {
      stdin: new TextEncoder().encode(source),
      stdout: "pipe",
      stderr: "pipe",
    });
    const diagOut = `${diag.stdout.toString()}${diag.stderr.toString()}`;
    if (diag.exitCode === 0)
      throw new Error(`${desc}: default human diagnostic should fail`);
    if (!diagOut.includes(compatFlag))
      throw new Error(`${desc}: expected the diagnostic to reference ${compatFlag}, got: ${diagOut}`);
    if (diagOut.includes("Unterminated template literal"))
      throw new Error(`${desc}: diagnostic should not mention an unterminated template literal, got: ${diagOut}`);
  }
}

console.log("\nAll test-cli-parser.ts tests passed.");
