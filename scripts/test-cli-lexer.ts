#!/usr/bin/env bun
/**
 * test-cli-lexer.ts
 *
 * Lexer-level CLI tests: lexical syntax errors and loader error envelopes.
 */

import { assertSyntaxError, runLoaderJson } from "./test-cli/assertions";

// -- Numeric separator rejection (9 cases) --------------------------------------

console.log("Numeric separator rejection...");
{
  const cases: [string, string][] = [
    ["1_000_", "trailing underscore"],
    ["0x_FF", "leading underscore after 0x"],
    ["0b_10", "leading underscore after 0b"],
    ["0o_77", "leading underscore after 0o"],
    ["1__000", "consecutive underscores"],
    ["0_1", "underscore after leading 0"],
    ["1._0", "underscore after decimal point"],
    ["1_.0", "underscore before decimal point"],
    ["1e_10", "underscore after exponent"],
  ];

  for (const [literal, desc] of cases) {
    assertSyntaxError(`${literal};\n`, `Numeric separator "${literal}" (${desc})`);
  }
}

// -- String literal line terminator rejection (LF, CR) ----------------------------

console.log("String line terminator rejection...");
{
  const cases: [string, string][] = [
    ['"hello\nworld"', "LF in double-quoted string"],
    ["'hello\nworld'", "LF in single-quoted string"],
    ['"hello\rworld"', "CR in double-quoted string"],
    ["'hello\rworld'", "CR in single-quoted string"],
    ['"hello\r\nworld"', "CRLF in double-quoted string"],
    ["'hello\r\nworld'", "CRLF in single-quoted string"],
  ];

  for (const [source, desc] of cases) {
    assertSyntaxError(source + ";\n", `String "${desc}"`);
  }
}

// -- LS/PS allowed in string literals (ES2019 proposal-json-superset) -------------

console.log("String allows LS/PS (ES2019)...");
{
  const ls = " ";
  const ps = " ";
  const cases: [string, string][] = [
    [`"hello${ls}world"`, "LS in double-quoted string"],
    [`'hello${ps}world'`, "PS in single-quoted string"],
  ];

  for (const [source, desc] of cases) {
    const { exitCode, stderr } = runLoaderJson(source + ";\n");
    if (exitCode !== 0) {
      throw new Error(`String "${desc}" should succeed, but failed: ${stderr.trim()}`);
    }
  }
}

// -- Line continuation still valid ------------------------------------------------

console.log("String line continuation...");
{
  const { exitCode, json } = runLoaderJson('"hello\\\nworld";\n', ["--print", "--compat-asi"]);
  if (exitCode !== 0) {
    throw new Error(`Line continuation should succeed, but failed`);
  }
  const result = json.files?.[0]?.result;
  if (result !== "helloworld") {
    throw new Error(`Line continuation should produce "helloworld", got ${JSON.stringify(result)}`);
  }
}

// -- Large padded braced Unicode escapes ----------------------------------------

console.log("String braced Unicode escapes with large padding...");
{
  const padding = "0".repeat(65536);
  const cases: [string, string][] = [
    [`"\\u{${padding}1234}".charCodeAt(0);\n`, "string literal"],
  ];

  for (const [source, desc] of cases) {
    const { exitCode, json, stderr } = runLoaderJson(source, ["--print"], { timeout: 10000 });
    if (exitCode !== 0) {
      throw new Error(`Large padded ${desc} escape should succeed, but failed: ${stderr.trim()}`);
    }
    const result = json.files?.[0]?.result;
    if (result !== 0x1234) {
      throw new Error(`Large padded ${desc} escape should produce 0x1234, got ${JSON.stringify(result)}`);
    }
  }
}

// -- Lexer errors surface as SyntaxError (#626) ---------------------------------

console.log("Lexer errors are SyntaxError...");
{
  const cases: [string, string][] = [
    ["'unterminated", "unterminated single-quoted string"],
    ['"unterminated', "unterminated double-quoted string"],
    ["`unterminated", "unterminated template literal"],
    ["'\\xZZ'", "invalid hex escape"],
    ["'\\u{ZZZZ}'", "invalid unicode escape"],
  ];

  for (const [source, desc] of cases) {
    assertSyntaxError(source + "\n", `Lexer error "${desc}"`);
  }
}

// -- Template literal lexer/parser errors surface as SyntaxError -----------------

console.log("Template literal errors are SyntaxError...");
{
  const cases: [string, string][] = [
    ["`head ${1", "missing close brace after interpolation expression"],
    ["`head ${1} tail", "unterminated template continuation"],
    ["`head ${} tail`", "empty interpolation expression"],
    ["`head ${/unterminated} tail`", "unterminated regex inside interpolation"],
    ["`head ${`inner ${1}`", "unterminated outer template after nested template"],
  ];

  for (const [source, desc] of cases) {
    assertSyntaxError(source + "\n", `Template literal error "${desc}"`);
  }
}

console.log("\nAll test-cli-lexer.ts tests passed.");
