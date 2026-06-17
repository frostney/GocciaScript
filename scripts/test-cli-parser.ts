#!/usr/bin/env bun
/**
 * test-cli-parser.ts
 *
 * Parser-level CLI tests: error display (caret, suggestions, JSON fields).
 */

import { $ } from "bun";
import { LOADER } from "./test-cli/binaries";
import { assertSyntaxError, normalizeLineEndings, runLoaderJson } from "./test-cli/assertions";

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

// -- Unsupported var recovery (ASI and compat-var flags) ------------------------

console.log("Unsupported var recovery (ASI and compat-var flags)...");
{
  const sourceBeforeBlockClose = [
    "if (true) {",
    "  var skipped = 1",
    "}",
    'console.log("after");',
    "",
  ].join("\n");
  const blockCloseRes = runLoaderJson(sourceBeforeBlockClose);
  if (blockCloseRes.exitCode !== 0) throw new Error(`Unsupported var before } should not consume the block close`);
  if (blockCloseRes.json.ok !== true) throw new Error(`Unsupported var before } should succeed, got: ${JSON.stringify(blockCloseRes.json)}`);
  if (normalizeLineEndings(blockCloseRes.json.output) !== "after\n") throw new Error(`Expected output after unsupported var block recovery, got: ${blockCloseRes.json.output}`);

  const sourceBeforeDeclaration = [
    "var skipped = 1",
    "const after = 2;",
    "console.log(after);",
    "",
  ].join("\n");
  const asiRes = runLoaderJson(sourceBeforeDeclaration, ["--compat-asi"]);
  if (asiRes.exitCode !== 0) throw new Error(`Unsupported var with ASI should preserve the following declaration`);
  if (asiRes.json.ok !== true) throw new Error(`Unsupported var with ASI should succeed, got: ${JSON.stringify(asiRes.json)}`);
  if (normalizeLineEndings(asiRes.json.output) !== "2\n") throw new Error(`Expected ASI recovery output 2, got: ${asiRes.json.output}`);

  const compatVarAsiRes = runLoaderJson(sourceBeforeDeclaration, ["--compat-asi", "--compat-var"]);
  if (compatVarAsiRes.exitCode !== 0) throw new Error(`compat-var with ASI should parse var without an explicit semicolon`);
  if (compatVarAsiRes.json.ok !== true) throw new Error(`compat-var with ASI should succeed, got: ${JSON.stringify(compatVarAsiRes.json)}`);
  if (normalizeLineEndings(compatVarAsiRes.json.output) !== "2\n") throw new Error(`Expected compat-var ASI output 2, got: ${compatVarAsiRes.json.output}`);

  const compatVarNoAsiRes = runLoaderJson(sourceBeforeDeclaration, ["--compat-var"]);
  if (compatVarNoAsiRes.exitCode === 0) throw new Error(`compat-var without ASI should require a semicolon before the following declaration`);
  if (compatVarNoAsiRes.json.ok !== false) throw new Error(`compat-var without ASI should fail, got: ${JSON.stringify(compatVarNoAsiRes.json)}`);
  if (compatVarNoAsiRes.json.error?.type !== "SyntaxError") throw new Error(`Expected SyntaxError without ASI, got: ${compatVarNoAsiRes.json.error?.type}`);
}

console.log("\nAll test-cli-parser.ts tests passed.");
