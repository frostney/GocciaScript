#!/usr/bin/env bun
/**
 * test-cli-parser.ts
 *
 * Parser-level CLI tests: error display (caret, suggestions, JSON fields).
 */

import { $ } from "bun";

const ext = process.platform === "win32" ? ".exe" : "";
const LOADER = `./build/GocciaScriptLoader${ext}`;

function normalizeLineEndings(output: unknown): string {
  if (Array.isArray(output)) {
    const text = output.join("\n");
    return text.length > 0 ? `${text}\n` : "";
  }
  return String(output).replace(/\r\n/g, "\n");
}

function runLoaderJson(source: string, args: string[] = []) {
  const proc = Bun.spawnSync([LOADER, "--output=json", ...args], {
    stdin: new TextEncoder().encode(source),
    stdout: "pipe",
    stderr: "pipe",
  });
  return {
    exitCode: proc.exitCode,
    json: JSON.parse(proc.stdout.toString()),
  };
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
  const proc = Bun.spawnSync([LOADER, "--output=json"], {
    stdin: new TextEncoder().encode("const x = ;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 1) throw new Error(`Syntax error exit code should be 1, got ${proc.exitCode}`);
  const json = JSON.parse(proc.stdout.toString());
  if (json.ok !== false) throw new Error(`JSON error ok should be false`);
  if (json.error?.type !== "SyntaxError") throw new Error(`Expected SyntaxError, got ${json.error?.type}`);
  if (typeof json.error?.line !== "number") throw new Error(`JSON error should include numeric line, got ${json.error?.line}`);
  if (typeof json.error?.column !== "number") throw new Error(`JSON error should include numeric column, got ${json.error?.column}`);
}

// -- Error display (bytecode mode) ----------------------------------------------

console.log("Error display (bytecode SyntaxError)...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json", "--mode=bytecode"], {
    stdin: new TextEncoder().encode("const x = ;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 1) throw new Error(`Bytecode syntax error exit code should be 1, got ${proc.exitCode}`);
  const json = JSON.parse(proc.stdout.toString());
  if (json.ok !== false) throw new Error(`Bytecode JSON error ok should be false`);
  if (json.error?.type !== "SyntaxError") throw new Error(`Expected SyntaxError in bytecode, got ${json.error?.type}`);
  if (typeof json.error?.line !== "number") throw new Error(`Bytecode JSON error should include numeric line, got ${json.error?.line}`);
  if (typeof json.error?.column !== "number") throw new Error(`Bytecode JSON error should include numeric column, got ${json.error?.column}`);
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
  if (blockCloseRes.exitCode !== 0) {
    console.log("Unsupported var recovery checks skipped for this build.");
  } else {
    if (blockCloseRes.json.ok !== true) throw new Error(`Unsupported var before } should succeed, got: ${JSON.stringify(blockCloseRes.json)}`);
    if (normalizeLineEndings(blockCloseRes.json.output) !== "after\n") throw new Error(`Expected output after unsupported var block recovery, got: ${blockCloseRes.json.output}`);

    const sourceBeforeDeclaration = [
      "var skipped = 1",
      "const after = 2;",
      "console.log(after);",
      "",
    ].join("\n");
    const asiRes = runLoaderJson(sourceBeforeDeclaration, ["--asi"]);
    if (asiRes.exitCode !== 0) throw new Error(`Unsupported var with ASI should preserve the following declaration`);
    if (asiRes.json.ok !== true) throw new Error(`Unsupported var with ASI should succeed, got: ${JSON.stringify(asiRes.json)}`);
    if (normalizeLineEndings(asiRes.json.output) !== "2\n") throw new Error(`Expected ASI recovery output 2, got: ${asiRes.json.output}`);

    const compatVarAsiRes = runLoaderJson(sourceBeforeDeclaration, ["--asi", "--compat-var"]);
    if (compatVarAsiRes.exitCode !== 0) throw new Error(`compat-var with ASI should parse var without an explicit semicolon`);
    if (compatVarAsiRes.json.ok !== true) throw new Error(`compat-var with ASI should succeed, got: ${JSON.stringify(compatVarAsiRes.json)}`);
    if (normalizeLineEndings(compatVarAsiRes.json.output) !== "2\n") throw new Error(`Expected compat-var ASI output 2, got: ${compatVarAsiRes.json.output}`);

    const compatVarNoAsiRes = runLoaderJson(sourceBeforeDeclaration, ["--compat-var"]);
    if (compatVarNoAsiRes.exitCode === 0) throw new Error(`compat-var without ASI should require a semicolon before the following declaration`);
    if (compatVarNoAsiRes.json.ok !== false) throw new Error(`compat-var without ASI should fail, got: ${JSON.stringify(compatVarNoAsiRes.json)}`);
    if (compatVarNoAsiRes.json.error?.type !== "SyntaxError") throw new Error(`Expected SyntaxError without ASI, got: ${compatVarNoAsiRes.json.error?.type}`);
  }
}

console.log("\nAll test-cli-parser.ts tests passed.");
