#!/usr/bin/env bun
/**
 * test-cli-parser.ts
 *
 * Parser-level CLI tests: error display (caret, suggestions, JSON fields).
 */

import { $ } from "bun";

const ext = process.platform === "win32" ? ".exe" : "";
const LOADER = `./build/GocciaScriptLoader${ext}`;

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

console.log("\nAll test-cli-parser.ts tests passed.");
