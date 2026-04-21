#!/usr/bin/env bun
/**
 * test-cli-parser.ts
 *
 * Parser-level CLI tests: error display (caret, suggestions, JSON fields),
 * numeric separator rejection.
 */

import { $ } from "bun";

const ext = process.platform === "win32" ? ".exe" : "";
const LOADER = `./build/GocciaScriptLoader${ext}`;

// -- Error display (SyntaxError with caret and suggestion) ----------------------

console.log("Error display (SyntaxError with caret and suggestion)...");
{
  const res = await $`echo 'const x = 1\nconst y = x +' | ${LOADER} 2>&1`.nothrow();
  const out = res.text();
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
  const json = JSON.parse(proc.stdout.toString());
  if (json.ok !== false) throw new Error(`JSON error ok should be false`);
  if (json.error?.type !== "SyntaxError") throw new Error(`Expected SyntaxError, got ${json.error?.type}`);
  if (json.error?.line === undefined) throw new Error(`JSON error should include line`);
  if (json.error?.column === undefined) throw new Error(`JSON error should include column`);
}

// -- Error display (bytecode mode) ----------------------------------------------

console.log("Error display (bytecode SyntaxError)...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json", "--mode=bytecode"], {
    stdin: new TextEncoder().encode("const x = ;\n"),
    stdout: "pipe",
    stderr: "pipe",
  });
  const json = JSON.parse(proc.stdout.toString());
  if (json.ok !== false) throw new Error(`Bytecode JSON error ok should be false`);
  if (json.error?.type !== "SyntaxError") throw new Error(`Expected SyntaxError in bytecode, got ${json.error?.type}`);
}

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
    const proc = Bun.spawnSync([LOADER, "--output=json"], {
      stdin: new TextEncoder().encode(`${literal};\n`),
      stdout: "pipe",
      stderr: "pipe",
    });
    const json = JSON.parse(proc.stdout.toString());
    if (json.ok !== false || json.error?.type !== "SyntaxError") {
      throw new Error(`Numeric separator "${literal}" (${desc}) should be SyntaxError, got ok=${json.ok} type=${json.error?.type}`);
    }
  }
}

console.log("\nAll test-cli-parser.ts tests passed.");
