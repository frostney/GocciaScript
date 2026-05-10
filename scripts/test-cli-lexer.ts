#!/usr/bin/env bun
/**
 * test-cli-lexer.ts
 *
 * Lexer-level CLI tests: numeric separator rejection.
 */

import { LOADER } from "./test-cli/binaries";

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
    if (proc.exitCode === 0) {
      throw new Error(`Numeric separator "${literal}" (${desc}) should fail, but exited 0`);
    }
    const json = JSON.parse(proc.stdout.toString());
    if (json.ok !== false || json.error?.type !== "SyntaxError") {
      throw new Error(`Numeric separator "${literal}" (${desc}) should be SyntaxError, got ok=${json.ok} type=${json.error?.type}`);
    }
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
  ];

  for (const [source, desc] of cases) {
    const proc = Bun.spawnSync([LOADER, "--output=json"], {
      stdin: new TextEncoder().encode(source + ";\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode === 0) {
      throw new Error(`String "${desc}" should fail, but exited 0`);
    }
    const json = JSON.parse(proc.stdout.toString());
    if (json.ok !== false || json.error?.type !== "SyntaxError") {
      throw new Error(`String "${desc}" should be SyntaxError, got ok=${json.ok} type=${json.error?.type}`);
    }
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
    const proc = Bun.spawnSync([LOADER, "--output=json"], {
      stdin: new TextEncoder().encode(source + ";\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (proc.exitCode !== 0) {
      const stderr = proc.stderr.toString().trim();
      throw new Error(`String "${desc}" should succeed, but failed: ${stderr}`);
    }
  }
}

// -- Line continuation still valid ------------------------------------------------

console.log("String line continuation...");
{
  const proc = Bun.spawnSync([LOADER, "--output=json", "--print", "--asi"], {
    stdin: new TextEncoder().encode('"hello\\\nworld";\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  if (proc.exitCode !== 0) {
    throw new Error(`Line continuation should succeed, but failed`);
  }
  const json = JSON.parse(proc.stdout.toString());
  const result = json.files?.[0]?.result;
  if (result !== "helloworld") {
    throw new Error(`Line continuation should produce "helloworld", got ${JSON.stringify(result)}`);
  }
}

console.log("\nAll test-cli-lexer.ts tests passed.");
