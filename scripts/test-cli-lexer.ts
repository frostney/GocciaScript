#!/usr/bin/env bun
/**
 * test-cli-lexer.ts
 *
 * Lexer-level CLI tests: numeric separator rejection.
 */

const ext = process.platform === "win32" ? ".exe" : "";
const LOADER = `./build/GocciaScriptLoader${ext}`;

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

console.log("\nAll test-cli-lexer.ts tests passed.");
