#!/usr/bin/env bun
/** Human-readable test summaries must not coerce realm-less report arrays. */
import { writeFileSync } from "fs";
import { join, resolve } from "path";
import { TESTRUNNER } from "./test-cli/binaries";
import { containsLine, normalizeLineEndings } from "./test-cli/assertions";
import { clean, mkdtemp } from "./test-cli/tmpdir";

export function verifyTestSummary(binary = TESTRUNNER): void {
  const tmp = mkdtemp("goccia-test-summary-");
  try {
    const first = join(tmp, "first.js");
    const second = join(tmp, "second.js");
    for (const jobs of [1, 2]) {
      for (const failing of [false, true]) {
        writeFileSync(first, `test("first result", () => { expect(1).toBe(${failing ? 2 : 1}); });\n`);
        writeFileSync(second, `test("second result", () => { expect(2).toBe(${failing ? 3 : 2}); });\n`);
        const proc = Bun.spawnSync([resolve(binary), "-P", "--no-progress", `--jobs=${jobs}`, first, second], {
          stdout: "pipe", stderr: "pipe",
        });
        const output = normalizeLineEndings(proc.stdout.toString());
        const summary = failing ? 'Test "first result": Expected 1 to be 2,Test "second result": Expected 2 to be 3' : "";
        if (proc.exitCode !== (failing ? 1 : 0) ||
            !containsLine(output, `Test Results Failed Tests: ${summary}`) ||
            !containsLine(output, `Test Results Failed: ${failing ? "2 (100.00%)" : "0 (0.00%)"}`) ||
            !containsLine(output, `Test Results Passed: ${failing ? "0 (0.00%)" : "2 (100.00%)"}`)) {
          throw new Error(`Summary mismatch (jobs=${jobs}, failing=${failing}, exit=${proc.exitCode}):\n${output}${proc.stderr.toString()}`);
        }
      }
    }
    const helper = join(tmp, "helper.js");
    writeFileSync(helper, 'export const match = value => expect(value).toMatchInlineSnapshot();\n');
    writeFileSync(first, 'import { match } from "./helper.js"; test("left", () => match("left"));\n');
    writeFileSync(second, 'import { match } from "./helper.js"; test("right", () => match("right"));\n');
    for (const attributed of [false, true]) {
      const paths = attributed ? [first, second, helper] : [first, second];
      const proc = Bun.spawnSync([resolve(binary), "-P", "--no-progress", "--update-snapshots", "--jobs=2", ...paths], {
        stdout: "pipe", stderr: "pipe",
      });
      const output = normalizeLineEndings(proc.stdout.toString());
      const summary = output.split("\n").find(line => line.startsWith("Test Results Failed Tests: ")) ?? "";
      if (proc.exitCode !== 1 ||
          summary.split("Conflicting inline snapshots").length !== 2 ||
          !summary.includes("helper.js")) {
        throw new Error(`Finalization summary mismatch (attributed=${attributed}):\n${output}${proc.stderr.toString()}`);
      }
    }
  } finally {
    clean(tmp);
  }
}

if (import.meta.main) {
  verifyTestSummary(process.argv[2] ?? TESTRUNNER);
  console.log("Test summary checks passed");
}
