#!/usr/bin/env bun
/** Raw UTF-8 output checks: decoding stdout first would hide malformed bytes. */
import { readFileSync, writeFileSync } from "fs";
import { join } from "path";
import { clean, mkdtemp } from "./test-cli/tmpdir";
import { RUNNER, TESTRUNNER } from "./test-cli/binaries";

export function verifyOutputEncoding(runner = RUNNER): void {
  const cases = [
    ["empty", '""', ""],
    ["NUL", '"a\\u0000b"', "a\0b"],
    ["ASCII", '"plain ?"', "plain ?"],
    ["valid pair", '"\\uD83D\\uDE00"', "😀"],
    ["lead", '"\\uD800"', "�"],
    ["trail", '"\\uDC00"', "�"],
    ["reversed", '"\\uDC00\\uD800"', "��"],
    ["mixed", '"a\\uD800b\\uDC00é\\uD83D\\uDE00"', "a�b�é😀"],
  ] as const;
  let failures = 0;
  for (const [label, expression, expected] of cases) {
    for (const [method, prefix] of [["log", ""], ["info", "Info: "], ["debug", "Debug: "], ["warn", "Warning: "], ["error", "Error: "]] as const) {
      const result = Bun.spawnSync([runner, "-"], {
        stdin: Buffer.from(`console.${method}(${expression});`),
        stdout: "pipe", stderr: "pipe",
      });
      const bytes = Buffer.from(prefix + expected + (process.platform === "win32" ? "\r\n" : "\n"));
      if (result.exitCode !== 0 || !result.stdout.subarray(0, result.stdout.indexOf(10) + 1).equals(bytes) || result.stderr.length !== 0) {
        console.error(`${method} ${label}: expected ${bytes.toString("hex")}, got ${result.stdout.toString("hex")} stderr=${result.stderr.toString("hex")}`);
        failures++;
      }
    }
    // Only the first line is script output; the runner adds its timing banner.
    // JSON capture crosses the RTL's string list before JSON escaping. A host
    // callback itself still receives UTF-16; these captured lines are UTF-8 text.
    const json = Bun.spawnSync([runner, "--output=json", "-"], {
      stdin: Buffer.from(`console.log(${expression});`), stdout: "pipe", stderr: "pipe",
    });
    const captured = JSON.parse(new TextDecoder("utf-8", { fatal: true }).decode(json.stdout)).output;
    if (json.exitCode !== 0 || captured.length !== 1 || captured[0] !== expected) {
      console.error(`JSON ${label}: ${JSON.stringify(captured)}`);
      failures++;
    }
    // The explicit log-file encoder is an already-passing control.
    const tmp = mkdtemp("goccia-output-encoding-");
    try {
      const path = join(tmp, "console.log");
      const logged = Bun.spawnSync([runner, `--log=${path}`, "-"], {
        stdin: Buffer.from(`console.log(${expression});`), stdout: "pipe", stderr: "pipe",
      });
      const expectedLog = Buffer.from(`[log] ${expected}` + (process.platform === "win32" ? "\r\n" : "\n"));
      if (logged.exitCode !== 0 || !readFileSync(path).equals(expectedLog)) {
        console.error(`log file ${label} differs`);
        failures++;
      }
    } finally { clean(tmp); }
    console.log(`checked ${label}`);
  }
  if (failures) throw new Error(`${failures} output encoding failures`);
  console.log("Output encoding checks passed.");
}

/** Test names and failed-test details cross the reporter's string lists. */
export function verifyReporterEncoding(binary = TESTRUNNER): void {
  const tmp = mkdtemp("goccia-reporter-encoding-");
  try {
    const path = join(tmp, "report.js");
    writeFileSync(path, 'test("name\\uD800\\uDC00\\uDC00\\uD800?", () => { expect(1).toBe(2); });');
    const result = Bun.spawnSync([binary, "-P", "--no-progress", path], { stdout: "pipe", stderr: "pipe" });
    if (result.exitCode !== 1 || !result.stdout.includes(Buffer.from("name𐀀��?")) ||
        result.stdout.includes(Buffer.from("name𐀀???")))
      throw new Error(`Reporter encoding differs: ${result.stdout.toString("hex")}`);
  } finally { clean(tmp); }
  console.log("Reporter encoding checks passed.");
}

if (import.meta.main) {
  verifyOutputEncoding(process.argv[2] ?? RUNNER);
  if (!process.argv[2]) verifyReporterEncoding();
}
