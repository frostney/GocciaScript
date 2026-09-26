import { afterAll, describe, expect, test } from "bun:test";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { resolveArchiveBinaryNames } from "../../scripts/lib/archive-binaries";

const root = mkdtempSync(join(tmpdir(), "archive-binaries-"));

afterAll(() => {
  rmSync(root, { recursive: true, force: true });
});

function archive(name: string, files: string[]): string {
  const dir = join(root, name);
  mkdirSync(dir, { recursive: true });
  for (const file of files) writeFileSync(join(dir, file), "");
  return dir;
}

describe("resolveArchiveBinaryNames", () => {
  test("finds GocciaRunner in a 0.14+ archive", () => {
    const dir = archive("runner", [
      "GocciaRunner",
      "GocciaTestRunner",
      "GocciaREPL",
    ]);
    expect(resolveArchiveBinaryNames(dir, false)).toEqual({
      loader: "GocciaRunner",
      testRunner: "GocciaTestRunner",
    });
  });

  test("falls back to GocciaScriptLoader in a 0.7-0.13 archive", () => {
    const dir = archive("script-loader", [
      "GocciaScriptLoader",
      "GocciaTestRunner",
    ]);
    expect(resolveArchiveBinaryNames(dir, false)).toEqual({
      loader: "GocciaScriptLoader",
      testRunner: "GocciaTestRunner",
    });
  });

  test("falls back to the unprefixed pre-0.7 names", () => {
    const dir = archive("legacy", ["ScriptLoader", "TestRunner"]);
    expect(resolveArchiveBinaryNames(dir, false)).toEqual({
      loader: "ScriptLoader",
      testRunner: "TestRunner",
    });
  });

  test("prefers GocciaRunner when an archive carries both runner names", () => {
    const dir = archive("both", [
      "GocciaScriptLoader",
      "GocciaRunner",
      "GocciaTestRunner",
    ]);
    expect(resolveArchiveBinaryNames(dir, false).loader).toBe("GocciaRunner");
  });

  test("probes the .exe names on Windows", () => {
    const dir = archive("windows", [
      "GocciaRunner.exe",
      "GocciaTestRunner.exe",
    ]);
    expect(resolveArchiveBinaryNames(dir, true)).toEqual({
      loader: "GocciaRunner.exe",
      testRunner: "GocciaTestRunner.exe",
    });
    expect(() => resolveArchiveBinaryNames(dir, false)).toThrow(
      /expected binary missing/,
    );
  });

  test("names every runner it looked for when none is present", () => {
    const dir = archive("no-runner", ["GocciaTestRunner"]);
    expect(() => resolveArchiveBinaryNames(dir, false)).toThrow(
      "GocciaRunner / GocciaScriptLoader / ScriptLoader",
    );
  });

  test("fails when the test runner is missing", () => {
    const dir = archive("no-test-runner", ["GocciaRunner"]);
    expect(() => resolveArchiveBinaryNames(dir, false)).toThrow(
      "GocciaTestRunner / TestRunner",
    );
  });
});
