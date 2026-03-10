#!/usr/bin/env node

import { mkdtempSync, readFileSync, readdirSync, rmSync, statSync } from "node:fs";
import { join, basename, extname, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";

const COLOR_GREEN = "\u001b[32m";
const COLOR_RED = "\u001b[31m";
const COLOR_YELLOW = "\u001b[33m";
const COLOR_RESET = "\u001b[0m";
const COLOR_BOLD = "\u001b[1m";

function fileExists(path) {
  try {
    return statSync(path).isFile();
  } catch {
    return false;
  }
}

function resolveScriptLoader() {
  const candidates = [
    resolve("build/ScriptLoader"),
    resolve("build/ScriptLoader.exe"),
  ];

  for (const candidate of candidates) {
    if (fileExists(candidate)) {
      return candidate;
    }
  }

  process.stderr.write(
    `${COLOR_RED}Error: ScriptLoader not found. Run ./build.pas loader first.${COLOR_RESET}\n`,
  );
  process.exit(1);
}

function resolveHostScript() {
  const hostScriptPath = resolve("tests-wasm/souffle-host.mjs");
  if (!fileExists(hostScriptPath)) {
    process.stderr.write(
      `${COLOR_RED}Error: tests-wasm/souffle-host.mjs not found.${COLOR_RESET}\n`,
    );
    process.exit(1);
  }

  return hostScriptPath;
}

function parseExpectedLines(fileName) {
  const expectedPrefix = "// Expected: ";
  const source = readFileSync(fileName, "utf8");
  return source
    .split(/\r?\n/)
    .filter((line) => line.startsWith(expectedPrefix))
    .map((line) => line.slice(expectedPrefix.length));
}

function isSkipped(fileName) {
  const source = readFileSync(fileName, "utf8");

  for (const rawLine of source.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (line.startsWith("// Skip:")) {
      return true;
    }
    if (line !== "" && !line.startsWith("//")) {
      break;
    }
  }

  return false;
}

function runProcess(executable, args) {
  const result = spawnSync(executable, args, {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  });

  return {
    ok: !result.error,
    stdout: result.stdout ?? "",
    stderr: result.stderr ?? (result.error ? result.error.message : ""),
    exitCode: result.status ?? -1,
  };
}

function listFixtures(fixtureDir) {
  return readdirSync(fixtureDir)
    .filter((name) => extname(name) === ".js")
    .map((name) => join(fixtureDir, name))
    .sort((left, right) => left.localeCompare(right));
}

function normalizeOutputLines(output) {
  const lines = output.split(/\r?\n/);
  while (lines.length > 0 && lines[lines.length - 1] === "") {
    lines.pop();
  }
  return lines;
}

function printIndented(text) {
  if (text.trim() === "") {
    return;
  }

  for (const line of text.trimEnd().split(/\r?\n/)) {
    process.stdout.write(`       ${line}\n`);
  }
}

const scriptLoaderPath = resolveScriptLoader();
const hostScriptPath = resolveHostScript();
const tempDir = mkdtempSync(join(tmpdir(), "wasm-test-"));

let passed = 0;
let failed = 0;
let skipped = 0;

function runFixture(fixtureFile) {
  const baseName = basename(fixtureFile);

  if (isSkipped(fixtureFile)) {
    process.stdout.write(`${COLOR_YELLOW}  SKIP ${COLOR_RESET}${baseName}\n`);
    skipped += 1;
    return;
  }

  const wasmFile = join(tempDir, `${basename(fixtureFile, ".js")}.wasm`);

  const compileResult = runProcess(scriptLoaderPath, [
    fixtureFile,
    "--emit=wasm",
    `--output=${wasmFile}`,
  ]);

  if (!compileResult.ok || compileResult.exitCode !== 0) {
    process.stdout.write(`${COLOR_RED}  FAIL ${COLOR_RESET}${baseName} (compile error)\n`);
    printIndented(compileResult.stderr || compileResult.stdout);
    failed += 1;
    return;
  }

  const executeResult = runProcess("node", [hostScriptPath, wasmFile]);
  if (!executeResult.ok) {
    process.stdout.write(`${COLOR_RED}  FAIL ${COLOR_RESET}${baseName} (node error)\n`);
    printIndented(executeResult.stderr);
    failed += 1;
    return;
  }

  if (executeResult.exitCode !== 0) {
    process.stdout.write(
      `${COLOR_RED}  FAIL ${COLOR_RESET}${baseName} (exit code ${executeResult.exitCode})\n`,
    );
    printIndented(executeResult.stderr);
    failed += 1;
    return;
  }

  const expected = parseExpectedLines(fixtureFile);
  const actual = normalizeOutputLines(executeResult.stdout);
  const matches =
    actual.length === expected.length &&
    actual.every((line, index) => line === expected[index]);

  if (matches) {
    process.stdout.write(`${COLOR_GREEN}  PASS ${COLOR_RESET}${baseName}\n`);
    passed += 1;
    return;
  }

  process.stdout.write(`${COLOR_RED}  FAIL ${COLOR_RESET}${baseName} (output mismatch)\n`);
  for (const line of expected) {
    process.stdout.write(`       expected: ${line}\n`);
  }
  for (const line of actual) {
    process.stdout.write(`       actual:   ${line}\n`);
  }
  failed += 1;
}

try {
  process.stdout.write(`${COLOR_BOLD}WASM Integration Tests${COLOR_RESET}\n\n`);

  const fixtures =
    process.argv.length > 2 ? process.argv.slice(2).map((path) => resolve(path)) : listFixtures("tests-wasm");

  for (const fixture of fixtures) {
    runFixture(fixture);
  }

  process.stdout.write(`\n${COLOR_BOLD}Results: ${COLOR_RESET}`);
  process.stdout.write(`${COLOR_GREEN}${passed} passed${COLOR_RESET}`);
  if (failed > 0) {
    process.stdout.write(`, ${COLOR_RED}${failed} failed${COLOR_RESET}`);
  }
  if (skipped > 0) {
    process.stdout.write(`, ${COLOR_YELLOW}${skipped} skipped${COLOR_RESET}`);
  }
  process.stdout.write("\n");

  if (failed > 0) {
    process.exitCode = 1;
  }
} finally {
  rmSync(tempDir, { recursive: true, force: true });
}
