#!/usr/bin/env bun
/**
 * gc-stress.ts
 *
 * Runs the GC-stress subset twice, under the GC-stress build
 * (`./build.pas --gc-stress`) and under an ordinary development build, and
 * reports the tests that fail only under stress. Tests that fail in both
 * builds are known failures and do not count, so the baseline comes from the
 * same commit and the same run rather than from a checked-in list.
 *
 * The subset is the whole `tests/` tree and the test262 directories where
 * values cross native code while guest code runs: promises, iterators, async
 * functions, generators, eval code and arguments objects. A `tests/` case
 * whose cost grows with its size times the live heap shrinks or skips its
 * large size when `Goccia.build.gcStress` is true.
 *
 * Each `tests/` file runs in a process of its own: an engine-integrity fault
 * stops a GocciaTestRunner run (exit 70), and one faulting file must not hide
 * the rest. GocciaTest262Runner already isolates every test in a worker
 * process.
 *
 * Usage:
 *   bun scripts/gc-stress.ts --suite-dir=<test262 checkout> [options]
 *
 * Options:
 *   --stress-dir=DIR      GC-stress binaries (default build/gc-stress)
 *   --baseline-dir=DIR    ordinary development binaries (default build)
 *   --out-dir=DIR         logs, results and report (default build/gc-stress-report)
 *   --jobs=N              concurrent tests (default: CPU count)
 *   --test262-timeout=D   per-test262-test timeout (default 20m; the slowest test
 *                         takes about 3 minutes under stress on one fast core)
 *   --file-timeout=SECS   wall-clock cap per tests/ file process (default 1800)
 *   --suites=LIST         test262,tests (default both)
 *   --filter=GLOB         test262 --filter, for a quick local run
 *
 * Writes <out-dir>/summary.json and <out-dir>/report.md. Exits 0 when no test
 * fails only under stress, 1 when one does, and 2 when a run could not be
 * completed or read.
 */

import { existsSync, mkdirSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from "node:fs";
import { availableParallelism } from "node:os";
import { join, resolve } from "node:path";

// The test262 directories (under test/) the stress run covers.
export const TEST262_CATEGORIES = [
  "built-ins/Promise",
  "built-ins/Iterator",
  "language/expressions/await",
  "language/expressions/async-function",
  "language/expressions/async-arrow-function",
  "language/statements/async-function",
  "language/eval-code",
  "language/arguments-object",
  "language/expressions/generators",
  "language/statements/generators",
];

// The tests/ paths the stress run covers: the whole tree (#1679).
export const TEST_PATHS = ["tests"];

// Mirrors ScriptExtensions (source/units/Goccia.FileExtensions.pas) and
// TEST_DISCOVERY_EXCLUDED_DIRECTORIES (source/app/GocciaTestRunner.dpr), so a
// file runs here exactly when a GocciaTestRunner directory run would pick it up.
const SCRIPT_EXTENSIONS = [".js", ".jsx", ".ts", ".tsx", ".mjs", ".mts"];
const EXCLUDED_DIRECTORIES = new Set(["node_modules"]);

// The longest the report prints of one failure message, and the most failures
// it lists: a GitHub issue body holds at most 65,536 characters.
const MESSAGE_LIMIT = 300;
const REPORT_LIST_LIMIT = 100;

export type Outcome = "PASS" | "FAIL" | "TIMEOUT" | "WRAPPER_INFRA" | "FAULT" | "CRASH" | "MISSING";

export interface TestOutcome {
  status: Outcome;
  message: string;
}

export interface StressOnlyFailure {
  suite: "test262" | "tests";
  id: string;
  baseline: Outcome;
  stress: Outcome;
  message: string;
}

interface SuiteResult {
  suite: "test262" | "tests";
  baseline: Map<string, TestOutcome>;
  stress: Map<string, TestOutcome>;
  baselineSeconds: number;
  stressSeconds: number;
}

interface Options {
  suiteDir: string;
  stressDir: string;
  baselineDir: string;
  outDir: string;
  jobs: number;
  test262Timeout: string;
  fileTimeoutSeconds: number;
  suites: Set<string>;
  filter: string;
}

class InfrastructureError extends Error {}

function parseOptions(argv: string[]): Options {
  const options: Options = {
    suiteDir: "",
    stressDir: "build/gc-stress",
    baselineDir: "build",
    outDir: "build/gc-stress-report",
    jobs: availableParallelism(),
    test262Timeout: "20m",
    fileTimeoutSeconds: 1800,
    suites: new Set(["test262", "tests"]),
    filter: "",
  };
  for (const argument of argv) {
    const match = /^--([a-z0-9-]+)=(.*)$/.exec(argument);
    if (!match) throw new InfrastructureError(`unknown argument: ${argument}`);
    const [, name, value] = match;
    switch (name) {
      case "suite-dir": options.suiteDir = value; break;
      case "stress-dir": options.stressDir = value; break;
      case "baseline-dir": options.baselineDir = value; break;
      case "out-dir": options.outDir = value; break;
      case "jobs": options.jobs = positiveInteger(name, value); break;
      case "test262-timeout": options.test262Timeout = value; break;
      case "file-timeout": options.fileTimeoutSeconds = positiveInteger(name, value); break;
      case "suites": options.suites = new Set(value.split(",").filter(Boolean)); break;
      case "filter": options.filter = value; break;
      default: throw new InfrastructureError(`unknown option: --${name}`);
    }
  }
  for (const suite of options.suites) {
    if (suite !== "test262" && suite !== "tests") {
      throw new InfrastructureError(`unknown suite: ${suite}`);
    }
  }
  if (options.suites.has("test262") && !options.suiteDir) {
    throw new InfrastructureError("--suite-dir is required for the test262 suite");
  }
  return options;
}

function positiveInteger(name: string, value: string): number {
  const parsed = Number(value);
  if (!Number.isInteger(parsed) || parsed < 1) {
    throw new InfrastructureError(`--${name} must be a positive integer, got: ${value}`);
  }
  return parsed;
}

function requireBinary(directory: string, name: string): string {
  const path = resolve(directory, name);
  if (!existsSync(path)) throw new InfrastructureError(`missing binary: ${path}`);
  return path;
}

function truncate(text: string, limit: number): string {
  const flat = text.replace(/\s+/g, " ").trim();
  return flat.length > limit ? `${flat.slice(0, limit - 1)}…` : flat;
}

// ---------------------------------------------------------------------------
// test262
// ---------------------------------------------------------------------------

async function runTest262(
  options: Options,
  build: "baseline" | "stress",
): Promise<Map<string, TestOutcome>> {
  const directory = build === "stress" ? options.stressDir : options.baselineDir;
  const runner = requireBinary(directory, "GocciaTest262Runner");
  const output = resolve(options.outDir, `test262-${build}.json`);
  const log = resolve(options.outDir, `test262-${build}.log`);
  const args = [
    runner,
    `--suite-dir=${options.suiteDir}`,
    `--categories=${TEST262_CATEGORIES.join(",")}`,
    `--jobs=${options.jobs}`,
    `--timeout=${options.test262Timeout}`,
    `--output=${output}`,
  ];
  if (options.filter) args.push(`--filter=${options.filter}`);
  // A report left in --out-dir by an earlier run must not stand in for this
  // run's when the runner exits before writing one.
  rmSync(output, { force: true });
  const child = Bun.spawn(args, { stdout: Bun.file(log), stderr: "pipe" });
  const stderr = await new Response(child.stderr).text();
  const exitCode = await child.exited;
  writeFileSync(`${log}.stderr`, stderr);
  if (!existsSync(output)) {
    throw new InfrastructureError(
      `test262 ${build} run wrote no report (exit ${exitCode}): ${truncate(stderr, MESSAGE_LIMIT)}`,
    );
  }
  const report = JSON.parse(readFileSync(output, "utf8")) as {
    results: { id: string; status: Outcome; message: string }[];
  };
  if (!Array.isArray(report.results) || report.results.length === 0) {
    throw new InfrastructureError(`test262 ${build} report has no results: ${output}`);
  }
  const outcomes = new Map<string, TestOutcome>();
  for (const result of report.results) {
    outcomes.set(result.id, { status: result.status, message: result.message ?? "" });
  }
  return outcomes;
}

// ---------------------------------------------------------------------------
// tests/
// ---------------------------------------------------------------------------

export function discoverTestFiles(paths: string[]): string[] {
  const files: string[] = [];
  const walk = (path: string): void => {
    const stats = statSync(path);
    if (stats.isFile()) {
      if (SCRIPT_EXTENSIONS.some((extension) => path.endsWith(extension))) files.push(path);
      return;
    }
    for (const entry of readdirSync(path)) {
      if (EXCLUDED_DIRECTORIES.has(entry)) continue;
      walk(join(path, entry));
    }
  };
  for (const path of paths) {
    if (!existsSync(path)) throw new InfrastructureError(`missing test path: ${path}`);
    walk(path);
  }
  return files.sort();
}

// Last lines of a file run's output: the failing assertions, or the fault
// and its backtrace.
function tailOf(text: string): string {
  return text.split("\n").filter((line) => line.trim() !== "").slice(-6).join(" | ");
}

async function runTestFile(
  runner: string,
  file: string,
  fileTimeoutSeconds: number,
): Promise<TestOutcome> {
  // --timeout=0: the stress build is slow, and a file that takes a minute
  // under stress is not a failure. The wall-clock cap below bounds real hangs.
  const child = Bun.spawn(
    [runner, "-P", file, "--timeout=0", "--jobs=1", "--no-progress"],
    { stdout: "pipe", stderr: "pipe" },
  );
  let timedOut = false;
  const timer = setTimeout(() => {
    timedOut = true;
    child.kill("SIGKILL");
  }, fileTimeoutSeconds * 1000);
  const [stdout, stderr] = await Promise.all([
    new Response(child.stdout).text(),
    new Response(child.stderr).text(),
  ]);
  const exitCode = await child.exited;
  clearTimeout(timer);
  if (timedOut) {
    return { status: "TIMEOUT", message: `no result after ${fileTimeoutSeconds}s` };
  }
  if (exitCode === 0) return { status: "PASS", message: "" };
  const output = `${stdout}\n${stderr}`;
  if (exitCode === 1) {
    const failed = /Test Results Failed: (\d+)/.exec(stdout);
    return {
      status: "FAIL",
      message: failed ? `${failed[1]} failed: ${tailOf(stderr || stdout)}` : tailOf(output),
    };
  }
  if (exitCode === 70) return { status: "FAULT", message: tailOf(stderr) };
  const signal = child.signalCode ? ` (${child.signalCode})` : "";
  return { status: "CRASH", message: `exit ${exitCode}${signal}: ${tailOf(output)}` };
}

async function runTests(
  options: Options,
  build: "baseline" | "stress",
  files: string[],
): Promise<Map<string, TestOutcome>> {
  const directory = build === "stress" ? options.stressDir : options.baselineDir;
  const runner = requireBinary(directory, "GocciaTestRunner");
  const outcomes = new Map<string, TestOutcome>();
  let next = 0;
  const worker = async (): Promise<void> => {
    while (next < files.length) {
      const file = files[next++];
      outcomes.set(file, await runTestFile(runner, file, options.fileTimeoutSeconds));
    }
  };
  await Promise.all(Array.from({ length: Math.min(options.jobs, files.length) }, worker));
  const lines = files.map((file) => {
    const outcome = outcomes.get(file)!;
    return `${outcome.status} ${file}${outcome.message ? ` -- ${outcome.message}` : ""}`;
  });
  writeFileSync(resolve(options.outDir, `tests-${build}.log`), `${lines.join("\n")}\n`);
  return outcomes;
}

// ---------------------------------------------------------------------------
// Comparison and report
// ---------------------------------------------------------------------------

// A test fails only under stress when it passes in the ordinary build and
// does anything else, or nothing at all, under stress.
export function stressOnlyFailures(
  suite: "test262" | "tests",
  baseline: Map<string, TestOutcome>,
  stress: Map<string, TestOutcome>,
): StressOnlyFailure[] {
  const failures: StressOnlyFailure[] = [];
  for (const [id, outcome] of baseline) {
    if (outcome.status !== "PASS") continue;
    const stressed = stress.get(id) ?? { status: "MISSING" as Outcome, message: "no result under stress" };
    if (stressed.status === "PASS") continue;
    failures.push({ suite, id, baseline: outcome.status, stress: stressed.status, message: stressed.message });
  }
  return failures.sort((a, b) => a.id.localeCompare(b.id));
}

function countPassed(outcomes: Map<string, TestOutcome>): number {
  let passed = 0;
  for (const outcome of outcomes.values()) if (outcome.status === "PASS") passed++;
  return passed;
}

function runUrl(): string {
  const { GITHUB_SERVER_URL, GITHUB_REPOSITORY, GITHUB_RUN_ID } = Bun.env;
  if (!GITHUB_SERVER_URL || !GITHUB_REPOSITORY || !GITHUB_RUN_ID) return "";
  return `${GITHUB_SERVER_URL}/${GITHUB_REPOSITORY}/actions/runs/${GITHUB_RUN_ID}`;
}

export function renderReport(
  results: SuiteResult[],
  failures: StressOnlyFailure[],
  context: { runUrl: string; commit: string; test262Timeout: string },
): string {
  const lines: string[] = [];
  lines.push(
    failures.length === 0
      ? "No test fails only under the GC-stress build."
      : `${failures.length} test${failures.length === 1 ? "" : "s"} pass in an ordinary build but fail under the GC-stress build.`,
  );
  lines.push("");
  const where = [
    context.runUrl ? `Run: ${context.runUrl}` : "",
    context.commit ? `Commit: \`${context.commit}\`` : "",
  ].filter(Boolean);
  if (where.length > 0) {
    lines.push(where.join(" · "));
    lines.push("");
  }
  lines.push("| Suite | Tests | Pass (ordinary) | Pass (stress) | Stress-only failures | Wall time (ordinary / stress) |");
  lines.push("|---|---|---|---|---|---|");
  for (const result of results) {
    const count = failures.filter((failure) => failure.suite === result.suite).length;
    lines.push(
      `| ${result.suite} | ${result.baseline.size} | ${countPassed(result.baseline)} | ${countPassed(result.stress)} | ${count} | ${result.baselineSeconds}s / ${result.stressSeconds}s |`,
    );
  }
  if (failures.length > 0) {
    lines.push("");
    lines.push("| Suite | Test | Stress outcome | Message |");
    lines.push("|---|---|---|---|");
    for (const failure of failures.slice(0, REPORT_LIST_LIMIT)) {
      const message = truncate(failure.message, MESSAGE_LIMIT).replace(/\|/g, "\\|");
      lines.push(`| ${failure.suite} | \`${failure.id}\` | ${failure.stress} | ${message} |`);
    }
    if (failures.length > REPORT_LIST_LIMIT) {
      lines.push("");
      lines.push(`${failures.length - REPORT_LIST_LIMIT} more are in the run's \`gc-stress-report\` artifact.`);
    }
    lines.push("");
    lines.push("Reproduce one locally:");
    lines.push("");
    lines.push("```bash");
    lines.push("./build.pas --gc-stress testrunner test262runner");
    lines.push("./build/gc-stress/GocciaTestRunner -P <tests/ file> --timeout=0");
    lines.push(
      `./build/gc-stress/GocciaTest262Runner --suite-dir=<test262 checkout> --filter=<test262 id> --timeout=${context.test262Timeout}`,
    );
    lines.push("```");
  }
  return `${lines.join("\n")}\n`;
}

async function main(): Promise<number> {
  const options = parseOptions(Bun.argv.slice(2));
  mkdirSync(options.outDir, { recursive: true });
  if (options.suites.has("test262")) {
    for (const category of TEST262_CATEGORIES) {
      if (!existsSync(join(options.suiteDir, "test", category))) {
        throw new InfrastructureError(`test262 category not found: ${join(options.suiteDir, "test", category)}`);
      }
    }
  }

  const results: SuiteResult[] = [];
  const timed = async <T>(work: () => Promise<T>): Promise<[T, number]> => {
    const start = performance.now();
    const value = await work();
    return [value, Math.round((performance.now() - start) / 1000)];
  };

  if (options.suites.has("test262")) {
    console.log(`test262: ${TEST262_CATEGORIES.join(", ")}`);
    const [baseline, baselineSeconds] = await timed(() => runTest262(options, "baseline"));
    console.log(`  ordinary build: ${countPassed(baseline)}/${baseline.size} passed in ${baselineSeconds}s`);
    const [stress, stressSeconds] = await timed(() => runTest262(options, "stress"));
    console.log(`  GC-stress build: ${countPassed(stress)}/${stress.size} passed in ${stressSeconds}s`);
    results.push({ suite: "test262", baseline, stress, baselineSeconds, stressSeconds });
  }
  if (options.suites.has("tests")) {
    const files = discoverTestFiles(TEST_PATHS);
    console.log(`tests: ${files.length} files under ${TEST_PATHS.join(", ")}`);
    const [baseline, baselineSeconds] = await timed(() => runTests(options, "baseline", files));
    console.log(`  ordinary build: ${countPassed(baseline)}/${baseline.size} passed in ${baselineSeconds}s`);
    const [stress, stressSeconds] = await timed(() => runTests(options, "stress", files));
    console.log(`  GC-stress build: ${countPassed(stress)}/${stress.size} passed in ${stressSeconds}s`);
    results.push({ suite: "tests", baseline, stress, baselineSeconds, stressSeconds });
  }

  const failures = results.flatMap((result) =>
    stressOnlyFailures(result.suite, result.baseline, result.stress),
  );
  const report = renderReport(results, failures, {
    runUrl: runUrl(),
    commit: Bun.env.GITHUB_SHA ?? "",
    test262Timeout: options.test262Timeout,
  });
  writeFileSync(resolve(options.outDir, "report.md"), report);
  writeFileSync(
    resolve(options.outDir, "summary.json"),
    `${JSON.stringify(
      {
        stressOnlyFailures: failures,
        suites: results.map((result) => ({
          suite: result.suite,
          tests: result.baseline.size,
          passedOrdinary: countPassed(result.baseline),
          passedStress: countPassed(result.stress),
          ordinarySeconds: result.baselineSeconds,
          stressSeconds: result.stressSeconds,
        })),
      },
      null,
      2,
    )}\n`,
  );
  console.log("");
  console.log(report);
  return failures.length === 0 ? 0 : 1;
}

if (import.meta.main) {
  main().then(
    (code) => process.exit(code),
    (error) => {
      console.error(error instanceof InfrastructureError ? `gc-stress: ${error.message}` : error);
      process.exit(2);
    },
  );
}
