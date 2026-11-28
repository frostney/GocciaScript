#!/usr/bin/env bun
/**
 * run-cli-checks.ts
 *
 * Runs the CLI behaviour checks the workflows' `cli` jobs run, several at a
 * time. Each check is its own process with its own temporary directories and
 * ephemeral ports, so they do not share state. A check's output is printed
 * whole once it finishes, and the run fails when any check fails.
 *
 * Usage: bun run scripts/run-cli-checks.ts [--jobs=N] [name ...]
 *
 * --jobs defaults to 3, or fewer on a machine with fewer logical CPUs. Names
 * pick checks from CHECKS; without them every check runs.
 *
 * test-cli-permissions.ts reads GOCCIA_HOSTS_FILE_FIXTURE and fetch-e2e.sh
 * reads GOCCIA_FETCH_IPV6_HOST from the environment, so the caller sets them.
 */

import { availableParallelism } from "node:os";

interface Check {
  name: string;
  command: string[];
  /** Skipped on Windows, where the check cannot run. */
  posixOnly?: boolean;
}

function bunScript(file: string): Check {
  return { name: file, command: [process.execPath, "run", `scripts/${file}`] };
}

// Longest first, from the macOS runners' times, so the longest check is never
// the one left running alone at the end. A new scripts/test-cli-*.ts check is
// added here, not as a workflow step.
const CHECKS: Check[] = [
  bunScript("test-cli-apps.ts"),
  bunScript("test-cli.ts"),
  bunScript("test-cli-permissions.ts"),
  bunScript("test-cli-parser.ts"),
  bunScript("test-cli-heap-snapshots.ts"),
  { name: "fetch-e2e.sh", command: ["bash", "scripts/fetch-e2e.sh"], posixOnly: true },
  bunScript("test-cli-config.ts"),
  bunScript("test-cli-sandbox-search.ts"),
  bunScript("test-cli-lexer.ts"),
  bunScript("test-cli-differential.ts"),
  bunScript("test-cli-embedded-resources.ts"),
  bunScript("test-cli-ffi-callback-slots.ts"),
  bunScript("test-cli-intl-taint.ts"),
  bunScript("test-cli-apps-selection.ts"),
  bunScript("test-cli-assertions.ts"),
  bunScript("test-benchmark-compare.ts"),
];

interface Result {
  check: Check;
  exitCode: number;
  seconds: number;
}

function parseArgs(argv: string[]): { jobs: number; names: string[] } {
  let jobs = Math.min(3, availableParallelism());
  const names: string[] = [];
  for (const arg of argv) {
    const match = arg.match(/^--jobs=(\d+)$/);
    if (match) {
      jobs = Number(match[1]);
      if (jobs < 1) throw new Error("--jobs must be at least 1");
    } else if (arg.startsWith("--")) {
      throw new Error(`Unknown option ${arg}`);
    } else {
      names.push(arg);
    }
  }
  return { jobs, names };
}

function selectChecks(names: string[]): Check[] {
  const available = (check: Check) => !(process.platform === "win32" && check.posixOnly);
  if (names.length === 0) return CHECKS.filter(available);
  return names.map((name) => {
    const check = CHECKS.find((candidate) => candidate.name === name);
    if (!check) throw new Error(`No check named ${name}; known: ${CHECKS.map((c) => c.name).join(", ")}`);
    if (!available(check)) throw new Error(`${name} does not run on Windows`);
    return check;
  });
}

const inActions = process.env.GITHUB_ACTIONS === "true";

function report({ check, exitCode, seconds }: Result, output: string): void {
  const status = exitCode === 0 ? "passed" : `FAILED with exit code ${exitCode}`;
  const header = `${check.name}: ${status} in ${seconds.toFixed(1)} s`;
  // A passing check's output is folded away; a failing one's stays open.
  if (inActions && exitCode === 0) {
    process.stdout.write(`::group::${header}\n${output}${output.endsWith("\n") ? "" : "\n"}::endgroup::\n`);
  } else {
    process.stdout.write(`=== ${header}\n${output}${output.endsWith("\n") ? "" : "\n"}`);
    if (inActions && exitCode !== 0) process.stdout.write(`::error::${header}\n`);
  }
}

async function collect(stream: ReadableStream<Uint8Array>, into: string[]): Promise<void> {
  const decoder = new TextDecoder();
  for await (const chunk of stream) into.push(decoder.decode(chunk, { stream: true }));
  into.push(decoder.decode());
}

async function run(check: Check): Promise<Result> {
  const started = performance.now();
  // Both streams go into one log in the order they arrive, so a check's
  // errors stay next to the output that led to them.
  const chunks: string[] = [];
  let exitCode: number;
  try {
    const child = Bun.spawn(check.command, { stdin: "ignore", stdout: "pipe", stderr: "pipe" });
    await Promise.all([collect(child.stdout, chunks), collect(child.stderr, chunks)]);
    exitCode = await child.exited;
  } catch (error) {
    // A check that cannot start fails on its own; the others still run.
    chunks.push(`could not start ${check.command.join(" ")}: ${error instanceof Error ? error.message : String(error)}\n`);
    exitCode = 127;
  }
  const result = { check, exitCode, seconds: (performance.now() - started) / 1000 };
  report(result, chunks.join(""));
  return result;
}

async function main(): Promise<number> {
  const { jobs, names } = parseArgs(process.argv.slice(2));
  const queue = selectChecks(names);
  console.log(`Running ${queue.length} CLI checks, ${jobs} at a time`);
  const started = performance.now();
  const results: Result[] = [];
  const workers = Array.from({ length: Math.min(jobs, queue.length) }, async () => {
    for (let check = queue.shift(); check; check = queue.shift()) {
      results.push(await run(check));
    }
  });
  await Promise.all(workers);

  const failed = results.filter((result) => result.exitCode !== 0);
  console.log(`\n${results.length - failed.length} of ${results.length} CLI checks passed in ${((performance.now() - started) / 1000).toFixed(1)} s`);
  for (const result of [...results].sort((a, b) => b.seconds - a.seconds)) {
    console.log(`  ${result.exitCode === 0 ? "ok  " : "FAIL"} ${result.seconds.toFixed(1).padStart(6)} s  ${result.check.name}`);
  }
  return failed.length === 0 ? 0 : 1;
}

process.exitCode = await main();
