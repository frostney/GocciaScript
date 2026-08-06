#!/usr/bin/env bun
/**
 * test-cli-differential.ts
 *
 * Differential battery runner: goccia-interpreted vs goccia-bytecode vs bun.
 *
 * Each battery file under `scripts/differential/` runs under all three
 * runtimes; per-file pass/fail counts and failed-test name sets are diffed.
 * Any disagreement is a divergence and the process exits 1 (0 when everything
 * agrees), so this gates CI directly.
 *
 * Two invariants:
 * 1. **Mode parity** — interpreter and bytecode must agree on the *set of
 *    failed test names*, and on the pass/fail counts. Counts alone are not
 *    enough: the two modes can fail the same number of different tests.
 * 2. **Bun as oracle** — the set of *failed test names* under goccia must
 *    equal the set under `bun test`, and bun's pass/fail counts must match
 *    both goccia modes. Names alone are not enough either: bun can agree on
 *    which tests fail while running a different number of them.
 *
 * Conventions:
 * - A battery compared against bun uses only the `describe`/`test`/`expect`
 *   globals all three runtimes inject. `.test.ts` batteries work because bun
 *   transpiles TS natively while goccia parses annotations as
 *   types-as-comments.
 * - A battery named `*.goccia.test.js` uses goccia-only globals (`mock`,
 *   `spyOn`): bun is skipped for it and only interp-vs-bytecode parity is
 *   checked.
 * - The goccia binary defaults to the built `GocciaTestRunner`; set GOCCIA_BIN
 *   to point somewhere else.
 * - A runtime that exceeds the per-file timeout (DIFFRUN_TIMEOUT seconds,
 *   default 60) reports as TIMEOUT and counts as a divergence — this is
 *   deliberate: a parser or executor hang is a finding, not an infrastructure
 *   error.
 */

import { readFileSync, readdirSync, rmSync } from "fs";
import { basename, join } from "path";

import { TESTRUNNER } from "./test-cli/binaries";
import { clean, mkdtemp } from "./test-cli/tmpdir";

const BATTERY_DIR = join(import.meta.dir, "differential");
const GOCCIA_BIN = process.env.GOCCIA_BIN ?? TESTRUNNER;
const TIMEOUT_MS = Number.parseInt(process.env.DIFFRUN_TIMEOUT ?? "60", 10) * 1000;
const GFLAGS = ["--source-type=module", "--compat-function", "--no-progress"];

type Verdict = { passed: number; failed: number; failedNames: Set<string> };
type Run = { verdict: Verdict | null; error: string | null };

const scratch = mkdtemp("goccia-differential-");

/** Runs one file under goccia and reads back its JSON result envelope. */
function gocciaResults(path: string, bytecode: boolean): Run {
  const mode = bytecode ? ["--mode=bytecode"] : [];
  const outPath = join(scratch, `${basename(path)}${bytecode ? ".bc" : ".it"}.json`);
  const proc = Bun.spawnSync([GOCCIA_BIN, path, `--output=${outPath}`, ...GFLAGS, ...mode], {
    stdout: "pipe",
    stderr: "pipe",
    timeout: TIMEOUT_MS,
  });
  if (proc.exitedDueToTimeout) return { verdict: null, error: "TIMEOUT" };

  let envelope: any;
  try {
    envelope = JSON.parse(readFileSync(outPath, "utf8"));
  } catch (e: any) {
    // Any unreadable envelope is the same finding.
    return { verdict: null, error: `NO-JSON (${e.message})` };
  } finally {
    rmSync(outPath, { force: true });
  }

  const file = envelope.files?.[0];
  if (!file) return { verdict: null, error: "NO-JSON (envelope has no files[0])" };
  if (file.errorMessage) {
    return { verdict: null, error: `LOAD-FAIL: ${file.errorMessage.split("\n")[0].slice(0, 100)}` };
  }

  const failedNames = new Set<string>();
  for (const entry of file.failedTests ?? []) {
    const m = /^Test "(.*?)" in suite/.exec(entry);
    failedNames.add(m ? m[1] : entry);
  }
  return { verdict: { passed: file.passed, failed: file.failed, failedNames }, error: null };
}

/** Runs one file under `bun test`; parses the human summary for verdicts. */
function bunResults(path: string): Run {
  const proc = Bun.spawnSync(["bun", "test", path], {
    stdout: "pipe",
    stderr: "pipe",
    timeout: TIMEOUT_MS,
  });
  if (proc.exitedDueToTimeout) return { verdict: null, error: "TIMEOUT" };

  const out = proc.stdout.toString() + proc.stderr.toString();
  const lines = out.split("\n");
  let passed = 0;
  let failed = 0;
  const failedNames = new Set<string>();
  for (const line of lines) {
    const mp = /^\s*(\d+) pass/.exec(line);
    if (mp) passed = Number.parseInt(mp[1], 10);
    const mf = /^\s*(\d+) fail/.exec(line);
    if (mf) failed = Number.parseInt(mf[1], 10);
    const mfn = /^\(fail\) (?:.+? > )?(.+?)(?: \[[\d.]+m?s\])?$/.exec(line.trim());
    if (mfn) failedNames.add(mfn[1]);
  }
  if (passed === 0 && failed === 0) {
    const tail = lines.length > 0 ? lines[lines.length - 1].slice(0, 100) : "EMPTY";
    return { verdict: null, error: `NO-TESTS: ${tail}` };
  }
  return { verdict: { passed, failed, failedNames }, error: null };
}

const fmt = (run: Run): string =>
  run.error ?? `${run.verdict!.passed}p/${run.verdict!.failed}f`;

const difference = (a: Set<string>, b: Set<string>): string[] =>
  [...a].filter((v) => !b.has(v)).sort();

const sameNames = (a: Set<string>, b: Set<string>): boolean =>
  a.size === b.size && [...a].every((v) => b.has(v));

const cliFiles = process.argv.slice(2);
const files =
  cliFiles.length > 0
    ? cliFiles
    : readdirSync(BATTERY_DIR)
        .filter((f) => f.endsWith(".test.js") || f.endsWith(".test.ts"))
        .sort()
        .map((f) => join(BATTERY_DIR, f));

const findings: string[] = [];

for (const path of files) {
  const name = basename(path);
  const gocciaOnly = name.includes(".goccia.");
  const it = gocciaResults(path, false);
  const bc = gocciaResults(path, true);
  const bn: Run = gocciaOnly ? { verdict: null, error: "SKIPPED" } : bunResults(path);

  const disagree: string[] = [];
  if (it.error || bc.error) {
    disagree.push("goccia load/exec failure");
  } else {
    // Mode parity is a name-set comparison first: the two modes failing the
    // same *number* of different tests is a divergence that a count-only check
    // reports as agreement. The counts are still compared, because a failed-name
    // set cannot reveal a test that ran in one mode and not the other.
    const onlyInterp = difference(it.verdict!.failedNames, bc.verdict!.failedNames);
    const onlyBytecode = difference(bc.verdict!.failedNames, it.verdict!.failedNames);
    if (onlyInterp.length > 0)
      disagree.push(`MODE-PARITY BROKEN: interp-only fails: ${JSON.stringify(onlyInterp)}`);
    if (onlyBytecode.length > 0)
      disagree.push(`MODE-PARITY BROKEN: bytecode-only fails: ${JSON.stringify(onlyBytecode)}`);
    if (it.verdict!.passed !== bc.verdict!.passed || it.verdict!.failed !== bc.verdict!.failed)
      disagree.push(`MODE-PARITY BROKEN: counts interp=${fmt(it)} bytecode=${fmt(bc)}`);
  }
  if (!gocciaOnly && disagree.length === 0) {
    if (bn.error) {
      disagree.push(`bun load/exec failure (${bn.error})`);
    } else {
      if (!sameNames(it.verdict!.failedNames, bn.verdict!.failedNames)) {
        const onlyGoccia = difference(it.verdict!.failedNames, bn.verdict!.failedNames);
        const onlyBun = difference(bn.verdict!.failedNames, it.verdict!.failedNames);
        if (onlyGoccia.length > 0) disagree.push(`goccia-only fails: ${JSON.stringify(onlyGoccia)}`);
        if (onlyBun.length > 0) disagree.push(`bun-only fails: ${JSON.stringify(onlyBun)}`);
      }
      // Agreeing on which tests fail is not the same as running the same tests,
      // so hold bun's counts against both goccia modes, not just the interpreter.
      for (const [label, run] of [["interp", it], ["bytecode", bc]] as [string, Run][])
        if (
          run.verdict!.passed !== bn.verdict!.passed ||
          run.verdict!.failed !== bn.verdict!.failed
        )
          disagree.push(`bun counts differ from ${label}: bun=${fmt(bn)} ${label}=${fmt(run)}`);
    }
  }

  const marker = disagree.length > 0 ? `  <<< ${disagree.join("; ")}` : "";
  console.log(
    `${name.padEnd(42)} interp=${fmt(it)}  bytecode=${fmt(bc)}  bun=${fmt(bn)}${marker}`,
  );
  if (disagree.length > 0) findings.push(name);
}

clean(scratch);

console.log(`\n${findings.length} file(s) with divergence`);
process.exit(findings.length > 0 ? 1 : 0);
