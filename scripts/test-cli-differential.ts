#!/usr/bin/env bun
/**
 * test-cli-differential.ts
 *
 * Differential suite runner: goccia-interpreted vs goccia-bytecode vs vitest
 * vs bun.
 *
 * Each differential suite under `scripts/differential/` runs under the two
 * goccia modes and under the external runtimes its classification names; per-file
 * counts and failed-test name sets are diffed. A gating disagreement exits 1
 * (0 when everything agrees), so this gates CI directly.
 *
 * Three invariants:
 * 1. **Mode parity** — interpreter and bytecode must agree on the *set of
 *    failed test names*, and on the pass/fail counts. Counts alone are not
 *    enough: the two modes can fail the same number of different tests.
 * 2. **Vitest as the semantic oracle** — goccia's testing API targets Vitest as
 *    an exact drop-in, so for differential suites about testing-API semantics
 *    (matchers, hook and describe accounting) Vitest decides. The failed-test
 *    name set must match, the pass/fail/skip counts must match both goccia modes,
 *    and the two runtimes must agree on whether the *file* failed — that last
 *    one is how a suite-level error (throwing describe, failed
 *    beforeAll/afterAll) is compared without depending on goccia's
 *    goccia-specific `suiteErrors` field.
 * 3. **Bun as the ECMAScript oracle** — for language and runtime differential
 *    suites (syntax, modules, builtins) bun is a sound oracle and gates. For
 *    testing-API suites it is *advisory*: a 223-probe three-way audit found
 *    bun and vitest disagreeing on 30 of 178 matcher probes in both directions,
 *    so bun cannot decide matcher semantics. Advisory drift is printed with a
 *    `~~~` marker and never changes the exit code.
 *
 * Conventions:
 * - Every differential suite must appear in `CLASSIFICATION` below; an
 *   unregistered suite is itself a finding, so adding one forces a deliberate
 *   choice of oracle rather than inheriting a default.
 * - A differential suite handed to an external runtime uses only the
 *   `describe`/`test`/`expect`/hook globals all runtimes inject. `.test.ts`
 *   suites work because bun transpiles TS natively while goccia parses
 *   annotations as types-as-comments.
 * - A differential suite that reaches for goccia-only globals (`mock`,
 *   `spyOn`) — or that asserts a deliberate divergence from both external
 *   runtimes — is named `*.goccia.test.js` and is classified `skip` for both
 *   external runtimes, so only mode parity is checked for it.
 * - A differential suite that needs a goccia capability the default profile
 *   seals (today: `--allow-node-modules`) names it in its classification's
 *   `gocciaFlags`. The flag reaches only that suite, so enabling a capability
 *   for one file cannot silently change what the other suites are testing.
 *   External runtimes get no equivalent knob — a suite belongs here only when
 *   the capability makes goccia match what the oracle already does natively.
 * - The goccia binary defaults to the built `GocciaTestRunner`; set GOCCIA_BIN
 *   to point somewhere else.
 * - A runtime that exceeds the per-file timeout (DIFFRUN_TIMEOUT seconds,
 *   default 60) reports as TIMEOUT and counts as a divergence — this is
 *   deliberate: a parser or executor hang is a finding, not an infrastructure
 *   error.
 */

import { existsSync, readFileSync, readdirSync, rmSync } from "fs";
import { basename, join } from "path";

import { TESTRUNNER } from "./test-cli/binaries";
import { clean, mkdtemp } from "./test-cli/tmpdir";

const DIFFERENTIAL_DIR = join(import.meta.dir, "differential");
const GOCCIA_BIN = process.env.GOCCIA_BIN ?? TESTRUNNER;
const TIMEOUT_MS = Number.parseInt(process.env.DIFFRUN_TIMEOUT ?? "60", 10) * 1000;
const GFLAGS = ["--source-type=module", "--compat-function", "--no-progress"];
const NODE_BIN = process.env.DIFFRUN_NODE ?? "node";
const VITEST_ENTRY = join(DIFFERENTIAL_DIR, "node_modules", "vitest", "vitest.mjs");

/**
 * How a differential suite is gated.
 * - `gate`: a disagreement with this runtime is a divergence and exits 1.
 * - `advisory`: a disagreement is reported but does not gate.
 * - `skip`: the differential suite is never handed to this runtime.
 */
type Role = "gate" | "advisory" | "skip";
type Classification = {
  kind: string;
  bun: Role;
  vitest: Role;
  /** Extra goccia flags for this suite only, on top of GFLAGS. */
  gocciaFlags?: string[];
};

/**
 * Differential suite classification. `kind` names why the oracle was chosen:
 * - `language` — ECMAScript syntax/semantics, where bun is a sound oracle and
 *   the testing API is incidental.
 * - `matcher` / `lifecycle` / `mocks` — testing-API semantics, where the
 *   product target is Vitest-exact behaviour and only vitest may decide. The
 *   `mocks` suite is not there yet: see its entry below.
 */
const CLASSIFICATION: Record<string, Classification> = {
  "a-typesyntax.test.ts": { kind: "language", bun: "gate", vitest: "skip" },
  "b-modules.test.js": { kind: "language", bun: "gate", vitest: "skip" },
  "c-builtins.test.js": { kind: "language", bun: "gate", vitest: "skip" },
  "d-matchers.test.js": { kind: "matcher", bun: "advisory", vitest: "gate" },
  // Now three-way: the differential suite imports `vi` from a bare `vitest`
  // specifier, which vitest resolves to itself and goccia resolves to its
  // bundled compatibility shim. Bun stays skipped — it injects its own `vi` under
  // `bun:test`, but importing the real `vitest` package from a `bun test` file
  // drops bun's injected globals and the file dies on `describe is not
  // defined`, so there is no bun-runnable spelling of this differential suite.
  "e-mocks.test.js": { kind: "mocks", bun: "skip", vitest: "gate" },
  "f-lifecycle.test.js": { kind: "lifecycle", bun: "advisory", vitest: "gate" },
  "g-filehook.test.js": { kind: "lifecycle", bun: "advisory", vitest: "gate" },
  // Module mocking. Vitest gates: `vi.mock` hoisting, factory semantics, and
  // mock/unmock source ordering are testing-API semantics where Vitest-exact
  // behaviour is the product target. Bun is skipped for the same reason as
  // e-mocks — the differential suite imports `vi` from a bare `vitest`
  // specifier, and importing the real `vitest` package from a `bun test` file
  // drops bun's injected globals and dies on `describe is not defined`.
  //
  // The differential suite deliberately exercises only the subset both
  // runtimes agree on. Automock, `vi.doMock`, and `importActual`-based partial
  // mocks are omitted because goccia throws on them by design; a spread-based
  // partial mock would pass under vitest and fail under goccia, which is a
  // documented gap rather than a divergence the harness should rediscover on
  // every run.
  "h-modulemock.test.js": { kind: "mocks", bun: "skip", vitest: "gate" },
  // The companion file for cross-file isolation: it mocks nothing and must see
  // the real module even though h-modulemock mocks it in the same `vitest run`.
  // It imports no `vi`, so bun could in principle run it — but pairing it with
  // its mocking half is the whole point, and that half cannot run under bun, so
  // bun would be checking an isolation property whose other half never ran.
  "i-modulemock-isolation.test.js": { kind: "mocks", bun: "skip", vitest: "gate" },
  // TypeScript's `.js`-specifier convention is module resolution, so bun gates.
  // Vitest is skipped: it resolves through vite, whose own `.js` -> `.ts`
  // fallback is a plugin-level concern rather than the testing-API semantics
  // vitest is the oracle for.
  "j-tsspecifier.test.ts": { kind: "language", bun: "gate", vitest: "skip" },
  // Call-site type arguments are TypeScript syntax over a JavaScript
  // expression grammar, so bun gates: it is the runtime that decides the same
  // ambiguity for the toolchain this runner replaces. Vitest is skipped —
  // nothing here is testing-API semantics.
  "k-callgenerics.test.ts": { kind: "language", bun: "gate", vitest: "skip" },
  // Construction and closure reads from a module's top-level function
  // declarations, which are created while linking rather than compiled with the
  // module body — the split that produced three bytecode-only bugs (the 0.11.0
  // TDZ break, its 0.12.0 fix, and the 0.13.0 break where `new X()` inside such
  // a declaration silently skipped the constructor body). Bun gates: this is
  // ECMAScript class and module semantics, and the testing API is incidental.
  // Vitest is skipped for the same reason as the other language suites.
  "l-modulefndecl.test.js": { kind: "language", bun: "gate", vitest: "skip" },
  // Bare-specifier resolution against a committed node_modules fixture under
  // `mods/nodemods/`. Bun gates: it resolves node_modules natively, which makes
  // it the only oracle that can say whether goccia picked the same file.
  // Vitest is skipped for the same reason as j-tsspecifier — resolution runs
  // through vite there, not through the testing API vitest is the oracle for.
  "m-nodemods.test.js": {
    kind: "language",
    bun: "gate",
    vitest: "skip",
    gocciaFlags: ["--allow-node-modules"],
  },
  // The two node_modules behaviours no external runtime shares: the
  // "module"-field preference (Node ignores the field; bun resolves that
  // package to its CommonJS "main") and the named CommonJS refusal (bun just
  // loads the CommonJS file). Both are deliberate and documented, so only mode
  // parity is checked.
  "n-nodemods.goccia.test.js": {
    kind: "language",
    bun: "skip",
    vitest: "skip",
    gocciaFlags: ["--allow-node-modules"],
  },
  // node:async_hooks context propagation. Bun gates: async-context propagation
  // is runtime semantics, not testing-API semantics, and bun implements
  // node:async_hooks. Vitest is skipped for the same reason — nothing here is
  // a matcher or a hook.
  //
  // The suite deliberately covers propagation only. Bun 1.3.14 does not honour
  // the `defaultValue` or `name` constructor options, leaks the store bound by
  // a `run` that follows a `disable`, and returns undefined rather than the
  // resource from `emitDestroy`; all four are covered against Node's behaviour
  // in tests/built-ins/AsyncHooks, where bun is not the oracle.
  "o-asynccontext.test.js": { kind: "language", bun: "gate", vitest: "skip" },
};

type Verdict = {
  passed: number;
  failed: number;
  /** `null` when the runtime's output does not report skips. */
  skipped: number | null;
  /** Every failed entry, including goccia's suite-level ones. Mode parity. */
  failedNames: Set<string>;
  /** Test-level failures only — the dimension vitest also reports. */
  failedTestNames: Set<string>;
  /** `null` when the runtime's output does not distinguish file failure. */
  fileFailed: boolean | null;
};
type Run = { verdict: Verdict | null; error: string | null };

const scratch = mkdtemp("goccia-differential-");

/** Runs one file under goccia and reads back its JSON result envelope. */
function gocciaResults(path: string, bytecode: boolean, extraFlags: string[] = []): Run {
  const mode = bytecode ? ["--mode=bytecode"] : [];
  const outPath = join(scratch, `${basename(path)}${bytecode ? ".bc" : ".it"}.json`);
  const proc = Bun.spawnSync([GOCCIA_BIN, path, `--output=${outPath}`, ...GFLAGS, ...extraFlags, ...mode], {
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
  const failedTestNames = new Set<string>();
  for (const entry of file.failedTests ?? []) {
    // Goccia reports test failures as `Test "<name>" in suite "<suite>"` and
    // suite-level errors as `Hook "..."` / `Describe "..."`. Vitest has no
    // per-name counterpart for the latter — it fails the file — so the two
    // kinds are tracked apart.
    const m = /^Test "(.*?)" in suite/.exec(entry);
    failedNames.add(m ? m[1] : entry);
    if (m) failedTestNames.add(m[1]);
  }
  return {
    verdict: {
      passed: file.passed,
      failed: file.failed,
      skipped: file.skipped ?? null,
      failedNames,
      failedTestNames,
      fileFailed: file.ok === false,
    },
    error: null,
  };
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
  return {
    verdict: {
      passed,
      failed,
      skipped: null,
      failedNames,
      failedTestNames: failedNames,
      fileFailed: null,
    },
    error: null,
  };
}

/**
 * Runs every vitest-compared differential suite in one `vitest run` and returns
 * the per-file verdicts, keyed by file basename.
 *
 * One invocation for the whole set is what makes vitest affordable as the
 * gating oracle: the suites complete in about a second in a single process,
 * where one process per file would pay vitest's startup cost for each one.
 */
function vitestResults(paths: string[]): Map<string, Run> {
  const results = new Map<string, Run>();
  if (paths.length === 0) return results;

  const outPath = join(scratch, "vitest.json");
  const names = paths.map((p) => basename(p));
  let proc;
  try {
    proc = Bun.spawnSync(
      [NODE_BIN, VITEST_ENTRY, "run", "--reporter=json", `--outputFile=${outPath}`, ...names],
      {
        cwd: DIFFERENTIAL_DIR,
        stdout: "pipe",
        stderr: "pipe",
        // The whole set runs in one process, so the budget is the per-file
        // timeout times the number of files: a hang is still a divergence.
        timeout: TIMEOUT_MS * paths.length,
      },
    );
  } catch (e: any) {
    return fillVitest(results, names, `NO-VITEST (${e.message})`);
  }
  if (proc.exitedDueToTimeout) return fillVitest(results, names, "TIMEOUT");

  let report: any;
  try {
    report = JSON.parse(readFileSync(outPath, "utf8"));
  } catch (e: any) {
    const tail = proc.stderr.toString().trim().split("\n").slice(-1)[0] ?? "";
    return fillVitest(results, names, `NO-JSON (${e.message}) ${tail.slice(0, 100)}`);
  } finally {
    rmSync(outPath, { force: true });
  }

  for (const file of report.testResults ?? []) {
    const key = basename(file.name ?? "");
    let passed = 0;
    let failed = 0;
    let skipped = 0;
    const failedNames = new Set<string>();
    for (const assertion of file.assertionResults ?? []) {
      // Vitest's json reporter reports `skipped` for a test the runner never
      // entered (including the cascade under a failed beforeAll) and `pending`
      // / `todo` for the declared forms; goccia counts all three as skipped.
      if (assertion.status === "passed") passed = passed + 1;
      else if (assertion.status === "failed") {
        failed = failed + 1;
        failedNames.add(assertion.title);
      } else skipped = skipped + 1;
    }
    results.set(key, {
      verdict: {
        passed,
        failed,
        skipped,
        failedNames,
        failedTestNames: failedNames,
        // A vitest file fails either because a test failed or because a suite
        // errored (throwing describe, failed beforeAll/afterAll) — the same
        // two reasons goccia clears its per-file `ok` for.
        fileFailed: file.status === "failed",
      },
      error: null,
    });
  }

  for (const name of names)
    if (!results.has(name))
      results.set(name, { verdict: null, error: "NO-RESULT (vitest reported no such file)" });
  return results;
}

const fillVitest = (results: Map<string, Run>, names: string[], error: string): Map<string, Run> => {
  for (const name of names) results.set(name, { verdict: null, error });
  return results;
};

/**
 * Renders a skip count. `null` means the runtime does not report skips at
 * all, which is not the same as reporting zero — printing it as an empty
 * string (or as the string "null") would claim agreement the output never
 * established.
 */
const fmtSkipped = (skipped: number | null): string =>
  skipped === null ? "/?s" : skipped > 0 ? `/${skipped}s` : "";

const fmt = (run: Run): string => {
  if (run.error) return run.error;
  const v = run.verdict!;
  return `${v.passed}p/${v.failed}f${fmtSkipped(v.skipped)}`;
};

const difference = (a: Set<string>, b: Set<string>): string[] =>
  [...a].filter((v) => !b.has(v)).sort();

const sameNames = (a: Set<string>, b: Set<string>): boolean =>
  a.size === b.size && [...a].every((v) => b.has(v));

/**
 * Diffs an external runtime against both goccia modes and returns one line per
 * disagreement. `label` names the runtime in the messages, and `options` turns
 * off the dimensions a given runtime's output cannot report. The caller decides
 * whether the result gates or is advisory.
 */
function compareOracle(
  label: string,
  oracle: Run,
  modes: [string, Run][],
  options: { compareSkipped: boolean; compareFileFailed: boolean },
): string[] {
  const disagree: string[] = [];
  if (oracle.error) return [`${label} load/exec failure (${oracle.error})`];

  const reference = modes[0][1].verdict!;
  if (!sameNames(reference.failedTestNames, oracle.verdict!.failedTestNames)) {
    const onlyGoccia = difference(reference.failedTestNames, oracle.verdict!.failedTestNames);
    const onlyOracle = difference(oracle.verdict!.failedTestNames, reference.failedTestNames);
    if (onlyGoccia.length > 0) disagree.push(`goccia-only fails: ${JSON.stringify(onlyGoccia)}`);
    if (onlyOracle.length > 0)
      disagree.push(`${label}-only fails: ${JSON.stringify(onlyOracle)}`);
  }
  // Agreeing on which tests fail is not the same as running the same tests,
  // so hold the oracle's counts against both goccia modes, not just one.
  for (const [modeLabel, run] of modes) {
    const v = run.verdict!;
    const o = oracle.verdict!;
    if (v.passed !== o.passed || v.failed !== o.failed)
      disagree.push(`${label} counts differ from ${modeLabel}: ${label}=${fmt(oracle)} ${modeLabel}=${fmt(run)}`);
    // Both sides must actually report skips before a difference means
    // anything: goccia carries `null` for an envelope without the field, and
    // comparing that against a number printed a divergence whose message
    // read `goccia=nulls`. The file-verdict comparison below already guards
    // both sides the same way.
    else if (
      options.compareSkipped &&
      v.skipped !== null &&
      o.skipped !== null &&
      v.skipped !== o.skipped
    )
      disagree.push(
        `${label} skip counts differ from ${modeLabel}: ${label}=${o.skipped}s ${modeLabel}=${v.skipped}s`,
      );
  }
  if (
    options.compareFileFailed &&
    reference.fileFailed !== null &&
    oracle.verdict!.fileFailed !== null &&
    reference.fileFailed !== oracle.verdict!.fileFailed
  )
    // Neither runtime names a suite-level error the other can match, so file
    // failure is the dimension that carries it: goccia clears `ok`, vitest
    // fails the file, and disagreeing means one of them swallowed the error.
    disagree.push(
      `${label} file verdict differs: ${label}=${oracle.verdict!.fileFailed ? "failed" : "ok"} goccia=${reference.fileFailed ? "failed" : "ok"}`,
    );
  return disagree;
}

const cliFiles = process.argv.slice(2);
const files =
  cliFiles.length > 0
    ? cliFiles
    : readdirSync(DIFFERENTIAL_DIR)
        .filter((f) => f.endsWith(".test.js") || f.endsWith(".test.ts"))
        .sort()
        .map((f) => join(DIFFERENTIAL_DIR, f));

const vitestFiles = files.filter((p) => {
  const classification = CLASSIFICATION[basename(p)];
  return classification !== undefined && classification.vitest !== "skip";
});

if (vitestFiles.length > 0 && !existsSync(VITEST_ENTRY)) {
  console.error(
    `Vitest is the semantic oracle for ${vitestFiles.length} of these differential suites and is not installed.\n` +
      `Install the pinned version first:\n\n  cd ${DIFFERENTIAL_DIR} && bun install\n`,
  );
  clean(scratch);
  process.exit(2);
}

const vitestRuns = vitestResults(vitestFiles);

const findings: string[] = [];
const advisories: string[] = [];

for (const path of files) {
  const name = basename(path);
  const classification = CLASSIFICATION[name];
  const gocciaFlags = classification?.gocciaFlags ?? [];
  const it = gocciaResults(path, false, gocciaFlags);
  const bc = gocciaResults(path, true, gocciaFlags);

  if (!classification) {
    console.log(
      `${name.padEnd(28)} UNCLASSIFIED — add it to CLASSIFICATION in scripts/test-cli-differential.ts`,
    );
    findings.push(name);
    continue;
  }

  const runBun = classification.bun !== "skip";
  const bn: Run = runBun ? bunResults(path) : { verdict: null, error: "SKIPPED" };
  const vt: Run =
    classification.vitest === "skip"
      ? { verdict: null, error: "SKIPPED" }
      : vitestRuns.get(name) ?? { verdict: null, error: "NO-RESULT" };

  const disagree: string[] = [];
  const advisory: string[] = [];

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

  // An oracle can only be read once goccia itself agrees with goccia.
  if (disagree.length === 0) {
    const modes: [string, Run][] = [
      ["interp", it],
      ["bytecode", bc],
    ];
    for (const [label, run, role] of [
      ["vitest", vt, classification.vitest],
      ["bun", bn, classification.bun],
    ] as [string, Run, Role][]) {
      if (role === "skip") continue;
      const messages = compareOracle(label, run, modes, {
        // Bun's human summary does not give a per-file skip count or a file
        // verdict, so those dimensions are vitest-only.
        compareSkipped: label === "vitest",
        compareFileFailed: label === "vitest",
      });
      if (role === "gate") disagree.push(...messages);
      else advisory.push(...messages);
    }
  }

  const marker = disagree.length > 0 ? `  <<< ${disagree.join("; ")}` : "";
  const advisoryMarker =
    advisory.length > 0 ? `  ~~~ ADVISORY(non-gating): ${advisory.join("; ")}` : "";
  console.log(
    `${name.padEnd(28)} [${classification.kind}] interp=${fmt(it)}  bytecode=${fmt(bc)}  vitest=${fmt(vt)}  bun=${fmt(bn)}${marker}${advisoryMarker}`,
  );
  if (disagree.length > 0) findings.push(name);
  else if (advisory.length > 0) advisories.push(name);
}

clean(scratch);

console.log(`\n${findings.length} file(s) with divergence`);
if (advisories.length > 0)
  console.log(
    `${advisories.length} file(s) with advisory-only drift (${advisories.join(", ")}) — reported, not gating`,
  );
process.exit(findings.length > 0 ? 1 : 0);
