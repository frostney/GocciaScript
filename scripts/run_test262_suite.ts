#!/usr/bin/env bun
/**
 * run_test262_suite.ts
 *
 * Run GocciaScript against the official TC39 test262 conformance suite,
 * aligned with the standard test262-harness / eshost convention:
 *
 *   - Each test runs in its own GocciaScriptLoaderBare subprocess.
 *   - Stock tc39/test262 harness files are loaded directly from the
 *     pinned checkout's harness/ directory (no bundled custom copies).
 *   - Sync positive: exit 0 = pass; non-zero = fail.
 *   - Async positive: doneprintHandle.js (stock) prints
 *     "Test262:AsyncTestComplete" / "Test262:AsyncTestFailure:<name>: <msg>"
 *     to stdout; the orchestrator scans for those markers.
 *   - Negative runtime: tiny try/catch wrapper prints
 *     "Test262:NegativeTestError:<name>" / "Test262:NegativeTestNoError";
 *     orchestrator matches the captured name against the expected type.
 *   - Negative parse: body alone; non-zero exit = pass (parse failed),
 *     zero exit = fail.
 *
 * No eligibility filter; every discovered test runs. Tests that depend on
 * features GocciaScript doesn't support fail honestly with a real
 * diagnostic in stderr. Per-test subprocess + --timeout + --max-memory
 * bounds the blast radius of any individual hang or OOM.
 *
 * Usage:
 *   bun scripts/run_test262_suite.ts [options]
 *   bun scripts/run_test262_suite.ts --comment <results.json> <baseline.json>
 *
 * See docs/test262.md for the full harness contract.
 */

import { $ } from "bun";
import { readFileSync, existsSync, mkdirSync, writeFileSync, statSync } from "fs";
import { readdirSync } from "fs";
import { join, dirname, relative, basename, resolve } from "path";
import { tmpdir } from "os";
import { mkdtempSync } from "fs";
import { BARE } from "./test-cli/binaries";

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const SUITE_REPO_URL = "https://github.com/tc39/test262.git";
const SUITE_BRANCH = "main";

/** Top-level test262 categories run by default (everything except annexB). */
const DEFAULT_CATEGORIES = [
  "built-ins",
  "harness",
  "intl402",
  "language",
  "staging",
] as const;

const DEFAULT_TIMEOUT_MS = 10_000;
const DEFAULT_MAX_MEMORY = 2 * 1024 * 1024 * 1024; // 2 GiB
const DEFAULT_JOBS = 4;

// Tests that are known to crash the engine (SIGSEGV / SIGBUS) at the
// native level — i.e. not catchable by the per-test timeout, and not
// representative of conformance failures.  Skipping them keeps the
// wrapper-infra counter trustworthy as a "harness broke" signal.
//
// Each entry MUST be paired with a GitHub issue tracking the engine
// bug.  Per docs/test262.md "Updating the contract", this list is the
// only allowed form of test-skipping; no generic eligibility filter.
const KNOWN_ENGINE_CRASHES = new Set<string>([
  // SIGSEGV: generator + iterator concat path. https://github.com/frostney/GocciaScript/issues/514
  "built-ins/Iterator/concat/throws-typeerror-when-generator-is-running-next.js",
  // SIGSEGV: RegExp.prototype.test trailing-input edge case. https://github.com/frostney/GocciaScript/issues/515
  "staging/sm/RegExp/test-trailing.js",
]);

// ---------------------------------------------------------------------------
// Frontmatter
// ---------------------------------------------------------------------------

const FRONTMATTER_RE = /\/\*---\s*\n([\s\S]*?)\n---\*\//;

interface Frontmatter {
  description?: string;
  flags: string[];
  includes: string[];
  features: string[];
  negative?: { phase?: string; type?: string };
  esid?: string;
}

interface ParsedTest {
  body: string;
  meta: Frontmatter;
}

function parseTest(source: string): ParsedTest {
  const match = FRONTMATTER_RE.exec(source);
  if (!match) {
    return { body: source, meta: { flags: [], includes: [], features: [] } };
  }
  const yamlText = match[1];
  // test262 frontmatter is loose YAML — `info:` / `description:` fields
  // routinely contain unbalanced brackets like
  // `String.fromCharCode ( [ char0 [ , char1 ] ] )` that Bun's strict
  // YAML rejects but other engines (and the historical Python parser)
  // accept.  Try strict YAML first, then fall back to a forgiving
  // line-based extractor that only resolves the fields the orchestrator
  // actually needs (`flags`, `includes`, `negative.phase`,
  // `negative.type`, plus `description` for reporting).  This keeps a
  // genuine YAML-shape bug visible (it would still fail the line
  // extractor in most cases) while not losing tests to free-form text
  // in fields we never read.
  let parsed: any;
  try {
    parsed = Bun.YAML.parse(yamlText) || {};
  } catch {
    parsed = parseTolerantFrontmatter(yamlText);
  }
  const before = source.slice(0, match.index).replace(/\s+$/, "");
  const after = source.slice(match.index + match[0].length).replace(/^\n+/, "");
  const body = (before + "\n" + after).replace(/^\n+/, "");
  return {
    body,
    meta: {
      description: parsed.description,
      flags: toStringArray(parsed.flags),
      includes: toStringArray(parsed.includes),
      features: toStringArray(parsed.features),
      negative:
        parsed.negative && typeof parsed.negative === "object"
          ? { phase: parsed.negative.phase, type: parsed.negative.type }
          : undefined,
      esid: parsed.esid,
    },
  };
}

function parseTolerantFrontmatter(yamlText: string): any {
  const out: any = {};
  const lines = yamlText.split("\n");
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const m = /^([A-Za-z][A-Za-z0-9_-]*)\s*:\s*(.*)$/.exec(line);
    if (!m) continue;
    const key = m[1];
    const rest = m[2];
    if (rest === "") {
      // Nested mapping — collect indented `key: value` lines.
      const nested: any = {};
      while (i + 1 < lines.length && /^\s+\S/.test(lines[i + 1])) {
        i++;
        const nm = /^\s+([A-Za-z][A-Za-z0-9_-]*)\s*:\s*(.*)$/.exec(lines[i]);
        if (nm) nested[nm[1]] = unquote(nm[2]);
      }
      out[key] = nested;
    } else {
      // Inline list `[a, b]`
      const lm = /^\[(.*)\]\s*$/.exec(rest);
      if (lm) {
        out[key] = lm[1]
          .split(",")
          .map((s) => unquote(s.trim()))
          .filter(Boolean);
      } else {
        out[key] = unquote(rest);
      }
    }
  }
  return out;
}

function unquote(s: string): string {
  if (s.length >= 2) {
    const f = s[0];
    if ((f === '"' || f === "'") && s[s.length - 1] === f) return s.slice(1, -1);
  }
  return s;
}

function toStringArray(value: unknown): string[] {
  if (Array.isArray(value)) return value.map((v) => String(v));
  if (typeof value === "string" && value.length > 0) return [value];
  return [];
}

// ---------------------------------------------------------------------------
// Harness loading
// ---------------------------------------------------------------------------

class HarnessCache {
  private cache = new Map<string, string>();
  constructor(private suiteDir: string) {}

  async read(name: string): Promise<string> {
    let cached = this.cache.get(name);
    if (cached !== undefined) return cached;
    const path = join(this.suiteDir, "harness", name);
    cached = await Bun.file(path).text();
    this.cache.set(name, cached);
    return cached;
  }

  /**
   * Stock tc39/test262 convention: assert.js depends on Test262Error from
   * sta.js, so prepend both. Then append every name from the test's
   * `includes:` list, in order, dedup'd. The `raw` flag (caller's
   * responsibility) skips this entirely.
   */
  async build(includes: string[]): Promise<string> {
    const order = ["sta.js", "assert.js", ...includes];
    const seen = new Set<string>();
    const parts: string[] = [];
    for (const name of order) {
      if (seen.has(name)) continue;
      seen.add(name);
      try {
        parts.push(await this.read(name));
      } catch (err) {
        // Missing include is a wrapper-infra failure; surface upstream by
        // throwing — the per-test runner classifies it.
        throw new Error(`Missing harness include "${name}": ${err}`);
      }
    }
    return parts.join("\n");
  }
}

// ---------------------------------------------------------------------------
// Wrapper / source builder
// ---------------------------------------------------------------------------

type WrapperKind =
  | "positive_sync"
  | "positive_async"
  | "negative_runtime"
  | "negative_parse";

interface BuildOptions {
  kind: WrapperKind;
  errorType?: string;
}

/**
 * Compose the source the engine actually executes. Sync, async, empty, and
 * script-scope tests are all `harness + body` (no special wrapping). The
 * negative-runtime wrapper is a tiny marker-emitting try/catch — the only
 * Goccia-specific addition to the stock convention. Negative-parse is
 * body-only.
 *
 * No "use strict" injection: the parser ignores the directive anyway, and
 * GocciaScript's curated semantics already enforce most strict-mode
 * behaviors statically. See docs/test262.md "Strict mode" section.
 */
function buildTestSource(
  harnessSource: string,
  body: string,
  opts: BuildOptions,
): string {
  if (opts.kind === "negative_parse") return body;
  if (opts.kind === "negative_runtime") {
    return `${harnessSource}
try {
${body}
  print("Test262:NegativeTestNoError");
} catch (__gocciaT262_e) {
  if (__gocciaT262_e && __gocciaT262_e.constructor && __gocciaT262_e.constructor.name) {
    print("Test262:NegativeTestError:" + __gocciaT262_e.constructor.name);
  } else {
    print("Test262:NegativeTestError:unknown");
  }
}
`;
  }
  // positive_sync / positive_async
  return `${harnessSource}\n${body}`;
}

// ---------------------------------------------------------------------------
// Outcome classification
// ---------------------------------------------------------------------------

type Outcome = "PASS" | "FAIL" | "WRAPPER_INFRA" | "TIMEOUT";

interface RunResult {
  exitCode: number | null;
  signalCode: NodeJS.Signals | null;
  stdout: string;
  stderr: string;
  durationMs: number;
  timedOut: boolean;
}

interface ClassifyArgs {
  kind: WrapperKind;
  expectedErrorType?: string;
  isAsync: boolean;
  result: RunResult;
}

const ASYNC_PASS = "Test262:AsyncTestComplete";
const ASYNC_FAIL_PREFIX = "Test262:AsyncTestFailure:";
const NEG_NO_ERROR = "Test262:NegativeTestNoError";
const NEG_ERROR_PREFIX = "Test262:NegativeTestError:";

// Pascal-side exception classes that indicate engine internals failing,
// not a JS-level throw. `Error: ` is NOT included here — Goccia's bytecode
// mode prefixes uncaught JS errors with "Error: " too, so the prefix
// alone is ambiguous. We rely on Pascal class names (`E...`) and the
// "External: SIG..." pattern for unhandled native signals.
const PASCAL_INFRA_RE =
  /^(?:EAccessViolation|EOutOf|EConvertError|EAssertion|EInvalid(?!Operation)|EOSError|EHeap|ESocket|External: SIG)/m;

function classifyRunResult(args: ClassifyArgs): {
  outcome: Outcome;
  reason: string;
  diagnostic: string;
} {
  const { kind, expectedErrorType, isAsync, result } = args;
  if (result.timedOut) {
    return { outcome: "TIMEOUT", reason: "timeout", diagnostic: "" };
  }

  // Signal exits or ridiculously high exit codes signal an engine crash —
  // these are wrapper-infra failures, not conformance failures, because
  // the engine could not produce a trustworthy verdict.  Exception:
  // SIGTERM is what AbortController sends when our wall-clock timeout
  // fires — classify that as TIMEOUT, not WRAPPER_INFRA.
  const exitCode = result.exitCode ?? -1;
  if (result.signalCode !== null) {
    if (result.signalCode === "SIGTERM" || result.signalCode === "SIGKILL") {
      return { outcome: "TIMEOUT", reason: `wall-clock ${result.signalCode}`, diagnostic: "" };
    }
    return {
      outcome: "WRAPPER_INFRA",
      reason: `signal ${result.signalCode}`,
      diagnostic: result.stderr.slice(0, 800),
    };
  }
  if (exitCode > 1 || exitCode < 0) {
    // Pascal-side abnormal exit; see GocciaScriptLoaderBare's exception
    // handlers (it uses 1 for clean JS throws and Halt(1) for parse/options
    // errors; anything else is unexpected).
    return {
      outcome: "WRAPPER_INFRA",
      reason: `unexpected exit ${exitCode}`,
      diagnostic: result.stderr.slice(0, 800),
    };
  }

  // Stderr starts with a Pascal exception class? Wrapper infra.
  if (PASCAL_INFRA_RE.test(result.stderr)) {
    return {
      outcome: "WRAPPER_INFRA",
      reason: "pascal-side error",
      diagnostic: result.stderr.slice(0, 800),
    };
  }

  if (kind === "negative_parse") {
    if (exitCode !== 0) return { outcome: "PASS", reason: "", diagnostic: "" };
    return {
      outcome: "FAIL",
      reason: "expected parse error, body executed cleanly",
      diagnostic: result.stdout.slice(0, 300),
    };
  }

  if (kind === "negative_runtime") {
    if (result.stdout.includes(NEG_NO_ERROR)) {
      return {
        outcome: "FAIL",
        reason: `expected ${expectedErrorType ?? "error"}, none thrown`,
        diagnostic: "",
      };
    }
    const idx = result.stdout.indexOf(NEG_ERROR_PREFIX);
    if (idx >= 0) {
      const newline = result.stdout.indexOf("\n", idx);
      const line = result.stdout.slice(
        idx + NEG_ERROR_PREFIX.length,
        newline >= 0 ? newline : undefined,
      );
      const actual = line.trim();
      if (
        expectedErrorType !== undefined &&
        actual !== expectedErrorType &&
        // Test262Error is the assertion-failure type used by stock
        // sta.js; it should never satisfy a non-Test262Error expectation.
        !(expectedErrorType === "Test262Error" && actual === "Test262Error")
      ) {
        return {
          outcome: "FAIL",
          reason: `expected ${expectedErrorType}, got ${actual}`,
          diagnostic: result.stderr.slice(0, 300),
        };
      }
      return { outcome: "PASS", reason: "", diagnostic: "" };
    }
    // No marker emitted at all — wrapper-infra (the catch path itself
    // failed, or the engine crashed before emit).
    return {
      outcome: "WRAPPER_INFRA",
      reason: "negative-runtime wrapper produced no marker",
      diagnostic: result.stderr.slice(0, 800),
    };
  }

  // positive_sync / positive_async
  if (isAsync) {
    if (result.stdout.includes(ASYNC_PASS)) {
      return { outcome: "PASS", reason: "", diagnostic: "" };
    }
    const idx = result.stdout.indexOf(ASYNC_FAIL_PREFIX);
    if (idx >= 0) {
      const newline = result.stdout.indexOf("\n", idx);
      const line = result.stdout.slice(
        idx + ASYNC_FAIL_PREFIX.length,
        newline >= 0 ? newline : undefined,
      );
      return {
        outcome: "FAIL",
        reason: line.trim().slice(0, 200),
        diagnostic: "",
      };
    }
    // No marker AND clean exit means the test never called $DONE.
    if (exitCode === 0) {
      return {
        outcome: "FAIL",
        reason: "async test did not call $DONE",
        diagnostic: result.stdout.slice(0, 300),
      };
    }
    return {
      outcome: "FAIL",
      reason: "async test threw before $DONE",
      diagnostic: result.stderr.slice(0, 300),
    };
  }

  // Sync positive
  if (exitCode === 0) return { outcome: "PASS", reason: "", diagnostic: "" };
  return {
    outcome: "FAIL",
    reason: firstLine(result.stderr),
    diagnostic: result.stderr.slice(0, 800),
  };
}

function firstLine(s: string): string {
  const idx = s.indexOf("\n");
  return (idx >= 0 ? s.slice(0, idx) : s).trim().slice(0, 200);
}

// ---------------------------------------------------------------------------
// Test discovery
// ---------------------------------------------------------------------------

interface DiscoveredTest {
  /** Posix-style relative path under suite_dir/test, e.g. "language/foo/bar.js" */
  id: string;
  /** Absolute filesystem path */
  path: string;
}

function normalizeTestId(id: string): string {
  return id.replace(/\\/g, "/");
}

function discoverTests(
  suiteDir: string,
  categories: readonly string[],
  filterGlob: string | null,
): DiscoveredTest[] {
  const testDir = join(suiteDir, "test");
  if (!existsSync(testDir)) {
    throw new Error(`test262 test/ directory not found: ${testDir}`);
  }
  const out: DiscoveredTest[] = [];
  for (const category of categories) {
    const categoryDir = join(testDir, category);
    if (!existsSync(categoryDir)) {
      console.warn(`Warning: category directory not found: ${categoryDir}`);
      continue;
    }
    walkJs(categoryDir, testDir, filterGlob, out);
  }
  out.sort((a, b) => a.id.localeCompare(b.id));
  return out;
}

function walkJs(
  dir: string,
  base: string,
  filterGlob: string | null,
  out: DiscoveredTest[],
): void {
  const entries = readdirSync(dir, { withFileTypes: true });
  for (const ent of entries) {
    if (ent.name.startsWith("_")) continue;
    const full = join(dir, ent.name);
    if (ent.isDirectory()) {
      walkJs(full, base, filterGlob, out);
      continue;
    }
    if (!ent.name.endsWith(".js")) continue;
    if (ent.name.endsWith("_FIXTURE.js")) continue; // test262 fixture convention
    const id = normalizeTestId(relative(base, full));
    if (filterGlob && !matchesGlob(id, filterGlob)) continue;
    if (KNOWN_ENGINE_CRASHES.has(id)) continue;
    out.push({ id, path: full });
  }
}

function matchesGlob(id: string, glob: string): boolean {
  // Convert simple glob (* and **) to RegExp. Sufficient for test262 patterns
  // like "built-ins/Array/*" or "language/expressions/**".
  const escaped = glob
    .replace(/[.+^${}()|[\]\\]/g, "\\$&")
    .replace(/\*\*/g, "__DOUBLESTAR__")
    .replace(/\*/g, "[^/]*")
    .replace(/__DOUBLESTAR__/g, ".*");
  return new RegExp("^" + escaped + "$").test(id) ||
    new RegExp("^.*/" + escaped + "$").test(id);
}

// ---------------------------------------------------------------------------
// Per-test execution
// ---------------------------------------------------------------------------

interface RunOptions {
  bare: string;
  suiteDir: string;
  timeoutMs: number;
  maxMemoryBytes: number;
  mode: "interpreted" | "bytecode";
}

interface PerTestRecord {
  id: string;
  status: Outcome;
  durationMs: number;
  message: string;
  diagnostic?: string;
}

async function runOneTest(
  test: DiscoveredTest,
  cache: HarnessCache,
  opts: RunOptions,
): Promise<PerTestRecord> {
  const sourceText = readFileSync(test.path, "utf-8");
  let parsed: ParsedTest;
  try {
    parsed = parseTest(sourceText);
  } catch (err) {
    return {
      id: test.id,
      status: "WRAPPER_INFRA",
      durationMs: 0,
      message: `frontmatter parse failed: ${(err as Error).message}`,
    };
  }
  const flags = parsed.meta.flags;
  const isAsync = flags.includes("async");
  const isRaw = flags.includes("raw");
  const isModule = flags.includes("module");
  const negative = parsed.meta.negative;
  const isNegative = negative !== undefined;

  // Test262 negative phases:
  //   parse, early       — caught at parse time, no body executes
  //   resolution         — caught during module link, no body executes
  //   runtime            — caught during execution
  //
  // Module-flagged negative tests (any phase) cannot be wrapped in
  // try/catch because top-level `import` is declarative and illegal
  // inside a try block.  Run those raw under --source-type=module and
  // detect failure via exit code, same as parse/resolution/early.
  let kind: WrapperKind;
  if (isNegative) {
    if (
      negative.phase === "parse" ||
      negative.phase === "early" ||
      negative.phase === "resolution" ||
      isModule
    ) {
      kind = "negative_parse";
    } else {
      kind = "negative_runtime";
    }
  } else if (isAsync) {
    kind = "positive_async";
  } else {
    kind = "positive_sync";
  }

  // Async always needs doneprintHandle.js (stock $DONE / marker emitter).
  const includes = [...parsed.meta.includes];
  if (isAsync && !includes.includes("doneprintHandle.js")) {
    includes.push("doneprintHandle.js");
  }

  let harnessSource = "";
  if (!isRaw && kind !== "negative_parse") {
    try {
      harnessSource = await cache.build(includes);
    } catch (err) {
      return {
        id: test.id,
        status: "WRAPPER_INFRA",
        durationMs: 0,
        message: `harness load failed: ${(err as Error).message}`,
      };
    }
  }

  const source = buildTestSource(harnessSource, parsed.body, {
    kind,
    errorType: negative?.type,
  });

  // test262 source is overwhelmingly semicolon-omitted; ASI is required to
  // parse the corpus.  --compat-all and --unsafe-function-constructor are
  // also unconditional: stock harness uses `var`, `function`, and
  // `Function("return this;")()`.
  const args = [
    "--asi",
    "--compat-all",
    "--unsafe-function-constructor",
    `--mode=${opts.mode}`,
    `--timeout=${opts.timeoutMs}`,
    `--max-memory=${opts.maxMemoryBytes}`,
  ];
  if (isModule) args.unshift("--source-type=module");
  args.push("-");

  const result = await spawnBareWithTimeout(
    opts.bare,
    args,
    source,
    opts.timeoutMs * 2 + 1000, // outer wall-clock fallback (engine timeout fires first)
  );

  const cls = classifyRunResult({
    kind,
    expectedErrorType: negative?.type,
    isAsync,
    result,
  });

  return {
    id: test.id,
    status: cls.outcome,
    durationMs: Math.round(result.durationMs),
    message: cls.reason,
    diagnostic: cls.diagnostic || undefined,
  };
}

async function spawnBareWithTimeout(
  bare: string,
  args: string[],
  stdin: string,
  wallClockMs: number,
): Promise<RunResult> {
  const start = performance.now();
  const ac = new AbortController();
  const timer = setTimeout(() => ac.abort(), wallClockMs);
  let timedOut = false;
  try {
    const proc = Bun.spawn([bare, ...args], {
      stdin: new Blob([stdin]),
      stdout: "pipe",
      stderr: "pipe",
      signal: ac.signal,
    });
    const [stdout, stderr] = await Promise.all([
      new Response(proc.stdout).text(),
      new Response(proc.stderr).text(),
    ]);
    await proc.exited;
    clearTimeout(timer);
    return {
      exitCode: proc.exitCode,
      signalCode: proc.signalCode as NodeJS.Signals | null,
      stdout,
      stderr,
      durationMs: performance.now() - start,
      timedOut,
    };
  } catch (err) {
    clearTimeout(timer);
    if (ac.signal.aborted) timedOut = true;
    return {
      exitCode: null,
      signalCode: null,
      stdout: "",
      stderr: timedOut ? "" : String(err),
      durationMs: performance.now() - start,
      timedOut,
    };
  }
}

// ---------------------------------------------------------------------------
// Driver: pool of N concurrent runOneTest calls
// ---------------------------------------------------------------------------

async function runAllTests(
  tests: DiscoveredTest[],
  cache: HarnessCache,
  opts: RunOptions,
  jobs: number,
  onProgress: (done: number, total: number, latest: PerTestRecord) => void,
): Promise<PerTestRecord[]> {
  const results: PerTestRecord[] = new Array(tests.length);
  let nextIndex = 0;
  let completed = 0;

  async function worker(): Promise<void> {
    while (true) {
      const myIndex = nextIndex++;
      if (myIndex >= tests.length) return;
      const rec = await runOneTest(tests[myIndex], cache, opts);
      results[myIndex] = rec;
      completed++;
      onProgress(completed, tests.length, rec);
    }
  }

  const workers = Array.from({ length: jobs }, () => worker());
  await Promise.all(workers);
  return results;
}

// ---------------------------------------------------------------------------
// Aggregation
// ---------------------------------------------------------------------------

interface CategorySummary {
  category: string;
  run: number;
  passed: number;
  failed: number;
  wrapperInfra: number;
  timeouts: number;
}

interface SuiteSummary {
  totalDiscovered: number;
  totalRun: number;
  passed: number;
  failed: number;
  wrapperInfraFailures: number;
  timeouts: number;
  durationSeconds: number;
  byCategory: CategorySummary[];
}

function aggregate(
  results: PerTestRecord[],
  durationSeconds: number,
  totalDiscovered: number,
): SuiteSummary {
  const byCat = new Map<string, CategorySummary>();
  let passed = 0;
  let failed = 0;
  let wrapperInfra = 0;
  let timeouts = 0;
  for (const r of results) {
    const cat = r.id.split("/")[0] || "unknown";
    let entry = byCat.get(cat);
    if (!entry) {
      entry = {
        category: cat,
        run: 0,
        passed: 0,
        failed: 0,
        wrapperInfra: 0,
        timeouts: 0,
      };
      byCat.set(cat, entry);
    }
    entry.run++;
    switch (r.status) {
      case "PASS":
        entry.passed++;
        passed++;
        break;
      case "FAIL":
        entry.failed++;
        failed++;
        break;
      case "WRAPPER_INFRA":
        entry.wrapperInfra++;
        wrapperInfra++;
        break;
      case "TIMEOUT":
        entry.timeouts++;
        timeouts++;
        break;
    }
  }
  const sorted = Array.from(byCat.values()).sort((a, b) =>
    a.category.localeCompare(b.category),
  );
  return {
    totalDiscovered,
    totalRun: results.length,
    passed,
    failed,
    wrapperInfraFailures: wrapperInfra,
    timeouts,
    durationSeconds,
    byCategory: sorted,
  };
}

// ---------------------------------------------------------------------------
// Console summary
// ---------------------------------------------------------------------------

function printConsoleSummary(s: SuiteSummary, results: PerTestRecord[]): void {
  console.log();
  console.log("=".repeat(60));
  console.log("test262 Conformance Summary");
  console.log("=".repeat(60));
  console.log(`  Discovered:           ${pad(s.totalDiscovered, 6)}`);
  console.log(`  Run:                  ${pad(s.totalRun, 6)}`);
  console.log(
    `  Passed:               ${pad(s.passed, 6)}  ` +
      `(${pct(s.passed, s.totalRun)})`,
  );
  console.log(`  Failed:               ${pad(s.failed, 6)}`);
  if (s.wrapperInfraFailures > 0) {
    console.log(`  Wrapper infra:        ${pad(s.wrapperInfraFailures, 6)}  *** non-zero blocks merge ***`);
  } else {
    console.log(`  Wrapper infra:        ${pad(s.wrapperInfraFailures, 6)}`);
  }
  if (s.timeouts > 0) console.log(`  Timeouts:             ${pad(s.timeouts, 6)}`);
  console.log(`  Duration:             ${s.durationSeconds.toFixed(1)}s`);
  console.log();

  console.log("Per-category breakdown:");
  console.log(
    "  Category   |    Run | Passed | Failed | Wrap-Infra | Pass-rate",
  );
  console.log(
    "  -----------+--------+--------+--------+------------+----------",
  );
  for (const c of s.byCategory) {
    console.log(
      `  ${padRight(c.category, 10)} | ${pad(c.run, 6)} | ${pad(c.passed, 6)} | ${pad(c.failed, 6)} | ${pad(c.wrapperInfra, 10)} | ${pct(c.passed, c.run)}`,
    );
  }
  console.log();

  const failing = results
    .filter((r) => r.status === "FAIL" || r.status === "TIMEOUT")
    .slice(0, 30);
  if (failing.length > 0) {
    console.log(`Failing tests (showing first 30 of ${results.filter((r) => r.status === "FAIL" || r.status === "TIMEOUT").length}):`);
    for (const r of failing) {
      console.log(`  [${r.status}] ${r.id}`);
      if (r.message) console.log(`         ${r.message.slice(0, 200)}`);
    }
    console.log();
  }
}

function pad(n: number, width: number): string {
  return String(n).padStart(width);
}

function padRight(s: string, width: number): string {
  return s.padEnd(width);
}

function pct(n: number, d: number): string {
  if (d === 0) return "0.0%";
  return ((n / d) * 100).toFixed(1) + "%";
}

// ---------------------------------------------------------------------------
// Step summary (GitHub Actions $GITHUB_STEP_SUMMARY)
// ---------------------------------------------------------------------------

async function emitStepSummary(s: SuiteSummary): Promise<void> {
  const path = process.env.GITHUB_STEP_SUMMARY;
  if (!path) return;
  let md = "## test262 Conformance\n\n";
  if (s.wrapperInfraFailures > 0) {
    md += `**${s.wrapperInfraFailures} wrapper infrastructure failure(s)** — conformance numbers untrustworthy\n\n`;
  } else {
    md += `Wrapper infra failures: 0\n\n`;
  }
  md += "| Category   |    Run | Passed | Failed | Pass-rate |\n";
  md += "|------------|-------:|-------:|-------:|----------:|\n";
  for (const c of s.byCategory) {
    md += `| ${c.category} | ${c.run} | ${c.passed} | ${c.failed} | ${pct(c.passed, c.run)} |\n`;
  }
  md += `| **total** | ${s.totalRun} | ${s.passed} | ${s.failed} | ${pct(s.passed, s.totalRun)} |\n`;
  md += `\nDuration: ${s.durationSeconds.toFixed(1)}s\n`;
  await Bun.write(path, md);
}

// ---------------------------------------------------------------------------
// Main run mode
// ---------------------------------------------------------------------------

interface MainArgs {
  suiteDir: string | null;
  output: string | null;
  filter: string | null;
  categories: readonly string[];
  maxTests: number;
  jobs: number;
  mode: "interpreted" | "bytecode";
  timeoutMs: number;
  maxMemoryBytes: number;
  verbose: boolean;
  bare: string;
}

function parsePositiveInt(name: string, raw: string | undefined): number {
  const n = parseInt(raw ?? "", 10);
  if (!Number.isInteger(n) || n <= 0) {
    console.error(`${name} requires a positive integer, got: ${raw}`);
    process.exit(2);
  }
  return n;
}

function parseMainArgs(argv: string[]): MainArgs {
  const out: MainArgs = {
    suiteDir: null,
    output: null,
    filter: process.env.TEST262_FILTER || null,
    categories: DEFAULT_CATEGORIES,
    maxTests: parseInt(process.env.TEST262_MAX_TESTS || "0", 10),
    jobs: DEFAULT_JOBS,
    mode: "bytecode",
    timeoutMs: parseInt(process.env.TEST262_TIMEOUT_MS || String(DEFAULT_TIMEOUT_MS), 10),
    maxMemoryBytes: parseInt(process.env.TEST262_MAX_MEMORY || String(DEFAULT_MAX_MEMORY), 10),
    verbose: false,
    bare: BARE,
  };
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--suite-dir") out.suiteDir = argv[++i];
    else if (arg.startsWith("--suite-dir=")) out.suiteDir = arg.slice("--suite-dir=".length);
    else if (arg === "--output") out.output = argv[++i];
    else if (arg.startsWith("--output=")) out.output = arg.slice("--output=".length);
    else if (arg === "--filter") out.filter = argv[++i];
    else if (arg.startsWith("--filter=")) out.filter = arg.slice("--filter=".length);
    else if (arg === "--categories") out.categories = argv[++i].split(",");
    else if (arg.startsWith("--categories=")) out.categories = arg.slice("--categories=".length).split(",");
    else if (arg === "--max-tests") out.maxTests = parseInt(argv[++i], 10);
    else if (arg.startsWith("--max-tests=")) out.maxTests = parseInt(arg.slice("--max-tests=".length), 10);
    else if (arg === "--jobs") out.jobs = parsePositiveInt("--jobs", argv[++i]);
    else if (arg.startsWith("--jobs=")) out.jobs = parsePositiveInt("--jobs", arg.slice("--jobs=".length));
    else if (arg === "--mode") out.mode = argv[++i] as "interpreted" | "bytecode";
    else if (arg.startsWith("--mode=")) out.mode = arg.slice("--mode=".length) as "interpreted" | "bytecode";
    else if (arg === "--timeout-ms") out.timeoutMs = parseInt(argv[++i], 10);
    else if (arg.startsWith("--timeout-ms=")) out.timeoutMs = parseInt(arg.slice("--timeout-ms=".length), 10);
    else if (arg === "--max-memory") out.maxMemoryBytes = parseInt(argv[++i], 10);
    else if (arg.startsWith("--max-memory=")) out.maxMemoryBytes = parseInt(arg.slice("--max-memory=".length), 10);
    else if (arg === "--bare") out.bare = argv[++i];
    else if (arg.startsWith("--bare=")) out.bare = arg.slice("--bare=".length);
    else if (arg === "--verbose" || arg === "-v") out.verbose = true;
    else if (arg === "--help" || arg === "-h") {
      printUsage();
      process.exit(0);
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }
  return out;
}

function printUsage(): void {
  console.log(`Usage: bun scripts/run_test262_suite.ts [options]
       bun scripts/run_test262_suite.ts --comment <results.json> <baseline.json|->

Run mode options:
  --suite-dir DIR        Existing test262 checkout (clones if omitted).
  --output FILE          JSON output path for the full report.
  --filter GLOB          Glob pattern for test paths.
  --categories LIST      Comma-separated categories (default: ${DEFAULT_CATEGORIES.join(",")}).
  --max-tests N          Cap total tests run (0 = unlimited).
  --jobs N               Parallel workers (default: ${DEFAULT_JOBS}).
  --mode interpreted|bytecode (default: bytecode).
  --timeout-ms MS        Per-test engine timeout (default: ${DEFAULT_TIMEOUT_MS}).
  --max-memory BYTES     Per-test GC heap cap (default: 2 GiB).
  --bare PATH            Path to GocciaScriptLoaderBare (default: ./build/...).
  --verbose              Print per-test results.

Comment mode:
  --comment RESULTS BASELINE
                         Print PR-comment markdown to stdout. Pass "-" for
                         BASELINE if no baseline is available.
`);
}

async function ensureSuiteCheckout(suiteDir: string | null): Promise<{
  dir: string;
  cleanup: (() => void) | null;
}> {
  if (suiteDir) return { dir: resolve(suiteDir), cleanup: null };
  const tmp = mkdtempSync(join(tmpdir(), "test262-suite."));
  const checkoutDir = join(tmp, "repo");
  console.log(`Cloning test262 (shallow) into ${checkoutDir} ...`);
  await $`git clone --depth 1 --branch ${SUITE_BRANCH} ${SUITE_REPO_URL} ${checkoutDir}`;
  console.log("Clone complete.");
  return {
    dir: checkoutDir,
    cleanup: () => {
      try {
        Bun.spawnSync(["rm", "-rf", tmp]);
      } catch {}
    },
  };
}

function ensureBareBinary(path: string): void {
  if (!existsSync(path)) {
    throw new Error(
      `GocciaScriptLoaderBare not found at ${path}. ` +
        `Build with ./build.pas loaderbare first, or pass --bare PATH.`,
    );
  }
  try {
    statSync(path);
  } catch (err) {
    throw new Error(`Cannot stat ${path}: ${err}`);
  }
}

async function runMain(argv: string[]): Promise<number> {
  const args = parseMainArgs(argv);
  ensureBareBinary(args.bare);
  const checkout = await ensureSuiteCheckout(args.suiteDir);
  try {
    console.log(`Bare:          ${args.bare}`);
    console.log(`Suite:         ${checkout.dir}`);
    console.log(`Categories:    ${args.categories.join(", ")}`);
    console.log(`Mode:          ${args.mode}`);
    console.log(`Jobs:          ${args.jobs}`);
    console.log(`Timeout:       ${args.timeoutMs}ms`);
    console.log(`Max memory:    ${args.maxMemoryBytes} bytes`);
    if (args.filter) console.log(`Filter:        ${args.filter}`);
    if (args.maxTests > 0) console.log(`Max tests:     ${args.maxTests}`);
    console.log();

    let tests = discoverTests(checkout.dir, args.categories, args.filter);
    console.log(`Discovered ${tests.length} test files.`);
    if (args.maxTests > 0 && tests.length > args.maxTests) {
      tests = tests.slice(0, args.maxTests);
      console.log(`Capped to ${tests.length} (--max-tests).`);
    }

    const cache = new HarnessCache(checkout.dir);
    const start = performance.now();

    const lastProgress = { line: 0 };
    const results = await runAllTests(
      tests,
      cache,
      {
        bare: args.bare,
        suiteDir: checkout.dir,
        timeoutMs: args.timeoutMs,
        maxMemoryBytes: args.maxMemoryBytes,
        mode: args.mode,
      },
      args.jobs,
      (done, total, rec) => {
        const should =
          args.verbose ||
          rec.status === "FAIL" ||
          rec.status === "WRAPPER_INFRA" ||
          rec.status === "TIMEOUT" ||
          done === total ||
          done - lastProgress.line >= 250;
        if (should) {
          const tag =
            rec.status === "PASS"
              ? "."
              : rec.status === "FAIL"
                ? "F"
                : rec.status === "TIMEOUT"
                  ? "T"
                  : "W";
          if (args.verbose || rec.status !== "PASS") {
            console.log(`[${pad(done, 6)}/${total}] ${tag} ${rec.id}`);
          } else {
            console.log(`[${pad(done, 6)}/${total}]`);
          }
          lastProgress.line = done;
        }
      },
    );

    const durationSeconds = (performance.now() - start) / 1000;
    const summary = aggregate(results, durationSeconds, tests.length);

    const report = { summary, results };
    if (args.output) {
      mkdirSync(dirname(args.output), { recursive: true });
      writeFileSync(args.output, JSON.stringify(report, null, 2) + "\n");
      console.log(`Full report written to: ${resolve(args.output)}`);
    }

    printConsoleSummary(summary, results);
    await emitStepSummary(summary);

    if (
      summary.failed > 0 ||
      summary.wrapperInfraFailures > 0 ||
      summary.timeouts > 0
    ) {
      return 1;
    }
    return 0;
  } finally {
    if (checkout.cleanup) checkout.cleanup();
  }
}

// ---------------------------------------------------------------------------
// Comment mode (PR-comment markdown builder)
// ---------------------------------------------------------------------------

function fmt(n: number): string {
  return n.toLocaleString("en-US");
}

function signed(n: number): string {
  if (n > 0) return "+" + fmt(n);
  if (n < 0) return fmt(n);
  return "±0";
}

function signedPp(rateNew: number, rateOld: number | null): string {
  if (rateOld == null) return "🆕";
  const dp = (rateNew - rateOld) * 100;
  if (Math.abs(dp) <= 0.05) return "±0pp";
  return (dp > 0 ? "+" : "") + dp.toFixed(1) + "pp";
}

interface BaselineSummary {
  byCategory?: CategorySummary[];
  totalRun?: number;
  passed?: number;
  failed?: number;
}

interface AreaBucket {
  key: string;
  attempted: number;
  passed: number;
}

function bucketAreas(results: PerTestRecord[]): Map<string, AreaBucket> {
  const out = new Map<string, AreaBucket>();
  for (const r of results) {
    const parts = r.id.split("/");
    if (parts.length < 2) continue;
    const key = parts.slice(0, 2).join("/");
    let b = out.get(key);
    if (!b) {
      b = { key, attempted: 0, passed: 0 };
      out.set(key, b);
    }
    b.attempted++;
    if (r.status === "PASS") b.passed++;
  }
  return out;
}

function buildCommentMarkdown(
  data: { summary: SuiteSummary; results: PerTestRecord[] } | null,
  baseline: { summary: BaselineSummary; results?: PerTestRecord[] } | null,
): string {
  const marker = "<!-- test262-results -->";
  let body = "## test262 Conformance\n\n";

  if (!data || !data.summary) {
    body +=
      "_test262 results were not produced for this run._\n\n" +
      "<sub>Non-blocking. The conformance job either timed out, crashed, or did not upload an artifact. See the workflow run for details.</sub>\n";
    return marker + "\n" + body;
  }

  const s = data.summary;
  const hasBaseline = !!(baseline && baseline.summary);
  const baselineByCat = new Map<string, CategorySummary>();
  if (hasBaseline) {
    for (const c of baseline!.summary.byCategory || []) {
      baselineByCat.set(c.category, c);
    }
  }

  if (s.wrapperInfraFailures > 0) {
    body += `> ⚠️ **${s.wrapperInfraFailures} wrapper infrastructure failure(s)** — conformance numbers untrustworthy until fixed.\n\n`;
  }

  body += "| Category | Run | Passed | Δ Pass | Failed | Pass-rate | Δ Rate |\n";
  body += "|----------|----:|-------:|-------:|-------:|----------:|-------:|\n";
  for (const c of s.byCategory) {
    const rate = c.run > 0 ? c.passed / c.run : 0;
    const ba = baselineByCat.get(c.category);
    const dPass = ba ? c.passed - ba.passed : c.passed;
    const baseRate = ba && ba.run > 0 ? ba.passed / ba.run : null;
    body += `| \`${c.category}\` | ${fmt(c.run)} | ${fmt(c.passed)} | ${ba ? signed(dPass) : "🆕"} | ${fmt(c.failed)} | ${pct(c.passed, c.run)} | ${signedPp(rate, baseRate)} |\n`;
  }
  // Totals row
  const totalRate = s.totalRun > 0 ? s.passed / s.totalRun : 0;
  const baseTotalRun = (baseline?.summary.totalRun) ?? 0;
  const baseTotalPassed = (baseline?.summary.passed) ?? 0;
  const baseTotalRate = baseTotalRun > 0 ? baseTotalPassed / baseTotalRun : null;
  const dTotalPass = hasBaseline ? s.passed - baseTotalPassed : s.passed;
  body += `| **total** | ${fmt(s.totalRun)} | ${fmt(s.passed)} | ${hasBaseline ? signed(dTotalPass) : "🆕"} | ${fmt(s.failed)} | ${pct(s.passed, s.totalRun)} | ${signedPp(totalRate, baseTotalRate)} |\n\n`;

  // Areas closest to 100%
  const MIN_SAMPLE = 25;
  const areas = bucketAreas(data.results);
  const baselineAreas = hasBaseline ? bucketAreas(baseline!.results || []) : new Map();
  const ranked = Array.from(areas.values())
    .filter((a) => a.attempted >= MIN_SAMPLE && a.passed < a.attempted)
    .map((a) => ({ ...a, rate: a.passed / a.attempted }))
    .sort(
      (x, y) =>
        y.rate - x.rate ||
        y.attempted - x.attempted ||
        x.key.localeCompare(y.key),
    )
    .slice(0, 3);

  if (ranked.length > 0) {
    body += "### Areas closest to 100%\n\n";
    if (hasBaseline) {
      body += "| Area | Pass rate | Δ vs main | Passing |\n";
      body += "|------|-----------|-----------|---------|\n";
      for (const a of ranked) {
        const ba = baselineAreas.get(a.key);
        const baseRate =
          ba && ba.attempted > 0 ? ba.passed / ba.attempted : null;
        body += `| \`${a.key}\` | ${pct(a.passed, a.attempted)} | ${signedPp(a.rate, baseRate)} | ${fmt(a.passed)} / ${fmt(a.attempted)} |\n`;
      }
    } else {
      body += "| Area | Pass rate | Passing |\n";
      body += "|------|-----------|---------|\n";
      for (const a of ranked) {
        body += `| \`${a.key}\` | ${pct(a.passed, a.attempted)} | ${fmt(a.passed)} / ${fmt(a.attempted)} |\n`;
      }
    }
    body += "\n";
  } else {
    body +=
      "_No areas yet meet the highlight criteria (≥ 25 attempted tests, below 100%)._\n\n";
  }

  // Per-test deltas (collapsible)
  if (hasBaseline) {
    const baselineStatusById = new Map<string, Outcome>();
    for (const r of baseline!.results || []) baselineStatusById.set(r.id, r.status);
    const newPasses: string[] = [];
    const newFails: string[] = [];
    for (const r of data.results) {
      const prev = baselineStatusById.get(r.id);
      if (!prev) continue;
      if (prev !== "PASS" && r.status === "PASS") newPasses.push(r.id);
      else if (prev === "PASS" && r.status !== "PASS") newFails.push(r.id);
    }
    if (newPasses.length > 0 || newFails.length > 0) {
      body += `<details>\n<summary>Per-test deltas (+${newPasses.length} / -${newFails.length})</summary>\n\n`;
      if (newFails.length > 0) {
        body += `**Newly failing (${newFails.length}):**\n\n`;
        for (const id of newFails.slice(0, 100)) body += `- \`${id}\`\n`;
        if (newFails.length > 100) body += `- _… ${newFails.length - 100} more_\n`;
        body += "\n";
      }
      if (newPasses.length > 0) {
        body += `**Newly passing (${newPasses.length}):**\n\n`;
        for (const id of newPasses.slice(0, 100)) body += `- \`${id}\`\n`;
        if (newPasses.length > 100) body += `- _… ${newPasses.length - 100} more_\n`;
        body += "\n";
      }
      body += "</details>\n\n";
    }
  }

  const baselineNote = hasBaseline
    ? " Δ vs main compares against the most recent cached `main` baseline."
    : " No `main` baseline cached yet — Δ columns will appear once a `main` run completes.";
  body += `<sub>Non-blocking. Measured on ubuntu-latest x64, bytecode mode. Areas grouped by the first two test262 path components; minimum 25 attempted tests, areas already at 100% excluded.${baselineNote}</sub>\n`;

  return marker + "\n" + body;
}

function runComment(argv: string[]): number {
  if (argv.length < 2) {
    console.error("Usage: --comment <results.json> <baseline.json|->");
    return 2;
  }
  const [resultsPath, baselinePath] = argv;
  let data: any = null;
  try {
    data = JSON.parse(readFileSync(resultsPath, "utf8"));
  } catch (e) {
    console.error(`Failed to read ${resultsPath}: ${(e as Error).message}`);
    // Still emit a markdown stub so the PR comment shows the skip.
  }
  let baseline: any = null;
  if (baselinePath !== "-") {
    try {
      baseline = JSON.parse(readFileSync(baselinePath, "utf8"));
    } catch {
      // Missing baseline is fine — surfaces as 🆕 in the table.
    }
  }
  process.stdout.write(buildCommentMarkdown(data, baseline));
  return 0;
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

const argv = process.argv.slice(2);
if (argv[0] === "--comment") {
  process.exit(runComment(argv.slice(1)));
} else {
  runMain(argv).then(
    (code) => process.exit(code),
    (err) => {
      console.error(err);
      process.exit(1);
    },
  );
}
