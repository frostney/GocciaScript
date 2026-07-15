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
 *     Top-level global declaration-instantiation runtime negatives run
 *     unwrapped because wrapping them in a block changes the semantics
 *     under test.
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
 *   bun scripts/run_test262_suite.ts --comment <results.json> <baseline.json|->
 *
 * --comment writes the PR comment markdown to stdout.  When run with a
 * cached `main` baseline, it also serves as the conformance gate: it
 * exits non-zero iff total pass count dropped or any previously-passing
 * test transitioned to non-PASS.  Steady-state failures (the long tail
 * of unimplemented features) remain non-blocking via continue-on-error
 * on the conformance job itself; only true regressions block.
 *
 * See docs/test262.md for the full harness contract.
 */

import { $ } from "bun";
import { readFileSync, existsSync, mkdirSync, writeFileSync, statSync, rmSync } from "fs";
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

const DEFAULT_TIMEOUT_MS = 20_000;
const DEFAULT_MAX_MEMORY = 2 * 1024 * 1024 * 1024; // 2 GiB
const DEFAULT_JOBS = 4;
const DEFAULT_PROFILE_MODE = "all" as const;
const PROFILE_REPORT_LIMIT = 50;

// Tests that are known to crash the engine (SIGSEGV / SIGBUS) at the
// native level — i.e. not catchable by the per-test timeout, and not
// representative of conformance failures.  Skipping them keeps the
// wrapper-infra counter trustworthy as a "harness broke" signal.
//
// Each entry MUST be paired with a GitHub issue tracking the engine
// bug.  Per docs/test262.md "Updating the contract", this list is the
// only allowed form of test-skipping; no generic eligibility filter.
const KNOWN_ENGINE_CRASHES = new Set<string>([
]);

// ---------------------------------------------------------------------------
// Frontmatter
// ---------------------------------------------------------------------------
//
// test262 frontmatter is YAML, but the orchestrator only consumes four
// fields: `flags` (e.g. async / module / raw), `includes` (extra harness
// files), `features` for feature-to-flag mapping, and
// `negative.{phase, type}`.  Free-form fields like `info`,
// `description`, `esid` are not read by the orchestrator —
// and `info` in particular routinely contains unbalanced brackets like
// `String.fromCharCode ( [ char0 [ , char1 [ , ... ] ] ] )` that Bun's
// strict YAML parser rejects.
//
// Strategy: line-walk the frontmatter block, copy lines that belong to
// `flags`, `includes`, `features`, or `negative` (top-level key + its indented
// continuation), and drop everything else.  Pass the cleaned text to
// Bun's built-in YAML parser.  No regex; no custom YAML; we just trim
// the input to what we need before handing it to a real YAML parser.

const KEPT_KEYS = new Set(["flags", "includes", "features", "negative"]);
const FRONTMATTER_OPEN = "/*---";
const FRONTMATTER_CLOSE = "---*/";

interface Frontmatter {
  flags: string[];
  includes: string[];
  features: string[];
  negative?: { phase?: string; type?: string };
}

interface ParsedTest {
  body: string;
  meta: Frontmatter;
}

function parseTest(source: string): ParsedTest {
  const open = source.indexOf(FRONTMATTER_OPEN);
  const close = open < 0 ? -1 : source.indexOf(FRONTMATTER_CLOSE, open);
  if (open < 0 || close < 0) {
    return { body: source, meta: { flags: [], includes: [], features: [] } };
  }
  const blockStart = frontmatterContentStart(source, open);
  const yaml = source.slice(blockStart, close).replace(/\r\n?/g, "\n").trimEnd();
  const cleaned = stripUnreadFields(yaml);
  const parsed = (Bun.YAML.parse(cleaned) || {}) as any;

  const before = source.slice(0, open).trimEnd();
  const after = source
    .slice(close + FRONTMATTER_CLOSE.length)
    .replace(/^(?:\r\n|[\r\n])+/, "");
  const body = (before + "\n" + after).replace(/^\n+/, "");

  return {
    body,
    meta: {
      flags: toStringArray(parsed.flags),
      includes: toStringArray(parsed.includes),
      features: toStringArray(parsed.features),
      negative:
        parsed.negative && typeof parsed.negative === "object"
          ? { phase: parsed.negative.phase, type: parsed.negative.type }
          : undefined,
    },
  };
}

function frontmatterContentStart(source: string, open: number): number {
  const afterOpen = open + FRONTMATTER_OPEN.length;
  if (source.startsWith("\r\n", afterOpen)) return afterOpen + 2;
  if (source[afterOpen] === "\n" || source[afterOpen] === "\r") return afterOpen + 1;
  return afterOpen;
}

// Walk the frontmatter line-by-line; copy through any block whose
// top-level key is one we care about, drop the rest.  A "block" is a
// line that starts in column 0 with `<key>:` plus all following lines
// indented under it.
function stripUnreadFields(yaml: string): string {
  const out: string[] = [];
  let inKept = false;
  for (const line of yaml.split(/\r\n|[\n\r]/)) {
    if (line.length === 0 || line.startsWith(" ") || line.startsWith("\t") ||
        line.startsWith("#")) {
      // Continuation of whatever block we're currently in (or comment).
      if (inKept) out.push(line);
      continue;
    }
    // New top-level key.
    const colon = line.indexOf(":");
    const key = colon > 0 ? line.slice(0, colon).trim() : "";
    inKept = KEPT_KEYS.has(key);
    if (inKept) out.push(line);
  }
  return out.join("\n");
}

function toStringArray(value: unknown): string[] {
  if (Array.isArray(value)) return value.map((v) => String(v));
  if (typeof value === "string" && value.length > 0) return [value];
  return [];
}

// ---------------------------------------------------------------------------
// Harness loading
// ---------------------------------------------------------------------------

// test262 expects each host to provide a `$262` object for host hooks.
// Everything else loads from the pinned stock harness checkout.
const BUNDLED_INCLUDES: Record<string, string> = {
  "$262.js": "$262.js",
  "goccia-global-shim.js": "goccia-global-shim.js",
};

const BUNDLED_HARNESS_DIR = join(import.meta.dir, "test262_harness");

class HarnessCache {
  private cache = new Map<string, string>();
  constructor(private suiteDir: string) {}

  async read(name: string): Promise<string> {
    let cached = this.cache.get(name);
    if (cached !== undefined) return cached;
    // Hybrid lookup: prefer bundled adaptation, fall back to stock.
    const bundled = BUNDLED_INCLUDES[name];
    const path = bundled
      ? join(BUNDLED_HARNESS_DIR, bundled)
      : join(this.suiteDir, "harness", name);
    cached = await Bun.file(path).text();
    this.cache.set(name, cached);
    return cached;
  }

  /**
   * Always prepend `$262.js` (host hooks), stock `sta.js`, and stock
   * `assert.js`. Then append every name from the test's `includes:` list,
   * dedup'd against names already covered by the prelude. The `raw` flag
   * (caller's responsibility) skips this entirely.
   */
  async build(includes: string[]): Promise<string> {
    const seen = new Set<string>(["$262.js", "sta.js", "assert.js"]);
    const parts: string[] = [
      await this.read("$262.js"),
      await this.read("sta.js"),
      await this.read("assert.js"),
    ];
    for (const name of includes) {
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
    parts.push(await this.read("goccia-global-shim.js"));
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
  | "negative_runtime_unwrapped"
  | "negative_parse";

interface BuildOptions {
  kind: WrapperKind;
  strictMode: boolean;
}

function joinPreludeAndBody(prelude: string, body: string): string {
  if (prelude.length === 0) return body;
  return `${prelude}${prelude.endsWith("\n") ? "" : "\n"}${body}`;
}

/**
 * Compose the source the engine actually executes. Sync, async, empty, and
 * script-scope tests are all `harness + body` (no special wrapping). The
 * negative-runtime wrapper is a tiny marker-emitting try/catch. Top-level
 * negative runtime tests that rely on Script global declaration instantiation
 * run unwrapped so the wrapper does not introduce a block scope.
 * Negative-parse is body-only.
 *
 * `onlyStrict` tests get a directive prefix so they exercise strict runtime
 * semantics while still receiving compatibility-gated parser support.
 */
function buildTestSource(
  harnessSource: string,
  body: string,
  opts: BuildOptions,
): string {
  const strictPrefix = opts.strictMode ? '"use strict";\n' : "";
  if (opts.kind === "negative_parse") return `${strictPrefix}${body}`;
  if (opts.kind === "negative_runtime_unwrapped") {
    return joinPreludeAndBody(`${strictPrefix}${harnessSource}`, body);
  }
  if (opts.kind === "negative_runtime") {
    return `${strictPrefix}${harnessSource}
try {
${body}
  print("Test262:NegativeTestNoError");
} catch (__gocciaT262_e) {
  var __gocciaT262_n = "unknown";
  if (__gocciaT262_e && typeof __gocciaT262_e === "object") {
    if (__gocciaT262_e.constructor && __gocciaT262_e.constructor.name) {
      __gocciaT262_n = __gocciaT262_e.constructor.name;
    }
  }
  print("Test262:NegativeTestError:" + __gocciaT262_n);
}
`;
  }
  const prelude = `${strictPrefix}${harnessSource}`;
  if (opts.kind === "positive_async") {
    return joinPreludeAndBody(prelude, body);
  }
  // positive_sync
  return joinPreludeAndBody(prelude, body);
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
const TEST262_TIMEOUT_EXIT_CODE = 124;
const TEST262_TIMEOUT_RE = /^GocciaTest262:Timeout:(\d+)$/m;

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
  // SIGTERM/SIGKILL when WE triggered the abort (timedOut flag set in
  // spawnBareWithTimeout's setTimeout callback).  An external SIGTERM
  // (operator kills the process) or SIGKILL from the OS is a real crash
  // signal and stays in WRAPPER_INFRA.
  const exitCode = result.exitCode ?? -1;
  if (result.signalCode !== null) {
    if (
      result.timedOut &&
      (result.signalCode === "SIGTERM" || result.signalCode === "SIGKILL")
    ) {
      return { outcome: "TIMEOUT", reason: `wall-clock ${result.signalCode}`, diagnostic: "" };
    }
    return {
      outcome: "WRAPPER_INFRA",
      reason: `signal ${result.signalCode}`,
      diagnostic: result.stderr.slice(0, 800),
    };
  }
  const engineTimeout = result.stderr.trim().match(TEST262_TIMEOUT_RE);
  if (engineTimeout && exitCode === TEST262_TIMEOUT_EXIT_CODE) {
    return {
      outcome: "TIMEOUT",
      reason: `engine timeout ${engineTimeout[1]}ms`,
      diagnostic: "",
    };
  }

  if (exitCode > 1 || exitCode < 0) {
    // Pascal-side abnormal exit; see GocciaScriptLoaderBare's exception
    // handlers (it uses 1 for clean JS throws and Halt(1) for parse/options
    // errors; test262-host timeouts are the only expected non-1 failure
    // exit, and they must carry the marker above).
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

  if (kind === "negative_runtime_unwrapped") {
    if (exitCode === 0) {
      return {
        outcome: "FAIL",
        reason: `expected ${expectedErrorType ?? "error"}, none thrown`,
        diagnostic: result.stdout.slice(0, 300),
      };
    }
    const actual = firstLine(result.stderr).split(":", 1)[0].trim();
    if (expectedErrorType !== undefined && actual !== expectedErrorType) {
      return {
        outcome: "FAIL",
        reason: `expected ${expectedErrorType}, got ${actual || "unknown"}`,
        diagnostic: result.stderr.slice(0, 300),
      };
    }
    return { outcome: "PASS", reason: "", diagnostic: "" };
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
  profileDir: string | null;
  profileMode: ProfileMode;
}

interface PerTestRecord {
  id: string;
  status: Outcome;
  durationMs: number;
  message: string;
  diagnostic?: string;
  profilePath?: string;
  profileMissing?: boolean;
}

type ProfileMode = "opcodes" | "functions" | "all";

// test262 source is overwhelmingly semicolon-omitted; ASI is required to parse
// the corpus.  --compat-var, --compat-function, --compat-arguments-object,
// --compat-traditional-for-loop, --compat-for-in-loop, --compat-while-loops,
// --compat-loose-equality, --compat-label, and --unsafe-function-constructor
// are also unconditional:
// stock harness and tests use `var`, `function`, traditional `for(;;)` loops,
// implicit arguments objects, for-in loops, while/do-while loops, loose
// equality, labels, and `Function("return this;")()`. Non-strict mode
// compatibility is enabled for Script tests separately: strict directives and
// modules decide strict semantics, while the flag exposes the sloppy-only
// runtime behavior needed by the corpus.
const TEST262_BARE_FLAGS: readonly string[] = [
  "--compat-asi",
  "--compat-var",
  "--compat-function",
  "--compat-arguments-object",
  "--compat-traditional-for-loop",
  "--compat-for-in-loop",
  "--compat-while-loops",
  "--compat-loose-equality",
  "--compat-label",
  "--unsafe-function-constructor",
  "--test262-host",
];

function needsNonStrictCompat(isModule: boolean): boolean {
  return !isModule;
}

function needsUnwrappedNegativeRuntime(
  testId: string,
  negative: Frontmatter["negative"],
): boolean {
  return negative?.phase === "runtime" &&
    negative?.type === "SyntaxError" &&
    testId.startsWith("language/global-code/");
}

function test262FeatureFlags(features: readonly string[]): string[] {
  const flags: string[] = [];
  if (features.includes("source-phase-imports-module-source")) {
    flags.push("--experimental-js-module-source");
  }
  if (features.includes("ShadowRealm") || features.includes("realms-tests")) {
    flags.push("--unsafe-shadowrealm");
  }
  return flags;
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
  const strictMode = flags.includes("onlyStrict");
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
    } else if (needsUnwrappedNegativeRuntime(test.id, negative)) {
      kind = "negative_runtime_unwrapped";
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
    strictMode,
  });

  const args = [
    ...TEST262_BARE_FLAGS,
    `--mode=${opts.mode}`,
    `--timeout=${opts.timeoutMs}`,
    `--max-memory=${opts.maxMemoryBytes}`,
  ];
  let profilePath: string | undefined;
  if (opts.profileDir) {
    profilePath = profilePathForTest(opts.profileDir, test.id);
    mkdirSync(dirname(profilePath), { recursive: true });
    args.push(`--profile=${opts.profileMode}`, `--profile-output=${profilePath}`);
  }
  if (needsNonStrictCompat(isModule)) {
    args.push("--compat-non-strict-mode");
  }
  args.push(...test262FeatureFlags(parsed.meta.features));
  if (isModule) args.unshift("--source-type=module");
  args.push(`--source-name=${test.path}`);
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
    profilePath: profilePath ? toPosixPath(profilePath) : undefined,
    profileMissing: profilePath ? !existsSync(profilePath) : undefined,
  };
}

function profilePathForTest(profileDir: string, testId: string): string {
  return join(profileDir, "per-test", ...normalizeTestId(testId).split("/")) +
    ".profile.json";
}

function toPosixPath(path: string): string {
  return path.replace(/\\/g, "/");
}

async function spawnBareWithTimeout(
  bare: string,
  args: string[],
  stdin: string,
  wallClockMs: number,
): Promise<RunResult> {
  const start = performance.now();
  const ac = new AbortController();
  let timedOut = false;
  let tempSourceDir: string | null = null;
  const timer = setTimeout(() => {
    timedOut = true;
    ac.abort();
  }, wallClockMs);
  try {
    const command = [bare, ...args];
    const spawnOptions: any = {
      stdout: "pipe",
      stderr: "pipe",
      signal: ac.signal,
    };
    if (stdin.includes("\r")) {
      tempSourceDir = mkdtempSync(join(tmpdir(), "goccia-test262-source."));
      const sourcePath = join(tempSourceDir, "test.js");
      writeFileSync(sourcePath, stdin);
      const stdinArgIndex = command.lastIndexOf("-");
      if (stdinArgIndex >= 0) {
        command[stdinArgIndex] = sourcePath;
      } else {
        command.push(sourcePath);
      }
      spawnOptions.stdin = "ignore";
    } else {
      spawnOptions.stdin = new Blob([stdin]);
    }

    const proc = Bun.spawn(command, spawnOptions);
    const [stdout, stderr] = await Promise.all([
      new Response(proc.stdout).text(),
      new Response(proc.stderr).text(),
    ]);
    await proc.exited;
    clearTimeout(timer);
    if (tempSourceDir) rmSync(tempSourceDir, { recursive: true, force: true });
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
    if (tempSourceDir) rmSync(tempSourceDir, { recursive: true, force: true });
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
// Profile aggregation
// ---------------------------------------------------------------------------

type ProfileJson = {
  opcodes?: Array<{ opcode?: string; count?: number }>;
  opcodePairs?: Array<{ prev?: string; cur?: string; count?: number }>;
  scalarFastPath?: { hits?: number; misses?: number; total?: number; hitRate?: number };
  shapeSaturation?: { depthLimitPrefixes?: number; tableCapacityEvents?: number };
  functions?: Array<{
    name?: string;
    sourceFile?: string;
    line?: number;
    calls?: number;
    selfTimeNs?: number;
    totalTimeNs?: number;
    allocations?: number;
  }>;
};

type CountEntry = { name: string; count: number; percentage?: number };
type FunctionEntry = {
  name: string;
  sourceFile: string;
  line: number;
  calls: number;
  selfTimeNs: number;
  totalTimeNs: number;
  allocations: number;
};
type TestProfileEntry = {
  id: string;
  status: Outcome;
  durationMs: number;
  profilePath: string;
  totalOpcodes: number;
  allocations: number;
  functionSelfTimeNs: number;
};
type ProfileGroupEntry = {
  key: string;
  profiles: number;
  totalOpcodes: number;
  allocations: number;
  functionSelfTimeNs: number;
};

type Test262ProfileSummary = {
  generatedAt: string;
  run: {
    suiteDir: string;
    test262Sha: string | null;
    mode: "bytecode";
    categories: string[];
    jobs: number;
    timeoutMs: number;
    maxMemoryBytes: number;
    totalDiscovered: number;
    totalRun: number;
    passed: number;
    failed: number;
    wrapperInfraFailures: number;
    timeouts: number;
    durationSeconds: number;
  };
  profileDir: string;
  profileCount: number;
  missingProfileCount: number;
  totalOpcodes: number;
  totalOpcodePairs: number;
  scalarFastPath: {
    hits: number;
    misses: number;
    total: number;
    hitRate: number | null;
  };
  shapeSaturation: {
    depthLimitPrefixes: number;
    tableCapacityEvents: number;
  };
  topOpcodes: CountEntry[];
  topOpcodePairs: CountEntry[];
  topFunctionsBySelfTime: FunctionEntry[];
  topFunctionsByCalls: FunctionEntry[];
  topFunctionsByAllocations: FunctionEntry[];
  topTestsByOpcodes: TestProfileEntry[];
  topTestsByAllocations: TestProfileEntry[];
  topTestsByDuration: TestProfileEntry[];
  categoryBreakdown: ProfileGroupEntry[];
  pathBreakdown: ProfileGroupEntry[];
};

function safeNumber(value: unknown): number {
  return typeof value === "number" && Number.isFinite(value) ? value : 0;
}

function addCount(map: Map<string, number>, key: string, count: number): void {
  if (!key || count <= 0) return;
  map.set(key, (map.get(key) ?? 0) + count);
}

function topCounts(map: Map<string, number>, total: number): CountEntry[] {
  return [...map.entries()]
    .map(([name, count]) => ({
      name,
      count,
      percentage: total > 0 ? (count / total) * 100 : undefined,
    }))
    .sort((a, b) => b.count - a.count || a.name.localeCompare(b.name))
    .slice(0, PROFILE_REPORT_LIMIT);
}

function totalOpcodeCount(profile: ProfileJson): number {
  return (profile.opcodes ?? []).reduce(
    (sum, entry) => sum + safeNumber(entry.count),
    0,
  );
}

function totalAllocations(profile: ProfileJson): number {
  return (profile.functions ?? []).reduce(
    (sum, entry) => sum + safeNumber(entry.allocations),
    0,
  );
}

function totalFunctionSelfTime(profile: ProfileJson): number {
  return (profile.functions ?? []).reduce(
    (sum, entry) => sum + safeNumber(entry.selfTimeNs),
    0,
  );
}

function addGroup(
  map: Map<string, ProfileGroupEntry>,
  key: string,
  profile: ProfileJson,
): void {
  const entry =
    map.get(key) ??
    ({
      key,
      profiles: 0,
      totalOpcodes: 0,
      allocations: 0,
      functionSelfTimeNs: 0,
    } satisfies ProfileGroupEntry);
  entry.profiles++;
  entry.totalOpcodes += totalOpcodeCount(profile);
  entry.allocations += totalAllocations(profile);
  entry.functionSelfTimeNs += totalFunctionSelfTime(profile);
  map.set(key, entry);
}

function functionKey(entry: NonNullable<ProfileJson["functions"]>[number]): string {
  return [
    entry.name || "<anonymous>",
    entry.sourceFile || "",
    safeNumber(entry.line),
  ].join("\u0000");
}

function addFunction(
  map: Map<string, FunctionEntry>,
  entry: NonNullable<ProfileJson["functions"]>[number],
): void {
  const key = functionKey(entry);
  const current =
    map.get(key) ??
    ({
      name: entry.name || "<anonymous>",
      sourceFile: entry.sourceFile || "",
      line: safeNumber(entry.line),
      calls: 0,
      selfTimeNs: 0,
      totalTimeNs: 0,
      allocations: 0,
    } satisfies FunctionEntry);
  current.calls += safeNumber(entry.calls);
  current.selfTimeNs += safeNumber(entry.selfTimeNs);
  current.totalTimeNs += safeNumber(entry.totalTimeNs);
  current.allocations += safeNumber(entry.allocations);
  map.set(key, current);
}

function topFunctions(
  map: Map<string, FunctionEntry>,
  metric: keyof Pick<FunctionEntry, "selfTimeNs" | "calls" | "allocations">,
): FunctionEntry[] {
  return [...map.values()]
    .sort(
      (a, b) =>
        b[metric] - a[metric] ||
        b.calls - a.calls ||
        a.name.localeCompare(b.name) ||
        a.sourceFile.localeCompare(b.sourceFile) ||
        a.line - b.line,
    )
    .slice(0, PROFILE_REPORT_LIMIT);
}

function topTests(
  tests: TestProfileEntry[],
  metric: keyof Pick<TestProfileEntry, "totalOpcodes" | "allocations" | "durationMs">,
): TestProfileEntry[] {
  return [...tests]
    .sort((a, b) => b[metric] - a[metric] || a.id.localeCompare(b.id))
    .slice(0, PROFILE_REPORT_LIMIT);
}

function topGroups(map: Map<string, ProfileGroupEntry>): ProfileGroupEntry[] {
  return [...map.values()]
    .sort(
      (a, b) =>
        b.totalOpcodes - a.totalOpcodes ||
        b.allocations - a.allocations ||
        a.key.localeCompare(b.key),
    )
    .slice(0, PROFILE_REPORT_LIMIT);
}

function profilePathForDisplay(profileDir: string, profilePath: string): string {
  return relative(profileDir, profilePath).split("\\").join("/");
}

function buildProfileSummary(
  results: PerTestRecord[],
  profileDir: string,
  run: Test262ProfileSummary["run"],
): Test262ProfileSummary {
  const opcodeCounts = new Map<string, number>();
  const opcodePairCounts = new Map<string, number>();
  const functionTotals = new Map<string, FunctionEntry>();
  const categories = new Map<string, ProfileGroupEntry>();
  const paths = new Map<string, ProfileGroupEntry>();
  const tests: TestProfileEntry[] = [];
  let missingProfileCount = 0;
  let scalarHits = 0;
  let scalarMisses = 0;
  let depthLimitPrefixes = 0;
  let tableCapacityEvents = 0;
  let totalOpcodes = 0;
  let totalOpcodePairs = 0;

  for (const result of results) {
    const profilePath = profilePathForTest(profileDir, result.id);
    if (!existsSync(profilePath)) {
      missingProfileCount++;
      continue;
    }
    let profile: ProfileJson;
    try {
      profile = JSON.parse(readFileSync(profilePath, "utf8")) as ProfileJson;
    } catch {
      missingProfileCount++;
      continue;
    }

    const profileOpcodes = totalOpcodeCount(profile);
    const allocations = totalAllocations(profile);
    const functionSelfTimeNs = totalFunctionSelfTime(profile);
    totalOpcodes += profileOpcodes;
    scalarHits += safeNumber(profile.scalarFastPath?.hits);
    scalarMisses += safeNumber(profile.scalarFastPath?.misses);
    depthLimitPrefixes += safeNumber(profile.shapeSaturation?.depthLimitPrefixes);
    tableCapacityEvents += safeNumber(profile.shapeSaturation?.tableCapacityEvents);

    for (const entry of profile.opcodes ?? []) {
      addCount(opcodeCounts, entry.opcode || "", safeNumber(entry.count));
    }
    for (const entry of profile.opcodePairs ?? []) {
      const prev = entry.prev || "";
      const cur = entry.cur || "";
      const count = safeNumber(entry.count);
      if (prev && cur) addCount(opcodePairCounts, `${prev} -> ${cur}`, count);
      totalOpcodePairs += count;
    }
    for (const entry of profile.functions ?? []) {
      addFunction(functionTotals, entry);
    }

    const parts = result.id.split("/");
    addGroup(categories, parts[0] || "unknown", profile);
    addGroup(paths, parts.slice(0, 2).join("/") || "unknown", profile);
    tests.push({
      id: result.id,
      status: result.status,
      durationMs: result.durationMs,
      profilePath: profilePathForDisplay(profileDir, profilePath),
      totalOpcodes: profileOpcodes,
      allocations,
      functionSelfTimeNs,
    });
  }

  const scalarTotal = scalarHits + scalarMisses;
  return {
    generatedAt: new Date().toISOString(),
    run,
    profileDir,
    profileCount: tests.length,
    missingProfileCount,
    totalOpcodes,
    totalOpcodePairs,
    scalarFastPath: {
      hits: scalarHits,
      misses: scalarMisses,
      total: scalarTotal,
      hitRate: scalarTotal > 0 ? scalarHits / scalarTotal : null,
    },
    shapeSaturation: {
      depthLimitPrefixes,
      tableCapacityEvents,
    },
    topOpcodes: topCounts(opcodeCounts, totalOpcodes),
    topOpcodePairs: topCounts(opcodePairCounts, totalOpcodePairs),
    topFunctionsBySelfTime: topFunctions(functionTotals, "selfTimeNs"),
    topFunctionsByCalls: topFunctions(functionTotals, "calls"),
    topFunctionsByAllocations: topFunctions(functionTotals, "allocations"),
    topTestsByOpcodes: topTests(tests, "totalOpcodes"),
    topTestsByAllocations: topTests(tests, "allocations"),
    topTestsByDuration: topTests(tests, "durationMs"),
    categoryBreakdown: topGroups(categories),
    pathBreakdown: topGroups(paths),
  };
}

function formatProfileInteger(value: number): string {
  return Math.round(value).toLocaleString("en-US");
}

function formatNanoseconds(ns: number): string {
  const ms = ns / 1e6;
  if (ms >= 1000) return `${(ms / 1000).toFixed(2)}s`;
  return `${ms.toFixed(1)}ms`;
}

function writeProfileMarkdown(summary: Test262ProfileSummary, outputPath: string): void {
  const lines: string[] = [];
  lines.push("# test262 Profile Report", "");
  lines.push(`Generated: ${summary.generatedAt}`);
  lines.push(`Mode: ${summary.run.mode}`);
  lines.push(`test262 SHA: ${summary.run.test262Sha ?? "unknown"}`);
  lines.push(`Categories: ${summary.run.categories.join(", ")}`);
  lines.push(
    `Run settings: ${summary.run.jobs} jobs, ` +
      `${summary.run.timeoutMs}ms timeout, ` +
      `${formatProfileInteger(summary.run.maxMemoryBytes)} byte heap cap`,
  );
  lines.push(
    `Conformance outcomes: ${formatProfileInteger(summary.run.passed)} passed, ` +
      `${formatProfileInteger(summary.run.failed)} failed, ` +
      `${formatProfileInteger(summary.run.wrapperInfraFailures)} wrapper-infra, ` +
      `${formatProfileInteger(summary.run.timeouts)} timeouts`,
  );
  lines.push(`Profiles: ${formatProfileInteger(summary.profileCount)}`);
  if (summary.missingProfileCount > 0) {
    lines.push(`Missing/corrupt profiles: ${formatProfileInteger(summary.missingProfileCount)}`);
  }
  lines.push(`Total opcodes: ${formatProfileInteger(summary.totalOpcodes)}`);
  lines.push(`Total opcode pairs: ${formatProfileInteger(summary.totalOpcodePairs)}`);
  const hitRate = summary.scalarFastPath.hitRate;
  lines.push(
    `Scalar fast-path: ${formatProfileInteger(summary.scalarFastPath.hits)} hits, ` +
      `${formatProfileInteger(summary.scalarFastPath.misses)} misses` +
      (hitRate === null ? "" : ` (${(hitRate * 100).toFixed(1)}% hit rate)`),
  );
  lines.push("");

  const addCountTable = (title: string, rows: CountEntry[]) => {
    lines.push(`## ${title}`, "", "| Rank | Name | Count | Share |", "|---:|---|---:|---:|");
    rows.slice(0, 20).forEach((row, index) => {
      lines.push(
        `| ${index + 1} | \`${row.name}\` | ${formatProfileInteger(row.count)} | ` +
          `${row.percentage === undefined ? "" : row.percentage.toFixed(1) + "%"} |`,
      );
    });
    lines.push("");
  };

  const addFunctionTable = (title: string, rows: FunctionEntry[]) => {
    lines.push(
      `## ${title}`,
      "",
      "| Rank | Function | Calls | Self time | Total time | Allocations |",
      "|---:|---|---:|---:|---:|---:|",
    );
    rows.slice(0, 20).forEach((row, index) => {
      const location = row.sourceFile ? ` @ ${row.sourceFile}:${row.line}` : "";
      lines.push(
        `| ${index + 1} | \`${row.name}${location}\` | ${formatProfileInteger(row.calls)} | ` +
          `${formatNanoseconds(row.selfTimeNs)} | ${formatNanoseconds(row.totalTimeNs)} | ` +
          `${formatProfileInteger(row.allocations)} |`,
      );
    });
    lines.push("");
  };

  const addTestTable = (title: string, rows: TestProfileEntry[]) => {
    lines.push(
      `## ${title}`,
      "",
      "| Rank | Test | Status | Opcodes | Allocations | Duration | Profile |",
      "|---:|---|---|---:|---:|---:|---|",
    );
    rows.slice(0, 20).forEach((row, index) => {
      lines.push(
        `| ${index + 1} | \`${row.id}\` | ${row.status} | ${formatProfileInteger(row.totalOpcodes)} | ` +
          `${formatProfileInteger(row.allocations)} | ${row.durationMs}ms | \`${row.profilePath}\` |`,
      );
    });
    lines.push("");
  };

  const addGroupTable = (title: string, rows: ProfileGroupEntry[]) => {
    lines.push(
      `## ${title}`,
      "",
      "| Rank | Path | Profiles | Opcodes | Self time | Allocations |",
      "|---:|---|---:|---:|---:|---:|",
    );
    rows.slice(0, 20).forEach((row, index) => {
      lines.push(
        `| ${index + 1} | \`${row.key}\` | ${formatProfileInteger(row.profiles)} | ` +
          `${formatProfileInteger(row.totalOpcodes)} | ${formatNanoseconds(row.functionSelfTimeNs)} | ` +
          `${formatProfileInteger(row.allocations)} |`,
      );
    });
    lines.push("");
  };

  addCountTable("Top Opcodes", summary.topOpcodes);
  addCountTable("Top Opcode Pairs", summary.topOpcodePairs);
  addFunctionTable("Top Functions By Self Time", summary.topFunctionsBySelfTime);
  addFunctionTable("Top Functions By Calls", summary.topFunctionsByCalls);
  addFunctionTable("Top Functions By Allocations", summary.topFunctionsByAllocations);
  addGroupTable("Category Hot Spots", summary.categoryBreakdown);
  addGroupTable("Path Hot Spots", summary.pathBreakdown);
  addTestTable("Top Tests By Opcodes", summary.topTestsByOpcodes);
  addTestTable("Top Tests By Allocations", summary.topTestsByAllocations);
  addTestTable("Top Tests By Duration", summary.topTestsByDuration);

  mkdirSync(dirname(outputPath), { recursive: true });
  writeFileSync(outputPath, `${lines.join("\n").trimEnd()}\n`);
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
  profileDir: string | null;
  profileSummaryOutput: string | null;
  profileMarkdownOutput: string | null;
  profileMode: ProfileMode;
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

// Use Number() rather than parseInt(): parseInt("4x") silently returns 4,
// hiding bad input; Number("4x") returns NaN so trailing junk is rejected.
// Empty input also fails because Number("") === 0 but the caller needs
// strict positivity.
function parseStrictInt(raw: string | undefined): number | null {
  const trimmed = (raw ?? "").trim();
  if (trimmed.length === 0) return null;
  const n = Number(trimmed);
  return Number.isInteger(n) ? n : null;
}

function parsePositiveInt(name: string, raw: string | undefined): number {
  const n = parseStrictInt(raw);
  if (n === null || n <= 0) {
    console.error(`${name} requires a positive integer, got: ${raw}`);
    process.exit(2);
  }
  return n;
}

function parseNonNegativeInt(name: string, raw: string | undefined): number {
  const n = parseStrictInt(raw);
  if (n === null || n < 0) {
    console.error(`${name} requires a non-negative integer, got: ${raw}`);
    process.exit(2);
  }
  return n;
}

function parseMode(raw: string | undefined): "interpreted" | "bytecode" {
  if (raw === "interpreted" || raw === "bytecode") return raw;
  console.error(`--mode requires one of {interpreted,bytecode}, got: ${raw}`);
  process.exit(2);
}

function parseProfileMode(raw: string | undefined): ProfileMode {
  if (raw === "opcodes" || raw === "functions" || raw === "all") return raw;
  console.error(`--profile-mode requires one of {opcodes,functions,all}, got: ${raw}`);
  process.exit(2);
}

function parseMainArgs(argv: string[]): MainArgs {
  const out: MainArgs = {
    suiteDir: null,
    output: null,
    profileDir: null,
    profileSummaryOutput: null,
    profileMarkdownOutput: null,
    profileMode: DEFAULT_PROFILE_MODE,
    filter: process.env.TEST262_FILTER || null,
    categories: DEFAULT_CATEGORIES,
    maxTests: parseInt(process.env.TEST262_MAX_TESTS || "0", 10),
    jobs: DEFAULT_JOBS,
    mode: "bytecode",
    timeoutMs: parsePositiveInt(
      "TEST262_TIMEOUT_MS",
      process.env.TEST262_TIMEOUT_MS || String(DEFAULT_TIMEOUT_MS),
    ),
    maxMemoryBytes: parsePositiveInt(
      "TEST262_MAX_MEMORY",
      process.env.TEST262_MAX_MEMORY || String(DEFAULT_MAX_MEMORY),
    ),
    verbose: false,
    bare: BARE,
  };
  if (process.env.TEST262_MAX_TESTS !== undefined) {
    out.maxTests = parseNonNegativeInt(
      "TEST262_MAX_TESTS",
      process.env.TEST262_MAX_TESTS,
    );
  }
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--suite-dir") out.suiteDir = argv[++i];
    else if (arg.startsWith("--suite-dir=")) out.suiteDir = arg.slice("--suite-dir=".length);
    else if (arg === "--output") out.output = argv[++i];
    else if (arg.startsWith("--output=")) out.output = arg.slice("--output=".length);
    else if (arg === "--profile-dir") out.profileDir = argv[++i];
    else if (arg.startsWith("--profile-dir=")) out.profileDir = arg.slice("--profile-dir=".length);
    else if (arg === "--profile-summary-output" || arg === "--profile-report-json") out.profileSummaryOutput = argv[++i];
    else if (arg.startsWith("--profile-summary-output=")) out.profileSummaryOutput = arg.slice("--profile-summary-output=".length);
    else if (arg.startsWith("--profile-report-json=")) out.profileSummaryOutput = arg.slice("--profile-report-json=".length);
    else if (arg === "--profile-markdown-output" || arg === "--profile-report-md" || arg === "--profile-report-markdown") out.profileMarkdownOutput = argv[++i];
    else if (arg.startsWith("--profile-markdown-output=")) out.profileMarkdownOutput = arg.slice("--profile-markdown-output=".length);
    else if (arg.startsWith("--profile-report-md=")) out.profileMarkdownOutput = arg.slice("--profile-report-md=".length);
    else if (arg.startsWith("--profile-report-markdown=")) out.profileMarkdownOutput = arg.slice("--profile-report-markdown=".length);
    else if (arg === "--profile-mode") out.profileMode = parseProfileMode(argv[++i]);
    else if (arg.startsWith("--profile-mode=")) out.profileMode = parseProfileMode(arg.slice("--profile-mode=".length));
    else if (arg === "--filter") out.filter = argv[++i];
    else if (arg.startsWith("--filter=")) out.filter = arg.slice("--filter=".length);
    else if (arg === "--categories") out.categories = argv[++i].split(",");
    else if (arg.startsWith("--categories=")) out.categories = arg.slice("--categories=".length).split(",");
    else if (arg === "--max-tests") out.maxTests = parseNonNegativeInt("--max-tests", argv[++i]);
    else if (arg.startsWith("--max-tests=")) out.maxTests = parseNonNegativeInt("--max-tests", arg.slice("--max-tests=".length));
    else if (arg === "--jobs") out.jobs = parsePositiveInt("--jobs", argv[++i]);
    else if (arg.startsWith("--jobs=")) out.jobs = parsePositiveInt("--jobs", arg.slice("--jobs=".length));
    else if (arg === "--mode") out.mode = parseMode(argv[++i]);
    else if (arg.startsWith("--mode=")) out.mode = parseMode(arg.slice("--mode=".length));
    else if (arg === "--timeout-ms") out.timeoutMs = parsePositiveInt("--timeout-ms", argv[++i]);
    else if (arg.startsWith("--timeout-ms=")) out.timeoutMs = parsePositiveInt("--timeout-ms", arg.slice("--timeout-ms=".length));
    else if (arg === "--max-memory") out.maxMemoryBytes = parsePositiveInt("--max-memory", argv[++i]);
    else if (arg.startsWith("--max-memory=")) out.maxMemoryBytes = parsePositiveInt("--max-memory", arg.slice("--max-memory=".length));
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
  if ((out.profileSummaryOutput || out.profileMarkdownOutput) && !out.profileDir) {
    throw new Error(
      "profile report output options require --profile-dir",
    );
  }
  if (out.profileDir && out.mode !== "bytecode") {
    throw new Error("--profile-dir requires --mode=bytecode because VM profiler output is bytecode-only");
  }
  if (out.profileDir) {
    out.profileSummaryOutput ??= join(out.profileDir, "aggregate.json");
    out.profileMarkdownOutput ??= join(out.profileDir, "aggregate.md");
  }
  return out;
}

function printUsage(): void {
  console.log(`Usage: bun scripts/run_test262_suite.ts [options]
       bun scripts/run_test262_suite.ts --comment <results.json> <baseline.json|->

Run mode options:
  --suite-dir DIR        Existing test262 checkout (clones if omitted).
  --output FILE          JSON output path for the full report.
  --profile-dir DIR      Enable VM profiling and write detailed per-test JSON
                         under DIR/per-test/<test-id>.profile.json.
  --profile-mode MODE    Profiler mode: opcodes, functions, all (default: all).
  --profile-summary-output FILE
                         Write aggregate profile summary JSON (default:
                         DIR/aggregate.json). Alias: --profile-report-json.
  --profile-markdown-output FILE
                         Write aggregate profile review Markdown (default:
                         DIR/aggregate.md). Alias: --profile-report-md.
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
        rmSync(tmp, { recursive: true, force: true });
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
    if (args.profileDir) {
      console.log(`Profile dir:   ${args.profileDir}`);
      console.log(`Profile mode:  ${args.profileMode}`);
      console.log(`Profile JSON:  ${args.profileSummaryOutput}`);
      console.log(`Profile MD:    ${args.profileMarkdownOutput}`);
    }
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
        profileDir: args.profileDir,
        profileMode: args.profileMode,
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

    if (args.profileDir && (args.profileSummaryOutput || args.profileMarkdownOutput)) {
      const profileSummary = buildProfileSummary(results, args.profileDir, {
        suiteDir: checkout.dir,
        test262Sha: process.env.TEST262_SUITE_SHA ?? null,
        mode: "bytecode",
        categories: [...args.categories],
        jobs: args.jobs,
        timeoutMs: args.timeoutMs,
        maxMemoryBytes: args.maxMemoryBytes,
        totalDiscovered: summary.totalDiscovered,
        totalRun: summary.totalRun,
        passed: summary.passed,
        failed: summary.failed,
        wrapperInfraFailures: summary.wrapperInfraFailures,
        timeouts: summary.timeouts,
        durationSeconds: summary.durationSeconds,
      });
      if (args.profileSummaryOutput) {
        mkdirSync(dirname(args.profileSummaryOutput), { recursive: true });
        writeFileSync(
          args.profileSummaryOutput,
          JSON.stringify(profileSummary, null, 2) + "\n",
        );
        console.log(`Profile summary written to: ${resolve(args.profileSummaryOutput)}`);
      }
      if (args.profileMarkdownOutput) {
        writeProfileMarkdown(profileSummary, args.profileMarkdownOutput);
        console.log(`Profile markdown written to: ${resolve(args.profileMarkdownOutput)}`);
      }
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
  wrapperInfraFailures?: number;
  timeouts?: number;
}

interface RegressionDelta {
  hasBaseline: boolean;
  /** current.passed - baseline.passed; equals current.passed when hasBaseline is false. */
  totalPassedDelta: number;
  /** Tests that were not PASS in baseline but are PASS in current. */
  newPasses: string[];
  /** Tests that were PASS in baseline but are now non-timeout failures. */
  newFails: string[];
  /** Tests that newly report TIMEOUT compared with baseline. */
  newTimeouts: string[];
  /** Tests that no longer report TIMEOUT compared with baseline. */
  resolvedTimeouts: string[];
}

function computeRegression(
  data: { summary: SuiteSummary; results: PerTestRecord[] } | null,
  baseline: { summary: BaselineSummary; results?: PerTestRecord[] } | null,
): RegressionDelta {
  const hasBaseline = !!(baseline && baseline.summary);
  if (!data) {
    return {
      hasBaseline,
      totalPassedDelta: 0,
      newPasses: [],
      newFails: [],
      newTimeouts: [],
      resolvedTimeouts: [],
    };
  }
  const baseTotalPassed = hasBaseline ? (baseline!.summary.passed ?? 0) : 0;
  const totalPassedDelta = hasBaseline
    ? data.summary.passed - baseTotalPassed
    : data.summary.passed;
  const newPasses: string[] = [];
  const newFails: string[] = [];
  const newTimeouts: string[] = [];
  const resolvedTimeouts: string[] = [];
  if (hasBaseline) {
    const prevById = new Map<string, Outcome>();
    for (const r of baseline!.results || []) prevById.set(r.id, r.status);
    for (const r of data.results) {
      const prev = prevById.get(r.id);
      if (!prev) continue;
      if (prev !== "TIMEOUT" && r.status === "TIMEOUT") newTimeouts.push(r.id);
      else if (prev === "TIMEOUT" && r.status !== "TIMEOUT") {
        resolvedTimeouts.push(r.id);
      } else if (prev !== "PASS" && r.status === "PASS") {
        newPasses.push(r.id);
      } else if (prev === "PASS" && r.status !== "PASS") {
        newFails.push(r.id);
      }
    }
  }
  return { hasBaseline, totalPassedDelta, newPasses, newFails, newTimeouts, resolvedTimeouts };
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
): { markdown: string; regressed: boolean } {
  const marker = "<!-- test262-results -->";
  let body = "## test262 Conformance\n\n";

  if (!data || !data.summary) {
    body +=
      "_test262 results were not produced for this run._\n\n" +
      "<sub>Non-blocking. The conformance job either timed out, crashed, or did not upload an artifact. See the workflow run for details.</sub>\n";
    return { markdown: marker + "\n" + body, regressed: false };
  }

  const s = data.summary;
  const delta = computeRegression(data, baseline);
  const hasBaseline = delta.hasBaseline;
  const regressed = hasBaseline && delta.newFails.length > 0;

  if (regressed) {
    body += `> 🚫 **Regression vs cached \`main\` baseline.** ${delta.newFails.length} previously-passing test(s) now fail; pass count Δ ${delta.totalPassedDelta >= 0 ? "+" : ""}${delta.totalPassedDelta}. This run blocks merge — see "Newly failing" below.\n\n`;
  }
  const baselineByCat = new Map<string, CategorySummary>();
  if (hasBaseline) {
    for (const c of baseline!.summary.byCategory || []) {
      baselineByCat.set(c.category, c);
    }
  }

  if (s.wrapperInfraFailures > 0) {
    body += `> ⚠️ **${s.wrapperInfraFailures} wrapper infrastructure failure(s)** — conformance numbers untrustworthy until fixed.\n\n`;
  }
  if (s.timeouts > 0) {
    body += `> **${s.timeouts} timeout(s)** — counted separately from ordinary conformance failures; inspect the full artifact for affected tests.\n\n`;
  }

  body += "| Category | Run | Passed | Δ Pass | Failed | Timeouts | Δ Timeout | Wrap-Infra | Pass-rate | Δ Rate |\n";
  body += "|----------|----:|-------:|-------:|-------:|---------:|----------:|-----------:|----------:|-------:|\n";
  for (const c of s.byCategory) {
    const rate = c.run > 0 ? c.passed / c.run : 0;
    const ba = baselineByCat.get(c.category);
    const dPass = ba ? c.passed - ba.passed : c.passed;
    const dTimeout = ba ? c.timeouts - (ba.timeouts ?? 0) : c.timeouts;
    const baseRate = ba && ba.run > 0 ? ba.passed / ba.run : null;
    body += `| \`${c.category}\` | ${fmt(c.run)} | ${fmt(c.passed)} | ${ba ? signed(dPass) : "🆕"} | ${fmt(c.failed)} | ${fmt(c.timeouts)} | ${ba ? signed(dTimeout) : "🆕"} | ${fmt(c.wrapperInfra)} | ${pct(c.passed, c.run)} | ${signedPp(rate, baseRate)} |\n`;
  }
  // Totals row
  const totalRate = s.totalRun > 0 ? s.passed / s.totalRun : 0;
  const baseTotalRun = (baseline?.summary.totalRun) ?? 0;
  const baseTotalPassed = (baseline?.summary.passed) ?? 0;
  const baseTotalTimeouts = (baseline?.summary.timeouts) ?? 0;
  const baseTotalRate = baseTotalRun > 0 ? baseTotalPassed / baseTotalRun : null;
  body += `| **total** | ${fmt(s.totalRun)} | ${fmt(s.passed)} | ${hasBaseline ? signed(delta.totalPassedDelta) : "🆕"} | ${fmt(s.failed)} | ${fmt(s.timeouts)} | ${hasBaseline ? signed(s.timeouts - baseTotalTimeouts) : "🆕"} | ${fmt(s.wrapperInfraFailures)} | ${pct(s.passed, s.totalRun)} | ${signedPp(totalRate, baseTotalRate)} |\n\n`;

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
  if (
    hasBaseline &&
    (delta.newPasses.length > 0 ||
      delta.newFails.length > 0 ||
      delta.newTimeouts.length > 0 ||
      delta.resolvedTimeouts.length > 0)
  ) {
    body += `<details${regressed ? " open" : ""}>\n<summary>Per-test deltas (+${delta.newPasses.length} / -${delta.newFails.length} / timeout +${delta.newTimeouts.length} / -${delta.resolvedTimeouts.length})</summary>\n\n`;
    if (delta.newFails.length > 0) {
      body += `**Newly failing (${delta.newFails.length}):**\n\n`;
      for (const id of delta.newFails.slice(0, 100)) body += `- \`${id}\`\n`;
      if (delta.newFails.length > 100) body += `- _… ${delta.newFails.length - 100} more_\n`;
      body += "\n";
    }
    if (delta.newPasses.length > 0) {
      body += `**Newly passing (${delta.newPasses.length}):**\n\n`;
      for (const id of delta.newPasses.slice(0, 100)) body += `- \`${id}\`\n`;
      if (delta.newPasses.length > 100) body += `- _… ${delta.newPasses.length - 100} more_\n`;
      body += "\n";
    }
    if (delta.newTimeouts.length > 0) {
      body += `**New timeouts (${delta.newTimeouts.length}):**\n\n`;
      for (const id of delta.newTimeouts.slice(0, 100)) body += `- \`${id}\`\n`;
      if (delta.newTimeouts.length > 100) body += `- _… ${delta.newTimeouts.length - 100} more_\n`;
      body += "\n";
    }
    if (delta.resolvedTimeouts.length > 0) {
      body += `**Resolved timeouts (${delta.resolvedTimeouts.length}):**\n\n`;
      for (const id of delta.resolvedTimeouts.slice(0, 100)) body += `- \`${id}\`\n`;
      if (delta.resolvedTimeouts.length > 100) body += `- _… ${delta.resolvedTimeouts.length - 100} more_\n`;
      body += "\n";
    }
    body += "</details>\n\n";
  }

  const baselineNote = hasBaseline
    ? " Δ vs main compares against the most recent cached `main` baseline."
    : " No `main` baseline cached yet — Δ columns will appear once a `main` run completes.";
  body += `<sub>Steady-state failures and timeouts are non-blocking; PASS → non-timeout failure transitions fail the conformance gate. Measured on ubuntu-latest x64, bytecode mode. Areas grouped by the first two test262 path components; minimum 25 attempted tests, areas already at 100% excluded.${baselineNote}</sub>\n`;

  return { markdown: marker + "\n" + body, regressed };
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
  const { markdown, regressed } = buildCommentMarkdown(data, baseline);
  process.stdout.write(markdown);
  if (regressed) {
    // Single-line CI annotation; the per-test list is already in the
    // markdown comment posted to the PR, so don't repeat it here.
    console.error(
      "::error title=test262 regression::A previously-passing test now fails. See the PR comment for per-test deltas.",
    );
    return 1;
  }
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
