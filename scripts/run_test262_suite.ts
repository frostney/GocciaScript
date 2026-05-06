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
//
// test262 frontmatter is YAML, but the orchestrator only consumes three
// fields: `flags` (e.g. async / module / raw), `includes` (extra harness
// files), and `negative.{phase, type}`.  Free-form fields like `info`,
// `description`, `esid`, `features` are not read by the orchestrator —
// and `info` in particular routinely contains unbalanced brackets like
// `String.fromCharCode ( [ char0 [ , char1 [ , ... ] ] ] )` that Bun's
// strict YAML parser rejects.
//
// Strategy: line-walk the frontmatter block, copy lines that belong to
// `flags`, `includes`, or `negative` (top-level key + its indented
// continuation), and drop everything else.  Pass the cleaned text to
// Bun's built-in YAML parser.  No regex; no custom YAML; we just trim
// the input to what we need before handing it to a real YAML parser.

const KEPT_KEYS = new Set(["flags", "includes", "negative"]);
const FRONTMATTER_OPEN = "/*---";
const FRONTMATTER_CLOSE = "---*/";

interface Frontmatter {
  flags: string[];
  includes: string[];
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
    return { body: source, meta: { flags: [], includes: [] } };
  }
  const blockStart = source.indexOf("\n", open) + 1;
  const yaml = source.slice(blockStart, close).trimEnd();
  const cleaned = stripUnreadFields(yaml);
  const parsed = (Bun.YAML.parse(cleaned) || {}) as any;

  const before = source.slice(0, open).trimEnd();
  const after = source.slice(close + FRONTMATTER_CLOSE.length).replace(/^\n+/, "");
  const body = (before + "\n" + after).replace(/^\n+/, "");

  return {
    body,
    meta: {
      flags: toStringArray(parsed.flags),
      includes: toStringArray(parsed.includes),
      negative:
        parsed.negative && typeof parsed.negative === "object"
          ? { phase: parsed.negative.phase, type: parsed.negative.type }
          : undefined,
    },
  };
}

// Walk the frontmatter line-by-line; copy through any block whose
// top-level key is one we care about, drop the rest.  A "block" is a
// line that starts in column 0 with `<key>:` plus all following lines
// indented under it.
function stripUnreadFields(yaml: string): string {
  const out: string[] = [];
  let inKept = false;
  for (const line of yaml.split("\n")) {
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

// Stock test262 harness JS depends on language features GocciaScript
// intentionally excludes (`arguments`, `with`, traditional
// `for (var i = 0; ...)` and `while` loops — Goccia's parser warns and
// then silently drops these constructs).  Loading affected stock files
// produces silently-broken helpers — `propertyHelper.verifyProperty` etc.
// throw `ReferenceError: arguments is not defined`, `decimalToHexString`
// returns wrong values because its loop body never runs, and so on.
// Tests that include those files then fail with harness-environment
// errors, not engine surface differences.
//
// To keep conformance numbers a faithful signal of engine behavior,
// `BUNDLED_INCLUDES` maps the test262 include names that need it to
// GocciaScript-compatible reimplementations under `scripts/test262_harness/`.
// Anything NOT listed here loads stock from the test262 checkout's
// `harness/` directory directly.
//
// To minimise our surface against the stock harness, only files that
// were empirically shown to break are bundled.  Each entry has a one-line
// rationale; if a future stock harness change makes a bundled file
// unnecessary, delete the entry and the corresponding file.
const BUNDLED_INCLUDES: Record<string, string> = {
  // $262 host hooks (detachArrayBuffer, etc.).  Always loaded before all
  // other harness files so stock includes like detachArrayBuffer.js find
  // the methods they expect.  No stock equivalent — this is host-provided.
  "$262.js": "$262.js",
  // Subsumes stock sta.js + assert.js + compareArray.js.  Stock assert.js
  // uses `arguments` in `assert.compareArray` and a `for (var i = 0; ...)`
  // loop body that Goccia's parser drops.
  "assert.js": "assert.js",
  "sta.js": "assert.js",
  "compareArray.js": "assert.js",
  // Stock uses `arguments` (3x) and `for (var i = 0; ...)` (1x) in
  // `verifyProperty`; without adaptation every property-descriptor test
  // fails before reaching the actual conformance check.
  "propertyHelper.js": "propertyHelper.js",
  // Stock uses `arguments` (1x) and `for (var i = 0; ...)` (2x) in
  // `assert.deepEqual`'s recursive comparator.
  "deepEqual.js": "deepEqual.js",
  // Stock uses `arguments` (3x) for variadic helpers used by every
  // Temporal test.
  "temporalHelpers.js": "temporalHelpers.js",
  // Stock uses `for (var i = 0; ...)` (7x) and `with (...)` (2x) in
  // `testWithTypedArrayConstructors` — the principal entry point used
  // by every TypedArray conformance test.
  "testTypedArray.js": "testTypedArray.js",
  "testBigIntTypedArray.js": "testTypedArray.js",
  // Stock uses `arguments` and `for (var i = 0; ...)` in the intrinsics
  // walker.
  "wellKnownIntrinsicObjects.js": "wellKnownIntrinsicObjects.js",
  // Stock uses `for (...)` loop bodies (Goccia parses them as a warning
  // then drops the body) in iterator comparators.
  "compareIterator.js": "compareIterator.js",
  // Stock uses `while (...)` for the digit-conversion loop; without it
  // the function returns wrong results silently.
  "decimalToHexString.js": "decimalToHexString.js",
  // Stock uses `while (...)` in the matcher loop.
  "nativeFunctionMatcher.js": "nativeFunctionMatcher.js",
  // Stock uses `for (...)` over RegExp result iterators.
  "regExpUtils.js": "regExpUtils.js",
  // Engine bug #516: stock probes constructor-ness via
  // `Reflect.construct(function(){}, [], f)`, but Goccia rejects function
  // expressions as the proxy target.  Adapted version uses
  // `Reflect.construct(class {}, [], f)` which Goccia accepts.  Delete
  // this entry and the bundled file when #516 is fixed.
  "isConstructor.js": "isConstructor.js",
  // Engine bug #517: stock uses `Function("return this;")()` but Goccia's
  // `Function` constructor doesn't bind `this` to the global, so the
  // stock helper returns the Function instance instead of globalThis.
  // Adapted version uses `() => globalThis`.  Delete this entry and the
  // bundled file when #517 is fixed.
  "fnGlobalObject.js": "fnGlobalObject.js",
  // Engine bug #518: stock would `print('Test262:AsyncTestComplete')` from
  // inside `$DONE`, but Goccia's bytecode VM has a Range check error in
  // the top-level `Promise.then` continuation drain (exercised by every
  // test written like `p.then(v => $DONE())`).  The bundled adaptation
  // routes completion through `__donePromise`; the `positive_async`
  // wrapper awaits it inside an async IIFE (which drains via the VM's
  // continuation machinery, not the broken top-level path) and prints
  // the markers itself.  Delete this entry, the bundled file, and the
  // async-IIFE wrapper template when #518 is fixed.
  "doneprintHandle.js": "doneprintHandle.js",
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
   * Always prepend `$262.js` (host hooks) and `assert.js` (which subsumes
   * stock `sta.js`, `assert.js`, and `compareArray.js`).  Then append
   * every name from the test's `includes:` list, dedup'd against names
   * already covered by the prepended bundle.  The `raw` flag (caller's
   * responsibility) skips this entirely.
   */
  async build(includes: string[]): Promise<string> {
    const seen = new Set<string>(["assert.js", "sta.js", "compareArray.js"]);
    const parts: string[] = [
      await this.read("$262.js"),
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
    // For the error-class identification we prefer `e.constructor.name`
    // (the spec-canonical path) but fall back to `e.name` because
    // Goccia's native Error class hierarchy is missing
    // `prototype.constructor` (#519) — caught Errors have
    // `e.constructor === undefined` despite `e.name` being set
    // correctly to "TypeError" / "ReferenceError" / etc.
    return `${harnessSource}
try {
${body}
  print("Test262:NegativeTestNoError");
} catch (__gocciaT262_e) {
  var __gocciaT262_n = "unknown";
  if (__gocciaT262_e && typeof __gocciaT262_e === "object") {
    if (__gocciaT262_e.constructor && __gocciaT262_e.constructor.name) {
      __gocciaT262_n = __gocciaT262_e.constructor.name;
    } else if (typeof __gocciaT262_e.name === "string") {
      __gocciaT262_n = __gocciaT262_e.name;
    }
  }
  print("Test262:NegativeTestError:" + __gocciaT262_n);
}
`;
  }
  if (opts.kind === "positive_async") {
    // The bundled doneprintHandle.js declares __donePromise; route completion
    // through it via an async IIFE so the await drains the body's microtasks
    // through the VM's continuation machinery (which works in bytecode
    // mode), and emit the stock marker strings from this wrapper rather
    // than from $DONE itself.  See scripts/test262_harness/doneprintHandle.js.
    //
    // The `;` between body and IIFE is mandatory: many test262 async test
    // bodies end with `foo().then(function() { $DONE(); })` (no trailing
    // semicolon — relying on ASI), and the leading `(` of our IIFE would
    // otherwise be parsed as a call on the body's last expression
    // (`then(...)(async () => ...)`), producing "object is not a function".
    return `${harnessSource}
${body};
(async () => {
  try {
    await __donePromise;
    print("Test262:AsyncTestComplete");
  } catch (__gocciaT262_e) {
    if (__gocciaT262_e && typeof __gocciaT262_e === "object" && "name" in __gocciaT262_e) {
      print("Test262:AsyncTestFailure:" + __gocciaT262_e.name + ": " + __gocciaT262_e.message);
    } else {
      print("Test262:AsyncTestFailure:Test262Error: " + String(__gocciaT262_e));
    }
  }
})();
`;
  }
  // positive_sync
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
  let timedOut = false;
  const timer = setTimeout(() => {
    timedOut = true;
    ac.abort();
  }, wallClockMs);
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

function parseMainArgs(argv: string[]): MainArgs {
  const out: MainArgs = {
    suiteDir: null,
    output: null,
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

interface RegressionDelta {
  hasBaseline: boolean;
  /** current.passed - baseline.passed; equals current.passed when hasBaseline is false. */
  totalPassedDelta: number;
  /** Tests that were not PASS in baseline but are PASS in current. */
  newPasses: string[];
  /** Tests that were PASS in baseline but are not PASS in current. */
  newFails: string[];
}

function computeRegression(
  data: { summary: SuiteSummary; results: PerTestRecord[] } | null,
  baseline: { summary: BaselineSummary; results?: PerTestRecord[] } | null,
): RegressionDelta {
  const hasBaseline = !!(baseline && baseline.summary);
  if (!data) return { hasBaseline, totalPassedDelta: 0, newPasses: [], newFails: [] };
  const baseTotalPassed = hasBaseline ? (baseline!.summary.passed ?? 0) : 0;
  const totalPassedDelta = hasBaseline
    ? data.summary.passed - baseTotalPassed
    : data.summary.passed;
  const newPasses: string[] = [];
  const newFails: string[] = [];
  if (hasBaseline) {
    const prevById = new Map<string, Outcome>();
    for (const r of baseline!.results || []) prevById.set(r.id, r.status);
    for (const r of data.results) {
      const prev = prevById.get(r.id);
      if (!prev) continue;
      if (prev !== "PASS" && r.status === "PASS") newPasses.push(r.id);
      else if (prev === "PASS" && r.status !== "PASS") newFails.push(r.id);
    }
  }
  return { hasBaseline, totalPassedDelta, newPasses, newFails };
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
  const regressed =
    hasBaseline && (delta.totalPassedDelta < 0 || delta.newFails.length > 0);

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
  body += `| **total** | ${fmt(s.totalRun)} | ${fmt(s.passed)} | ${hasBaseline ? signed(delta.totalPassedDelta) : "🆕"} | ${fmt(s.failed)} | ${pct(s.passed, s.totalRun)} | ${signedPp(totalRate, baseTotalRate)} |\n\n`;

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
  if (hasBaseline && (delta.newPasses.length > 0 || delta.newFails.length > 0)) {
    body += `<details${regressed ? " open" : ""}>\n<summary>Per-test deltas (+${delta.newPasses.length} / -${delta.newFails.length})</summary>\n\n`;
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
    body += "</details>\n\n";
  }

  const baselineNote = hasBaseline
    ? " Δ vs main compares against the most recent cached `main` baseline."
    : " No `main` baseline cached yet — Δ columns will appear once a `main` run completes.";
  body += `<sub>Steady-state failures are non-blocking; regressions vs the cached main baseline (lower total pass count, or any PASS → non-PASS transition) fail the conformance gate. Measured on ubuntu-latest x64, bytecode mode. Areas grouped by the first two test262 path components; minimum 25 attempted tests, areas already at 100% excluded.${baselineNote}</sub>\n`;

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
      "::error title=test262 regression::Pass count dropped or a previously-passing test now fails. See the PR comment for per-test deltas.",
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
