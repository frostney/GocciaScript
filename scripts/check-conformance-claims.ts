#!/usr/bin/env npx tsx
/**
 * check-conformance-claims.ts
 *
 * Governing rule: no hand-maintained conformance number anywhere. A test262
 * pass rate, pass count, or percentage typed into prose is stale the moment
 * the next main-branch run lands — the "80%" that outlived the real figure by
 * months is the failure mode this check exists to stop. Conformance figures
 * come from the generated reports: link `/compatibility`, or render the number
 * from live dashboard data.
 *
 * Scans markdown prose (every root-level `.md` except the excluded historical
 * records, every `.md` under docs/, and the website's root `.md` files) and
 * website copy (every `.ts` and `.tsx` under website/src/) for a percentage or
 * an absolute pass count sitting next to test262 vocabulary. Exits non-zero if
 * any hand-typed claim is found.
 *
 * What convicts a number:
 *   - test262 vocabulary within +/- 3 lines, AND
 *   - pass- or conformance-strength vocabulary within +/- 3 lines.
 * Generic words alone (corpus, coverage, compatible) never convict, so an
 * unrelated percentage does not start failing because an edit moved a test262
 * sentence three lines closer to it.
 *
 * Not checked (by design):
 *   - docs/adr/ and docs/spikes/ — immutable, dated measurement records
 *   - CHANGELOG.md — generated release history, not maintained prose
 *   - Fenced (``` and ~~~) and indented code blocks, inline code spans, and
 *     link targets — sample report output, CLI flags, and pinned SHAs are not
 *     prose claims
 *   - Website test files — synthetic fixtures, not published copy
 *   - Percentages computed or formatted from live data: the extraction drops
 *     `${...}` expressions, so a rendered figure leaves no literal behind
 *   - Letterless literals in a CSS or layout position (`width: "100%"`)
 *   - Non-test262 suite snapshots (YAML, TOML, JSON5) and benchmark deltas —
 *     those are allowed when version-stamped, so the check requires test262
 *     vocabulary before it fires
 *   - `100%` / `0%` written as a ceiling or floor ("closest to 100%", "below
 *     100%"). A rate hand-typed as an achieved result ("raised the pass rate
 *     to 100%") is still a claim and is reported.
 *   - Totals and denominators ("the corpus holds 52,233 tests") — the count
 *     rule reports the numerator, not the corpus size
 *
 * Written in GocciaScript-compatible style (arrow functions, const/let,
 * for...of, no var, strict equality) so it can be bootstrapped later.
 *
 * Usage:
 *   npx tsx scripts/check-conformance-claims.ts
 *   npx tsx scripts/check-conformance-claims.ts --verbose
 *   npx tsx scripts/check-conformance-claims.ts --self-test
 */

import { readFileSync, readdirSync, existsSync, lstatSync, realpathSync } from "fs";
import { join, relative, dirname } from "path";
import { fileURLToPath } from "url";

// -- Config -------------------------------------------------------------------

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const VERBOSE = process.argv.includes("--verbose");
const SELF_TEST = process.argv.includes("--self-test");

// Root-level prose is scanned wholesale so a new entry-point document is
// covered the day it lands. CHANGELOG.md is generated release history — its
// entries describe what a release measured at the time and are never rewritten.
const EXCLUDED_ROOT_MD = new Set(["CHANGELOG.md"]);
const MD_DIR = "docs";
// Historical records: an ADR or spike states what was measured on a given date
// and is never updated afterwards, so a frozen figure there is the point.
const MD_IGNORE_PATH_PREFIXES = ["docs/adr/", "docs/spikes/"];
const WEB_ROOT = "website";
const WEB_SRC_DIR = "website/src";
const WEB_EXTENSIONS = [".ts", ".tsx"];
// Fixtures are invented numbers that assert formatting, not claims about the engine.
const WEB_IGNORE_PATTERNS = [/(?:^|\/)__tests__\//, /\.test\.tsx?$/, /\.spec\.tsx?$/];

// Deliberate exemptions. Add an entry only for a figure that is version-stamped
// and sourced — never to quiet a stale number. Each entry needs a reason.
// An inline `conformance-claim-ok` marker does the same thing for one-off
// cases: put it on the claim line, or on the nearest non-blank line above it
// (a blank line in between is fine, so the marker can sit on its own).
//   <!-- conformance-claim-ok: figure from the 2026-08-01 pinned run -->
//   // conformance-claim-ok: figure from the 2026-08-01 pinned run
interface Allowance {
  file: string;      // repo-relative path
  pattern: RegExp;   // must match the flagged line
  reason: string;
}

const ALLOWLIST: Allowance[] = [];

const INLINE_MARKER = "conformance-claim-ok";

// -- Vocabulary ---------------------------------------------------------------

// The rule is about test262 specifically. Other suites (yaml-test-suite,
// toml-test, JSON5) are allowed to carry version-stamped snapshots, so a
// figure only becomes a finding when test262 is what is being described.
const TEST262_VOCAB = /test\s?-?262|tc39\s+(?:conformance|suite)/i;

// The claim side. Only pass- and conformance-strength words convict: "corpus",
// "coverage", "score", and "compatible" are ambient in this repository, and a
// percentage about something else must not become a finding because one of
// them happens to sit in the window.
const CLAIM_VOCAB = /\bpass(?:es|ed|ing)?\b|\bpass[-\s]?rate\b|\bconform(?:ance|ant|s|ing)\b/i;

// Counts are noisier than percentages (timeouts, job counts, SHAs), so they
// additionally need a pass word in the window.
const PASS_WORD = /\bpass(?:es|ed|ing)?\b/i;

// How many lines on each side count as context. Markdown wraps mid-sentence and
// JSX splits a sentence across several lines, so a claim's vocabulary often
// sits on a neighbouring line.
const CONTEXT_LINES = 3;

// -- Path rules ---------------------------------------------------------------

const toRepoPath = (full: string): string => relative(ROOT, full).split("\\").join("/");

/**
 * Single source of truth for what the check covers, so the self-test can
 * assert the exclusions without touching the filesystem.
 */
const shouldScan = (repoPath: string): boolean => {
  if (repoPath.endsWith(".md")) {
    const segments = repoPath.split("/");
    if (segments.length === 1) return !EXCLUDED_ROOT_MD.has(repoPath);
    if (segments.length === 2 && segments[0] === WEB_ROOT) return !EXCLUDED_ROOT_MD.has(segments[1]);
    if (repoPath.startsWith(`${MD_DIR}/`)) {
      return !MD_IGNORE_PATH_PREFIXES.some((prefix) => repoPath.startsWith(prefix));
    }
    return false;
  }

  if (WEB_EXTENSIONS.some((ext) => repoPath.endsWith(ext))) {
    if (!repoPath.startsWith(`${WEB_SRC_DIR}/`)) return false;
    return !WEB_IGNORE_PATTERNS.some((pattern) => pattern.test(repoPath));
  }

  return false;
};

// -- File discovery -----------------------------------------------------------

const collectFiles = (): string[] => {
  const files: string[] = [];
  const seen = new Set<string>();

  const add = (full: string): void => {
    if (!shouldScan(toRepoPath(full))) return;
    // AGENTS.md and CLAUDE.md are the same document behind a symlink.
    const real = lstatSync(full).isSymbolicLink() ? realpathSync(full) : full;
    if (seen.has(real)) return;
    seen.add(real);
    files.push(full);
  };

  const walk = (dir: string, recurse: boolean): void => {
    if (!existsSync(dir)) return;
    for (const entry of readdirSync(dir, { withFileTypes: true })) {
      const full = join(dir, entry.name);
      if (entry.isDirectory()) {
        if (!recurse) continue;
        if (entry.name === "node_modules" || entry.name === ".next") continue;
        walk(full, true);
      } else {
        add(full);
      }
    }
  };

  walk(ROOT, false);
  walk(join(ROOT, WEB_ROOT), false);
  walk(join(ROOT, MD_DIR), true);
  walk(join(ROOT, WEB_SRC_DIR), true);

  return files;
};

// -- Prose extraction ---------------------------------------------------------

/**
 * A line's readable spans. An empty array means the line carries no prose
 * (code block, pure code, blank).
 */
type LineSpans = string[];

/**
 * Markdown: everything outside fenced (``` / ~~~) and indented code blocks,
 * inline code spans, and link targets. A CLI flag (`--timeout-ms=20000`) or a
 * report filename is not a claim about the engine.
 */
const markdownProse = (lines: string[]): LineSpans[] => {
  const prose: LineSpans[] = [];
  let fence: string | null = null;
  // Four-space indentation is a code block only outside list bodies, where the
  // same indentation is ordinary continuation prose.
  let inList = false;

  for (const line of lines) {
    const fenceMatch = line.trimStart().match(/^(```+|~~~+)/);
    if (fenceMatch) {
      const marker = fenceMatch[1][0];
      if (fence === null) fence = marker;
      else if (fence === marker) fence = null;
      prose.push([]);
      continue;
    }
    if (fence !== null) {
      prose.push([]);
      continue;
    }

    const indented = /^(?: {4,}|\t)/.test(line);
    if (line.trim() !== "") {
      if (/^\s*(?:[-*+]|\d+[.)])\s/.test(line)) inList = true;
      else if (!indented) inList = false;
    }
    if (indented && !inList) {
      prose.push([]);
      continue;
    }

    prose.push([
      line
        .replace(/`[^`]*`/g, " ")           // inline code spans
        .replace(/\]\([^)]*\)/g, "] ")      // link targets
        .replace(/https?:\/\/\S+/g, " "),   // bare URLs
    ]);
  }

  return prose;
};

/**
 * TypeScript / TSX: the parts a reader sees — string-literal contents and JSX
 * text. Expressions are dropped, so a percentage computed from live data
 * (`${(rate * 100).toFixed(1)}%`) leaves no literal behind, while a typed-out
 * figure survives.
 *
 * Letterless literals are kept: `{ label: "test262 pass rate", value: "88.4%" }`
 * is the idiomatic stat-object shape, and the number lives in a span of its
 * own with the vocabulary on the neighbouring line.
 */
const sourceProse = (lines: string[]): LineSpans[] => {
  const prose: LineSpans[] = [];
  const literalRe = /"((?:[^"\\]|\\.)*)"|'((?:[^'\\]|\\.)*)'|`((?:[^`\\]|\\.)*)`/g;

  for (const line of lines) {
    const spans: string[] = [];

    for (const m of line.matchAll(literalRe)) {
      const literal = m[1] ?? m[2] ?? m[3] ?? "";
      const text = literal.replace(/\$\{[^}]*\}/g, " ");
      if (text.trim() === "") continue;
      // A letterless literal is only copy when it sits in a data position. In a
      // layout position it is a CSS value, and in a computed expression it is a
      // formatter's fallback (`run <= 0 ? "0.0%" : ...`) rather than a figure.
      if (!/[A-Za-z]/.test(text) && (CSS_CONTEXT.test(line) || COMPUTED_CONTEXT.test(line))) continue;
      spans.push(text);
    }

    const jsxText = line
      .replace(literalRe, " ")
      .replace(/\{[^}]*\}/g, " ")
      .replace(/<[^>]*>?/g, " ");
    if (jsxText.trim() !== "") spans.push(jsxText);

    prose.push(spans);
  }

  return prose;
};

// Expression positions: a letterless literal here is produced or chosen by
// code, and the visible number comes from the computation next to it.
const COMPUTED_CONTEXT =
  /\breturn\b|\?|=>|[<>!=]=|\|\||&&|\$\{|\.(?:toFixed|toLocaleString|replace|map|join|padStart)\b/;

// Layout and styling positions, where `"100%"` is a dimension.
const CSS_CONTEXT =
  /\b(?:width|height|top|left|right|bottom|inset|padding|margin|gap|flex|basis|size|opacity|background|gradient|transform|translate|scale|rotate|stroke|fill|color|colour|radius|offset|viewBox|className|class|style|stop|spread|blur)\b/i;

// -- Claim detection ----------------------------------------------------------

const PERCENT_RE = /\d{1,3}(?:\.\d+)?\s*(?:%|percent\b)/gi;
const COUNT_RE = /\d{1,3}(?:[,\u202F]\d{3})+|\d{4,}/g;

// A ceiling or floor describes the scale, not a measurement: "Areas closest to
// 100%", "**below 100%**". Bare "to"/"reached" are deliberately absent — "the
// pass rate is up to 100%" is a target, but "raised the pass rate to 100%" is a
// hand-typed result and must be reported. Markdown emphasis and quotes can sit
// between the qualifier and the number, so trailing non-word characters are
// allowed.
const CEILING_PREFIX =
  /(?:closest|close|nearest|next)\s+to\W*$|\b(?:below|under|above|over|beyond|toward|towards|up\s+to|short\s+of|approaching|nearing)\W*$/i;

// The corpus size is a denominator, not a pass count. "passes 41,765 of 52,233"
// must report 41,765 and stay quiet about the total.
const DENOMINATOR_PREFIX =
  /\b(?:of|out\s+of|contains?|containing|holds?|holding|comprises?|comprising|includes?|including|totals?|across|among|amongst|size\s+of)\W*$|\/\s*$/i;

const DATE_LIKE = /\d{4}-\d{2}-\d{2}/;
const COUNT_SUFFIX_UNITS = /^(?:ms|s|px|x|MB|KB|GB|kb|mb|gb|bit|bits|Hz)\b/;

interface Finding {
  file: string;
  line: number;
  claim: string;
  snippet: string;
  reason: string;
}

const findClaimsInSpan = (span: string, countsAllowed: boolean): { claim: string; reason: string }[] => {
  const claims: { claim: string; reason: string }[] = [];
  const textBefore = (index: number): string => span.slice(Math.max(0, index - 40), index);
  const textAfter = (index: number): string => span.slice(index, index + 8);

  for (const m of span.matchAll(PERCENT_RE)) {
    const value = Number(m[0].replace(/\s*(?:%|percent)$/i, ""));
    if ((value === 100 || value === 0) && CEILING_PREFIX.test(textBefore(m.index))) continue;
    claims.push({
      claim: m[0].replace(/\s+/g, " ").trim(),
      reason: "hand-typed conformance percentage",
    });
  }

  if (countsAllowed) {
    for (const m of span.matchAll(COUNT_RE)) {
      const digits = m[0];
      const before = textBefore(m.index);
      const after = textAfter(m.index + digits.length);
      // Version pins, flag values, SHAs, paths, and decimal tails.
      if (/[=#/\w.-]$/.test(before)) continue;
      if (/^[\d.-]/.test(after)) continue;
      if (DATE_LIKE.test(`${before.slice(-6)}${digits}${after}`)) continue;
      // Unit-suffixed numbers are durations and sizes, not corpus counts.
      if (COUNT_SUFFIX_UNITS.test(after.trimStart())) continue;
      // Bare four-digit years read as dates.
      if (/^(?:19|20)\d{2}$/.test(digits)) continue;
      // Corpus totals and denominators.
      if (DENOMINATOR_PREFIX.test(before)) continue;
      claims.push({ claim: digits, reason: "hand-typed conformance pass count" });
    }
  }

  return claims;
};

// -- Allowlist ----------------------------------------------------------------

interface Allowed {
  reason: string;
}

const markerReason = (line: string): string => {
  const idx = line.indexOf(INLINE_MARKER);
  if (idx === -1) return "";
  return line
    .slice(idx + INLINE_MARKER.length)
    .replace(/-->|\*\/|`/g, "")
    .replace(/^[\s:—-]+/, "")
    .trim();
};

/**
 * The marker counts on the claim line or on the nearest non-blank line above
 * it. Markdown wants a blank line around an HTML comment, so requiring strict
 * adjacency would make the documented form fail.
 */
const allowanceFor = (repoPath: string, lines: string[], index: number): Allowed | null => {
  const entry = ALLOWLIST.find(
    (candidate) => candidate.file === repoPath && candidate.pattern.test(lines[index]),
  );
  if (entry) return { reason: entry.reason };

  if (lines[index].includes(INLINE_MARKER)) {
    return { reason: markerReason(lines[index]) || "inline marker" };
  }

  for (let i = index - 1; i >= 0; i--) {
    if (lines[i].trim() === "") continue;
    if (lines[i].includes(INLINE_MARKER)) {
      return { reason: markerReason(lines[i]) || "inline marker" };
    }
    break;
  }

  return null;
};

// -- Analysis -----------------------------------------------------------------

const analyzeContent = (repoPath: string, content: string): Finding[] => {
  const lines = content.split("\n");
  const prose = repoPath.endsWith(".md") ? markdownProse(lines) : sourceProse(lines);
  const findings: Finding[] = [];

  for (const [i, spans] of prose.entries()) {
    if (spans.length === 0) continue;

    const from = Math.max(0, i - CONTEXT_LINES);
    const to = Math.min(lines.length, i + CONTEXT_LINES + 1);
    const context = lines.slice(from, to).join("\n");

    if (!TEST262_VOCAB.test(context)) continue;
    if (!CLAIM_VOCAB.test(context)) continue;

    const countsAllowed = PASS_WORD.test(context);
    const claims = spans.flatMap((span) => findClaimsInSpan(span, countsAllowed));
    if (claims.length === 0) continue;

    const allowed = allowanceFor(repoPath, lines, i);
    if (allowed !== null) {
      if (VERBOSE) {
        console.log(`  ALLOW ${repoPath}:${i + 1} \u2014 ${claims[0].claim} (${allowed.reason})`);
      }
      continue;
    }

    // One finding per line: the line is the unit a contributor rewrites, and a
    // table row would otherwise report its rate and both of its counts.
    findings.push({
      file: repoPath,
      line: i + 1,
      claim: claims[0].claim,
      snippet: lines[i].trim().slice(0, 100),
      reason: claims[0].reason,
    });
  }

  return findings;
};

// -- Self-test ----------------------------------------------------------------

interface SelfTestCase {
  name: string;
  path: string;
  content: string;
  expected: number;     // findings expected
  claims?: string[];    // exact claim texts, when the value matters
}

/**
 * Matched controls: every case that must fail is paired with the nearest
 * phrasing that must stay clean. These encode the gaps a fresh-context review
 * found, so the heuristic cannot silently regress into them again.
 */
const SELF_TEST_CASES: SelfTestCase[] = [
  {
    name: "letterless stat-object value is examined against its context",
    path: "website/src/components/demo.tsx",
    content: `const STATS = [\n  { label: "test262 pass rate", value: "88.4%" },\n];\n`,
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "letterless CSS dimension stays clean",
    path: "website/src/components/demo.tsx",
    content: `// test262 pass rate dashboard shell\nconst style = { width: "100%", height: "100%" };\n`,
    expected: 0,
  },
  {
    name: "rendered figure leaves no literal",
    path: "website/src/lib/demo.ts",
    content:
      "// test262 corpus pass rate\n" +
      "const rate = (passed: number, run: number): string =>\n" +
      '  run <= 0 ? "0.0%" : `${((passed / run) * 100).toFixed(1)}%`;\n',
    expected: 0,
  },
  {
    name: "perf wording does not excuse a claim with conformance context",
    path: "docs/demo.md",
    content: "The test262 conformance lane moved this week.\n\nAfter the runtime fix landed, the figure is 96%.\n",
    expected: 1,
    claims: ["96%"],
  },
  {
    name: "parser-fix twin behaves identically",
    path: "docs/demo.md",
    content: "The test262 conformance lane moved this week.\n\nAfter the parser fix landed, the figure is 96%.\n",
    expected: 1,
    claims: ["96%"],
  },
  {
    name: "perf delta with no claim vocabulary stays clean",
    path: "docs/demo.md",
    content: "The test262 job runtime moved by 0.87% week over week.\n",
    expected: 0,
  },
  {
    name: "count split across a line break is caught",
    path: "docs/demo.md",
    content: "The engine clears 41,203 of the\ntest262 corpus tests that pass today.\n",
    expected: 1,
    claims: ["41,203"],
  },
  {
    name: "a rate hand-typed as an achieved result is caught at 100%",
    path: "docs/demo.md",
    content: "The last release raised the test262 pass rate to 100%.\n",
    expected: 1,
    claims: ["100%"],
  },
  {
    name: "ceiling phrasing stays clean",
    path: "docs/demo.md",
    content:
      "- An **Areas closest to 100%** table listing the test262 directories with\n" +
      "  the highest pass rate, filtered to areas **below 100%**.\n",
    expected: 0,
  },
  {
    name: "unrelated percentage near generic vocabulary stays clean",
    path: "docs/demo.md",
    content:
      "The bundler drops 42% on the sample corpus.\n\n\nThe test262 suite is pinned by SHA and the corpus is checked out in CI.\n",
    expected: 0,
  },
  {
    name: "the same percentage with a conformance claim is caught",
    path: "docs/demo.md",
    content: "test262 conformance is 42%.\n",
    expected: 1,
    claims: ["42%"],
  },
  {
    name: "corpus size denominator stays clean",
    path: "docs/demo.md",
    content: "The test262 corpus holds 52,233 tests and the report says which ones pass.\n",
    expected: 0,
  },
  {
    name: "numerator is reported, denominator is not",
    path: "docs/demo.md",
    content: "GocciaScript passes 41,765 of 52,233 test262 tests.\n",
    expected: 1,
    claims: ["41,765"],
  },
  {
    name: "marker separated by a blank line still exempts",
    path: "docs/demo.md",
    content:
      "<!-- conformance-claim-ok: 2026-08-01 pinned run -->\n\ntest262 pass rate at the pinned run: 88.4%.\n",
    expected: 0,
  },
  {
    name: "marker on the claim line exempts",
    path: "website/src/components/demo.tsx",
    content: 'const label = "test262 pass rate: 88.4%"; // conformance-claim-ok: pinned run\n',
    expected: 0,
  },
  {
    name: "spelled-out percent is not a bypass",
    path: "docs/demo.md",
    content: "The test262 pass rate is 88 percent today.\n",
    expected: 1,
    claims: ["88 percent"],
  },
  {
    name: "fenced sample output stays clean",
    path: "docs/demo.md",
    content: "Sample test262 report:\n\n```text\nTotal pass rate: 80.0% (41,765 passed / 52,233 run)\n```\n",
    expected: 0,
  },
  {
    name: "tilde-fenced sample output stays clean",
    path: "docs/demo.md",
    content: "Sample test262 report:\n\n~~~text\nTotal pass rate: 80.0% (41,765 passed / 52,233 run)\n~~~\n",
    expected: 0,
  },
  {
    name: "indented code block stays clean",
    path: "docs/demo.md",
    content: "Sample test262 report:\n\n    Total pass rate: 80.0% (41,765 passed)\n",
    expected: 0,
  },
  {
    name: "list continuation is prose, not code",
    path: "docs/demo.md",
    content: "- The test262 lane:\n\n    the pass rate is 80.0% today.\n",
    expected: 1,
    claims: ["80.0%"],
  },
  {
    name: "JSX prose claim is caught",
    path: "website/src/components/demo.tsx",
    content: "<p>\n  Current test262 conformance sits at 80% of the corpus.\n</p>\n",
    expected: 1,
    claims: ["80%"],
  },
  {
    name: "table row claim is caught",
    path: "docs/demo.md",
    content: "| Suite | Pass rate | Passing |\n|---|---|---|\n| test262 | 80.1% | 41,765 / 52,233 |\n",
    expected: 1,
    claims: ["80.1%"],
  },
  {
    name: "version-stamped non-test262 suite snapshot stays clean",
    path: "docs/demo.md",
    content:
      "A parse-validity rerun on 2026-07-21 against `yaml-test-suite` commit 6ad3d2c\n" +
      "matched the expected result for 336 of 402 cases (83.6%).\n",
    expected: 0,
  },
];

const SELF_TEST_PATHS: { path: string; scanned: boolean }[] = [
  { path: "README.md", scanned: true },
  { path: "AGENTS.md", scanned: true },
  { path: "DEFINITION_OF_DONE.md", scanned: true },
  { path: "website/README.md", scanned: true },
  { path: "CHANGELOG.md", scanned: false },
  { path: "docs/test262.md", scanned: true },
  { path: "docs/contributing/tooling.md", scanned: true },
  { path: "docs/adr/0042-test262-loaderbare-harness.md", scanned: false },
  { path: "docs/spikes/some-spike.md", scanned: false },
  { path: "website/src/lib/site-markdown.ts", scanned: true },
  { path: "website/src/__tests__/test262-dashboard.test.ts", scanned: false },
  { path: "website/src/lib/positioning.spec.ts", scanned: false },
  { path: "scripts/run_test262_suite.ts", scanned: false },
];

const runSelfTest = (): void => {
  console.log("Self-testing the conformance-claim heuristic...\n");
  let failures = 0;

  for (const testCase of SELF_TEST_CASES) {
    const findings = analyzeContent(testCase.path, testCase.content);
    const claims = findings.map((finding) => finding.claim);
    const countOk = findings.length === testCase.expected;
    const claimsOk =
      testCase.claims === undefined ||
      (claims.length === testCase.claims.length &&
        testCase.claims.every((claim, index) => claims[index] === claim));

    if (countOk && claimsOk) {
      const verdict = testCase.expected === 0 ? "clean" : `flags ${claims.join(", ")}`;
      console.log(`  OK    ${testCase.name} \u2014 ${verdict}`);
    } else {
      failures++;
      console.error(`  FAIL  ${testCase.name}`);
      console.error(`        expected ${testCase.expected} finding(s)${testCase.claims ? ` [${testCase.claims.join(", ")}]` : ""}`);
      console.error(`        actual   ${findings.length} finding(s) [${claims.join(", ")}]`);
    }
  }

  for (const pathCase of SELF_TEST_PATHS) {
    const actual = shouldScan(pathCase.path);
    if (actual === pathCase.scanned) {
      console.log(`  OK    ${pathCase.path} \u2014 ${actual ? "scanned" : "not scanned"}`);
    } else {
      failures++;
      console.error(`  FAIL  ${pathCase.path} \u2014 expected ${pathCase.scanned ? "scanned" : "not scanned"}`);
    }
  }

  const total = SELF_TEST_CASES.length + SELF_TEST_PATHS.length;
  console.log(`\n${total - failures}/${total} self-test cases passed.`);

  if (failures > 0) process.exit(1);
};

// -- Main ---------------------------------------------------------------------

const main = (): void => {
  if (SELF_TEST) {
    runSelfTest();
    return;
  }

  console.log("Checking for hand-typed test262 conformance claims...\n");

  const files = collectFiles();
  const findings: Finding[] = [];

  for (const file of files) {
    const repoPath = toRepoPath(file);
    const fileFindings = analyzeContent(repoPath, readFileSync(file, "utf-8"));
    findings.push(...fileFindings);

    if (VERBOSE && fileFindings.length === 0) {
      console.log(`  OK    ${repoPath}`);
    }
  }

  for (const finding of findings) {
    console.error(`  FAIL  ${finding.file}:${finding.line} \u2014 ${finding.claim} (${finding.reason})`);
    console.error(`        ${finding.snippet}`);
  }

  console.log(`\nScanned ${files.length} files. ${findings.length} hand-typed conformance claim(s).`);

  if (findings.length > 0) {
    console.error(
      "\nConformance figures come from the generated test262 reports. Link the\n" +
      "/compatibility dashboard or render the number from live dashboard data.\n" +
      "If a figure is deliberate and version-stamped, mark it with a\n" +
      `\`${INLINE_MARKER}: <reason>\` comment on the claim line or on the nearest\n` +
      "non-blank line above it, or add an allowlist entry in\n" +
      "scripts/check-conformance-claims.ts.",
    );
    process.exit(1);
  }

  console.log("No hand-typed conformance claims found.");
};

main();
