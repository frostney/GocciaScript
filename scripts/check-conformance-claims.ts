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
 * sentence three lines closer to it. Both the claim and the vocabulary must be
 * visible prose: context is built from the extracted spans, so a fenced or
 * indented block cannot supply the words that convict its neighbour. Inline
 * code still can — backticks are typography, and `test262` in a sentence is
 * what the reader is being told about — but its content is never itself a
 * candidate, so a CLI flag cannot become a figure.
 *
 * Symlinks resolve before anything is read, and the target must be a regular
 * file inside the checkout. A link out of the tree would scan host state, and
 * one to a character device would never reach EOF.
 *
 * Not checked (by design):
 *   - docs/adr/ and docs/spikes/ — immutable, dated measurement records
 *   - CHANGELOG.md — generated release history, not maintained prose
 *   - TypeScript comments — a `//` or block comment is not published copy;
 *     string and template literals (URLs included) are left intact
 *   - Fenced (``` and ~~~) and indented code blocks, inline code spans, and
 *     link targets — sample report output, CLI flags, and pinned SHAs are not
 *     prose claims. Inline code is excluded as a claim only; its text still
 *     counts as vocabulary for the lines around it.
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

import {
  readFileSync,
  readdirSync,
  existsSync,
  statSync,
  realpathSync,
  mkdirSync,
  mkdtempSync,
  writeFileSync,
  symlinkSync,
  rmSync,
} from "fs";
import { join, relative, dirname, sep } from "path";
import { tmpdir } from "os";
import { fileURLToPath } from "url";

// -- Config -------------------------------------------------------------------

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
// Canonical checkout root. Symlink containment is decided against this, not
// against ROOT: on macOS a path under /tmp resolves into /private/tmp, so an
// uncanonicalised prefix test would reject legitimate targets.
const ROOT_REAL = realpathSync(ROOT);
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

/**
 * What the scan will actually read, or null when the entry must not be read.
 *
 * The repository contains symlinks (AGENTS.md and CLAUDE.md are one document)
 * and a pull request can add more, so the link target — not the link — decides
 * whether an entry is scanned:
 *
 *   - Outside the checkout (`docs/x.md -> /etc/passwd`): reading it would scan
 *     host state that no reviewer of the diff ever sees.
 *   - Not a regular file (`docs/x.md -> /dev/zero`): readFileSync on a
 *     character device never reaches EOF and takes the runner down on memory.
 *
 * Both are reachable from an untrusted branch, which is exactly when this
 * check runs in CI.
 */
const resolveScannable = (full: string): string | null => {
  let real: string;
  try {
    real = realpathSync(full);
  } catch {
    return null; // broken link or an unresolvable path
  }

  if (real !== ROOT_REAL && !real.startsWith(ROOT_REAL + sep)) return null;

  try {
    if (!statSync(real).isFile()) return null;
  } catch {
    return null;
  }

  return real;
};

/** A file to scan: the path reported, and the canonical path read. */
interface ScanTarget {
  repoPath: string;
  realPath: string;
}

const collectFiles = (): ScanTarget[] => {
  const files: ScanTarget[] = [];
  const seen = new Set<string>();

  const add = (full: string): void => {
    const repoPath = toRepoPath(full);
    if (!shouldScan(repoPath)) return;
    const realPath = resolveScannable(full);
    if (realPath === null) return;
    // AGENTS.md and CLAUDE.md are the same document behind a symlink.
    if (seen.has(realPath)) return;
    seen.add(realPath);
    files.push({ repoPath, realPath });
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
 * One line under its two readings.
 *
 *   spans   — the candidate side: text that may hold a claim. Inline code is
 *             excluded, so `--timeout-ms=20000` never becomes a figure.
 *   context — the vocabulary side: everything a reader actually sees, inline
 *             code included. "The `test262` suite pass rate is 88.4%" puts the
 *             subject in backticks, and it is still the word test262 on the
 *             rendered page, so it must be able to convict the figure beside
 *             it. Fenced and indented blocks contribute to neither.
 */
interface LineProse {
  spans: LineSpans;
  context: string;
}

/** A line that reads as neither a claim nor vocabulary. */
const EMPTY_PROSE: LineProse = { spans: [], context: "" };

/**
 * Markdown: everything outside fenced (``` / ~~~) and indented code blocks,
 * inline code spans, and link targets. A CLI flag (`--timeout-ms=20000`) or a
 * report filename is not a claim about the engine.
 */
const markdownProse = (lines: string[]): LineProse[] => {
  const prose: LineProse[] = [];
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
      prose.push(EMPTY_PROSE);
      continue;
    }
    if (fence !== null) {
      prose.push(EMPTY_PROSE);
      continue;
    }

    const indented = /^(?: {4,}|\t)/.test(line);
    if (line.trim() !== "") {
      if (/^\s*(?:[-*+]|\d+[.)])\s/.test(line)) inList = true;
      else if (!indented) inList = false;
    }
    if (indented && !inList) {
      prose.push(EMPTY_PROSE);
      continue;
    }

    // Link targets and bare URLs leave both readings: a path or a pinned SHA
    // is neither a claim nor vocabulary. Inline code leaves only the candidate
    // side — its text is on the page, so it still supplies vocabulary.
    const visible = line
      .replace(/\]\([^)]*\)/g, "] ")      // link targets
      .replace(/https?:\/\/\S+/g, " ");   // bare URLs
    prose.push({
      spans: [visible.replace(/`[^`]*`/g, " ")],
      context: visible.replace(/`/g, " "),
    });
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
/**
 * Blank out `//` line comments and block comments, leaving string and template
 * literals — including the `//` inside a URL — untouched.
 *
 * Comment text is not published copy: `// test262 pass rate: 80%` is invisible
 * to every reader of the site and must not be convicted. Stripping it with a
 * plain `//` search would instead cut `"https://gocciascript.dev"` in half and
 * silently drop the rest of the line, so the scan is character-wise with
 * string state. Comment bodies are replaced by spaces rather than removed, so
 * every surviving span keeps its original line and column.
 *
 * Regex literals are not tracked, matching the tokenisation the literal
 * extractor below already uses: a quote inside a regex is read as a string
 * start by both, so the two stay consistent.
 */
const stripComments = (lines: string[]): string[] => {
  const stripped: string[] = [];
  let inBlock = false;
  let quote = "";

  for (const line of lines) {
    let out = "";
    let i = 0;

    while (i < line.length) {
      const ch = line[i];
      const next = i + 1 < line.length ? line[i + 1] : "";

      if (inBlock) {
        if (ch === "*" && next === "/") {
          inBlock = false;
          out += "  ";
          i += 2;
          continue;
        }
        out += " ";
        i++;
        continue;
      }

      if (quote !== "") {
        out += ch;
        if (ch === "\\") {
          out += next;
          i += 2;
          continue;
        }
        if (ch === quote) quote = "";
        i++;
        continue;
      }

      if (ch === "/" && next === "/") break; // rest of the line is a comment
      if (ch === "/" && next === "*") {
        inBlock = true;
        out += "  ";
        i += 2;
        continue;
      }
      if (ch === '"' || ch === "'" || ch === "`") quote = ch;

      out += ch;
      i++;
    }

    // Only a template literal legally spans lines. A `'` or `"` still open at
    // the newline was never a string: it is an apostrophe in JSX text
    // ("It's the built-in runner") or a quote character inside a regex class
    // (/['"]/g). Carrying that state forward would treat the rest of the file
    // as one long string — comments would stop being stripped and the next
    // `// test262 pass rate: 80%` would be reported as published copy.
    if (quote !== "`") quote = "";

    stripped.push(out);
  }

  return stripped;
};

const sourceProse = (rawLines: string[]): LineProse[] => {
  const prose: LineProse[] = [];
  const literalRe = /"((?:[^"\\]|\\.)*)"|'((?:[^'\\]|\\.)*)'|`((?:[^`\\]|\\.)*)`/g;
  const lines = stripComments(rawLines);

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

    // No inline-code notion in TypeScript: what a reader sees is exactly the
    // literal and JSX text already extracted, so both readings coincide.
    prose.push({ spans, context: spans.join(" ") });
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

  for (const [i, line] of prose.entries()) {
    if (line.spans.length === 0) continue;

    const from = Math.max(0, i - CONTEXT_LINES);
    const to = Math.min(lines.length, i + CONTEXT_LINES + 1);
    // Context comes from the visible reading of the neighbouring lines, not the
    // raw text. A fenced or indented block contributes nothing: sample report
    // output three lines above an unrelated percentage must not convict it.
    // Inline code does contribute — backticks are typography, and `test262` in
    // a sentence is still the reader's subject.
    const context = prose
      .slice(from, to)
      .map((neighbour) => neighbour.context)
      .join("\n");

    if (!TEST262_VOCAB.test(context)) continue;
    if (!CLAIM_VOCAB.test(context)) continue;

    const countsAllowed = PASS_WORD.test(context);
    const claims = line.spans.flatMap((span) => findClaimsInSpan(span, countsAllowed));
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
    // Vocabulary is deliberately in a visible literal, not a comment: with
    // comments stripped, a commented heading would make this case pass
    // whether or not the CSS exclusion still works.
    name: "letterless CSS dimension stays clean",
    path: "website/src/components/demo.tsx",
    content: `const heading = "test262 pass rate";\nconst style = { width: "100%", height: "100%" };\n`,
    expected: 0,
  },
  {
    // Visible vocabulary again, so the case tests the expression drop rather
    // than the comment strip.
    name: "rendered figure leaves no literal",
    path: "website/src/lib/demo.ts",
    content:
      'const LABEL = "test262 corpus pass rate";\n' +
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
  {
    name: "line-comment claim is not published copy",
    path: "website/src/lib/demo.ts",
    content: "// test262 pass rate: 80%\nconst enabled = true;\n",
    expected: 0,
  },
  {
    name: "block-comment claim is not published copy",
    path: "website/src/lib/demo.ts",
    content: "/*\n * test262 pass rate is 88.4% as of the pinned run.\n */\nconst enabled = true;\n",
    expected: 0,
  },
  {
    // Guards the comment strip against a naive `//` search: cutting the line
    // at the URL's slashes would drop the claim that follows it.
    name: "a URL inside a literal does not hide the claim after it",
    path: "website/src/lib/demo.ts",
    content:
      'const COPY = "See https://www.gocciascript.dev/compatibility — test262 pass rate 88.4%";\n',
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "fenced sample output does not convict a neighbouring figure",
    path: "docs/demo.md",
    content:
      "```text\ntest262 pass rate: 88.4%\n```\n\nThe published bundle is 42% smaller.\n",
    expected: 0,
  },
  {
    // Inline code is typography, not a code block: the reader sees the word
    // test262, so it convicts the figure in the same sentence.
    name: "backticked vocabulary still convicts a visible figure",
    path: "docs/demo.md",
    content: "The `test262` suite pass rate is 88.4% on the pinned run.\n",
    expected: 1,
    claims: ["88.4%"],
  },
  {
    // ...but inline code is never itself a candidate, so a flag value in
    // backticks is not a figure however much vocabulary surrounds it.
    name: "a number inside inline code is not a claim",
    path: "docs/demo.md",
    content: "The test262 runner passes with `--timeout-ms=20000` set.\n",
    expected: 0,
  },
  {
    // An apostrophe in JSX text is not a string delimiter. Carrying the quote
    // state past the newline would stop comments being stripped for the rest
    // of the file, and the comment below would be reported as copy.
    name: "an apostrophe in JSX text does not leak string state",
    path: "website/src/components/demo.tsx",
    content:
      "export const Blurb = () => (\n" +
      "  <p>It's the built-in runner.</p>\n" +
      ");\n" +
      "// test262 pass rate: 80%\n" +
      "const enabled = true;\n",
    expected: 0,
  },
  {
    name: "quote characters in a regex literal do not leak string state",
    path: "website/src/lib/demo.ts",
    content:
      "const strip = (s: string) => s.replace(/['\"]/g, \"\");\n" +
      "// test262 pass rate: 80%\n" +
      "const enabled = true;\n",
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

/**
 * Symlink containment cannot be expressed as a string fixture — the battery
 * above feeds content straight to analyzeContent and never touches a path — so
 * resolveScannable is tested directly against real links. Fixtures live under
 * the checkout (containment is judged against it) and are removed afterwards.
 */
interface ResolveCase {
  name: string;
  /** Builds the fixture and returns the path to hand to resolveScannable. */
  make: (dir: string, outside: string) => string;
  accepted: boolean;
}

const SELF_TEST_RESOLVE: ResolveCase[] = [
  {
    name: "a regular file inside the checkout is read",
    make: (dir) => {
      const file = join(dir, "plain.md");
      writeFileSync(file, "# plain\n");
      return file;
    },
    accepted: true,
  },
  {
    name: "a symlink to a file inside the checkout is read",
    make: (dir) => {
      const target = join(dir, "target.md");
      const link = join(dir, "link.md");
      writeFileSync(target, "# target\n");
      symlinkSync(target, link);
      return link;
    },
    accepted: true,
  },
  {
    name: "a symlink escaping the checkout is refused",
    make: (dir, outside) => {
      const link = join(dir, "escape.md");
      symlinkSync(outside, link);
      return link;
    },
    accepted: false,
  },
  {
    name: "a symlink to a directory is refused",
    make: (dir) => {
      const target = join(dir, "subdir");
      const link = join(dir, "dirlink.md");
      mkdirSync(target, { recursive: true });
      symlinkSync(target, link);
      return link;
    },
    accepted: false,
  },
  {
    name: "a broken symlink is refused",
    make: (dir) => {
      const link = join(dir, "broken.md");
      symlinkSync(join(dir, "does-not-exist.md"), link);
      return link;
    },
    accepted: false,
  },
];

// /dev/zero is the memory-exhaustion case, and it only exists on Unix.
if (existsSync("/dev/zero")) {
  SELF_TEST_RESOLVE.push({
    name: "a symlink to a character device is refused",
    make: (dir) => {
      const link = join(dir, "zero.md");
      symlinkSync("/dev/zero", link);
      return link;
    },
    accepted: false,
  });
}

const runResolveCases = (): number => {
  const dir = join(ROOT_REAL, `.conformance-selftest-${process.pid}`);
  const outsideDir = mkdtempSync(join(realpathSync(tmpdir()), "goccia-conformance-"));
  const outside = join(outsideDir, "host.md");
  let failures = 0;

  try {
    mkdirSync(dir, { recursive: true });
    writeFileSync(outside, "# outside the checkout\n");

    for (const resolveCase of SELF_TEST_RESOLVE) {
      const path = resolveCase.make(dir, outside);
      const actual = resolveScannable(path) !== null;
      if (actual === resolveCase.accepted) {
        console.log(`  OK    ${resolveCase.name} — ${actual ? "read" : "refused"}`);
      } else {
        failures++;
        console.error(
          `  FAIL  ${resolveCase.name} — expected ${resolveCase.accepted ? "read" : "refused"}`,
        );
      }
    }
  } finally {
    rmSync(dir, { recursive: true, force: true });
    rmSync(outsideDir, { recursive: true, force: true });
  }

  return failures;
};

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

  failures += runResolveCases();

  const total = SELF_TEST_CASES.length + SELF_TEST_PATHS.length + SELF_TEST_RESOLVE.length;
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
    const fileFindings = analyzeContent(file.repoPath, readFileSync(file.realPath, "utf-8"));
    findings.push(...fileFindings);

    if (VERBOSE && fileFindings.length === 0) {
      console.log(`  OK    ${file.repoPath}`);
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
