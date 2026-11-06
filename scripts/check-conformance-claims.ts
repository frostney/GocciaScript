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
 * Scans every file that can carry published or posted copy:
 *   - markdown (`.md`, `.mdx`) anywhere in the checkout except the excluded
 *     historical records, so root documents, `docs/`, subdirectory READMEs,
 *     `.github/` templates, `.agents/skills/` and `website/` are all covered
 *     the day they land;
 *   - TypeScript, JavaScript and JSON under `website/`, `scripts/`, `.github/`
 *     (`scripts/run_test262_suite.ts` builds the markdown posted to every pull
 *     request, so a rate hand-typed there would be published repo-wide);
 *   - YAML and shell under `.github/`, `website/` and `scripts/` (inline
 *     `github-script` markdown, install scripts, issue forms).
 * Files come from `git ls-files` (tracked plus untracked-not-ignored), so a
 * gitignored tree is never entered; without git the checkout is walked.
 * It looks for a percentage, a fraction, an absolute pass count or an
 * unquantified strength claim ("near-complete conformance") sitting next to
 * test262 vocabulary. Exits non-zero if any hand-typed claim is found.
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
 *   - Source comments — a `//` or block comment (and a `#` comment in YAML and
 *     shell) is not published copy; string and template literals (URLs
 *     included) are left intact
 *   - This script and the other self-testing scripts (`*.self-test.ts`,
 *     `scripts/test-*`) — their fixtures are invented claims on purpose
 *   - Fenced (``` and ~~~) and indented code blocks, inline code spans, and
 *     link targets — sample report output, CLI flags, and pinned SHAs are not
 *     prose claims. Inline code is excluded as a claim only; its text still
 *     counts as vocabulary for the lines around it.
 *   - Website and script test files — synthetic fixtures, not published copy
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
import { spawnSync } from "child_process";
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

// Prose is scanned wholesale so a new document is covered the day it lands.
// CHANGELOG.md (at any depth) is generated release history — its entries
// describe what a release measured at the time and are never rewritten.
const EXCLUDED_MD_NAMES = new Set(["CHANGELOG.md"]);
// Historical records: an ADR or spike states what was measured on a given date
// and is never updated afterwards, so a frozen figure there is the point.
// website/content/docs/ is synced from docs/ at build time and validated at
// the source location.
const MD_IGNORE_PATH_PREFIXES = ["docs/adr/", "docs/spikes/", "website/content/docs/"];
const MARKDOWN_EXTENSIONS = [".md", ".mdx"];

// Code and data that can publish copy: the website, the scripts that build
// posted reports, and the workflows with their inline markdown. Everything else
// (tests/, source/, examples/, benchmarks/) holds GocciaScript fixtures and
// Pascal, not copy; its READMEs are still covered by the markdown rule.
const SOURCE_ROOTS = ["website/", "scripts/", ".github/"];
const SOURCE_EXTENSIONS = [".ts", ".tsx", ".mts", ".js", ".jsx", ".mjs", ".cjs"];
// JSON is data almost everywhere; only package metadata, the site's static
// files and the repository's own automation config are copy.
const JSON_PATH_PATTERNS = [/^(?:website\/)?package\.json$/, /^website\/public\//, /^\.github\//];
const TEXT_ROOTS = ["website/", "scripts/", ".github/"];
const TEXT_EXTENSIONS = [".yml", ".yaml", ".sh", ".ps1", ".txt"];
// Fixtures are invented numbers that assert formatting, not claims about the
// engine. The sentinel itself and its siblings hold deliberate fake claims;
// third-party license text under website/src/fonts is not ours.
const IGNORE_PATTERNS = [
  // Vendored or fixture dependency trees (the differential and module tests ship
  // tracked node_modules directories).
  /(?:^|\/)node_modules\//,
  /(?:^|\/)__tests__\//,
  /\.test\.[cm]?[jt]sx?$/,
  /\.spec\.[cm]?[jt]sx?$/,
  /\.self-test\.[cm]?[jt]sx?$/,
  /^scripts\/test-[^/]+$/,
  /^scripts\/check-conformance-claims\.ts$/,
  /^website\/src\/fonts\//,
];
// Directories the fallback walk never enters (git-tracked discovery needs no
// such list: it already honours .gitignore).
const WALK_SKIP_DIRS = new Set(["node_modules", ".git", ".next", ".agent", "build", "dist", "vendor"]);

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

const ALLOWLIST: Allowance[] = [
  {
    // Repo-native skill, edited only when asked to change that skill. The
    // percentage is the magnitude of benchmark runner noise, and the test262
    // vocabulary two lines away belongs to the sentence about profile reports.
    file: ".agents/skills/profile-report-review/SKILL.md",
    pattern: /\(order ~10%\)/,
    reason: "benchmark runner-noise magnitude, not a test262 figure",
  },
  {
    // Repo-native skill. The "80%" is the quoted example of the stale figure
    // the rule exists to stop, written in the sentence that states the rule.
    file: ".agents/skills/prepare-release/SKILL.md",
    pattern: /the kind that read "80%"/,
    reason: "quoted as the example of what the rule forbids, not a claim",
  },
];

const INLINE_MARKER = "conformance-claim-ok";

// -- Vocabulary ---------------------------------------------------------------

// The rule is about test262 specifically. Other suites (yaml-test-suite,
// toml-test, JSON5) are allowed to carry version-stamped snapshots, so a
// figure only becomes a finding when test262 is what is being described.
const TEST262_VOCAB = new RegExp(
  [
    String.raw`test\s?-?262`,
    // "TC39 conformance", "TC39 suite", "TC39's test suite"
    String.raw`\btc39(?:['\u2019]s)?\s+(?:conformance|(?:test\s+)?suite)`,
    // "ECMAScript conformance suite", "ECMAScript test suite", "ECMA-262 conformance"
    String.raw`\becmascript\s+(?:conformance|test)\s+suite`,
    String.raw`\becma-?\s?262\s+conformance`,
    // "the 262 suite", "the 262 conformance suite"
    String.raw`\b262\s+(?:conformance\s+)?suite`,
  ].join("|"),
  "i",
);

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

/** How a file's copy is extracted: see markdownProse, sourceProse, textProse. */
type ProseKind = "markdown" | "source" | "text";

const hasExtension = (repoPath: string, extensions: string[]): boolean =>
  extensions.some((ext) => repoPath.endsWith(ext));

const baseName = (repoPath: string): string => repoPath.slice(repoPath.lastIndexOf("/") + 1);

/**
 * Single source of truth for what the check covers, so the self-test can
 * assert the exclusions without touching the filesystem. Returns how the file
 * is read, or null when it is not scanned.
 */
const proseKindOf = (repoPath: string): ProseKind | null => {
  if (IGNORE_PATTERNS.some((pattern) => pattern.test(repoPath))) return null;

  if (hasExtension(repoPath, MARKDOWN_EXTENSIONS)) {
    if (EXCLUDED_MD_NAMES.has(baseName(repoPath))) return null;
    if (MD_IGNORE_PATH_PREFIXES.some((prefix) => repoPath.startsWith(prefix))) return null;
    return "markdown";
  }

  if (SOURCE_ROOTS.some((root) => repoPath.startsWith(root))) {
    if (hasExtension(repoPath, SOURCE_EXTENSIONS)) return "source";
  }

  if (repoPath.endsWith(".json") && JSON_PATH_PATTERNS.some((pattern) => pattern.test(repoPath))) {
    return "source";
  }

  if (TEXT_ROOTS.some((root) => repoPath.startsWith(root)) && hasExtension(repoPath, TEXT_EXTENSIONS)) {
    return "text";
  }

  return null;
};

const shouldScan = (repoPath: string): boolean => proseKindOf(repoPath) !== null;

// -- File discovery -----------------------------------------------------------

/**
 * What the scan will actually read, or null when the entry must not be read.
 *
 * The repository contains symlinks (`.claude/skills`, `.mcp.json`) and a pull
 * request can add more, so the link target — not the link — decides whether an
 * entry is scanned:
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

/**
 * The files git tracks plus those it would add, as repo-relative paths, or null
 * when git is unavailable or ROOT is not a work tree. A gitignored tree (a
 * test262 checkout, a repository copy under tmp/, build output) is therefore
 * never entered. Any other git failure throws: walking instead would scan the
 * ignored trees this listing exists to skip.
 */
const listGitFiles = (): string[] | null => {
  const result = spawnSync("git", ["ls-files", "-z", "--cached", "--others", "--exclude-standard"], {
    cwd: ROOT,
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
    env: { ...process.env, LC_ALL: "C" },
  });
  if (result.error) {
    if ((result.error as NodeJS.ErrnoException).code === "ENOENT") return null;
    throw result.error;
  }
  if (result.status === 0) return result.stdout.split("\0").filter((path) => path.length > 0);
  if (/not a git repository/i.test(result.stderr)) return null;
  throw new Error(`git ls-files failed (exit ${result.status}): ${result.stderr.trim()}`);
};

const walkFiles = (): string[] => {
  const paths: string[] = [];
  const walk = (dir: string): void => {
    for (const entry of readdirSync(dir, { withFileTypes: true })) {
      const full = join(dir, entry.name);
      if (entry.isDirectory()) {
        if (!WALK_SKIP_DIRS.has(entry.name)) walk(full);
      } else {
        paths.push(toRepoPath(full));
      }
    }
  };
  walk(ROOT);
  return paths;
};

const collectFiles = (): ScanTarget[] => {
  const files: ScanTarget[] = [];
  const seen = new Set<string>();

  for (const repoPath of listGitFiles() ?? walkFiles()) {
    if (!shouldScan(repoPath)) continue;
    const realPath = resolveScannable(join(ROOT, repoPath));
    if (realPath === null) continue;
    // Two paths that resolve to the same file (a symlink) are scanned once.
    if (seen.has(realPath)) continue;
    seen.add(realPath);
    files.push({ repoPath, realPath });
  }

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

      // An escaped character outside a string is regex-literal territory
      // (`/\/\//`, `/\/*/`): copy both so the `/` after a backslash can
      // never read as a comment opener — otherwise the rest of the line
      // (or, via a phantom `/*`, the rest of the file) would be dropped
      // and a real claim there never seen.
      if (ch === "\\") {
        out += ch + next;
        i += 2;
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

const LITERAL_RE = /"((?:[^"\\]|\\.)*)"|'((?:[^'\\]|\\.)*)'|`((?:[^`\\]|\\.)*)`/g;

/** A literal's visible text: `${...}` expressions leave nothing behind. */
const literalText = (match: RegExpMatchArray): string =>
  (match[1] ?? match[2] ?? match[3] ?? "").replace(/\$\{[^}]*\}/g, " ");

/**
 * Literals joined by `+` read as one string: `"88" + ".4%"` shows the reader
 * 88.4%, and no single literal holds it. A chain that continues onto the next
 * line (`"88" +\n ".4%"`) is followed too, and reported at the line it starts
 * on. Only chains of two or more literals are returned; a lone literal is
 * already a span of its own.
 */
const concatenatedLiterals = (lines: string[], start: number): string[] => {
  let joined = lines[start];
  for (let next = start + 1; next < lines.length && next < start + 8 && /\+\s*$/.test(joined); next++) {
    joined += " " + lines[next].trim();
  }

  const chains: string[] = [];
  let chain: string[] = [];
  let end = -1;
  const flush = (): void => {
    if (chain.length > 1) chains.push(chain.join(""));
    chain = [];
  };

  for (const match of joined.matchAll(LITERAL_RE)) {
    const gap = end === -1 ? "" : joined.slice(end, match.index);
    if (!/^\s*\+\s*$/.test(gap)) flush();
    chain.push(literalText(match));
    end = match.index + match[0].length;
  }
  flush();

  return chains;
};

const sourceProse = (rawLines: string[]): LineProse[] => {
  const prose: LineProse[] = [];
  const lines = stripComments(rawLines);

  for (const [index, line] of lines.entries()) {
    const spans: string[] = [];

    for (const m of line.matchAll(LITERAL_RE)) {
      const text = literalText(m);
      if (text.trim() === "") continue;
      // A letterless literal is only copy when it sits in a data position. In a
      // layout position it is a CSS value, and in a computed expression it is a
      // formatter's fallback (`run <= 0 ? "0.0%" : ...`) rather than a figure.
      if (!/[A-Za-z]/.test(text) && (CSS_CONTEXT.test(line) || COMPUTED_CONTEXT.test(line))) continue;
      spans.push(text);
    }

    spans.push(...concatenatedLiterals(lines, index));

    const withoutLiterals = line.replace(LITERAL_RE, " ").replace(/\{[^}]*\}/g, " ");
    const jsxText = withoutLiterals.replace(/<[^>]*>?/g, " ");
    if (jsxText.trim() !== "") {
      spans.push(jsxText);
      // Adjacent text nodes split by a tag (`<b>88</b>.4%`) read as one
      // figure on the page, so the tags are also read as nothing at all.
      const tight = withoutLiterals.replace(/<[^>]*>?/g, "");
      if (tight !== jsxText && tight.trim() !== "") spans.push(tight);
    }

    // No inline-code notion in TypeScript: what a reader sees is exactly the
    // literal and JSX text already extracted, so both readings coincide.
    prose.push({ spans, context: spans.join(" ") });
  }

  return prose;
};

/**
 * YAML and shell: the whole line, minus whole-line `#` comments. The copy in a
 * workflow lives in `run:` and `script:` block scalars (markdown assembled for
 * a pull-request comment), which have no literal syntax worth parsing.
 */
const textProse = (lines: string[]): LineProse[] =>
  lines.map((line) => {
    if (/^\s*#(?:\s|$)/.test(line) || line.trim() === "") return EMPTY_PROSE;
    const visible = line.replace(/https?:\/\/\S+/g, " ");
    return { spans: [visible], context: visible };
  });

// Expression positions: a letterless literal here is produced or chosen by
// code, and the visible number comes from the computation next to it.
const COMPUTED_CONTEXT =
  /\breturn\b|\?|=>|[<>!=]=|\|\||&&|\$\{|\.(?:toFixed|toLocaleString|replace|map|join|padStart)\b/;

// Layout and styling positions, where `"100%"` is a dimension.
const CSS_CONTEXT =
  /\b(?:width|height|top|left|right|bottom|inset|padding|margin|gap|flex|basis|size|opacity|background|gradient|transform|translate|scale|rotate|stroke|fill|color|colour|radius|offset|viewBox|className|class|style|stop|spread|blur)\b/i;

// -- Claim detection ----------------------------------------------------------

// The digits must not be glued to a word: "test262 %" is the suite's name, not
// 262 percent.
const PERCENT_RE = /(?<![\w.])\d{1,3}(?:\.\d+)?\s*(?:%|percent\b)/gi;
// Grouped counts: comma, period (41.765), or a (thin, no-break) space between
// the groups, and bare runs of four or more digits.
const COUNT_RE = /\d{1,3}(?:(?:[,.\u00A0\u2009\u202F])\d{3})+|\d{4,}/g;
// A bare three-digit number is too common to convict on its own ("500 ms",
// "port 443"); it is a pass count only when a pass word sits right beside it:
// "passes 940 of the language tests", "940 tests pass".
const BARE_COUNT_RE = /(?<![\w.,-])\d{3}(?![\w%-]|[.,]\d)/g;
const BARE_COUNT_BEFORE = /\bpass(?:es|ed|ing)?\W+(?:(?:about|around|exactly|just|nearly|only|over|roughly)\W+)?$/i;
const BARE_COUNT_AFTER =
  /^\W*(?:of\b|(?:[a-z-]+\W+){0,3}(?:tests?|cases?|checks?|specs?)\W+pass(?:es|ed|ing)?\b)/i;

// A fraction or ratio says the same thing as a percentage in other words:
// "four fifths of the corpus", "9 in 10", "9/10 of the tests". A numerator
// is required, so "half the work" in unrelated prose is not a shape.
const FRACTION_WORDS =
  "(?:half|halves|thirds?|quarters?|fourths?|fifths?|sixths?|sevenths?|eighths?|ninths?|tenths?)";
const NUMBER_WORDS = "(?:one|two|three|four|five|six|seven|eight|nine|ten)";
const FRACTION_RE = new RegExp(
  [
    // "four fifths", "nine-tenths", "three quarters", "one half"
    String.raw`\b${NUMBER_WORDS}[-\s]+${FRACTION_WORDS}\b`,
    // "9 in 10", "19 out of 20" — the denominator is checked in code
    String.raw`(?<![\w.,/-])\d{1,2}\s+(?:in|out\s+of)\s+\d{1,3}\b(?![.,]?\d)`,
    // "9/10"
    String.raw`(?<![\w.,/-])\d{1,2}/(?:2|3|4|5|6|8|10|20)\b(?![\w/-])`,
  ].join("|"),
  "gi",
);

// Strength claims with no number in them. A hedged measured state ("nearly all
// of test262", "near-complete conformance") is a hand-typed result exactly as a
// percentage is, and goes stale the same way. Unhedged "full conformance" is
// not listed: it is the project's stated target, not a measurement.
const STRENGTH_RE =
  /\bnear(?:ly)?[-\s]+(?:complete|perfect|total|full|universal)\b|\b(?:nearly|almost|virtually|practically|essentially|effectively|basically)\s+(?:all|every(?:thing)?|complete(?:ly)?|entire(?:ly)?|full(?:y)?|perfect(?:ly)?|conformant|compliant)\b|\b(?:vast|overwhelming)\s+majority\b/i;

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

  for (const m of span.matchAll(FRACTION_RE)) {
    const ratio = m[0].match(/^(\d+)\s+(?:in|out\s+of)\s+(\d+)$/i);
    // "5 in 1000" is not a ratio claim and "10 in 5" is not a fraction.
    if (ratio !== null && !(Number(ratio[1]) < Number(ratio[2]) && Number(ratio[2]) <= 100)) continue;
    claims.push({
      claim: m[0].replace(/\s+/g, " ").trim(),
      reason: "hand-typed conformance fraction",
    });
  }

  if (countsAllowed) {
    for (const m of span.matchAll(COUNT_RE)) {
      const digits = m[0];
      const before = textBefore(m.index);
      const after = textAfter(m.index + digits.length);
      // Version pins, flag values, SHAs, paths, and decimal tails.
      if (/[=#/\w.-]$/.test(before)) continue;
      if (/^(?:\.\d|[\d-])/.test(after)) continue;
      // A percentage is reported by the percentage rule.
      if (/^\s*(?:%|percent\b)/i.test(after)) continue;
      if (DATE_LIKE.test(`${before.slice(-6)}${digits}${after}`)) continue;
      // Unit-suffixed numbers are durations and sizes, not corpus counts.
      if (COUNT_SUFFIX_UNITS.test(after.trimStart())) continue;
      // Bare four-digit years read as dates.
      if (/^(?:19|20)\d{2}$/.test(digits)) continue;
      // Corpus totals and denominators.
      if (DENOMINATOR_PREFIX.test(before)) continue;
      claims.push({ claim: digits, reason: "hand-typed conformance pass count" });
    }

    for (const m of span.matchAll(BARE_COUNT_RE)) {
      const digits = m[0];
      if (DENOMINATOR_PREFIX.test(textBefore(m.index))) continue;
      if (COUNT_SUFFIX_UNITS.test(textAfter(m.index + digits.length).trimStart())) continue;
      if (
        BARE_COUNT_BEFORE.test(textBefore(m.index)) ||
        BARE_COUNT_AFTER.test(span.slice(m.index + digits.length, m.index + digits.length + 40))
      ) {
        claims.push({ claim: digits, reason: "hand-typed conformance pass count" });
      }
    }
  }

  return claims;
};

const findStrengthClaims = (span: string): { claim: string; reason: string }[] => {
  const match = span.match(STRENGTH_RE);
  if (match === null) return [];
  return [{ claim: match[0].replace(/\s+/g, " ").trim(), reason: "hand-typed conformance strength claim" }];
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
  const kind = proseKindOf(repoPath);
  const prose =
    kind === "markdown" ? markdownProse(lines) : kind === "text" ? textProse(lines) : sourceProse(lines);
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

    // A strength claim ("nearly all of test262") needs no pass word beside it:
    // the hedge is the claim. Numbers still need pass- or conformance-strength
    // vocabulary, so an unrelated figure is not convicted by the suite name.
    const countsAllowed = PASS_WORD.test(context);
    const numeric = CLAIM_VOCAB.test(context)
      ? line.spans.flatMap((span) => findClaimsInSpan(span, countsAllowed))
      : [];
    const claims = [...numeric, ...line.spans.flatMap(findStrengthClaims)];
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

  {
    name: "an escaped slash in a regex does not end the line scan early",
    path: "website/src/lib/demo.ts",
    content:
      'const isUrl = /^https?:\\/\\//g; const label = "test262 pass rate: 80%";\n' +
      "const enabled = true;\n",
    expected: 1,
  },
  {
    name: "an escaped slash before a star does not latch block-comment state",
    path: "website/src/lib/demo.ts",
    content:
      "const slashStars = /\\/*/g;\n" +
      'const label = "test262 pass rate: 80%";\n' +
      "const enabled = true;\n",
    expected: 1,
  },
  // -- Claim shapes the first battery missed -----------------------------------
  {
    name: "a test262 name glued to a percent sign is not 262 percent",
    path: "docs/demo.md",
    content: "A hand-typed test262 % is a finding when the pass rate is quoted.\n",
    expected: 0,
  },
  {
    name: "a count ending the sentence is still a count",
    path: "docs/demo.md",
    content: "GocciaScript passes 41,765 test262 tests.\n",
    expected: 1,
    claims: ["41,765"],
  },
  {
    name: "period-grouped count is caught",
    path: "docs/demo.md",
    content: "The engine passes 41.765 of the test262 tests.\n",
    expected: 1,
    claims: ["41.765"],
  },
  {
    name: "thin-space-grouped count is caught",
    path: "docs/demo.md",
    content: "The engine passes 41\u2009765 of the test262 tests.\n",
    expected: 1,
    claims: ["41\u2009765"],
  },
  {
    name: "three-digit count beside a pass word is caught",
    path: "docs/demo.md",
    content: "GocciaScript passes 940 of the language tests in test262.\n",
    expected: 1,
    claims: ["940"],
  },
  {
    name: "three-digit count trailing a pass word is caught",
    path: "docs/demo.md",
    content: "In test262, 940 language tests pass today.\n",
    expected: 1,
    claims: ["940"],
  },
  {
    name: "three-digit duration and port beside test262 stay clean",
    path: "docs/demo.md",
    content: "The test262 runner uses port 443 and passes a 500 ms timeout to every test.\n",
    expected: 0,
  },
  {
    name: "a three-digit count ending the sentence is caught",
    path: "docs/demo.md",
    content: "On the pinned test262 run, GocciaScript passes 940.\n",
    expected: 1,
    claims: ["940"],
  },
  {
    name: "a three-digit count followed by a comma is caught",
    path: "docs/demo.md",
    content: "test262 passed 940, failed 12 on the pinned run.\n",
    expected: 1,
    claims: ["940"],
  },
  {
    name: "a three-digit decimal tail beside a pass word stays clean",
    path: "docs/demo.md",
    content: "The test262 runner passes 940.5 ms of budget.\n",
    expected: 0,
  },
  {
    name: "three-digit count with no pass word nearby stays clean",
    path: "docs/demo.md",
    content: "The test262 conformance job runs on 120 shards.\n",
    expected: 0,
  },
  {
    name: "spelled-out fraction is caught",
    path: "docs/demo.md",
    content: "GocciaScript passes four fifths of the test262 corpus.\n",
    expected: 1,
    claims: ["four fifths"],
  },
  {
    name: "N-in-M ratio is caught",
    path: "docs/demo.md",
    content: "Nine in 10 test262 tests pass.\n\nOr put another way, 9 in 10 test262 tests pass.\n",
    expected: 1,
    claims: ["9 in 10"],
  },
  {
    name: "slash fraction is caught",
    path: "docs/demo.md",
    content: "About 9/10 of the test262 tests pass.\n",
    expected: 1,
    claims: ["9/10"],
  },
  {
    name: "fraction wording without test262 stays clean",
    path: "docs/demo.md",
    content: "Four fifths of the sample files pass the linter, 9 in 10 within a day.\n",
    expected: 0,
  },
  {
    name: "a fraction word with no numerator stays clean",
    path: "docs/demo.md",
    content: "The test262 conformance work is half done by design: halves of the pass criteria.\n",
    expected: 0,
  },
  {
    name: "a ratio that is not a fraction of the corpus stays clean",
    path: "docs/demo.md",
    content: "The test262 pass job retries 3 in 2000 flaky runs.\n",
    expected: 0,
  },
  {
    name: "hedged strength claim is caught without a number",
    path: "docs/demo.md",
    content: "GocciaScript has near-complete conformance with the language tests in test262.\n",
    expected: 1,
    claims: ["near-complete"],
  },
  {
    name: "virtually-all strength claim is caught with no pass word",
    path: "website/src/components/demo.tsx",
    content: "<p>\n  Virtually all of test262 runs green.\n</p>\n",
    expected: 1,
    claims: ["Virtually all"],
  },
  {
    name: "strength wording without test262 stays clean",
    path: "docs/demo.md",
    content: "Nearly all of the sample files pass, a near-complete rewrite of the loader.\n",
    expected: 0,
  },
  {
    name: "an unhedged target is not a measurement",
    path: "docs/demo.md",
    content: "The target is full test262 conformance in every mode.\n",
    expected: 0,
  },
  {
    name: "TC39's test suite is test262 vocabulary",
    path: "docs/demo.md",
    content: "TC39\u2019s test suite passes at 88.4% today.\n",
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "the ECMAScript conformance suite is test262 vocabulary",
    path: "docs/demo.md",
    content: "The ECMAScript conformance suite pass rate is 88.4%.\n",
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "ECMA-262 conformance is test262 vocabulary",
    path: "docs/demo.md",
    content: "ECMA-262 conformance sits at 88.4%.\n",
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "the 262 suite is test262 vocabulary",
    path: "docs/demo.md",
    content: "The 262 suite pass rate is 88.4%.\n",
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "ECMAScript conformance alone is not test262 vocabulary",
    path: "docs/demo.md",
    content: "ECMAScript conformance matters: the parser passes 88.4% of its own cases.\n",
    expected: 0,
  },
  {
    name: "concatenated literals are read as one figure",
    path: "website/src/lib/demo.ts",
    content: 'const COPY = "test262 pass rate: " + "88" + ".4%";\n',
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "a figure split across a concatenation line break is caught",
    path: "website/src/lib/demo.ts",
    content: 'const COPY = "test262 pass rate: 88" +\n  ".4%";\n',
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "a computed value between literals leaves no figure",
    path: "website/src/lib/demo.ts",
    content: 'const COPY = "test262 pass rate: " + rate + "%";\n',
    expected: 0,
  },
  {
    name: "adjacent JSX text nodes are read as one figure",
    path: "website/src/components/demo.tsx",
    content: "<p>test262 pass rate <b>88</b>.4%</p>\n",
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "a script that builds a posted comment is scanned",
    path: "scripts/run_test262_suite.ts",
    content: "body += `The test262 pass rate is 88.4% on main.`;\n",
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "a script's generated figure leaves no literal",
    path: "scripts/run_test262_suite.ts",
    content: "body += `The test262 pass rate is ${rate.toFixed(1)}% on main.`;\n",
    expected: 0,
  },
  {
    name: "inline github-script markdown in a workflow is scanned",
    path: ".github/workflows/ci.yml",
    content: "        run: |\n          echo '| test262 | pass rate | 88.4% |'\n",
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "a workflow comment is not published copy",
    path: ".github/workflows/ci.yml",
    content: "      # The test262 pass rate was 88.4% when this was written.\n      - uses: actions/checkout\n",
    expected: 0,
  },
  {
    name: "a skill document is scanned",
    path: ".agents/skills/demo/SKILL.md",
    content: "The test262 pass rate is 88.4% today.\n",
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "a subdirectory README is scanned",
    path: "benchmarks/README.md",
    content: "The test262 pass rate is 88.4% today.\n",
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "package metadata is scanned",
    path: "website/package.json",
    content: '{ "description": "GocciaScript passes 88.4% of test262" }\n',
    expected: 1,
    claims: ["88.4%"],
  },
  {
    name: "a markdown file under website/src is scanned as markdown",
    path: "website/src/content/docs/demo.mdx",
    content: "The test262 pass rate is 88.4% today.\n",
    expected: 1,
    claims: ["88.4%"],
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
  { path: "scripts/run_test262_suite.ts", scanned: true },
  { path: "scripts/aggregate_benchmark_profiles.ts", scanned: true },
  { path: "scripts/awfy-comment.js", scanned: true },
  { path: "scripts/check-conformance-claims.ts", scanned: false },
  { path: "scripts/doc-checks/markdown-files.self-test.ts", scanned: false },
  { path: "scripts/test-cli.ts", scanned: false },
  { path: "scripts/test-skill-inventory-validator.js", scanned: false },
  { path: ".agents/skills/prepare-release/SKILL.md", scanned: true },
  { path: ".github/pull_request_template.md", scanned: true },
  { path: ".github/ISSUE_TEMPLATE/bug_report.yml", scanned: true },
  { path: ".github/workflows/ci.yml", scanned: true },
  { path: ".github/scripts/publish-website-report.sh", scanned: true },
  { path: ".github/delivery/review-automations.json", scanned: true },
  { path: "benchmarks/README.md", scanned: true },
  { path: "source/units/README.md", scanned: true },
  { path: "website/package.json", scanned: true },
  { path: "website/public/install.sh", scanned: true },
  { path: "scripts/differential/mods/nodemods/node_modules/alpha/index.js", scanned: false },
  { path: "tests/language/modules/node-modules/node_modules/pkg-main/package.json", scanned: false },
  { path: "website/src/content/docs/intro.mdx", scanned: true },
  { path: "website/src/fonts/ibm-plex-sans/OFL.txt", scanned: false },
  { path: "website/content/docs/intro.md", scanned: false },
  { path: "website/CHANGELOG.md", scanned: false },
  { path: "docs/adr/0176-performance.md", scanned: false },
  { path: "tests/language/example.js", scanned: false },
  { path: "source/units/Goccia.Engine.pas", scanned: false },
  { path: "source/generated/data.json", scanned: false },
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

// These cases build real symlink fixtures with symlinkSync and therefore
// need Unix symbolic-link support; CI runs --self-test on ubuntu-latest
// only. On a platform without symlink support the run fails loudly rather
// than skipping.
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
