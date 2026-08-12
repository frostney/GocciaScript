#!/usr/bin/env npx tsx
/**
 * build-fuzz-corpus.ts
 *
 * Assembles a seed corpus for GocciaFuzzHarness from sources already in the
 * repo: fixtures/, tests/, and — when a test262 checkout is available — a
 * minimized sample of its language/ and built-ins/ trees.
 *
 * Seeds are deduplicated by content hash and capped in size, because AFL++
 * spends its early cycles trimming large inputs and a corpus of near-identical
 * multi-kilobyte test files wastes that budget. Small, syntactically diverse
 * seeds are what make coverage-blind fuzzing productive here.
 *
 * Written in GocciaScript-compatible style (arrow functions, const/let,
 * for...of, no var, strict equality) so it can be bootstrapped later.
 *
 * Usage:
 *   npx tsx scripts/build-fuzz-corpus.ts
 *   npx tsx scripts/build-fuzz-corpus.ts --out build/fuzz/corpus
 *   TEST262_PATH=../test262 npx tsx scripts/build-fuzz-corpus.ts
 */

import { createHash } from "crypto";
import { mkdirSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync } from "fs";
import { dirname, join, relative, resolve } from "path";
import { fileURLToPath } from "url";

// ── Config ─────────────────────────────────────────────────────────────

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");

const argOut = process.argv.indexOf("--out");
let OUT_DIR: string;
if (argOut === -1) {
  OUT_DIR = join(ROOT, "build", "fuzz", "corpus");
} else {
  // OUT_DIR feeds a recursive rmSync below, so a missing or out-of-tree value
  // must be rejected before anything is deleted.
  const value = process.argv[argOut + 1];
  if (value === undefined || value.startsWith("--")) {
    console.error("--out requires a directory path");
    process.exit(1);
  }
  OUT_DIR = resolve(ROOT, value);
  const rel = relative(ROOT, OUT_DIR);
  if (rel === "" || rel.startsWith("..")) {
    console.error(`Refusing to use ${OUT_DIR} as the corpus directory (outside the repository)`);
    process.exit(1);
  }
}
const VERBOSE = process.argv.includes("--verbose");

// AFL++ trims inputs it cannot shrink; seeds above this size cost more than
// they contribute. 8 KiB keeps whole test files while excluding generated
// data blobs.
const MAX_SEED_BYTES = 8 * 1024;
// A seed that is only a copyright header teaches the fuzzer nothing.
const MIN_SEED_BYTES = 8;

// test262 is a submodule-free optional checkout; skipped silently when absent.
const TEST262_PATH = process.env.TEST262_PATH ?? "";
// Sampling stride through test262. The suite is ~50k files; taking every
// Nth keeps the corpus small while preserving feature spread, since the
// suite is laid out by feature directory rather than randomly.
const TEST262_STRIDE = 37;

const SOURCE_EXTENSIONS = new Set([".js", ".mjs", ".jsx", ".ts", ".tsx"]);
const IGNORE_DIRS = new Set(["node_modules", ".git", "build", "dist", "vendor", "generated"]);

// ── Collection ─────────────────────────────────────────────────────────

/** Recursively yields source files under a directory, skipping ignored trees. */
const collectFiles = (dir: string, out: string[] = []): string[] => {
  let entries: string[];
  try {
    entries = readdirSync(dir);
  } catch {
    return out;
  }
  for (const entry of entries) {
    if (IGNORE_DIRS.has(entry)) continue;
    const full = join(dir, entry);
    let info;
    try {
      info = statSync(full);
    } catch {
      continue;
    }
    if (info.isDirectory()) {
      collectFiles(full, out);
    } else {
      const dot = entry.lastIndexOf(".");
      if (dot !== -1 && SOURCE_EXTENSIONS.has(entry.slice(dot))) out.push(full);
    }
  }
  return out;
};

const shortHash = (content: string): string =>
  createHash("sha256").update(content).digest("hex").slice(0, 16);

// ── Build ──────────────────────────────────────────────────────────────

const seen = new Set<string>();
let written = 0;
let skippedSize = 0;
let skippedDuplicate = 0;

const addSeed = (content: string, label: string, allowShort = false): void => {
  const bytes = Buffer.byteLength(content, "utf8");
  if ((!allowShort && bytes < MIN_SEED_BYTES) || bytes > MAX_SEED_BYTES) {
    skippedSize += 1;
    return;
  }
  const hash = shortHash(content);
  if (seen.has(hash)) {
    skippedDuplicate += 1;
    return;
  }
  seen.add(hash);
  writeFileSync(join(OUT_DIR, `${label}-${hash}.js`), content, "utf8");
  written += 1;
};

const addTree = (dir: string, label: string, stride = 1): void => {
  const files = collectFiles(dir).sort();
  let index = 0;
  for (const file of files) {
    index += 1;
    if (index % stride !== 0) continue;
    let content: string;
    try {
      content = readFileSync(file, "utf8");
    } catch {
      continue;
    }
    addSeed(content, label);
  }
  if (VERBOSE) console.log(`  ${label}: scanned ${files.length} file(s) in ${relative(ROOT, dir)}`);
};

rmSync(OUT_DIR, { recursive: true, force: true });
mkdirSync(OUT_DIR, { recursive: true });

console.log(`Building fuzz corpus in ${relative(ROOT, OUT_DIR)}`);

addTree(join(ROOT, "fixtures"), "fixture");
addTree(join(ROOT, "tests"), "test");
addTree(join(ROOT, "examples"), "example");

if (TEST262_PATH) {
  for (const sub of ["test/language", "test/built-ins"]) {
    addTree(join(TEST262_PATH, sub), "test262", TEST262_STRIDE);
  }
} else {
  console.log("  test262: skipped (set TEST262_PATH to include a sample)");
}

// A handful of hand-written seeds for shapes the repo's own tests avoid by
// construction: deep nesting, unterminated literals, and mixed dialect
// features the default parser rejects. These are cheap and reach parser
// paths that valid test files never do.
const SYNTHETIC_SEEDS: Record<string, string> = {
  "deep-nesting": `${"(".repeat(200)}1${")".repeat(200)}`,
  "deep-array": `${"[".repeat(200)}${"]".repeat(200)}`,
  "unterminated-string": 'const s = "abc',
  "unterminated-template": "const t = `abc${",
  "unterminated-regex": "const r = /abc",
  "unterminated-comment": "/* abc",
  "lone-surrogate": 'const s = "\\uD800";',
  "dialect-mix": "var a = 1; function f() { for (var i in {}) { label: while (a == 1) break label; } } f();",
  "getter-setter": "const o = { get x() { return 1; }, set x(v) {} }; o.x = o.x;",
  "class-private": "class C { #x = 1; static { } get x() { return this.#x; } } new C().x;",
  "destructure": "const [{ a = 1, ...rest }, [b], ...more] = [{}, [2], 3];",
  "generator-async": "async function* g() { yield await 1; } g().next();",
  "optional-chain": "const o = {}; o?.a?.[0]?.();",
  "bigint-typed": "const b = 1n << 64n; new BigInt64Array(1)[0] = b;",
  "regex-unicode": "const r = /\\p{Script=Greek}+/u; r.test('abc');",
  "proxy-reflect": "new Proxy({}, { get: (t, k) => Reflect.get(t, k) }).x;",
};

// Synthetic seeds are hand-picked to reach specific parser paths; bypass the
// minimum-size filter so short ones (e.g. "/* abc") are not silently dropped.
for (const [name, content] of Object.entries(SYNTHETIC_SEEDS)) {
  addSeed(content, `synthetic-${name}`, true);
}

console.log(
  `Wrote ${written} seed(s); skipped ${skippedDuplicate} duplicate(s), ${skippedSize} out-of-range`,
);

if (written === 0) {
  console.error("No seeds produced — corpus would be empty");
  process.exit(1);
}
