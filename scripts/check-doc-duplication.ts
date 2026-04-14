#!/usr/bin/env npx tsx
/**
 * check-doc-duplication.ts
 *
 * Detects exact and near-duplicate prose across markdown documentation.
 * Uses suffix-array clone detection for exact matches and MinHash + LSH
 * banding for fuzzy paragraph-level similarity.
 *
 * Written in GocciaScript-compatible style (arrow functions, const/let,
 * for...of, no var, strict equality) so it can be bootstrapped later.
 *
 * Usage:
 *   npx tsx scripts/check-doc-duplication.ts                # scan project root
 *   npx tsx scripts/check-doc-duplication.ts --suggest       # include fix suggestions
 *   npx tsx scripts/check-doc-duplication.ts --json          # JSON output
 *   MD_FALLOW_MIN_WORDS=15 npx tsx ...                       # tune exact threshold
 */

import { readFileSync, readdirSync, existsSync, lstatSync, realpathSync } from "fs";
import { join, relative, basename, dirname } from "path";
import { fileURLToPath } from "url";

// ── Types ──────────────────────────────────────────────────────────────

interface WordLocation {
  file: string;
  line: number;
  col: number;
}

interface WordEntry {
  word: string;
  loc: WordLocation;
}

interface CloneLocation {
  file: string;
  line: number;
  endLine: number;
  section: string | null;
}

interface Clone {
  words: string[];
  locations: CloneLocation[];
}

interface Paragraph {
  file: string;
  startLine: number;
  endLine: number;
  words: string[];
  contentWords: string[];
  text: string;
  section: string | null;
}

interface FuzzyCluster {
  paragraphs: Paragraph[];
  canonical: Paragraph;
  avgSimilarity: number;
}

interface Suggestion {
  type: "exact" | "fuzzy";
  canonical: { file: string; line: number; section: string | null; slug: string | null };
  duplicates: { file: string; line: number; endLine: number; section: string | null }[];
  linkText: string;
}

interface HeadingEntry {
  line: number;
  depth: number;
  text: string;
  slug: string;
}

// ── Stopwords ─────────────────────────────────────────────────────────

const STOPWORDS = new Set([
  "a", "an", "the", "and", "or", "but", "in", "on", "at", "to", "for",
  "of", "with", "by", "from", "is", "are", "was", "were", "be", "been",
  "being", "have", "has", "had", "do", "does", "did", "will", "would",
  "could", "should", "may", "might", "shall", "can", "need", "must",
  "it", "its", "this", "that", "these", "those", "i", "you", "he", "she",
  "we", "they", "me", "him", "her", "us", "them", "my", "your", "his",
  "our", "their", "what", "which", "who", "whom", "how", "when", "where",
  "why", "if", "then", "else", "so", "as", "not", "no", "nor", "also",
  "just", "only", "very", "too", "more", "most", "such", "each", "every",
  "all", "any", "both", "few", "some", "many", "much", "own", "other",
  "about", "up", "out", "into", "over", "after", "before", "between",
  "through", "during", "above", "below", "than", "because", "while",
]);

const filterStopwords = (words: string[]): string[] =>
  words.filter((w) => !STOPWORDS.has(w) && w.length > 1);

// ── Config ─────────────────────────────────────────────────────────────

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");

// ── Thresholds ────────────────────────────────────────────────────────
//
// Exact clones:   MIN_WORDS consecutive words that appear ≥ MIN_LOCATIONS times.
//   35 words filters out API-reference table rows that naturally share method
//   signatures (e.g. Temporal types with identical `until`/`since`/`round` rows)
//   while still catching real prose duplication.
//
// Fuzzy clusters:  paragraphs with blended Jaccard similarity ≥ FUZZY_THRESHOLD
//   after stopword removal.  Only paragraphs with ≥ FUZZY_MIN_WORDS are compared.
//
// All thresholds are overridable via environment variables.
const MIN_WORDS = parseInt(process.env.MD_FALLOW_MIN_WORDS ?? "35", 10);
const MIN_LOCATIONS = parseInt(process.env.MD_FALLOW_MIN_LOCATIONS ?? "2", 10);
const FUZZY_THRESHOLD = parseFloat(process.env.MD_FALLOW_FUZZY_THRESHOLD ?? "0.65");
const FUZZY_MIN_WORDS = parseInt(process.env.MD_FALLOW_FUZZY_MIN_WORDS ?? "15", 10);
const NUM_HASHES = 128;
const NUM_BANDS = 32;
const ROWS_PER_BAND = NUM_HASHES / NUM_BANDS; // 4
const EXTENSIONS = new Set([".md", ".mdx"]);
const IGNORE_DIRS = new Set(["node_modules", ".git", "dist", "build", ".next", "vendor"]);

// Utility: generate an index array [0, 1, ..., n-1]
const range = (n: number): number[] => Array.from({ length: n }, (_, i) => i);

// Utility: generate an index array [start, start+1, ..., end-1]
const rangeFrom = (start: number, end: number): number[] =>
  Array.from({ length: end - start }, (_, i) => start + i);

// ── File discovery ─────────────────────────────────────────────────────

const findMarkdownFiles = (dir: string): string[] => {
  const results: string[] = [];
  const seen = new Set<string>();
  const walk = (d: string): void => {
    const entries = readdirSync(d, { withFileTypes: true });
    for (const entry of entries) {
      const full = join(d, entry.name);
      if (entry.isDirectory()) {
        if (!IGNORE_DIRS.has(entry.name)) walk(full);
      } else if (EXTENSIONS.has(entry.name.slice(entry.name.lastIndexOf(".")))) {
        // Resolve symlinks to avoid scanning the same file twice
        const real = lstatSync(full).isSymbolicLink() ? realpathSync(full) : full;
        if (!seen.has(real)) {
          seen.add(real);
          results.push(full);
        }
      }
    }
  };
  walk(dir);
  return results;
};

// ── Section heading index ─────────────────────────────────────────────

const buildHeadingIndex = (content: string): HeadingEntry[] => {
  const headings: HeadingEntry[] = [];
  const lines = content.split("\n");
  let inCode = false;
  for (const [i, line] of lines.entries()) {
    if (/^```/.test(line.trim())) { inCode = !inCode; continue; }
    if (inCode) continue;
    const m = line.match(/^(#{1,6})\s+(.+)/);
    if (m) {
      const text = m[2].replace(/\s*#+\s*$/, "").trim();
      const slug = text.toLowerCase().replace(/[^\w\s-]/g, "").replace(/\s+/g, "-");
      headings.push({ line: i + 1, depth: m[1].length, text, slug });
    }
  }
  return headings;
};

const findSection = (headings: HeadingEntry[], line: number): string | null => {
  let best: HeadingEntry | null = null;
  for (const h of headings) {
    if (h.line <= line) best = h; else break;
  }
  return best ? best.text : null;
};

const findSectionSlug = (headings: HeadingEntry[], line: number): string | null => {
  let best: HeadingEntry | null = null;
  for (const h of headings) {
    if (h.line <= line) best = h; else break;
  }
  return best ? best.slug : null;
};

// ── Tokenisation ───────────────────────────────────────────────────────

const WORD_RE = /[a-zA-Z0-9_\-'.]+/g;

const tokenise = (content: string, file: string): WordEntry[] => {
  const entries: WordEntry[] = [];
  const lines = content.split("\n");
  for (const [li, line] of lines.entries()) {
    if (/^```/.test(line) || /^<!--/.test(line) || /^---$/.test(line.trim())) continue;
    for (const m of line.matchAll(WORD_RE)) {
      entries.push({ word: m[0].toLowerCase(), loc: { file, line: li + 1, col: m.index! + 1 } });
    }
  }
  return entries;
};

// ── Suffix array + LCP ────────────────────────────────────────────────

const buildSuffixArray = (words: string[]): Int32Array => {
  const n = words.length;
  const indices = new Int32Array(n);
  for (const i of range(n)) indices[i] = i;
  // Pre-allocate full range once to avoid per-comparison allocation
  const fullRange = range(n);
  indices.sort((a, b) => {
    const len = Math.min(n - a, n - b);
    for (const k of fullRange) {
      if (k >= len) break;
      if (words[a + k] < words[b + k]) return -1;
      if (words[a + k] > words[b + k]) return 1;
    }
    return (n - a) - (n - b);
  });
  return indices;
};

const buildLCPArray = (words: string[], sa: Int32Array): Int32Array => {
  const n = sa.length;
  const rank = new Int32Array(n);
  for (const i of range(n)) rank[sa[i]] = i;
  const lcp = new Int32Array(n);
  let h = 0;
  for (const i of range(n)) {
    if (rank[i] > 0) {
      const j = sa[rank[i] - 1];
      for (const _ of range(n)) {
        if (!(i + h < n && j + h < n && words[i + h] === words[j + h])) break;
        h++;
      }
      lcp[rank[i]] = h;
      if (h > 0) h--;
    }
  }
  return lcp;
};

// ── Clone detection ────────────────────────────────────────────────────

const detectClones = (
  corpus: WordEntry[], sa: Int32Array, lcp: Int32Array,
  minWords: number, minLocations: number,
  headingsByFile: Map<string, HeadingEntry[]>,
): Clone[] => {
  const n = sa.length;
  const words = corpus.map((e) => e.word);
  const clones: Clone[] = [];
  const seen = new Set<string>();

  let skip = 0;
  for (const i of range(n)) {
    if (skip > 0) { skip--; continue; }
    if (lcp[i] < minWords) continue;
    const clusterStart = i - 1;
    let clusterEnd = i;
    let shared = lcp[i];
    for (const next of rangeFrom(i + 1, n)) {
      if (lcp[next] < minWords) break;
      shared = Math.min(shared, lcp[next]);
      clusterEnd = next;
    }
    const matchLen = shared;
    const phrase = words.slice(sa[clusterStart], sa[clusterStart] + matchLen).join(" ");
    if (seen.has(phrase)) { skip = clusterEnd - i; continue; }
    seen.add(phrase);

    const locMap = new Map<string, CloneLocation>();
    for (const j of rangeFrom(clusterStart, clusterEnd + 1)) {
      const startIdx = sa[j];
      const endIdx = startIdx + matchLen - 1;
      const loc = corpus[startIdx].loc;
      const endLoc = corpus[endIdx].loc;
      const key = `${loc.file}:${loc.line}`;
      if (!locMap.has(key)) {
        const headings = headingsByFile.get(loc.file) ?? [];
        locMap.set(key, { file: loc.file, line: loc.line, endLine: endLoc.line, section: findSection(headings, loc.line) });
      }
    }
    const locations = [...locMap.values()];
    if (locations.length >= minLocations) {
      clones.push({ words: words.slice(sa[clusterStart], sa[clusterStart] + matchLen), locations });
    }
    skip = clusterEnd - i;
  }
  return clones;
};

const deduplicateClones = (clones: Clone[]): Clone[] => {
  clones.sort((a, b) => b.words.length - a.words.length);
  const kept: Clone[] = [];
  const covered = new Set<string>();
  for (const clone of clones) {
    const locKeys = clone.locations.map((l) => `${l.file}:${l.line}`);
    if (!locKeys.every((k) => covered.has(k))) {
      kept.push(clone);
      for (const k of locKeys) covered.add(k);
    }
  }
  return kept;
};

// ── Paragraph extraction ──────────────────────────────────────────────

const pushParagraph = (
  lines: string[], file: string, startLine: number, endLineExcl: number,
  headings: HeadingEntry[], out: Paragraph[],
): void => {
  const text = lines.join(" ");
  const words: string[] = [];
  for (const m of text.matchAll(WORD_RE)) words.push(m[0].toLowerCase());
  if (words.length >= FUZZY_MIN_WORDS) {
    out.push({
      file, startLine, endLine: endLineExcl, words,
      contentWords: filterStopwords(words), text,
      section: findSection(headings, startLine),
    });
  }
};

const extractParagraphs = (content: string, file: string, headings: HeadingEntry[]): Paragraph[] => {
  const paragraphs: Paragraph[] = [];
  const lines = content.split("\n");
  let current: string[] = [];
  let startLine = 1;
  let inCode = false;

  for (const [i, line] of lines.entries()) {
    if (/^```/.test(line.trim())) {
      inCode = !inCode;
      if (current.length > 0) { pushParagraph(current, file, startLine, i, headings, paragraphs); current = []; }
      startLine = i + 2;
      continue;
    }
    if (inCode) continue;
    const trimmed = line.trim();
    if (trimmed === "") {
      if (current.length > 0) { pushParagraph(current, file, startLine, i, headings, paragraphs); current = []; }
      startLine = i + 2;
    } else {
      current.push(trimmed);
    }
  }
  if (current.length > 0) pushParagraph(current, file, startLine, lines.length, headings, paragraphs);
  return paragraphs;
};

// ── Hashing ───────────────────────────────────────────────────────────

const hashString = (s: string): number => {
  let h = 0;
  for (const ch of s) h = ((h << 5) - h + ch.charCodeAt(0)) | 0;
  return h >>> 0;
};

// Modular multiplication that stays within safe integer range (no BigInt)
const mulMod = (a: number, b: number, m: number): number => {
  let result = 0;
  let base = a % m;
  let exp = b;
  // exp is at most ~2^31, so log2(exp) iterations max
  for (const _ of range(32)) {
    if (exp <= 0) break;
    if (exp % 2 === 1) result = (result + base) % m;
    base = (base * 2) % m;
    exp = Math.floor(exp / 2);
  }
  return result;
};

const LARGE_PRIME = 4294967311;

const hashCoeffs: [number, number][] = (() => {
  let seed = 42;
  const next = (): number => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed; };
  return Array.from({ length: NUM_HASHES }, (): [number, number] => [next(), next()]);
})();

const computeUnigrams = (words: string[]): Set<number> => {
  const s = new Set<number>();
  for (const w of words) s.add(hashString(w));
  return s;
};

const computeBigrams = (words: string[]): Set<string> => {
  const s = new Set<string>();
  const pairs = words.slice(0, -1);
  for (const [i, w] of pairs.entries()) s.add(`${w} ${words[i + 1]}`);
  return s;
};

const trueJaccard = (a: Set<string>, b: Set<string>): number => {
  let inter = 0;
  for (const x of a) { if (b.has(x)) inter++; }
  const union = a.size + b.size - inter;
  return union === 0 ? 0 : inter / union;
};

const blendedSimilarity = (a: Paragraph, b: Paragraph): number => {
  const uniA = new Set(a.contentWords);
  const uniB = new Set(b.contentWords);
  const uniSim = trueJaccard(uniA, uniB);
  const biA = computeBigrams(a.contentWords);
  const biB = computeBigrams(b.contentWords);
  const biSim = trueJaccard(biA, biB);
  return 0.6 * uniSim + 0.4 * biSim;
};

const minHash = (shingles: Set<number>): Uint32Array => {
  const sig = new Uint32Array(NUM_HASHES).fill(0xFFFFFFFF);
  for (const s of shingles) {
    for (const i of range(NUM_HASHES)) {
      const [a, b] = hashCoeffs[i];
      const h = (mulMod(a, s, LARGE_PRIME) + b) % LARGE_PRIME;
      if (h < sig[i]) sig[i] = h;
    }
  }
  return sig;
};

// ── LSH banding ───────────────────────────────────────────────────────

const lshCandidates = (signatures: Uint32Array[]): Set<string> => {
  const candidates = new Set<string>();
  const buckets = new Map<string, number[]>();
  for (const band of range(NUM_BANDS)) {
    buckets.clear();
    const offset = band * ROWS_PER_BAND;
    for (const [docIdx, sig] of signatures.entries()) {
      let key = `${band}:`;
      for (const r of range(ROWS_PER_BAND)) key += sig[offset + r].toString(36) + ",";
      const bucket = buckets.get(key);
      if (bucket) {
        for (const other of bucket) {
          const pk = other < docIdx ? `${other}:${docIdx}` : `${docIdx}:${other}`;
          candidates.add(pk);
        }
        bucket.push(docIdx);
      } else {
        buckets.set(key, [docIdx]);
      }
    }
  }
  return candidates;
};

// ── Union-Find ────────────────────────────────────────────────────────

class UnionFind {
  parent: number[];
  rank: number[];
  constructor(n: number) {
    this.parent = Array.from({ length: n }, (_, i) => i);
    this.rank = new Array(n).fill(0);
  }
  find(x: number): number {
    if (this.parent[x] !== x) this.parent[x] = this.find(this.parent[x]);
    return this.parent[x];
  }
  union(a: number, b: number): void {
    const ra = this.find(a);
    const rb = this.find(b);
    if (ra === rb) return;
    if (this.rank[ra] < this.rank[rb]) { this.parent[ra] = rb; }
    else if (this.rank[ra] > this.rank[rb]) { this.parent[rb] = ra; }
    else { this.parent[rb] = ra; this.rank[ra]++; }
  }
}

// ── Fuzzy detection with clustering ───────────────────────────────────

const detectFuzzyClusters = (paragraphs: Paragraph[], threshold: number): FuzzyCluster[] => {
  if (paragraphs.length < 2) return [];

  const signatures: Uint32Array[] = paragraphs.map((p) => minHash(computeUnigrams(p.contentWords)));
  const candidates = lshCandidates(signatures);
  const uf = new UnionFind(paragraphs.length);
  const edges: { ai: number; bi: number; sim: number }[] = [];

  for (const pair of candidates) {
    const parts = pair.split(":");
    const ai = parseInt(parts[0], 10);
    const bi = parseInt(parts[1], 10);
    if (paragraphs[ai].file === paragraphs[bi].file && paragraphs[ai].startLine === paragraphs[bi].startLine) continue;
    const sim = blendedSimilarity(paragraphs[ai], paragraphs[bi]);
    if (sim >= threshold) {
      uf.union(ai, bi);
      edges.push({ ai, bi, sim });
    }
  }

  const groups = new Map<number, { indices: Set<number>; totalSim: number; edgeCount: number }>();
  for (const { ai, bi, sim } of edges) {
    const root = uf.find(ai);
    let g = groups.get(root);
    if (!g) { g = { indices: new Set(), totalSim: 0, edgeCount: 0 }; groups.set(root, g); }
    g.indices.add(ai);
    g.indices.add(bi);
    g.totalSim += sim;
    g.edgeCount++;
  }

  const clusters: FuzzyCluster[] = [];
  for (const g of groups.values()) {
    const paras = [...g.indices].map((i) => paragraphs[i]);
    const canonical = paras.reduce((best, p) => p.words.length > best.words.length ? p : best);
    clusters.push({ paragraphs: paras, canonical, avgSimilarity: g.totalSim / g.edgeCount });
  }
  clusters.sort((a, b) => b.paragraphs.length - a.paragraphs.length || b.avgSimilarity - a.avgSimilarity);
  return clusters;
};

const filterClustersAgainstExact = (clusters: FuzzyCluster[], exact: Clone[]): FuzzyCluster[] => {
  const exactCovered = new Set<string>();
  for (const c of exact) {
    for (const loc of c.locations) {
      for (const l of range(loc.endLine - loc.line + 1)) {
        exactCovered.add(`${loc.file}:${loc.line + l}`);
      }
    }
  }
  return clusters.filter((cl) => cl.paragraphs.some((p) => !exactCovered.has(`${p.file}:${p.startLine}`)));
};

// ── Suggestion generation ─────────────────────────────────────────────

const generateSuggestions = (
  clones: Clone[], clusters: FuzzyCluster[],
  headingsByFile: Map<string, HeadingEntry[]>, root: string,
): Suggestion[] => {
  const suggestions: Suggestion[] = [];

  for (const c of clones) {
    const sorted = [...c.locations].sort((a, b) => a.file.localeCompare(b.file) || a.line - b.line);
    const canon = sorted[0];
    const headings = headingsByFile.get(canon.file) ?? [];
    const slug = findSectionSlug(headings, canon.line);
    const relPath = relative(root, canon.file);
    const anchor = slug ? `#${slug}` : "";
    const label = canon.section ?? basename(canon.file, ".md");
    suggestions.push({
      type: "exact",
      canonical: { file: canon.file, line: canon.line, section: canon.section, slug },
      duplicates: sorted.slice(1),
      linkText: `See [${label}](./${relPath}${anchor})`,
    });
  }

  for (const cl of clusters) {
    const canon = cl.canonical;
    const headings = headingsByFile.get(canon.file) ?? [];
    const slug = findSectionSlug(headings, canon.startLine);
    const relPath = relative(root, canon.file);
    const anchor = slug ? `#${slug}` : "";
    const label = canon.section ?? basename(canon.file, ".md");
    suggestions.push({
      type: "fuzzy",
      canonical: { file: canon.file, line: canon.startLine, section: canon.section, slug },
      duplicates: cl.paragraphs.filter((p) => p !== canon).map((p) => ({
        file: p.file, line: p.startLine, endLine: p.endLine, section: p.section,
      })),
      linkText: `See [${label}](./${relPath}${anchor})`,
    });
  }

  return suggestions;
};

// ── Reporting ──────────────────────────────────────────────────────────

const report = (clones: Clone[], clusters: FuzzyCluster[], root: string): void => {
  if (clones.length === 0 && clusters.length === 0) {
    console.log("✅ No duplicated documentation found.");
    return;
  }

  if (clones.length > 0) {
    console.log(`\n🔍 Found ${clones.length} exact duplicate(s):\n`);
    for (const [i, c] of clones.entries()) {
      const preview = c.words.slice(0, 25).join(" ") + (c.words.length > 25 ? " …" : "");
      const crossFile = new Set(c.locations.map((l) => l.file)).size > 1;
      console.log(`  ┌─ Clone #${i + 1} (${c.words.length} words${crossFile ? ", cross-file" : ""})`);
      console.log(`  │  "${preview}"`);
      for (const loc of c.locations) {
        const rel = relative(root, loc.file);
        const rangeStr = loc.line === loc.endLine ? `L${loc.line}` : `L${loc.line}-${loc.endLine}`;
        const sec = loc.section ? ` (§ ${loc.section})` : "";
        console.log(`  │  📄 ${rel}:${rangeStr}${sec}`);
      }
      console.log("  └");
    }
  }

  if (clusters.length > 0) {
    console.log(`\n🔎 Found ${clusters.length} near-duplicate cluster(s):\n`);
    for (const [i, cl] of clusters.entries()) {
      const pct = (cl.avgSimilarity * 100).toFixed(0);
      console.log(`  ┌─ Cluster #${i + 1} (${cl.paragraphs.length} paragraphs, ~${pct}% similar)`);
      for (const p of cl.paragraphs) {
        const rel = relative(root, p.file);
        const preview = p.words.slice(0, 16).join(" ") + (p.words.length > 16 ? " …" : "");
        const sec = p.section ? ` (§ ${p.section})` : "";
        const tag = p === cl.canonical ? " ⭐ canonical" : "";
        console.log(`  │  📄 ${rel}:L${p.startLine}-${p.endLine}${sec}${tag}`);
        console.log(`  │     "${preview}"`);
      }
      console.log("  └");
    }
  }

  const crossFileCount = clones.filter((c) => new Set(c.locations.map((l) => l.file)).size > 1).length;
  console.log(`\n  ${clones.length} exact clone(s) (${crossFileCount} cross-file), ${clusters.length} near-duplicate cluster(s)\n`);
};

const reportSuggestions = (suggestions: Suggestion[], root: string): void => {
  if (suggestions.length === 0) return;
  console.log(`\n💡 Suggestions:\n`);
  for (const [i, s] of suggestions.entries()) {
    const canonRel = relative(root, s.canonical.file);
    const canonSec = s.canonical.section ? ` § ${s.canonical.section}` : "";
    console.log(`  ┌─ ${s.type === "exact" ? "Exact" : "Near"}-duplicate #${i + 1}`);
    console.log(`  │  Keep canonical: ${canonRel}:L${s.canonical.line}${canonSec}`);
    console.log(`  │  Replace ${s.duplicates.length} duplicate(s) with:`);
    console.log(`  │    ${s.linkText}`);
    for (const d of s.duplicates) {
      const rel = relative(root, d.file);
      const sec = d.section ? ` (§ ${d.section})` : "";
      console.log(`  │  ✂️  ${rel}:L${d.line}-${d.endLine}${sec}`);
    }
    console.log("  └");
  }
};

// ── JSON output ────────────────────────────────────────────────────────

const reportJSON = (clones: Clone[], clusters: FuzzyCluster[], suggestions: Suggestion[], root: string): void => {
  const output = {
    exact: clones.map((c) => ({
      wordCount: c.words.length,
      preview: c.words.slice(0, 40).join(" "),
      locations: c.locations.map((l) => ({
        file: relative(root, l.file), startLine: l.line, endLine: l.endLine, section: l.section,
      })),
    })),
    fuzzy: clusters.map((cl) => ({
      paragraphCount: cl.paragraphs.length,
      avgSimilarity: Math.round(cl.avgSimilarity * 100),
      canonical: {
        file: relative(root, cl.canonical.file),
        startLine: cl.canonical.startLine, endLine: cl.canonical.endLine,
        section: cl.canonical.section,
      },
      duplicates: cl.paragraphs.filter((p) => p !== cl.canonical).map((p) => ({
        file: relative(root, p.file), startLine: p.startLine, endLine: p.endLine,
        section: p.section,
        preview: p.words.slice(0, 30).join(" "),
      })),
    })),
    suggestions: suggestions.map((s) => ({
      type: s.type,
      canonical: { file: relative(root, s.canonical.file), line: s.canonical.line, section: s.canonical.section },
      replacements: s.duplicates.map((d) => ({
        file: relative(root, d.file), startLine: d.line, endLine: d.endLine, section: d.section,
      })),
      suggestedLink: s.linkText,
    })),
  };
  console.log(JSON.stringify(output, null, 2));
};

// ── Main ───────────────────────────────────────────────────────────────

const main = (): void => {
  const args = process.argv.slice(2);
  const jsonMode = args.includes("--json");
  const suggestMode = args.includes("--suggest");
  const filtered = args.filter((a) => !a.startsWith("--"));
  const root = filtered[0] ?? ROOT;

  console.error(`md-fallow: scanning ${root} (min ${MIN_WORDS} exact, fuzzy ≥${(FUZZY_THRESHOLD * 100).toFixed(0)}%, stopwords filtered)`);

  const files = findMarkdownFiles(root);
  if (files.length === 0) { console.error("No markdown files found."); process.exit(0); }
  console.error(`  Found ${files.length} file(s)`);

  const headingsByFile = new Map<string, HeadingEntry[]>();
  const corpus: WordEntry[] = [];
  const allParagraphs: Paragraph[] = [];

  for (const file of files) {
    const content = readFileSync(file, "utf-8");
    const headings = buildHeadingIndex(content);
    headingsByFile.set(file, headings);

    const lines = content.split("\n");
    const cleaned: string[] = [];
    let inCode = false;
    for (const line of lines) {
      if (/^```/.test(line.trim())) { inCode = !inCode; cleaned.push(""); continue; }
      cleaned.push(inCode ? "" : line);
    }
    corpus.push(...tokenise(cleaned.join("\n"), file));
    allParagraphs.push(...extractParagraphs(content, file, headings));
  }

  console.error(`  Corpus: ${corpus.length} words, ${allParagraphs.length} paragraphs`);

  // Pass 1: Exact clones
  let clones: Clone[] = [];
  if (corpus.length >= MIN_WORDS) {
    const words = corpus.map((e) => e.word);
    const sa = buildSuffixArray(words);
    const lcpArr = buildLCPArray(words, sa);
    clones = detectClones(corpus, sa, lcpArr, MIN_WORDS, MIN_LOCATIONS, headingsByFile);
    clones = deduplicateClones(clones);
  }

  // Pass 2: Fuzzy clusters
  let clusters = detectFuzzyClusters(allParagraphs, FUZZY_THRESHOLD);
  clusters = filterClustersAgainstExact(clusters, clones);

  // Suggestions
  const suggestions = suggestMode ? generateSuggestions(clones, clusters, headingsByFile, root) : [];

  if (jsonMode) {
    reportJSON(clones, clusters, suggestions, root);
  } else {
    report(clones, clusters, root);
    if (suggestMode) reportSuggestions(suggestions, root);
  }

  // Exact clones are hard failures; fuzzy clusters are warnings only
  // (near-duplicates in API tables and executive summaries are expected)
  if (clones.length > 0) {
    process.exit(1);
  } else if (clusters.length > 0) {
    console.error(`\n⚠️  ${clusters.length} fuzzy cluster(s) detected (warnings only — review recommended)`);
    process.exit(0);
  } else {
    process.exit(0);
  }
};

main();
