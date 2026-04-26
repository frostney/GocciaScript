#!/usr/bin/env npx tsx
/**
 * check-doc-links.ts
 *
 * Walks markdown files and verifies that all internal links resolve correctly.
 * Checks relative file links, anchor links, and combined file+anchor links.
 * Exits non-zero if any broken links are found.
 *
 * Written in GocciaScript-compatible style (arrow functions, const/let,
 * for...of, no var, strict equality) so it can be bootstrapped later.
 *
 * Usage:
 *   npx tsx scripts/check-doc-links.ts
 *   npx tsx scripts/check-doc-links.ts --verbose
 */

import { readFileSync, readdirSync, existsSync, lstatSync, realpathSync } from "fs";
import { join, relative, dirname, resolve, extname } from "path";
import { fileURLToPath } from "url";

// -- Config -------------------------------------------------------------------

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const VERBOSE = process.argv.includes("--verbose");
const EXTENSIONS = new Set([".md", ".mdx"]);
const IGNORE_DIRS = new Set(["node_modules", ".git", "dist", "build", ".next", "vendor"]);
// Build artifacts whose contents are synced from elsewhere and validated at
// the source location. Path-prefix matched against repo-relative paths.
const IGNORE_PATH_PREFIXES = ["website/content/docs/"];

// -- File discovery -----------------------------------------------------------

const findMarkdownFiles = (dir: string): string[] => {
  const results: string[] = [];
  const seen = new Set<string>();
  const isIgnoredPath = (full: string): boolean => {
    const rel = relative(ROOT, full).split("\\").join("/");
    return IGNORE_PATH_PREFIXES.some((p) => rel === p.replace(/\/$/, "") || rel.startsWith(p));
  };
  const walk = (d: string): void => {
    const entries = readdirSync(d, { withFileTypes: true });
    for (const entry of entries) {
      const full = join(d, entry.name);
      if (entry.isDirectory()) {
        if (IGNORE_DIRS.has(entry.name)) continue;
        if (isIgnoredPath(full)) continue;
        walk(full);
      } else if (EXTENSIONS.has(entry.name.slice(entry.name.lastIndexOf(".")))) {
        if (isIgnoredPath(full)) continue;
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

// -- GitHub-style heading slug ------------------------------------------------

const githubSlug = (headingText: string): string => {
  // GitHub's algorithm: lowercase, strip non-word chars except hyphens and spaces,
  // then replace each space with a hyphen (without collapsing runs — two adjacent
  // spaces produce two adjacent hyphens, which matters for em-dash headings).
  return headingText
    .toLowerCase()
    .replace(/[^\w\s-]/g, "")
    .replace(/ /g, "-");
};

// -- Heading index with duplicate handling ------------------------------------

interface HeadingSlug {
  slug: string;
  line: number;
}

const buildHeadingIndex = (content: string): Map<string, HeadingSlug[]> => {
  const slugMap = new Map<string, HeadingSlug[]>();
  const slugCounts = new Map<string, number>();
  const lines = content.split("\n");
  let inCode = false;
  let inHtmlComment = false;

  for (const [i, line] of lines.entries()) {
    // Track HTML comments (multi-line)
    if (inHtmlComment) {
      if (line.includes("-->")) inHtmlComment = false;
      continue;
    }
    if (line.includes("<!--") && !line.includes("-->")) {
      inHtmlComment = true;
      continue;
    }

    // Track code blocks
    if (line.trimStart().startsWith("```")) {
      inCode = !inCode;
      continue;
    }
    if (inCode) continue;

    // Match ATX headings: # Heading text
    const m = line.match(/^(#{1,6})\s+(.+)/);
    if (!m) continue;

    // Strip trailing # characters and trim
    const text = m[2].replace(/\s*#+\s*$/, "").trim();
    const baseSlug = githubSlug(text);

    // Handle duplicate slugs (GitHub appends -1, -2, etc.)
    const count = slugCounts.get(baseSlug) ?? 0;
    const finalSlug = count === 0 ? baseSlug : `${baseSlug}-${count}`;
    slugCounts.set(baseSlug, count + 1);

    const entry: HeadingSlug = { slug: finalSlug, line: i + 1 };
    const existing = slugMap.get(finalSlug);
    if (existing) {
      existing.push(entry);
    } else {
      slugMap.set(finalSlug, [entry]);
    }
  }

  return slugMap;
};

// -- Heading index cache ------------------------------------------------------

const headingCache = new Map<string, Map<string, HeadingSlug[]>>();

const getHeadingIndex = (filePath: string): Map<string, HeadingSlug[]> => {
  const cached = headingCache.get(filePath);
  if (cached) return cached;

  const content = readFileSync(filePath, "utf-8");
  const index = buildHeadingIndex(content);
  headingCache.set(filePath, index);
  return index;
};

// -- Link extraction ----------------------------------------------------------

interface LinkRef {
  file: string;       // source file (absolute)
  line: number;       // 1-based line number
  text: string;       // link text [text]
  target: string;     // raw target from (target)
  filePart: string;   // file portion (before #)
  anchor: string;     // anchor portion (after #), empty if none
}

const extractLinks = (filePath: string): LinkRef[] => {
  const content = readFileSync(filePath, "utf-8");
  const lines = content.split("\n");
  const refs: LinkRef[] = [];
  let inCode = false;
  let inHtmlComment = false;

  for (const [i, line] of lines.entries()) {
    // Track multi-line HTML comments
    if (inHtmlComment) {
      if (line.includes("-->")) inHtmlComment = false;
      continue;
    }
    if (line.includes("<!--")) {
      if (!line.includes("-->")) {
        inHtmlComment = true;
      }
      continue;
    }

    // Track fenced code blocks
    if (line.trimStart().startsWith("```")) {
      inCode = !inCode;
      continue;
    }
    if (inCode) continue;

    // Match markdown links: [text](target)
    // Skip image links: ![alt](path)
    const linkRe = /(?<!!)\[([^\]]*)\]\(([^)]+)\)/g;
    for (const m of line.matchAll(linkRe)) {
      const text = m[1];
      const target = m[2];

      // Skip external URLs
      if (target.startsWith("http://") || target.startsWith("https://") || target.startsWith("mailto:")) {
        continue;
      }

      // Skip targets that look like parameter lists rather than paths (e.g.
      // `[Symbol.match](string)` in API doc tables — these contain spaces or
      // commas, which real file paths never do)
      if (/[\s,]/.test(target)) continue;

      // Skip targets that resolve above the project root (e.g. `../../pull/66`
      // for GitHub PR links which are only valid on github.com)
      const resolvedTarget = resolve(dirname(filePath), target.split("#")[0] || ".");
      if (!resolvedTarget.startsWith(ROOT)) continue;

      // Split into file part and anchor
      const hashIdx = target.indexOf("#");
      let filePart: string;
      let anchor: string;
      if (hashIdx === -1) {
        filePart = target;
        anchor = "";
      } else if (hashIdx === 0) {
        filePart = "";
        anchor = target.slice(1);
      } else {
        filePart = target.slice(0, hashIdx);
        anchor = target.slice(hashIdx + 1);
      }

      // Only check links that point to markdown files.  Skip:
      //   - Non-markdown extensions (e.g. .pas, .js, .ts, .json)
      //   - Bare words with no extension and no path separator — these are
      //     method-signature artifacts like `[Symbol.match](string)` in tables
      if (filePart !== "") {
        const ext = extname(filePart);
        if (ext !== "" && !EXTENSIONS.has(ext)) continue;
        if (ext === "" && !filePart.includes("/")) continue;
      }

      // Skip same-file anchor links when there is no anchor (bare # would be empty)
      if (filePart === "" && anchor === "") continue;

      refs.push({
        file: filePath,
        line: i + 1,
        text,
        target,
        filePart,
        anchor,
      });
    }
  }

  return refs;
};

// -- Similar anchor suggestions -----------------------------------------------

// Utility: generate an index array [start, start+1, ..., end-1]
const rangeFrom = (start: number, end: number): number[] =>
  Array.from({ length: end - start }, (_, i) => start + i);

const suggestAnchors = (slugMap: Map<string, HeadingSlug[]>, target: string, max: number): string[] => {
  // Simple Levenshtein-based similarity for short strings
  const distance = (a: string, b: string): number => {
    const m = a.length;
    const n = b.length;
    const dp: number[][] = Array.from({ length: m + 1 }, () => new Array(n + 1).fill(0));
    for (const i of rangeFrom(0, m + 1)) dp[i][0] = i;
    for (const j of rangeFrom(0, n + 1)) dp[0][j] = j;
    for (const i of rangeFrom(1, m + 1)) {
      for (const j of rangeFrom(1, n + 1)) {
        dp[i][j] = Math.min(
          dp[i - 1][j] + 1,
          dp[i][j - 1] + 1,
          dp[i - 1][j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1),
        );
      }
    }
    return dp[m][n];
  };

  const allSlugs = [...slugMap.keys()];
  const scored = allSlugs
    .map((s) => ({ slug: s, dist: distance(s, target) }))
    .sort((a, b) => a.dist - b.dist);

  return scored.slice(0, max).map((s) => s.slug);
};

// -- Link validation ----------------------------------------------------------

interface LinkError {
  file: string;      // relative path
  line: number;
  text: string;
  target: string;
  reason: string;
}

const validateLink = (ref: LinkRef): LinkError | null => {
  const sourceDir = dirname(ref.file);

  // Resolve the target file
  let targetPath: string;
  if (ref.filePart === "") {
    // Same-file anchor link
    targetPath = ref.file;
  } else {
    targetPath = resolve(sourceDir, ref.filePart);
  }

  // Check file existence
  if (ref.filePart !== "" && !existsSync(targetPath)) {
    return {
      file: relative(ROOT, ref.file),
      line: ref.line,
      text: ref.text,
      target: ref.target,
      reason: "file not found",
    };
  }

  // Check anchor if present
  if (ref.anchor !== "") {
    // Only check anchors for markdown files
    const ext = extname(targetPath);
    if (!EXTENSIONS.has(ext)) return null;

    const slugMap = getHeadingIndex(targetPath);
    if (!slugMap.has(ref.anchor)) {
      const suggestions = suggestAnchors(slugMap, ref.anchor, 5);
      const available = suggestions.length > 0
        ? ` (available: ${suggestions.join(", ")})`
        : "";
      return {
        file: relative(ROOT, ref.file),
        line: ref.line,
        text: ref.text,
        target: ref.target,
        reason: `anchor not found${available}`,
      };
    }
  }

  return null;
};

// -- Main ---------------------------------------------------------------------

const main = (): void => {
  console.log("Checking documentation links...\n");

  const files = findMarkdownFiles(ROOT);
  let totalLinks = 0;
  let brokenCount = 0;
  const errors: LinkError[] = [];

  for (const file of files) {
    const refs = extractLinks(file);

    for (const ref of refs) {
      totalLinks++;
      const error = validateLink(ref);

      if (error !== null) {
        brokenCount++;
        errors.push(error);
        console.log(`  FAIL  ${error.file}:${error.line} \u2014 [${error.text}](${error.target}) (${error.reason})`);
      } else if (VERBOSE) {
        console.log(`  OK    ${relative(ROOT, ref.file)}:${ref.line} \u2014 [${ref.text}](${ref.target})`);
      }
    }
  }

  console.log(`\nChecked ${files.length} files, ${totalLinks} links. ${brokenCount} broken.`);

  if (brokenCount > 0) {
    process.exit(1);
  }
};

main();
