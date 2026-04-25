#!/usr/bin/env npx tsx
/**
 * check-doc-length.ts
 *
 * Ensures documentation files stay digestible for humans and AI agents.
 * Exits non-zero if any markdown file exceeds the configured line limit.
 *
 * Files can opt out with a frontmatter or HTML comment:
 *   <!-- doc-length-limit: 1600 -->
 *
 * Written in GocciaScript-compatible style (arrow functions, const/let,
 * for...of, no var, strict equality) so it can be bootstrapped later.
 *
 * Usage:
 *   npx tsx scripts/check-doc-length.ts
 *   npx tsx scripts/check-doc-length.ts --verbose
 *   DOC_LENGTH_LIMIT=600 npx tsx scripts/check-doc-length.ts
 */

import { readFileSync, readdirSync, lstatSync, realpathSync } from "fs";
import { join, relative, dirname } from "path";
import { fileURLToPath } from "url";

// ── Config ─────────────────────────────────────────────────────────────

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const DEFAULT_LIMIT = parseInt(process.env.DOC_LENGTH_LIMIT ?? "800", 10);
const VERBOSE = process.argv.includes("--verbose");
const EXTENSIONS = new Set([".md", ".mdx"]);
const IGNORE_DIRS = new Set(["node_modules", ".git", "dist", "build", ".next", "vendor"]);
// Build artifacts whose contents are synced from elsewhere and validated at
// the source location. Path-prefix matched against repo-relative paths.
const IGNORE_PATH_PREFIXES = ["website/content/docs/"];

// Files that are inherently large by nature (API references, test catalogs)
// get a higher limit.  The per-file override comment takes precedence.
const ELEVATED_LIMITS: Record<string, number> = {
  // All files currently fit within the default 800-line limit.
  // Add entries here if a file legitimately needs more space.
};

// ── File discovery ─────────────────────────────────────────────────────

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

// ── Per-file limit override ────────────────────────────────────────────

const parseFileLimit = (content: string): number | null => {
  // Look for <!-- doc-length-limit: NNN --> in the first 10 lines
  const lines = content.split("\n").slice(0, 10);
  for (const line of lines) {
    const m = line.match(/<!--\s*doc-length-limit:\s*(\d+)\s*-->/);
    if (m) return parseInt(m[1], 10);
  }
  return null;
};

// ── Main ───────────────────────────────────────────────────────────────

const main = (): void => {
  const files = findMarkdownFiles(ROOT);
  let failures = 0;
  const results: { file: string; lines: number; limit: number }[] = [];

  for (const file of files) {
    const rel = relative(ROOT, file);
    const content = readFileSync(file, "utf-8");
    const lineCount = content.split("\n").length;

    // Priority: per-file comment > elevated limits map > default
    const fileLimit = parseFileLimit(content) ?? ELEVATED_LIMITS[rel] ?? DEFAULT_LIMIT;

    if (lineCount > fileLimit) {
      failures++;
      results.push({ file: rel, lines: lineCount, limit: fileLimit });
      console.log(`  FAIL  ${rel}: ${lineCount} lines (limit ${fileLimit})`);
    } else if (VERBOSE) {
      console.log(`  OK    ${rel}: ${lineCount}/${fileLimit}`);
    }
  }

  console.log(`\nChecked ${files.length} files, default limit ${DEFAULT_LIMIT} lines.`);

  if (failures > 0) {
    console.log(`\n${failures} file(s) exceed their line limit.\n`);
    console.log("To fix: split large files into focused documents, or add a per-file override:");
    console.log("  <!-- doc-length-limit: 1200 -->\n");
    process.exit(1);
  } else {
    console.log("All files within limits.");
  }
};

main();
