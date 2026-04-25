#!/usr/bin/env node
// Mirror ../docs/**/*.md and ../README.md into website/content/docs/.
// Single source of truth lives in the repo root; this directory is gitignored
// and rebuilt from scratch on every dev/build.

import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const websiteRoot = path.resolve(here, "..");
const repoRoot = path.resolve(websiteRoot, "..");
const docsSrc = path.join(repoRoot, "docs");
const readmeSrc = path.join(repoRoot, "README.md");
const docsDest = path.join(websiteRoot, "content", "docs");

async function exists(p) {
  try {
    await fs.access(p);
    return true;
  } catch (err) {
    // Only "missing" should map to false. Permission / IO failures must
    // propagate so the prebuild aborts loudly instead of silently skipping
    // copies.
    if (err && err.code === "ENOENT") return false;
    throw err;
  }
}

async function copyMarkdownTree(src, dest) {
  if (!(await exists(src))) return 0;
  const entries = await fs.readdir(src, { withFileTypes: true });
  let count = 0;
  for (const entry of entries) {
    const srcPath = path.join(src, entry.name);
    const destPath = path.join(dest, entry.name);
    if (entry.isDirectory()) {
      await fs.mkdir(destPath, { recursive: true });
      count += await copyMarkdownTree(srcPath, destPath);
    } else if (entry.isFile() && entry.name.toLowerCase().endsWith(".md")) {
      await fs.copyFile(srcPath, destPath);
      count++;
    }
  }
  return count;
}

async function main() {
  await fs.rm(docsDest, { recursive: true, force: true });
  await fs.mkdir(docsDest, { recursive: true });

  const docCount = await copyMarkdownTree(docsSrc, docsDest);

  const readmeCopied = await exists(readmeSrc);
  if (readmeCopied) {
    await fs.copyFile(readmeSrc, path.join(docsDest, "readme.md"));
  }

  const suffix = readmeCopied ? " + README" : "";
  console.log(
    `[sync-docs] copied ${docCount} doc files${suffix} from ${path.relative(websiteRoot, repoRoot)}/docs → content/docs`,
  );
}

main().catch((err) => {
  console.error("[sync-docs] failed:", err);
  process.exit(1);
});
