#!/usr/bin/env npx tsx
/**
 * Self-test for the markdown discovery shared by the check-doc-*.ts scripts.
 *
 * Builds a throwaway git repository and checks which files findMarkdownFiles
 * returns, then removes .git and checks the directory-walk fallback. Needs git
 * and Unix symbolic-link support; CI runs it on ubuntu-latest.
 *
 * Usage:
 *   npx tsx scripts/doc-checks/markdown-files.self-test.ts
 */

import { mkdirSync, readFileSync, rmSync, symlinkSync, writeFileSync } from "fs";
import { spawnSync } from "child_process";
import { dirname, join, relative } from "path";
import { clean, mkdtemp } from "../test-cli/tmpdir";
import { findMarkdownFiles } from "./markdown-files";

const write = (root: string, path: string, content = "# doc\n"): void => {
  mkdirSync(dirname(join(root, path)), { recursive: true });
  writeFileSync(join(root, path), content);
};

const git = (root: string, ...args: string[]): void => {
  const result = spawnSync("git", args, { cwd: root, encoding: "utf8" });
  if (result.status !== 0) throw new Error(`git ${args.join(" ")} failed: ${result.stderr}`);
};

const discover = (root: string, dir = root): string[] =>
  findMarkdownFiles(root, dir).map((file) => relative(root, file).split("\\").join("/")).sort();

let failures = 0;
const expectFiles = (name: string, actual: string[], expected: string[]): void => {
  const want = [...expected].sort();
  if (JSON.stringify(actual) === JSON.stringify(want)) {
    console.log(`  PASS  ${name}`);
  } else {
    failures++;
    console.log(`  FAIL  ${name}\n        expected ${JSON.stringify(want)}\n        actual   ${JSON.stringify(actual)}`);
  }
};

const expectThrows = (name: string, run: () => unknown, pattern: RegExp): void => {
  try {
    const result = run();
    failures++;
    console.log(`  FAIL  ${name}\n        expected an error matching ${pattern}, got ${JSON.stringify(result)}`);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    if (pattern.test(message)) {
      console.log(`  PASS  ${name}`);
    } else {
      failures++;
      console.log(`  FAIL  ${name}\n        expected an error matching ${pattern}, got ${JSON.stringify(message)}`);
    }
  }
};

const root = mkdtemp("goccia-doc-files-");
try {
  git(root, "init", "-q");
  write(root, ".gitignore", "tmp/\n");
  write(root, "README.md");
  write(root, "docs/guide.md");
  write(root, "docs/page.mdx");
  write(root, "docs/notes.txt", "not markdown\n");
  write(root, "website/content/docs/synced.md");
  write(root, "node_modules/pkg/README.md");
  write(root, "docs/deleted.md");
  git(root, "add", "-A");
  git(root, "add", "-f", "node_modules/pkg/README.md");
  rmSync(join(root, "docs/deleted.md"));
  write(root, "docs/untracked.md");
  write(root, "tmp/copy/README.md");
  write(root, "tmp/copy/docs/guide.md");
  symlinkSync(join(root, "docs/guide.md"), join(root, "docs/guide-link.md"));

  const inGit = ["README.md", "docs/guide.md", "docs/page.mdx", "docs/untracked.md"];
  expectFiles(
    "git checkout: tracked and untracked docs, without gitignored, IGNORE_DIRS, prefix-ignored, deleted or symlinked duplicates",
    discover(root),
    inGit,
  );
  expectFiles("git checkout: a subdirectory scans only its own files", discover(root, join(root, "docs")), [
    "docs/guide.md",
    "docs/page.mdx",
    "docs/untracked.md",
  ]);

  // A git failure inside a work tree must not fall back to the walk, which
  // would scan the gitignored copy.
  const index = join(root, ".git", "index");
  const savedIndex = readFileSync(index);
  writeFileSync(index, "not an index");
  expectThrows("git checkout: a failing git ls-files throws instead of walking", () => discover(root), /git ls-files failed/);
  writeFileSync(index, savedIndex);

  rmSync(join(root, ".git"), { recursive: true, force: true });
  expectFiles(
    "no git checkout: walks the tree, so the gitignored copy is included",
    discover(root),
    [...inGit, "tmp/copy/README.md", "tmp/copy/docs/guide.md"],
  );
} finally {
  clean(root);
}

console.log(failures === 0 ? "\nAll markdown discovery self-tests passed." : `\n${failures} markdown discovery self-test(s) failed.`);
process.exit(failures === 0 ? 0 : 1);
