#!/usr/bin/env bun
/**
 * test262-bump-pin.ts
 *
 * Update the test262 SHA pin in `.github/workflows/ci.yml` and `pr.yml`.
 * Invoked by the weekly `.github/workflows/test262-bump.yml` cron job.
 *
 * Usage:
 *   bun scripts/test262-bump-pin.ts <new-sha>
 *
 * Exits with status 0 and reports "no-op" when every pinned SHA already
 * matches the argument. Exits non-zero only on argument or I/O errors.
 *
 * Scope: only rewrites the `ref:` line that lives inside a
 * `tc39/test262` checkout step. Other 40-hex `ref:` values in the
 * workflows (e.g. unrelated action pins) are not touched.
 */

import { readFileSync, writeFileSync } from "fs";

const SHA_RE = /^[0-9a-f]{40}$/;
const TARGETS = [".github/workflows/ci.yml", ".github/workflows/pr.yml"];

// Match a checkout step that points at tc39/test262 (`repository: tc39/test262`)
// followed within the same `with:` block by a `ref:` line carrying a 40-hex SHA.
// Multi-line, dot matches newline. Captures the prefix-up-to-and-including
// `ref:`, then the SHA, so the replacement leaves indentation/comments intact.
const TEST262_REF_RE =
  /(repository:\s*tc39\/test262[\s\S]*?\bref:\s*)([0-9a-f]{40})/g;

function main(argv: string[]): number {
  if (argv.length !== 1 || !SHA_RE.test(argv[0])) {
    console.error("Usage: bun scripts/test262-bump-pin.ts <40-hex-sha>");
    return 2;
  }
  const newSha = argv[0];
  let totalChanged = 0;
  let totalSeen = 0;
  for (const target of TARGETS) {
    let text: string;
    try {
      text = readFileSync(target, "utf8");
    } catch (err) {
      console.error(`Failed to read ${target}: ${(err as Error).message}`);
      return 1;
    }
    let fileChanged = 0;
    let fileSeen = 0;
    const updated = text.replace(TEST262_REF_RE, (full, prefix, oldSha) => {
      fileSeen++;
      if (oldSha === newSha) return full;
      fileChanged++;
      return `${prefix}${newSha}`;
    });
    if (fileSeen === 0) {
      console.error(
        `${target}: no tc39/test262 checkout step found — refusing to modify ` +
          "(pin location must be inside a `repository: tc39/test262` block)",
      );
      return 1;
    }
    totalSeen += fileSeen;
    if (fileChanged > 0) {
      writeFileSync(target, updated);
      console.log(`${target}: bumped ${fileChanged} pin(s) -> ${newSha}`);
      totalChanged += fileChanged;
    } else {
      console.log(`${target}: already at ${newSha}`);
    }
  }
  if (totalChanged === 0) {
    console.log(`No changes; all ${totalSeen} pin(s) already at ${newSha}.`);
  }
  return 0;
}

process.exit(main(process.argv.slice(2)));
