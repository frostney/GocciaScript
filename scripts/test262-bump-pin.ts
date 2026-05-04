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
 * Idempotent: no-op if every pinned SHA already matches the argument.
 */

import { readFileSync, writeFileSync } from "fs";

const SHA_RE = /^[0-9a-f]{40}$/;
const TARGETS = [".github/workflows/ci.yml", ".github/workflows/pr.yml"];

// Match any line of the form `ref: <40-hex-sha>` inside the test262 checkout
// step. Each workflow has exactly one such line; the pattern is robust to
// indentation changes and surrounding YAML structure.
const REF_RE = /(\bref:\s*)([0-9a-f]{40})/g;

function main(argv: string[]): number {
  if (argv.length !== 1 || !SHA_RE.test(argv[0])) {
    console.error("Usage: bun scripts/test262-bump-pin.ts <40-hex-sha>");
    return 2;
  }
  const newSha = argv[0];
  let totalChanged = 0;
  for (const target of TARGETS) {
    let text: string;
    try {
      text = readFileSync(target, "utf8");
    } catch (err) {
      console.error(`Failed to read ${target}: ${(err as Error).message}`);
      return 1;
    }
    let fileChanged = 0;
    const updated = text.replace(REF_RE, (full, prefix, oldSha) => {
      if (oldSha === newSha) return full;
      fileChanged++;
      return `${prefix}${newSha}`;
    });
    if (fileChanged > 0) {
      writeFileSync(target, updated);
      console.log(`${target}: bumped ${fileChanged} pin(s) -> ${newSha}`);
      totalChanged += fileChanged;
    } else {
      console.log(`${target}: already at ${newSha}`);
    }
  }
  if (totalChanged === 0) console.log("No changes; pin already up to date.");
  return 0;
}

process.exit(main(process.argv.slice(2)));
