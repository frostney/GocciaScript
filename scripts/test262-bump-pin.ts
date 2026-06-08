#!/usr/bin/env bun
/**
 * test262-bump-pin.ts
 *
 * Update the test262 SHA pin used by CI and PR workflows.
 * Invoked by the weekly `.github/workflows/test262-bump.yml` cron job.
 *
 * Usage:
 *   bun scripts/test262-bump-pin.ts <new-sha>
 *
 * Exits with status 0 and reports "no-op" when every pinned SHA already
 * matches the argument. Exits non-zero only on argument or I/O errors.
 *
 * Scope: only rewrites the dedicated pin file, so the weekly bump PR does
 * not need to modify workflow files.
 */

import { readFileSync, writeFileSync } from "fs";

const SHA_RE = /^[0-9a-f]{40}$/;
const TARGET = "scripts/test262-suite-sha.txt";

function main(argv: string[]): number {
  if (argv.length !== 1 || !SHA_RE.test(argv[0])) {
    console.error("Usage: bun scripts/test262-bump-pin.ts <40-hex-sha>");
    return 2;
  }
  const newSha = argv[0];

  let text: string;
  try {
    text = readFileSync(TARGET, "utf8").trim();
  } catch (err) {
    console.error(`Failed to read ${TARGET}: ${(err as Error).message}`);
    return 1;
  }

  if (!SHA_RE.test(text)) {
    console.error(`${TARGET}: expected one 40-hex SHA`);
    return 1;
  }

  if (text === newSha) {
    console.log(`${TARGET}: already at ${newSha}`);
    console.log(`No changes; pin already at ${newSha}.`);
    return 0;
  }

  writeFileSync(TARGET, `${newSha}\n`);
  console.log(`${TARGET}: bumped pin -> ${newSha}`);
  return 0;
}

process.exit(main(process.argv.slice(2)));
