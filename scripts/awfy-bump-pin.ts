#!/usr/bin/env bun
/**
 * awfy-bump-pin.ts
 *
 * Update the AWFY corpus SHA pin used by the cross-engine benchmark driver.
 * Invoked by the weekly `.github/workflows/awfy-bump.yml` cron job.
 *
 * Usage:
 *   bun scripts/awfy-bump-pin.ts <new-sha> [manifest-path]
 *
 * Exits with status 0 and reports "no-op" when the pinned SHA already
 * matches the argument. Exits non-zero only on argument, manifest, or I/O
 * errors.
 */

import { readFileSync, writeFileSync } from "fs";

const SHA_RE = /^[0-9a-f]{40}$/;
const DEFAULT_TARGET = "perf/awfy/manifest.json";

function main(argv: string[]): number {
  if ((argv.length !== 1 && argv.length !== 2) || !SHA_RE.test(argv[0])) {
    console.error("Usage: bun scripts/awfy-bump-pin.ts <40-hex-sha> [manifest-path]");
    return 2;
  }

  const [newSha, target = DEFAULT_TARGET] = argv;

  let text: string;
  try {
    text = readFileSync(target, "utf8");
  } catch (err) {
    console.error(`Failed to read ${target}: ${(err as Error).message}`);
    return 1;
  }

  let manifest: unknown;
  try {
    manifest = JSON.parse(text);
  } catch (err) {
    console.error(`${target}: invalid JSON: ${(err as Error).message}`);
    return 1;
  }

  const awfy = (manifest as { awfy?: { commit?: unknown } }).awfy;
  const oldSha = awfy?.commit;
  if (typeof oldSha !== "string" || !SHA_RE.test(oldSha)) {
    console.error(`${target}: expected awfy.commit to be one 40-hex SHA`);
    return 1;
  }

  if (oldSha === newSha) {
    console.log(`${target}: already at ${newSha}`);
    console.log(`No changes; pin already at ${newSha}.`);
    return 0;
  }

  const commitRe = new RegExp(`("commit"\\s*:\\s*")${oldSha}(")`);
  const updated = text.replace(commitRe, `$1${newSha}$2`);
  if (updated === text) {
    console.error(`${target}: could not rewrite awfy.commit`);
    return 1;
  }

  writeFileSync(target, updated);
  console.log(`${target}: bumped AWFY pin -> ${newSha}`);
  return 0;
}

process.exit(main(process.argv.slice(2)));
