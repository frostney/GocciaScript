#!/usr/bin/env bun
/**
 * suite-bump-pin.ts
 *
 * Update a SUITE_SHA pin in a compliance suite runner script.
 * Invoked by weekly bump workflows (toml-test-bump.yml, yaml-test-bump.yml).
 *
 * Usage:
 *   bun scripts/suite-bump-pin.ts <target-file> <new-sha>
 */

import { readFileSync, writeFileSync } from "fs";

const SHA_RE = /^[0-9a-f]{40}$/;
const SUITE_SHA_RE = /(SUITE_SHA\s*=\s*")([0-9a-f]{40})(")/g;

function main(argv: string[]): number {
  if (argv.length !== 2 || !SHA_RE.test(argv[1])) {
    console.error(
      "Usage: bun scripts/suite-bump-pin.ts <target-file> <40-hex-sha>",
    );
    return 2;
  }
  const [target, newSha] = argv;

  let text: string;
  try {
    text = readFileSync(target, "utf8");
  } catch (err) {
    console.error(`Failed to read ${target}: ${(err as Error).message}`);
    return 1;
  }

  let seen = 0;
  let changed = 0;
  const updated = text.replace(SUITE_SHA_RE, (full, prefix, oldSha, suffix) => {
    seen++;
    if (oldSha === newSha) return full;
    changed++;
    return `${prefix}${newSha}${suffix}`;
  });

  if (seen === 0) {
    console.error(`${target}: no SUITE_SHA constant found`);
    return 1;
  }

  if (changed > 0) {
    writeFileSync(target, updated);
    console.log(`${target}: bumped ${changed} pin(s) -> ${newSha}`);
  } else {
    console.log(`${target}: already at ${newSha}`);
  }

  return 0;
}

process.exit(main(process.argv.slice(2)));
