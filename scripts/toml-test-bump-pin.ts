#!/usr/bin/env bun
/**
 * toml-test-bump-pin.ts
 *
 * Update the toml-test SHA pin in `scripts/run_toml_test_suite.py`.
 * Invoked by the weekly `.github/workflows/toml-test-bump.yml` cron job.
 *
 * Usage:
 *   bun scripts/toml-test-bump-pin.ts <new-sha>
 */

import { readFileSync, writeFileSync } from "fs";

const SHA_RE = /^[0-9a-f]{40}$/;
const TARGET = "scripts/run_toml_test_suite.py";

const SUITE_SHA_RE = /(SUITE_SHA\s*=\s*")([0-9a-f]{40})(")/g;

function main(argv: string[]): number {
  if (argv.length !== 1 || !SHA_RE.test(argv[0])) {
    console.error("Usage: bun scripts/toml-test-bump-pin.ts <40-hex-sha>");
    return 2;
  }
  const newSha = argv[0];

  let text: string;
  try {
    text = readFileSync(TARGET, "utf8");
  } catch (err) {
    console.error(`Failed to read ${TARGET}: ${(err as Error).message}`);
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
    console.error(`${TARGET}: no SUITE_SHA constant found`);
    return 1;
  }

  if (changed > 0) {
    writeFileSync(TARGET, updated);
    console.log(`${TARGET}: bumped ${changed} pin(s) -> ${newSha}`);
  } else {
    console.log(`${TARGET}: already at ${newSha}`);
  }

  return 0;
}

process.exit(main(process.argv.slice(2)));
