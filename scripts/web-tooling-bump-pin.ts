#!/usr/bin/env bun
/**
 * Update the V8 Web Tooling corpus SHA in the benchmark manifest.
 *
 * Usage:
 *   bun scripts/web-tooling-bump-pin.ts <new-sha> [manifest-path]
 */

import { readFileSync, writeFileSync } from "fs";

const SHA_RE = /^[0-9a-f]{40}$/;
const DEFAULT_MANIFEST = "perf/web-tooling/manifest.json";

function main(argv: string[]): number {
  if ((argv.length !== 1 && argv.length !== 2) || !SHA_RE.test(argv[0] ?? "")) {
    console.error(
      "Usage: bun scripts/web-tooling-bump-pin.ts " +
        "<40-hex-sha> [manifest-path]",
    );
    return 2;
  }

  const [newSha, manifestPath = DEFAULT_MANIFEST] = argv;

  let text: string;
  try {
    text = readFileSync(manifestPath, "utf8");
  } catch (error) {
    console.error(
      `Failed to read ${manifestPath}: ${(error as Error).message}`,
    );
    return 1;
  }

  let manifest: unknown;
  try {
    manifest = JSON.parse(text);
  } catch (error) {
    console.error(
      `${manifestPath}: invalid JSON: ${(error as Error).message}`,
    );
    return 1;
  }

  const oldSha =
    manifest && typeof manifest === "object"
      ? (manifest as { webTooling?: { commit?: unknown } }).webTooling?.commit
      : undefined;
  if (typeof oldSha !== "string" || !SHA_RE.test(oldSha)) {
    console.error(
      `${manifestPath}: expected webTooling.commit to be one 40-hex SHA`,
    );
    return 1;
  }
  if (oldSha === newSha) {
    console.log(`${manifestPath}: already at ${newSha}`);
    return 0;
  }

  const commitRe = new RegExp(`("commit"\\s*:\\s*")${oldSha}(")`);
  const updated = text.replace(commitRe, `$1${newSha}$2`);
  if (updated === text) {
    console.error(`${manifestPath}: could not rewrite webTooling.commit`);
    return 1;
  }

  writeFileSync(manifestPath, updated);
  console.log(`${manifestPath}: bumped Web Tooling pin -> ${newSha}`);
  return 0;
}

process.exit(main(process.argv.slice(2)));
