#!/usr/bin/env bun
/**
 * Update the JetStream corpus SHA in the benchmark manifest and current
 * benchmark documentation.
 *
 * Usage:
 *   bun scripts/jetstream-bump-pin.ts <new-sha> [manifest-path] [docs-path]
 */

import { readFileSync, writeFileSync } from "fs";

const SHA_RE = /^[0-9a-f]{40}$/;
const DEFAULT_MANIFEST = "perf/jetstream/manifest.json";
const DEFAULT_DOCS = "docs/benchmarks.md";

function readText(target: string): string {
  try {
    return readFileSync(target, "utf8");
  } catch (error) {
    throw new Error(`Failed to read ${target}: ${(error as Error).message}`);
  }
}

function main(argv: string[]): number {
  if (argv.length < 1 || argv.length > 3 || !SHA_RE.test(argv[0] ?? "")) {
    console.error(
      "Usage: bun scripts/jetstream-bump-pin.ts " +
        "<40-hex-sha> [manifest-path] [docs-path]",
    );
    return 2;
  }

  const [newSha, manifestPath = DEFAULT_MANIFEST, docsPath = DEFAULT_DOCS] =
    argv;

  try {
    const manifestText = readText(manifestPath);
    const docsText = readText(docsPath);

    let manifest: unknown;
    try {
      manifest = JSON.parse(manifestText);
    } catch (error) {
      throw new Error(
        `${manifestPath}: invalid JSON: ${(error as Error).message}`,
      );
    }

    const oldSha = (
      manifest as { jetStream?: { commit?: unknown } }
    ).jetStream?.commit;
    if (typeof oldSha !== "string" || !SHA_RE.test(oldSha)) {
      throw new Error(
        `${manifestPath}: expected jetStream.commit to be one 40-hex SHA`,
      );
    }

    if (!docsText.includes(oldSha)) {
      throw new Error(`${docsPath}: expected current JetStream pin ${oldSha}`);
    }
    if (oldSha === newSha) {
      console.log(`${manifestPath}: already at ${newSha}`);
      return 0;
    }

    writeFileSync(manifestPath, manifestText.replace(oldSha, newSha));
    writeFileSync(docsPath, docsText.replaceAll(oldSha, newSha));
    console.log(`${manifestPath}: bumped JetStream pin -> ${newSha}`);
    console.log(`${docsPath}: synchronized JetStream pin -> ${newSha}`);
    return 0;
  } catch (error) {
    console.error((error as Error).message);
    return 1;
  }
}

process.exit(main(process.argv.slice(2)));
