import { existsSync } from "node:fs";
import path from "node:path";

/** Runner names across releases, newest first: `GocciaRunner` from 0.14,
 *  `GocciaScriptLoader` from 0.7 (PR #333 added the `Goccia` prefix), and
 *  `ScriptLoader` before that. */
export const RUNNER_BINARY_NAMES = [
  "GocciaRunner",
  "GocciaScriptLoader",
  "ScriptLoader",
] as const;

/** Test runner names across releases, newest first. */
export const TEST_RUNNER_BINARY_NAMES = [
  "GocciaTestRunner",
  "TestRunner",
] as const;

export type ArchiveBinaryNames = { loader: string; testRunner: string };

function firstPresent(
  archiveRoot: string,
  candidates: readonly string[],
  exe: string,
  exists: (file: string) => boolean,
): string | null {
  for (const name of candidates) {
    const file = `${name}${exe}`;
    if (exists(path.join(archiveRoot, file))) return file;
  }
  return null;
}

/** Find the runner and test runner in an extracted release archive.
 *
 *  The names changed twice, and the nightly tag says nothing about which
 *  release line it follows, so the archive itself is the source of truth:
 *  probe for each known name, newest first. The manifest records whichever
 *  name was found, so the API needs no per-version awareness. */
export function resolveArchiveBinaryNames(
  archiveRoot: string,
  isWindows: boolean,
  exists: (file: string) => boolean = existsSync,
): ArchiveBinaryNames {
  const exe = isWindows ? ".exe" : "";
  const loader = firstPresent(archiveRoot, RUNNER_BINARY_NAMES, exe, exists);
  const testRunner = firstPresent(
    archiveRoot,
    TEST_RUNNER_BINARY_NAMES,
    exe,
    exists,
  );
  const missing: string[] = [];
  if (!loader) missing.push(RUNNER_BINARY_NAMES.join(" / "));
  if (!testRunner) missing.push(TEST_RUNNER_BINARY_NAMES.join(" / "));
  if (!loader || !testRunner) {
    throw new Error(
      `expected binary missing in archive ${archiveRoot}: ${missing.join(", ")}`,
    );
  }
  return { loader, testRunner };
}
