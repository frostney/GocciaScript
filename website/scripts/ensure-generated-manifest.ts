#!/usr/bin/env bun
/**
 * Create `src/generated/vendor-manifest.json` if it does not exist yet.
 *
 * `src/lib/vendor-manifest-server.ts` imports that file statically, which is
 * what puts the vendored engine list inside every bundle that needs it (a
 * `process.cwd()` read is invisible to file tracing, so the playground page
 * shipped without one). A static import means the file has to exist before
 * anything typechecks, tests, or builds — including in a fresh checkout that
 * has never fetched a binary.
 *
 * So this runs from `postinstall`, the same place Fumadocs generates its
 * `.source/` adapter, and writes an *empty* manifest: no versions, which the
 * page already renders as an empty picker. `prebuild` then overwrites it with
 * the real vendored set (`scripts/fetch-binaries.ts`).
 *
 * Never overwrites an existing file — a vendoring run must survive a
 * subsequent `bun install`.
 *
 * Honoured env overrides:
 *   GOCCIA_GENERATED_MANIFEST_PATH — write somewhere else (tests)
 */

import { existsSync } from "node:fs";
import { mkdir, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));

export const GENERATED_MANIFEST_PATH =
  process.env.GOCCIA_GENERATED_MANIFEST_PATH ??
  path.resolve(here, "..", "src", "generated", "vendor-manifest.json");

export const GENERATED_MANIFEST_NOTE =
  "Generated file — do not edit or commit. `postinstall` creates it empty (scripts/ensure-generated-manifest.ts); `prebuild` fills it with the engines vendored for this build (scripts/fetch-binaries.ts). It exists so the playground page's bundle carries the version list instead of reading a path the bundler cannot see.";

export async function ensureGeneratedManifest(): Promise<"created" | "kept"> {
  if (existsSync(GENERATED_MANIFEST_PATH)) return "kept";
  await mkdir(path.dirname(GENERATED_MANIFEST_PATH), { recursive: true });
  const placeholder = {
    _note: GENERATED_MANIFEST_NOTE,
    defaultVersion: "nightly",
    versions: [],
  };
  await writeFile(
    GENERATED_MANIFEST_PATH,
    `${JSON.stringify(placeholder, null, 2)}\n`,
    "utf8",
  );
  return "created";
}

if (import.meta.main) {
  const result = await ensureGeneratedManifest();
  console.log(
    result === "created"
      ? `[ensure-generated-manifest] wrote empty placeholder ${GENERATED_MANIFEST_PATH}`
      : "[ensure-generated-manifest] already present — keeping the vendored set",
  );
}
