import "server-only";

import { existsSync, readFileSync } from "node:fs";
import path from "node:path";

import generatedManifest from "@/generated/vendor-manifest.json";
import {
  pickVendorManifestSource,
  type VendorManifest,
} from "@/lib/vendor-manifest";

let cached: VendorManifest | null = null;

/** Read `vendor/manifest.json` from the working directory, or `null` when it
 *  is absent or unparseable. Only the API route bundles trace `vendor/**`
 *  (see `next.config.mjs`), so this returns `null` in the playground page's
 *  bundle, and during `next dev` before any vendoring run. */
function readDiskManifest(): unknown {
  const file = path.join(
    /* turbopackIgnore: true */ process.cwd(),
    "vendor",
    "manifest.json",
  );
  if (!existsSync(/* turbopackIgnore: true */ file)) return null;
  try {
    return JSON.parse(readFileSync(/* turbopackIgnore: true */ file, "utf8"));
  } catch {
    return null;
  }
}

/** The vendored engine set, cached for the lifetime of the Node module
 *  (Vercel reuses module state across warm function invocations, so the work
 *  happens once per cold start).
 *
 *  Resolution order is `vendor/manifest.json` on disk, then the statically
 *  imported `src/generated/vendor-manifest.json` — see
 *  `pickVendorManifestSource` for why both exist. Never throws: a checkout
 *  that has not run `prebuild` gets the empty manifest instead of a 500. */
export function getVendorManifest(): VendorManifest {
  if (cached) return cached;
  cached = pickVendorManifestSource(readDiskManifest(), generatedManifest);
  return cached;
}

/** Test-only: drop the cached manifest so a fresh read picks up edits. */
export function __resetVendorManifestCacheForTests(): void {
  cached = null;
}
