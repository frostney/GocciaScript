import "server-only";

import { existsSync, readFileSync } from "node:fs";
import path from "node:path";

import {
  EMPTY_MANIFEST,
  normalizeManifest,
  type VendorManifest,
} from "@/lib/vendor-manifest";

let cached: VendorManifest | null = null;

/** Read `vendor/manifest.json`, cached for the lifetime of the Node module
 *  (Vercel reuses module state across warm function invocations, so the JSON
 *  parse happens once per cold start). Returns an empty manifest when the file
 *  is missing — local dev without `prebuild` shouldn't 500. */
export function getVendorManifest(): VendorManifest {
  if (cached) return cached;
  const file = path.join(process.cwd(), "vendor", "manifest.json");
  if (!existsSync(file)) {
    cached = EMPTY_MANIFEST;
    return cached;
  }
  try {
    cached = normalizeManifest(JSON.parse(readFileSync(file, "utf8")));
  } catch {
    cached = EMPTY_MANIFEST;
  }
  return cached;
}

/** Test-only: drop the cached manifest so a fresh read picks up edits. */
export function __resetVendorManifestCacheForTests(): void {
  cached = null;
}
