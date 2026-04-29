/** Long-option flag names (e.g. `"--asi"`, `"--max-memory"`) the binary's own
 *  `--help`/usage text advertises. Probed at vendor time and stored so the
 *  API can drop sandbox flags older engines don't recognize — picking a
 *  pre-0.7.0 version still executes instead of erroring on first
 *  unknown-option. The flag *name* is captured (no `=value` suffix); see
 *  `isFlagSupported`. */
export type VendorFeatureSet = {
  loader: string[];
  testRunner: string[];
};

/** A single vendored engine release. `binaries.loader` and `binaries.testRunner`
 *  are paths *relative to* the `vendor/` directory — joining with
 *  `process.cwd()/vendor` yields the absolute path the API spawns.
 *
 *  Pre-0.7.0 archives ship `ScriptLoader` / `TestRunner`; 0.7.0+ ship
 *  `GocciaScriptLoader` / `GocciaTestRunner` (rename in #333). The build
 *  script preserves whichever name was in the archive, so the per-version
 *  paths captured here are the source of truth — no naming heuristics in
 *  the API. */
export type VendorEntry = {
  tag: string;
  isPrerelease?: boolean;
  publishedAt?: string | null;
  binaries: {
    loader: string;
    testRunner: string;
  };
  features?: VendorFeatureSet;
};

export type VendorManifest = {
  /** The tag the API picks when the request omits `version`. Set by the
   *  build script to the latest stable pick (`versions[0]` once stable
   *  entries are sorted newest-first); falls back to `"nightly"` only when
   *  no stable was vendored, e.g. an early-stage repo with no releases. */
  defaultVersion: string;
  /** Newest-first stable picks plus `nightly`. Order matches the
   *  order in which tags should appear in the playground dropdown. */
  versions: VendorEntry[];
};

export const EMPTY_MANIFEST: VendorManifest = {
  defaultVersion: "nightly",
  versions: [],
};

/** Coerce arbitrary JSON into a `VendorManifest`, dropping malformed entries
 *  and supplying defaults. Exposed so the server-side loader and tests both
 *  go through the same validation path. */
export function normalizeManifest(raw: unknown): VendorManifest {
  if (!raw || typeof raw !== "object") return EMPTY_MANIFEST;
  const r = raw as Partial<VendorManifest>;
  const versions = Array.isArray(r.versions)
    ? r.versions.filter(isValidEntry)
    : [];
  return {
    defaultVersion:
      typeof r.defaultVersion === "string" && r.defaultVersion.length > 0
        ? r.defaultVersion
        : "nightly",
    versions,
  };
}

function isValidEntry(value: unknown): value is VendorEntry {
  if (!value || typeof value !== "object") return false;
  const entry = value as Partial<VendorEntry>;
  return (
    typeof entry.tag === "string" &&
    entry.tag.length > 0 &&
    typeof entry.binaries === "object" &&
    entry.binaries !== null &&
    typeof entry.binaries.loader === "string" &&
    typeof entry.binaries.testRunner === "string" &&
    isValidFeatures(entry.features)
  );
}

function isValidFeatures(
  value: unknown,
): value is VendorFeatureSet | undefined {
  if (value === undefined) return true;
  if (!value || typeof value !== "object") return false;
  const f = value as Partial<VendorFeatureSet>;
  return (
    Array.isArray(f.loader) &&
    f.loader.every((s) => typeof s === "string") &&
    Array.isArray(f.testRunner) &&
    f.testRunner.every((s) => typeof s === "string")
  );
}

/** Test whether the engine accepts an arg that begins with a known flag name.
 *  Long options come in two shapes:
 *
 *  - `--name=value` — split on `=` and look up the prefix.
 *  - `--name <value>` (separate args, e.g. `--allowed-host api.example.com`)
 *    — the caller passes just `--name`; we look it up directly.
 *
 *  Bare positional args (e.g. the test file path) and short options aren't
 *  flag-shaped so they always pass through. */
export function isFlagSupported(
  features: VendorFeatureSet | undefined,
  arg: string,
  kind: "loader" | "testRunner",
): boolean {
  if (!arg.startsWith("--")) return true;
  if (!features) return true; // Pre-feature manifest: don't filter.
  const name = arg.split("=", 1)[0];
  return features[kind].includes(name);
}

/** Strip a leading `v` so `"v0.7.0"` and `"0.7.0"` compare equal.
 *  `"nightly"` and other non-semver tags pass through unchanged. */
function canonicalTag(tag: string): string {
  return tag.startsWith("v") ? tag.slice(1) : tag;
}

/** Look up a manifest entry by tag, accepting both `"v0.7.0"` and `"0.7.0"`.
 *  Returns `null` when the tag isn't vendored. */
export function findVersion(
  manifest: VendorManifest,
  tag: string,
): VendorEntry | null {
  const wanted = canonicalTag(tag);
  for (const entry of manifest.versions) {
    if (canonicalTag(entry.tag) === wanted) return entry;
  }
  return null;
}

/** The tag list rendered in the playground's version dropdown.
 *  Preserves the manifest's order (newest stable first, `nightly` last). */
export function listPlaygroundVersions(manifest: VendorManifest): string[] {
  return manifest.versions.map((entry) => entry.tag);
}
