#!/usr/bin/env bun
/**
 * Fetch GocciaScript engine release archives at prebuild time and stage
 * them under `website/vendor/<tag>/` for the API route handlers to spawn
 * at request time. The set of vendored versions is `nightly` plus the
 * top three stable picks from `pickPrecedenceVersions` — same algorithm
 * the docs already use for "patch / minor / major" precedence.
 *
 * Wired up from `package.json` as part of `prebuild`, so it only runs
 * during `next build`. `next dev` skips it; the API's dev fallback chain
 * uses the locally compiled `../build/<binary>` instead.
 *
 * Set `SKIP_VENDOR_FETCH=1` to bypass when offline or when the binaries
 * have already been hand-placed under `vendor/`.
 *
 * Honoured env overrides:
 *   GOCCIA_REPO              — `owner/repo` (default: frostney/GocciaScript)
 *   GOCCIA_NIGHTLY_TAG       — release tag for the rolling nightly (default: nightly)
 *   GOCCIA_PLAYGROUND_TAGS   — comma-separated tag list to vendor instead of
 *                              the precedence-picked set (debug only;
 *                              `nightly` is always implicitly included)
 *   GITHUB_TOKEN             — used as Bearer for higher-quota API calls
 *   SKIP_VENDOR_FETCH=1      — early-out, no network fetch
 */

import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import {
  chmod,
  copyFile,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { parseSemverTag, pickPrecedenceVersions } from "../src/lib/github";

const REPO = process.env.GOCCIA_REPO ?? "frostney/GocciaScript";
const NIGHTLY_TAG = process.env.GOCCIA_NIGHTLY_TAG ?? "nightly";
const STABLE_PICKS = 3;
const here = path.dirname(fileURLToPath(import.meta.url));
const VENDOR_DIR = path.resolve(here, "..", "vendor");
const MANIFEST_PATH = path.join(VENDOR_DIR, "manifest.json");

type PlatformAsset = {
  os: "linux" | "macos" | "windows";
  arch: "x64" | "arm64" | "x86";
  ext: "zip" | "tar.gz";
};

type GhAsset = {
  name: string;
  browser_download_url: string;
  digest?: string | null;
};

type GhRelease = {
  tag_name: string;
  name?: string | null;
  draft?: boolean;
  prerelease?: boolean;
  published_at?: string | null;
  assets: GhAsset[];
};

type FeatureSet = {
  /** Long-option flags (e.g. `"--asi"`, `"--max-memory"`) the binary's own
   *  `--help` / usage text advertises. The runtime API consults this set to
   *  drop sandbox flags older engines don't recognize, so picking a
   *  pre-0.7.0 version still executes instead of erroring on first
   *  unknown-option. The flag *name* is captured (no `=value` suffix). */
  loader: string[];
  testRunner: string[];
};

type ManifestEntry = {
  tag: string;
  isPrerelease?: boolean;
  publishedAt?: string | null;
  binaries: { loader: string; testRunner: string };
  features?: FeatureSet;
  /** SHA-256 digest of the release archive — used for cache invalidation
   *  on re-runs (skip download when the GitHub-side digest still matches). */
  sourceAssetDigest?: string | null;
};

type Manifest = {
  defaultVersion: string;
  versions: ManifestEntry[];
};

/** Pre-0.7.0 archives ship binaries without the `Goccia` prefix (PR #333
 *  added the rename). The runtime path captured in the manifest preserves
 *  whichever name was in the archive, so the API doesn't need per-version
 *  awareness — the manifest is the source of truth. */
function archiveBinaryNames(
  tag: string,
  isWindows: boolean,
): { loader: string; testRunner: string } {
  const exe = isWindows ? ".exe" : "";
  if (tag === NIGHTLY_TAG) {
    return {
      loader: `GocciaScriptLoader${exe}`,
      testRunner: `GocciaTestRunner${exe}`,
    };
  }
  const semver = parseSemverTag(tag);
  const isLegacy = !!semver && semver.major === 0 && semver.minor < 7;
  return isLegacy
    ? { loader: `ScriptLoader${exe}`, testRunner: `TestRunner${exe}` }
    : {
        loader: `GocciaScriptLoader${exe}`,
        testRunner: `GocciaTestRunner${exe}`,
      };
}

function platformAsset(): PlatformAsset {
  const p = process.platform;
  const a = process.arch;
  // Match exactly the asset matrix `.github/workflows/ci.yml` publishes.
  // Anything else is a real "no asset built for this host" — fail loudly.
  if (p === "darwin") {
    if (a === "arm64" || a === "x64")
      return { os: "macos", arch: a, ext: "zip" };
    throw new Error(`unsupported macOS arch: ${a}`);
  }
  if (p === "linux") {
    if (a === "arm64" || a === "x64")
      return { os: "linux", arch: a, ext: "tar.gz" };
    throw new Error(`unsupported Linux arch: ${a}`);
  }
  if (p === "win32") {
    if (a === "ia32") return { os: "windows", arch: "x86", ext: "zip" };
    if (a === "x64") return { os: "windows", arch: "x64", ext: "zip" };
    throw new Error(`unsupported Windows arch: ${a}`);
  }
  throw new Error(`unsupported build host: ${p}/${a}`);
}

function ghHeaders(): Record<string, string> {
  const headers: Record<string, string> = {
    Accept: "application/vnd.github+json",
    "X-GitHub-Api-Version": "2022-11-28",
    "User-Agent": "gocciascript-website-build",
  };
  if (process.env.GITHUB_TOKEN) {
    headers.Authorization = `Bearer ${process.env.GITHUB_TOKEN}`;
  }
  return headers;
}

async function fetchReleases(): Promise<GhRelease[]> {
  const url = `https://api.github.com/repos/${REPO}/releases?per_page=30`;
  const res = await fetch(url, { headers: ghHeaders() });
  if (!res.ok) {
    throw new Error(`releases list HTTP ${res.status} ${res.statusText}`);
  }
  return (await res.json()) as GhRelease[];
}

async function fetchSingleRelease(tag: string): Promise<GhRelease> {
  const url = `https://api.github.com/repos/${REPO}/releases/tags/${encodeURIComponent(tag)}`;
  const res = await fetch(url, { headers: ghHeaders() });
  if (!res.ok) {
    throw new Error(`release ${tag} HTTP ${res.status} ${res.statusText}`);
  }
  return (await res.json()) as GhRelease;
}

function pickStableTags(releases: GhRelease[]): string[] {
  const stable: string[] = [];
  for (const r of releases) {
    if (r.draft || r.prerelease) continue;
    const tag = r.tag_name;
    if (typeof tag !== "string") continue;
    if (tag.toLowerCase() === NIGHTLY_TAG.toLowerCase()) continue;
    stable.push(tag);
  }
  return pickPrecedenceVersions(stable);
}

function selectExplicitTags(): string[] | null {
  const raw = process.env.GOCCIA_PLAYGROUND_TAGS;
  if (!raw) return null;
  const tags = raw
    .split(",")
    .map((s) => s.trim())
    .filter(Boolean);
  if (tags.length === 0) return null;
  return tags.filter((t) => t.toLowerCase() !== NIGHTLY_TAG.toLowerCase());
}

function findAsset(
  release: GhRelease,
  asset: PlatformAsset,
  tag: string,
): GhAsset {
  // Asset filenames follow `gocciascript-<tag>-<os>-<arch>.<ext>` from 0.4.0
  // onward (and for nightly). Older releases don't ship the same matrix; we
  // skip vendoring them in `pickStableTags` indirectly because precedence
  // picks always select the latest patch in each line.
  const wanted = `gocciascript-${tag}-${asset.os}-${asset.arch}.${asset.ext}`;
  const match = release.assets.find((a) => a.name === wanted);
  if (!match) {
    throw new Error(
      `release ${tag} has no asset named ${wanted}; available: ${release.assets
        .map((a) => a.name)
        .join(", ")}`,
    );
  }
  return match;
}

/** Spawn a binary in two probe modes — `--help` and no-args — and union
 *  every long-option flag advertised in the resulting usage text.
 *
 *  Pre-0.7.0 `TestRunner` doesn't recognize `--help` and prints a friendly
 *  usage banner only when run with no args; modern binaries respond to
 *  `--help` cleanly. Probing both modes catches the union without needing
 *  per-version awareness here.
 *
 *  The regex anchors on indented lines (`^\s+--name`) so flags mentioned
 *  inside descriptions ("Run with --help for usage", "implies --coverage")
 *  aren't mistaken for option entries. */
function probeFlags(binaryPath: string): string[] {
  const flags = new Set<string>();
  const probes: string[][] = [["--help"], []];
  for (const args of probes) {
    const result = spawnSync(binaryPath, args, {
      encoding: "utf8",
      timeout: 5_000,
      stdio: ["ignore", "pipe", "pipe"],
    });
    const text = `${result.stdout ?? ""}\n${result.stderr ?? ""}`;
    const re = /^\s+(--[a-z][a-z0-9-]*)/gm;
    let m: RegExpExecArray | null;
    // biome-ignore lint/suspicious/noAssignInExpressions: idiomatic exec loop
    while ((m = re.exec(text)) !== null) {
      flags.add(m[1]);
    }
  }
  return Array.from(flags).sort();
}

function probeBinaryFeatures(binaries: {
  loader: string;
  testRunner: string;
}): FeatureSet {
  return {
    loader: probeFlags(path.join(VENDOR_DIR, binaries.loader)),
    testRunner: probeFlags(path.join(VENDOR_DIR, binaries.testRunner)),
  };
}

async function extractAsset(
  archivePath: string,
  ext: PlatformAsset["ext"],
  dest: string,
): Promise<void> {
  const { status, stderr } =
    ext === "zip"
      ? spawnSync("unzip", ["-q", archivePath, "-d", dest], {
          encoding: "utf8",
        })
      : spawnSync("tar", ["xzf", archivePath, "-C", dest], {
          encoding: "utf8",
        });
  if (status !== 0) {
    throw new Error(`extraction failed: ${stderr ?? "no stderr"}`);
  }
}

/** Copy the two needed binaries from an extracted archive into
 *  `vendor/<tag>/`, preserving their archive-side filenames so the
 *  manifest's recorded path matches what's on disk. */
async function vendorBinaries(
  tag: string,
  archiveRoot: string,
  isWindows: boolean,
): Promise<{ loader: string; testRunner: string }> {
  const names = archiveBinaryNames(tag, isWindows);
  const versionDir = path.join(VENDOR_DIR, tag);
  await mkdir(versionDir, { recursive: true });

  for (const fname of [names.loader, names.testRunner]) {
    const src = path.join(archiveRoot, fname);
    if (!existsSync(src)) {
      throw new Error(`expected binary missing in archive: ${fname}`);
    }
    const dest = path.join(versionDir, fname);
    await rm(dest, { force: true });
    // `rename()` fails with EXDEV when `tmpdir()` is on a different mount —
    // common on CI workers. Use `copyFile` for a reliable cross-device move.
    await copyFile(src, dest);
    if (!isWindows) {
      // Defensive: tar/unzip should preserve POSIX modes, but a permissive
      // 0o755 ensures the function can `execve` the binary.
      await chmod(dest, 0o755);
    }
  }

  // Manifest paths are relative to `vendor/`.
  return {
    loader: path.posix.join(tag, names.loader),
    testRunner: path.posix.join(tag, names.testRunner),
  };
}

async function loadExistingManifest(): Promise<Manifest | null> {
  if (!existsSync(MANIFEST_PATH)) return null;
  try {
    const raw = await readFile(MANIFEST_PATH, "utf8");
    return JSON.parse(raw) as Manifest;
  } catch {
    return null;
  }
}

async function fetchOne(
  tag: string,
  release: GhRelease,
  asset: PlatformAsset,
  cached: ManifestEntry | null,
): Promise<ManifestEntry> {
  const ghAsset = findAsset(release, asset, tag);
  const isWindows = asset.os === "windows";

  // Cache hit: same digest *and* both binaries already on disk.
  if (
    cached?.sourceAssetDigest &&
    ghAsset.digest &&
    cached.sourceAssetDigest === ghAsset.digest &&
    existsSync(path.join(VENDOR_DIR, cached.binaries.loader)) &&
    existsSync(path.join(VENDOR_DIR, cached.binaries.testRunner))
  ) {
    console.log(`[fetch-binaries] cache hit: ${tag} (${ghAsset.digest})`);
    // Reprobe features when the cached entry predates this field — keeps
    // older manifests forward-compatible without forcing a re-download.
    const features = cached.features ?? probeBinaryFeatures(cached.binaries);
    return {
      ...cached,
      features,
      isPrerelease: release.prerelease ? true : undefined,
      publishedAt: release.published_at ?? cached.publishedAt ?? null,
    };
  }

  console.log(
    `[fetch-binaries] fetching ${tag} ← ${ghAsset.browser_download_url}`,
  );
  const work = await mkdtemp(path.join(tmpdir(), `goccia-vendor-${tag}-`));
  try {
    const archive = path.join(work, ghAsset.name);
    const res = await fetch(ghAsset.browser_download_url, {
      headers: { "user-agent": "gocciascript-website-build" },
    });
    if (!res.ok) {
      throw new Error(`download ${tag} HTTP ${res.status} ${res.statusText}`);
    }
    await writeFile(archive, Buffer.from(await res.arrayBuffer()));
    await extractAsset(archive, asset.ext, work);

    const archiveRoot = path.join(
      work,
      `gocciascript-${tag}-${asset.os}-${asset.arch}`,
    );
    const binaries = await vendorBinaries(tag, archiveRoot, isWindows);
    const features = probeBinaryFeatures(binaries);

    return {
      tag,
      isPrerelease: release.prerelease ? true : undefined,
      publishedAt: release.published_at ?? null,
      binaries,
      features,
      sourceAssetDigest: ghAsset.digest ?? null,
    };
  } finally {
    await rm(work, { recursive: true, force: true });
  }
}

async function main(): Promise<void> {
  if (process.env.SKIP_VENDOR_FETCH === "1") {
    console.log("[fetch-binaries] SKIP_VENDOR_FETCH=1 — leaving vendor/ as-is");
    return;
  }

  const asset = platformAsset();
  console.log(
    `[fetch-binaries] platform: ${asset.os}/${asset.arch} (.${asset.ext})`,
  );

  const cachedManifest = await loadExistingManifest();
  const cachedByTag = new Map<string, ManifestEntry>();
  if (cachedManifest) {
    for (const entry of cachedManifest.versions) {
      cachedByTag.set(entry.tag, entry);
    }
  }

  // Pick which stable tags to vendor. `GOCCIA_PLAYGROUND_TAGS` overrides for
  // debugging; otherwise fall back to the precedence picker. Either way we
  // always include `nightly`.
  const explicit = selectExplicitTags();
  let stableTags: string[];
  if (explicit) {
    stableTags = explicit.slice(0, STABLE_PICKS);
    console.log(
      `[fetch-binaries] GOCCIA_PLAYGROUND_TAGS override: ${stableTags.join(", ")}`,
    );
  } else {
    const releases = await fetchReleases();
    stableTags = pickStableTags(releases);
    console.log(
      `[fetch-binaries] precedence picks: ${stableTags.join(", ") || "(none)"}`,
    );
  }

  await mkdir(VENDOR_DIR, { recursive: true });

  const versions: ManifestEntry[] = [];

  // Fetch stable picks first (newest-first), then nightly so it ends up last
  // in the manifest order — that mirrors the dropdown UX.
  for (const tag of stableTags) {
    try {
      const release = await fetchSingleRelease(tag);
      const entry = await fetchOne(
        tag,
        release,
        asset,
        cachedByTag.get(tag) ?? null,
      );
      versions.push(entry);
    } catch (err) {
      // Stable miss is non-fatal: the playground will simply omit that tag
      // from the dropdown. Nightly is the hard requirement (below).
      console.warn(
        `[fetch-binaries] WARN: ${tag} skipped: ${(err as Error).message}`,
      );
    }
  }

  // Nightly: hard-fail if it doesn't land *and* there's no cached binary,
  // matching the existing fail-loud behavior of fetch-nightly-binary.mjs.
  let nightlyEntry: ManifestEntry | null = null;
  try {
    const release = await fetchSingleRelease(NIGHTLY_TAG);
    nightlyEntry = await fetchOne(
      NIGHTLY_TAG,
      release,
      asset,
      cachedByTag.get(NIGHTLY_TAG) ?? null,
    );
  } catch (err) {
    const cached = cachedByTag.get(NIGHTLY_TAG);
    if (
      cached &&
      existsSync(path.join(VENDOR_DIR, cached.binaries.loader)) &&
      existsSync(path.join(VENDOR_DIR, cached.binaries.testRunner))
    ) {
      console.warn(
        `[fetch-binaries] nightly fetch failed (${(err as Error).message}); reusing cached vendor/${NIGHTLY_TAG}`,
      );
      nightlyEntry = cached;
    } else {
      throw new Error(
        `nightly is required but fetch failed and no cached binary exists: ${(err as Error).message}`,
      );
    }
  }
  versions.push(nightlyEntry);

  const manifest: Manifest = {
    defaultVersion: NIGHTLY_TAG,
    versions,
  };
  await writeFile(
    MANIFEST_PATH,
    `${JSON.stringify(manifest, null, 2)}\n`,
    "utf8",
  );
  console.log(
    `[fetch-binaries] wrote ${MANIFEST_PATH} (${versions.length} version${versions.length === 1 ? "" : "s"})`,
  );
}

main().catch((err) => {
  console.error("[fetch-binaries] failed:", err.message ?? err);
  process.exit(1);
});
