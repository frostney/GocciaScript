#!/usr/bin/env node
/**
 * Fetch the latest `nightly` GocciaScript release archive for the
 * current build host's platform and place the executables under
 * `website/vendor/`. The `/api/run` Route Handler picks them up at
 * request time so the playground / sandbox preview can execute real
 * GocciaScript on Vercel without a checked-in binary.
 *
 * Wired up from `package.json` as part of `prebuild` so it only runs
 * during `next build` — `next dev` (and its `predev` hook) doesn't
 * touch `vendor/`, leaving locally built `../build/GocciaScriptLoader`
 * untouched and preferred by the route's candidate-path order.
 *
 * Set `SKIP_VENDOR_FETCH=1` to bypass when the network is offline or
 * a hand-placed binary already lives under `vendor/`.
 *
 * Honoured env overrides:
 *   GOCCIA_REPO            — `owner/repo` (default: frostney/GocciaScript)
 *   GOCCIA_NIGHTLY_TAG     — release tag    (default: nightly)
 *   SKIP_VENDOR_FETCH=1    — early-out, no network fetch
 */

import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { copyFile, mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const REPO = process.env.GOCCIA_REPO ?? "frostney/GocciaScript";
const TAG = process.env.GOCCIA_NIGHTLY_TAG ?? "nightly";
const here = path.dirname(fileURLToPath(import.meta.url));
const VENDOR_DIR = path.resolve(here, "..", "vendor");
const EXES = ["GocciaScriptLoader", "GocciaTestRunner", "GocciaREPL"];

function platformAsset() {
  const p = process.platform;
  const a = process.arch;
  // Match exactly the asset matrix `.github/workflows/ci.yml` publishes on
  // each `nightly` release. Anything else is a real "no asset built for
  // this host" — better to fail loudly with the OS/arch in the error than
  // to coerce to `x64` and 404 from the GitHub release with a misleading
  // message.
  if (p === "darwin") {
    if (a === "arm64" || a === "x64") {
      return { os: "macos", arch: a, ext: "zip" };
    }
    throw new Error(`unsupported macOS arch: ${a}`);
  }
  if (p === "linux") {
    if (a === "arm64" || a === "x64") {
      return { os: "linux", arch: a, ext: "tar.gz" };
    }
    throw new Error(`unsupported Linux arch: ${a}`);
  }
  if (p === "win32") {
    if (a === "ia32") return { os: "windows", arch: "x86", ext: "zip" };
    if (a === "x64") return { os: "windows", arch: "x64", ext: "zip" };
    throw new Error(`unsupported Windows arch: ${a}`);
  }
  throw new Error(`unsupported build host: ${p}/${a}`);
}

async function extract(archivePath, ext, dest) {
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

async function main() {
  if (process.env.SKIP_VENDOR_FETCH === "1") {
    console.log("[fetch-nightly] SKIP_VENDOR_FETCH=1 — leaving vendor/ as-is");
    return;
  }

  const { os, arch, ext } = platformAsset();
  const asset = `gocciascript-${TAG}-${os}-${arch}.${ext}`;
  const url = `https://github.com/${REPO}/releases/download/${TAG}/${asset}`;
  console.log(`[fetch-nightly] ${os}/${arch} ← ${url}`);

  const work = await mkdtemp(path.join(tmpdir(), "goccia-nightly-"));
  try {
    const archive = path.join(work, asset);
    const res = await fetch(url, {
      headers: { "user-agent": "gocciascript-website-build" },
    });
    if (!res.ok) {
      throw new Error(`download failed: HTTP ${res.status} ${res.statusText}`);
    }
    await writeFile(archive, Buffer.from(await res.arrayBuffer()));

    await extract(archive, ext, work);

    // Release archives are produced by `.github/scripts/stage-build-artifacts.sh`
    // and contain a single top-level directory matching the archive base
    // name — binaries sit directly inside it, not under a `build/` subdir.
    const archiveRoot = path.join(work, `gocciascript-${TAG}-${os}-${arch}`);

    await mkdir(VENDOR_DIR, { recursive: true });
    const isWindows = os === "windows";
    const loaderName = isWindows
      ? "GocciaScriptLoader.exe"
      : "GocciaScriptLoader";
    let copied = 0;
    for (const exe of EXES) {
      const fname = isWindows ? `${exe}.exe` : exe;
      const src = path.join(archiveRoot, fname);
      if (!existsSync(src)) {
        console.warn(`[fetch-nightly] ${fname} missing in archive — skipped`);
        continue;
      }
      const dest = path.join(VENDOR_DIR, fname);
      await rm(dest, { force: true });
      // `rename()` fails with EXDEV when `tmpdir()` is on a different mount
      // than the repo (common on CI workers and some Linux setups), so use
      // `copyFile` for a reliable cross-device move. The cleanup `rm()` of
      // `work` in the outer `finally` handles the source.
      await copyFile(src, dest);
      copied++;
    }
    // The loader is the *only* binary `/api/run` actually invokes — the
    // others are nice-to-haves for local CLI use. If it's missing the
    // build looks fine but the playground 500s at runtime, so make this
    // a hard fail at vendor time instead.
    if (!existsSync(path.join(VENDOR_DIR, loaderName))) {
      throw new Error(
        `required binary missing after extraction: ${loaderName}`,
      );
    }
    console.log(`[fetch-nightly] vendored ${copied} binaries → ${VENDOR_DIR}`);
  } finally {
    await rm(work, { recursive: true, force: true });
  }
}

main().catch((err) => {
  console.error("[fetch-nightly] failed:", err.message ?? err);
  // Don't fail the build over a vendor-fetch problem when we already
  // have a binary cached locally — only error out on first install.
  // Check both shapes (`GocciaScriptLoader` on POSIX, `.exe` on Windows)
  // so the cached fallback works on any CI host that's previously been
  // primed.
  const cachedLoader =
    existsSync(path.join(VENDOR_DIR, "GocciaScriptLoader")) ||
    existsSync(path.join(VENDOR_DIR, "GocciaScriptLoader.exe"));
  if (cachedLoader) {
    console.error(
      "[fetch-nightly] keeping previously vendored binaries; build continues",
    );
    return;
  }
  process.exit(1);
});
