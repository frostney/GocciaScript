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
import { mkdir, mkdtemp, rename, rm, writeFile } from "node:fs/promises";
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
  if (p === "darwin") {
    return { os: "macos", arch: a === "arm64" ? "arm64" : "x64", ext: "zip" };
  }
  if (p === "linux") {
    return {
      os: "linux",
      arch: a === "arm64" ? "arm64" : "x64",
      ext: "tar.gz",
    };
  }
  if (p === "win32") {
    return { os: "windows", arch: a === "ia32" ? "x86" : "x64", ext: "zip" };
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

    await mkdir(VENDOR_DIR, { recursive: true });
    const isWindows = os === "windows";
    let copied = 0;
    for (const exe of EXES) {
      const fname = isWindows ? `${exe}.exe` : exe;
      const src = path.join(work, "build", fname);
      if (!existsSync(src)) {
        console.warn(`[fetch-nightly] ${fname} missing in archive — skipped`);
        continue;
      }
      const dest = path.join(VENDOR_DIR, fname);
      await rm(dest, { force: true });
      await rename(src, dest);
      copied++;
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
  if (existsSync(path.join(VENDOR_DIR, "GocciaScriptLoader"))) {
    console.error(
      "[fetch-nightly] keeping previously vendored binaries; build continues",
    );
    return;
  }
  process.exit(1);
});
