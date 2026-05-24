#!/usr/bin/env bun
/**
 * Trace a raster logo into SVG using ImageMagick and vtracer.
 *
 * Pipeline:
 *   1. ImageMagick strips the near-black background to alpha.
 *   2. vtracer clusters colors and emits clean SVG paths.
 *
 * One-time setup:
 *   brew install imagemagick
 *   mkdir -p ~/.cache/goccia-logo-venv/bin
 *   curl -sL https://github.com/visioncortex/vtracer/releases/download/0.6.4/vtracer-aarch64-apple-darwin.tar.gz \
 *     | tar -xz -C ~/.cache/goccia-logo-venv/bin/
 *
 * Usage:
 *   scripts/build-logo.ts <source.png> [output.svg]
 */

import { $ } from "bun";
import { existsSync, mkdtempSync, rmSync, statSync } from "node:fs";
import { homedir, tmpdir } from "node:os";
import { basename, join } from "node:path";

const [, scriptName, sourceArg, outputArg] = Bun.argv;
const usage = `usage: ${basename(scriptName ?? "build-logo.ts")} <source.png> [output.svg]`;

function fail(message: string): never {
  console.error(message);
  process.exit(1);
}

function isExecutable(path: string): boolean {
  try {
    return (statSync(path).mode & 0o111) !== 0;
  } catch {
    return false;
  }
}

const source = sourceArg ?? "";
const output = outputArg ?? "logo.svg";
const magick = Bun.which("magick");
const vtracer =
  process.env.VTRACER ??
  Bun.which("vtracer") ??
  join(homedir(), ".cache/goccia-logo-venv/bin/vtracer");

if (source.length === 0) fail(usage);
if (!existsSync(source) || !statSync(source).isFile()) {
  fail(`source not found: ${source}`);
}
if (!magick) {
  fail("ImageMagick is missing; install it with `brew install imagemagick`.");
}
if (!isExecutable(vtracer)) {
  fail(`vtracer binary missing at ${vtracer} (see header comments).`);
}

const scratch = mkdtempSync(join(tmpdir(), "goccia-logo-"));

try {
  const cutout = join(scratch, "cut.png");

  await $`${magick} ${source} -resize 512x512 -gaussian-blur 0x0.8 -fuzz 14% -transparent black -channel A -morphology Erode Octagon:1 +channel ${cutout}`;

  await $`${vtracer} --input ${cutout} --output ${output} --colormode color --hierarchical stacked --mode spline --filter_speckle 12 --color_precision 3 --gradient_step 24 --corner_threshold 60 --segment_length 4 --splice_threshold 45 --path_precision 2`;

  console.log(`wrote ${output} (${statSync(output).size} bytes)`);
} finally {
  rmSync(scratch, { recursive: true, force: true });
}
