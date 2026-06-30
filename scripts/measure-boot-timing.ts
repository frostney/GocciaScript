#!/usr/bin/env bun
/**
 * measure-boot-timing.ts
 *
 * Measure empty-script engine-boot wall-clock for GocciaScriptLoader in both
 * execution modes and emit a small JSON summary the PR suite-timing comment
 * renders.
 *
 * Boot wall-clock on a shared runner is dominated by a fixed process /
 * dynamic-loader floor, so we report the min (least-contended run) alongside
 * the median over many runs rather than a single timing. The two modes are
 * measured back-to-back so transient load affects both alike.
 *
 * Run: bun run scripts/measure-boot-timing.ts <loader> <output.json>
 * Env: BOOT_TIMING_RUNS (default 60), BOOT_TIMING_WARMUP (default 10)
 */

import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const [, , loader, output] = Bun.argv;
if (!loader || !output) {
  console.error("usage: bun run scripts/measure-boot-timing.ts <loader> <output.json>");
  process.exit(2);
}

function intEnv(name: string, fallback: number, min: number): number {
  const raw = Bun.env[name];
  if (raw === undefined || raw === "") return fallback;
  const value = Number(raw);
  if (!Number.isInteger(value) || value < min) {
    throw new Error(`${name} must be an integer >= ${min}, got: ${raw}`);
  }
  return value;
}

const runs = intEnv("BOOT_TIMING_RUNS", 60, 1);
const warmup = intEnv("BOOT_TIMING_WARMUP", 10, 0);

interface ModeTiming {
  minMs: number;
  medianMs: number;
}

function measure(script: string, modeArgs: string[]): ModeTiming {
  const samples: number[] = [];
  const label = modeArgs.join(" ") || "interpreter";
  for (let i = 0; i < warmup + runs; i++) {
    const start = Bun.nanoseconds();
    const result = Bun.spawnSync([loader, script, ...modeArgs], {
      stdout: "ignore",
      stderr: "ignore",
    });
    const elapsedMs = (Bun.nanoseconds() - start) / 1e6;
    // A crashed/non-zero child run is not a valid boot sample — fail rather than
    // write boot-timing.json from bogus timings.
    if (result.exitCode !== 0) {
      throw new Error(`loader exited ${result.exitCode} measuring boot (${label})`);
    }
    if (i >= warmup) samples.push(elapsedMs);
  }
  samples.sort((a, b) => a - b);
  const n = samples.length;
  const median =
    n % 2 ? samples[(n - 1) / 2] : (samples[n / 2 - 1] + samples[n / 2]) / 2;
  return {
    minMs: Math.round(samples[0] * 100) / 100,
    medianMs: Math.round(median * 100) / 100,
  };
}

const dir = mkdtempSync(join(tmpdir(), "goccia-boot-"));
try {
  const script = join(dir, "empty.js");
  writeFileSync(script, "0;\n");

  const report = {
    runs,
    warmup,
    interpreted: measure(script, []),
    bytecode: measure(script, ["--mode=bytecode"]),
  };

  const json = JSON.stringify(report, null, 2);
  writeFileSync(output, json + "\n");
  console.log(json);
} finally {
  rmSync(dir, { recursive: true, force: true });
}
