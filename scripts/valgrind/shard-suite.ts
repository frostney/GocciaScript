#!/usr/bin/env bun
/**
 * Splits the JavaScript suite into shards of about equal native run time, so
 * the memory-safety job can run one single-worker Valgrind process per shard
 * side by side (Valgrind runs one thread of a process at a time).
 *
 * Usage: bun scripts/valgrind/shard-suite.ts <runner-output.json> <shards> <prefix>
 *
 * <runner-output.json> is GocciaTestRunner's `--output=<file>` envelope from a
 * native run of the suite: its `files` list is the runner's own discovery, so
 * the shards hold exactly the files a directory run would, and each file's
 * `timing.total_ns` is its cost. Files go longest first to the shard with the
 * least time so far. Writes `<prefix>-<n>.txt`, one path per line.
 */

import { writeFileSync, readFileSync } from "node:fs";

type RunnerFile = { fileName: string; timing?: { total_ns?: number } };

const [planPath, shardArg, prefix] = process.argv.slice(2);
const shardCount = Number(shardArg);
if (!planPath || !prefix || !Number.isInteger(shardCount) || shardCount < 1) {
  console.error("usage: shard-suite.ts <runner-output.json> <shards> <prefix>");
  process.exit(2);
}

const plan = JSON.parse(readFileSync(planPath, "utf8")) as { files?: RunnerFile[] };
const files = plan.files ?? [];
if (files.length === 0) {
  console.error(`${planPath} lists no files`);
  process.exit(1);
}

const byCost = [...files].sort(
  (a, b) => (b.timing?.total_ns ?? 0) - (a.timing?.total_ns ?? 0),
);
const shards = Array.from({ length: shardCount }, () => ({ cost: 0, files: [] as string[] }));
for (const file of byCost) {
  const lightest = shards.reduce((min, shard) => (shard.cost < min.cost ? shard : min));
  lightest.files.push(file.fileName);
  lightest.cost += file.timing?.total_ns ?? 0;
}

shards.forEach((shard, index) => {
  writeFileSync(`${prefix}-${index}.txt`, shard.files.map((name) => `${name}\n`).join(""));
  console.log(
    `shard ${index}: ${shard.files.length} files, ${(shard.cost / 1e9).toFixed(1)} s native`,
  );
});
