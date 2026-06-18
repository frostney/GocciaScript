#!/usr/bin/env bun
import { readFile } from "node:fs/promises";
import {
  type BenchmarkProfileBlobPublishEntry,
  benchmarkProfileBlobDailyPathForDay,
  benchmarkProfileBlobPrefix,
  benchmarkProfileBlobReportPathForArtifactId,
  publishBenchmarkProfileReportsToBlob,
} from "../src/lib/benchmark-profile-blob-store";
import { GITHUB_REPO_URL } from "../src/lib/github";

function log(message: string) {
  console.log(`[publish-benchmark-profile] ${message}`);
}

function numberFromEnv(name: string): number | null {
  const value = process.env[name];
  if (!value) return null;
  const parsed = Number(value);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : null;
}

function timestampFromEnv(name: string): string | null {
  const value = process.env[name]?.trim();
  if (!value) return null;
  if (/^\d+$/.test(value)) {
    const seconds = Number(value);
    if (Number.isSafeInteger(seconds) && seconds > 0) {
      return new Date(seconds * 1000).toISOString();
    }
  }
  const time = Date.parse(value);
  return Number.isNaN(time) ? null : new Date(time).toISOString();
}

function parseArgs(args: string[]): {
  aggregate: string;
  markdown: string | null;
  detailsArchive: string | null;
} {
  let aggregate: string | null = null;
  let markdown: string | null = null;
  let detailsArchive: string | null = null;

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "--aggregate") aggregate = args[++i] ?? null;
    else if (arg.startsWith("--aggregate=")) {
      aggregate = arg.slice("--aggregate=".length);
    } else if (arg === "--markdown") markdown = args[++i] ?? null;
    else if (arg.startsWith("--markdown=")) {
      markdown = arg.slice("--markdown=".length);
    } else if (arg === "--details-archive") detailsArchive = args[++i] ?? null;
    else if (arg.startsWith("--details-archive=")) {
      detailsArchive = arg.slice("--details-archive=".length);
    } else if (arg === "--help" || arg === "-h") {
      usage();
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }

  if (!aggregate) usage();
  return { aggregate, markdown, detailsArchive };
}

async function readJsonFile(filePath: string): Promise<string> {
  const raw = await readFile(filePath, "utf8");
  const parsed = JSON.parse(raw) as unknown;
  return `${JSON.stringify(parsed, null, 2)}\n`;
}

async function readEntryFromFiles(
  aggregatePath: string,
  markdownPath: string | null,
  detailsArchivePath: string | null,
): Promise<BenchmarkProfileBlobPublishEntry> {
  const runId = numberFromEnv("GITHUB_RUN_ID");
  const runNumber = numberFromEnv("GITHUB_RUN_NUMBER");
  const artifactId =
    numberFromEnv("BENCHMARK_PROFILE_ARTIFACT_ID") ??
    numberFromEnv("GITHUB_RUN_ID") ??
    Date.now();
  const repository = process.env.GITHUB_REPOSITORY ?? "frostney/GocciaScript";
  const server = process.env.GITHUB_SERVER_URL ?? "https://github.com";
  const headSha = process.env.GITHUB_SHA ?? "unknown";
  const now = new Date().toISOString();
  const createdAt = timestampFromEnv("BENCHMARK_PROFILE_RUN_CREATED_AT") ?? now;

  return {
    runId: runId ?? artifactId,
    runNumber: runNumber ?? 0,
    artifactId,
    title: process.env.GITHUB_WORKFLOW ?? "CI",
    headSha,
    shortSha: headSha.slice(0, 8),
    runUrl: runId
      ? `${server}/${repository}/actions/runs/${runId}`
      : GITHUB_REPO_URL,
    createdAt,
    updatedAt: now,
    artifactCreatedAt: now,
    aggregateJson: await readJsonFile(aggregatePath),
    markdown: markdownPath ? await readFile(markdownPath) : undefined,
    detailsArchive: detailsArchivePath
      ? await readFile(detailsArchivePath)
      : undefined,
  };
}

function usage(): never {
  console.error(`Usage:
  bun scripts/publish-benchmark-profile-reports.ts --aggregate benchmark-profile-aggregate.json [--markdown benchmark-profile-aggregate.md] [--details-archive benchmark-profile-details.tar.gz]

Environment:
  BLOB_READ_WRITE_TOKEN              Required Vercel Blob token.
  BENCHMARK_PROFILE_BLOB_ACCESS      public (default) or private.
  BENCHMARK_PROFILE_BLOB_PREFIX      Blob path prefix, default "benchmark-profiles".
`);
  process.exit(2);
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  if (!process.env.BLOB_READ_WRITE_TOKEN) {
    throw new Error(
      "Set BLOB_READ_WRITE_TOKEN to publish benchmark profile Blob data",
    );
  }

  const entry = await readEntryFromFiles(
    args.aggregate,
    args.markdown,
    args.detailsArchive,
  );
  const prefix = benchmarkProfileBlobPrefix();
  const day = entry.createdAt.slice(0, 10);

  log(
    `publishing ${args.aggregate} to ${benchmarkProfileBlobReportPathForArtifactId(entry.artifactId, "aggregate", prefix)}`,
  );
  if (args.markdown) {
    log(
      `publishing ${args.markdown} to ${benchmarkProfileBlobReportPathForArtifactId(entry.artifactId, "markdown", prefix)}`,
    );
  }
  if (args.detailsArchive) {
    log(
      `publishing ${args.detailsArchive} to ${benchmarkProfileBlobReportPathForArtifactId(entry.artifactId, "detailsArchive", prefix)}`,
    );
  }
  log(
    `publishing daily pointer to ${benchmarkProfileBlobDailyPathForDay(day, prefix)}`,
  );
  const runs = await publishBenchmarkProfileReportsToBlob([entry]);
  log(`published ${runs.length} benchmark profile report set(s)`);
}

main().catch((err) => {
  const message = err instanceof Error ? err.message : String(err);
  console.error(`[publish-benchmark-profile] failed: ${message}`);
  process.exit(1);
});
