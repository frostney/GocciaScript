#!/usr/bin/env bun
import {
  type BenchmarkProfileBlobPublishEntry,
  benchmarkProfileBlobDailyPathForDay,
  benchmarkProfileBlobPrefix,
  benchmarkProfileBlobReportPathForArtifactId,
  publishBenchmarkProfileReportsToBlob,
} from "../src/lib/benchmark-profile-blob-store";
import {
  currentRunMetadata,
  parseProfileArgs,
  readProfileFiles,
  timestampFromEnv,
} from "./lib/report-publishing";

function log(message: string) {
  console.log(`[publish-benchmark-profile] ${message}`);
}

async function readEntryFromFiles(
  aggregatePath: string,
  markdownPath: string | null,
  detailsArchivePath: string | null,
): Promise<BenchmarkProfileBlobPublishEntry> {
  return {
    ...currentRunMetadata(
      "BENCHMARK_PROFILE_ARTIFACT_ID",
      timestampFromEnv("BENCHMARK_PROFILE_RUN_CREATED_AT"),
    ),
    ...(await readProfileFiles(
      aggregatePath,
      markdownPath,
      detailsArchivePath,
    )),
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
  const args = parseProfileArgs(process.argv.slice(2), usage);
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
