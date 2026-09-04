#!/usr/bin/env bun
import {
  publishTest262ProfileReportsToBlob,
  type Test262ProfileBlobPublishEntry,
  test262ProfileBlobDailyPathForDay,
  test262ProfileBlobPrefix,
  test262ProfileBlobReportPathForArtifactId,
} from "../src/lib/test262-blob-store";
import {
  currentRunMetadata,
  parseProfileArgs,
  readProfileFiles,
  timestampFromEnv,
} from "./lib/report-publishing";

function log(message: string) {
  console.log(`[publish-test262-profile] ${message}`);
}

async function readEntryFromFiles(
  aggregatePath: string,
  markdownPath: string | null,
  detailsArchivePath: string | null,
): Promise<Test262ProfileBlobPublishEntry> {
  return {
    ...currentRunMetadata(
      "TEST262_PROFILE_ARTIFACT_ID",
      timestampFromEnv("TEST262_PROFILE_RUN_CREATED_AT") ??
        timestampFromEnv("TEST262_RUN_CREATED_AT"),
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
  bun scripts/publish-test262-profile-reports.ts --aggregate test262-profile-aggregate.json [--markdown test262-profile-aggregate.md] [--details-archive test262-profile-details.tar.gz]

Environment:
  BLOB_READ_WRITE_TOKEN          Required Vercel Blob token.
  TEST262_BLOB_ACCESS            public (default) or private.
  TEST262_PROFILE_BLOB_PREFIX    Blob path prefix, default "test262-profiles".
`);
  process.exit(2);
}

async function main() {
  const args = parseProfileArgs(process.argv.slice(2), usage);
  if (!process.env.BLOB_READ_WRITE_TOKEN) {
    throw new Error(
      "Set BLOB_READ_WRITE_TOKEN to publish test262 profile Blob data",
    );
  }

  const entry = await readEntryFromFiles(
    args.aggregate,
    args.markdown,
    args.detailsArchive,
  );
  const prefix = test262ProfileBlobPrefix();
  const day = entry.createdAt.slice(0, 10);

  log(
    `publishing ${args.aggregate} to ${test262ProfileBlobReportPathForArtifactId(entry.artifactId, "aggregate", prefix)}`,
  );
  if (args.markdown) {
    log(
      `publishing ${args.markdown} to ${test262ProfileBlobReportPathForArtifactId(entry.artifactId, "markdown", prefix)}`,
    );
  }
  if (args.detailsArchive) {
    log(
      `publishing ${args.detailsArchive} to ${test262ProfileBlobReportPathForArtifactId(entry.artifactId, "detailsArchive", prefix)}`,
    );
  }
  log(
    `publishing daily pointer to ${test262ProfileBlobDailyPathForDay(day, prefix)}`,
  );
  const runs = await publishTest262ProfileReportsToBlob([entry]);
  log(`published ${runs.length} profile report set(s)`);
}

main().catch((err) => {
  const message = err instanceof Error ? err.message : String(err);
  console.error(`[publish-test262-profile] failed: ${message}`);
  process.exit(1);
});
