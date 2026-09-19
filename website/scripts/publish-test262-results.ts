#!/usr/bin/env bun
import { readFile } from "node:fs/promises";
import {
  publishTest262ReportsToBlob,
  type Test262BlobPublishEntry,
  test262BlobDailyPathForDay,
  test262BlobPrefix,
  test262BlobReportPathForArtifactId,
} from "../src/lib/test262-blob-store";
import {
  jsonUrlForArtifact,
  normalizeTest262Report,
  type Test262Report,
  type Test262TimelinePoint,
} from "../src/lib/test262-dashboard";
import { currentRunMetadata, timestampFromEnv } from "./lib/report-publishing";

function log(message: string) {
  console.log(`[publish-test262] ${message}`);
}

function pointFromCurrentRun(report: Test262Report): Test262TimelinePoint {
  const metadata = currentRunMetadata(
    "TEST262_ARTIFACT_ID",
    timestampFromEnv("TEST262_RUN_CREATED_AT"),
  );
  return {
    ...metadata,
    jsonUrl: jsonUrlForArtifact(metadata.artifactId),
    summary: report.summary,
  };
}

async function readEntryFromFile(
  filePath: string,
): Promise<Test262BlobPublishEntry> {
  const reportJson = await readFile(filePath, "utf8");
  const report = normalizeTest262Report(JSON.parse(reportJson));
  if (!report) throw new Error(`${filePath} is not a valid test262 report`);
  return {
    point: pointFromCurrentRun(report),
    report,
    reportJson: `${JSON.stringify(report, null, 2)}\n`,
  };
}

function usage(): never {
  console.error(`Usage:
  bun scripts/publish-test262-results.ts test262-results.json

Environment:
  BLOB_READ_WRITE_TOKEN   Required Vercel Blob token.
  TEST262_BLOB_ACCESS     public (default) or private.
  TEST262_BLOB_PREFIX     Blob path prefix, default "test262".
`);
  process.exit(2);
}

async function main() {
  const args = process.argv.slice(2);
  if (args.length === 0 || args.includes("--help")) usage();
  if (!process.env.BLOB_READ_WRITE_TOKEN) {
    throw new Error("Set BLOB_READ_WRITE_TOKEN to publish test262 Blob data");
  }

  const filePath = args.find((arg) => !arg.startsWith("--")) ?? usage();
  const entry = await readEntryFromFile(filePath);
  const prefix = test262BlobPrefix();

  log(
    `publishing ${filePath} to ${test262BlobReportPathForArtifactId(entry.point.artifactId, prefix)} and ${test262BlobDailyPathForDay(entry.point.createdAt.slice(0, 10), prefix)}`,
  );
  const runs = await publishTest262ReportsToBlob([entry]);
  log(`published ${runs.length} report(s)`);
}

main().catch((err) => {
  const message = err instanceof Error ? err.message : String(err);
  console.error(`[publish-test262] failed: ${message}`);
  process.exit(1);
});
