#!/usr/bin/env bun
import { readFile } from "node:fs/promises";
import {
  publishWebToolingReportsToBlob,
  type WebToolingBlobPublishEntry,
  webToolingBlobDailyPathForDay,
  webToolingBlobPrefix,
  webToolingBlobReportPathForArtifactId,
} from "../src/lib/web-tooling-blob-store";
import { currentRunMetadata, timestampFromEnv } from "./lib/report-publishing";

type WebToolingReport = {
  summary?: Partial<WebToolingBlobPublishEntry["summary"]>;
  metadata?: {
    options?: {
      repetitions?: unknown;
    };
  };
  targets?: unknown[];
};

function log(message: string) {
  console.log(`[publish-web-tooling] ${message}`);
}

function safeCount(value: unknown): number {
  return Number.isSafeInteger(value) && Number(value) >= 0 ? Number(value) : 0;
}

export function summaryFromReport(
  report: WebToolingReport,
): WebToolingBlobPublishEntry["summary"] {
  const summary = report.summary ?? {};
  const targetCount = Array.isArray(report.targets) ? report.targets.length : 0;
  return {
    workloadCount: safeCount(summary.workloadCount) || targetCount,
    builtCount: safeCount(summary.builtCount),
    completedCount: safeCount(summary.completedCount),
    buildFailedCount: safeCount(summary.buildFailedCount),
    timeoutCount: safeCount(summary.timeoutCount),
    crashCount: safeCount(summary.crashCount),
    syntaxErrorCount: safeCount(summary.syntaxErrorCount),
    runtimeErrorCount: safeCount(summary.runtimeErrorCount),
    oomCount: safeCount(summary.oomCount),
    missingResultCount: safeCount(summary.missingResultCount),
    repetitions:
      typeof report.metadata?.options?.repetitions === "number"
        ? report.metadata.options.repetitions
        : null,
  };
}

async function readEntryFromFile(
  filePath: string,
): Promise<WebToolingBlobPublishEntry> {
  const reportJson = await readFile(filePath, "utf8");
  const parsed = JSON.parse(reportJson) as WebToolingReport;
  if (!Array.isArray(parsed.targets)) {
    throw new Error(`${filePath} is not a valid Web Tooling report`);
  }

  return {
    ...currentRunMetadata(
      "WEB_TOOLING_ARTIFACT_ID",
      timestampFromEnv("WEB_TOOLING_RUN_CREATED_AT"),
    ),
    summary: summaryFromReport(parsed),
    reportJson: `${JSON.stringify(parsed, null, 2)}\n`,
  };
}

function usage(): never {
  console.error(`Usage:
  bun scripts/publish-web-tooling-report.ts web-tooling-report.json

Environment:
  BLOB_READ_WRITE_TOKEN       Required Vercel Blob token.
  WEB_TOOLING_BLOB_ACCESS     public (default) or private.
  WEB_TOOLING_BLOB_PREFIX     Blob path prefix, default "web-tooling".
`);
  process.exit(2);
}

async function main() {
  const args = process.argv.slice(2);
  if (args.length === 0 || args.includes("--help") || args.includes("-h")) {
    usage();
  }
  if (!process.env.BLOB_READ_WRITE_TOKEN) {
    throw new Error(
      "Set BLOB_READ_WRITE_TOKEN to publish Web Tooling Blob data",
    );
  }

  const filePath = args.find((arg) => !arg.startsWith("--")) ?? usage();
  const entry = await readEntryFromFile(filePath);
  const prefix = webToolingBlobPrefix();
  const day = entry.createdAt.slice(0, 10);

  log(
    `publishing ${filePath} to ${webToolingBlobReportPathForArtifactId(entry.artifactId, prefix)}`,
  );
  log(
    `publishing daily pointer to ${webToolingBlobDailyPathForDay(day, prefix)}`,
  );
  const runs = await publishWebToolingReportsToBlob([entry]);
  log(`published ${runs.length} Web Tooling report(s)`);
}

if (import.meta.main) {
  main().catch((err) => {
    const message = err instanceof Error ? err.message : String(err);
    console.error(`[publish-web-tooling] failed: ${message}`);
    process.exit(1);
  });
}
