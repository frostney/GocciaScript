#!/usr/bin/env bun
import { readFile } from "node:fs/promises";
import { GITHUB_REPO_URL } from "../src/lib/github";
import {
  publishWebToolingReportsToBlob,
  type WebToolingBlobPublishEntry,
  webToolingBlobDailyPathForDay,
  webToolingBlobPrefix,
  webToolingBlobReportPathForArtifactId,
} from "../src/lib/web-tooling-blob-store";

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

  const runId = numberFromEnv("GITHUB_RUN_ID");
  const runNumber = numberFromEnv("GITHUB_RUN_NUMBER");
  const artifactId =
    numberFromEnv("WEB_TOOLING_ARTIFACT_ID") ??
    numberFromEnv("GITHUB_RUN_ID") ??
    Date.now();
  const repository = process.env.GITHUB_REPOSITORY ?? "frostney/GocciaScript";
  const server = process.env.GITHUB_SERVER_URL ?? "https://github.com";
  const headSha = process.env.GITHUB_SHA ?? "unknown";
  const now = new Date().toISOString();
  const createdAt = timestampFromEnv("WEB_TOOLING_RUN_CREATED_AT") ?? now;

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
