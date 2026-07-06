#!/usr/bin/env bun
import { readFile } from "node:fs/promises";
import {
  type AwfyBlobPublishEntry,
  awfyBlobDailyPathForDay,
  awfyBlobPrefix,
  awfyBlobReportPathForArtifactId,
  publishAwfyReportsToBlob,
} from "../src/lib/awfy-blob-store";
import { GITHUB_REPO_URL } from "../src/lib/github";

type AwfyReport = {
  metadata?: {
    options?: {
      repetitions?: unknown;
    };
  };
  targets?: Array<{
    kind?: unknown;
  }>;
};

function log(message: string) {
  console.log(`[publish-awfy] ${message}`);
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

function summaryFromReport(report: AwfyReport) {
  const targets = Array.isArray(report.targets) ? report.targets : [];
  return {
    targetCount: targets.length,
    awfyCount: targets.filter((target) => target.kind === "awfy").length,
    probeCount: targets.filter((target) => target.kind === "probe").length,
    repetitions:
      typeof report.metadata?.options?.repetitions === "number"
        ? report.metadata.options.repetitions
        : null,
  };
}

async function readEntryFromFile(
  filePath: string,
): Promise<AwfyBlobPublishEntry> {
  const reportJson = await readFile(filePath, "utf8");
  const parsed = JSON.parse(reportJson) as AwfyReport;
  if (!Array.isArray(parsed.targets)) {
    throw new Error(`${filePath} is not a valid AWFY report`);
  }

  const runId = numberFromEnv("GITHUB_RUN_ID");
  const runNumber = numberFromEnv("GITHUB_RUN_NUMBER");
  const artifactId =
    numberFromEnv("AWFY_ARTIFACT_ID") ??
    numberFromEnv("GITHUB_RUN_ID") ??
    Date.now();
  const repository = process.env.GITHUB_REPOSITORY ?? "frostney/GocciaScript";
  const server = process.env.GITHUB_SERVER_URL ?? "https://github.com";
  const headSha = process.env.GITHUB_SHA ?? "unknown";
  const now = new Date().toISOString();
  const createdAt = timestampFromEnv("AWFY_RUN_CREATED_AT") ?? now;

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
  bun scripts/publish-awfy-report.ts awfy-report.json

Environment:
  BLOB_READ_WRITE_TOKEN   Required Vercel Blob token.
  AWFY_BLOB_ACCESS        public (default) or private.
  AWFY_BLOB_PREFIX        Blob path prefix, default "awfy".
`);
  process.exit(2);
}

async function main() {
  const args = process.argv.slice(2);
  if (args.length === 0 || args.includes("--help") || args.includes("-h")) {
    usage();
  }
  if (!process.env.BLOB_READ_WRITE_TOKEN) {
    throw new Error("Set BLOB_READ_WRITE_TOKEN to publish AWFY Blob data");
  }

  const filePath = args.find((arg) => !arg.startsWith("--")) ?? usage();
  const entry = await readEntryFromFile(filePath);
  const prefix = awfyBlobPrefix();
  const day = entry.createdAt.slice(0, 10);

  log(
    `publishing ${filePath} to ${awfyBlobReportPathForArtifactId(entry.artifactId, prefix)}`,
  );
  log(`publishing daily pointer to ${awfyBlobDailyPathForDay(day, prefix)}`);
  const runs = await publishAwfyReportsToBlob([entry]);
  log(`published ${runs.length} AWFY report(s)`);
}

main().catch((err) => {
  const message = err instanceof Error ? err.message : String(err);
  console.error(`[publish-awfy] failed: ${message}`);
  process.exit(1);
});
