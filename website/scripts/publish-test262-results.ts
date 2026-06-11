#!/usr/bin/env bun
import { readFile } from "node:fs/promises";
import { GITHUB_REPO_URL } from "../src/lib/github";
import {
  jsonUrlForArtifact,
  normalizeTest262Report,
  type Test262Report,
  type Test262TimelinePoint,
} from "../src/lib/test262-dashboard";
import {
  publishTest262ReportsToBlob,
  type Test262BlobPublishEntry,
  test262BlobManifestPath,
  test262BlobPrefix,
} from "./test262-blob-store";

function log(message: string) {
  console.log(`[publish-test262] ${message}`);
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

function pointFromCurrentRun(report: Test262Report): Test262TimelinePoint {
  const runId = numberFromEnv("GITHUB_RUN_ID");
  const runNumber = numberFromEnv("GITHUB_RUN_NUMBER");
  const artifactId =
    numberFromEnv("TEST262_ARTIFACT_ID") ??
    numberFromEnv("GITHUB_RUN_ID") ??
    Date.now();
  const repository = process.env.GITHUB_REPOSITORY ?? "frostney/GocciaScript";
  const server = process.env.GITHUB_SERVER_URL ?? "https://github.com";
  const headSha = process.env.GITHUB_SHA ?? "unknown";
  const now = new Date().toISOString();
  const createdAt = timestampFromEnv("TEST262_RUN_CREATED_AT") ?? now;
  return {
    runId: runId ?? artifactId,
    runNumber: runNumber ?? 0,
    title: process.env.GITHUB_WORKFLOW ?? "CI",
    headSha,
    shortSha: headSha.slice(0, 8),
    runUrl: runId
      ? `${server}/${repository}/actions/runs/${runId}`
      : GITHUB_REPO_URL,
    createdAt,
    updatedAt: now,
    artifactId,
    artifactCreatedAt: now,
    jsonUrl: jsonUrlForArtifact(artifactId),
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

  log(
    `publishing ${filePath} to ${test262BlobManifestPath(test262BlobPrefix())}`,
  );
  const manifest = await publishTest262ReportsToBlob([entry]);
  log(
    `published report; manifest now has ${manifest.runs.length} run(s) and ${manifest.daily.length} daily point(s)`,
  );
}

main().catch((err) => {
  const message = err instanceof Error ? err.message : String(err);
  console.error(`[publish-test262] failed: ${message}`);
  process.exit(1);
});
