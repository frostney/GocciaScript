#!/usr/bin/env bun

import { readFile } from "node:fs/promises";
import { GITHUB_REPO_URL } from "../src/lib/github";
import {
  type JetStreamBlobPublishEntry,
  jetStreamBlobDailyPathForDay,
  jetStreamBlobPrefix,
  jetStreamBlobReportPathForArtifactId,
  publishJetStreamReportsToBlob,
} from "../src/lib/jetstream-blob-store";

type EngineStats = {
  ok?: number;
  timeout?: number;
  crash?: number;
  oom?: number;
  verificationFailed?: number;
  missingResult?: number;
};

export type JetStreamReport = {
  metadata?: {
    driver?: { version?: unknown };
    corpus?: { jetStream?: { commit?: unknown } };
    engines?: Array<{ name?: unknown; version?: unknown }>;
    options?: { repetitions?: unknown };
  };
  targets?: Array<{ summary?: { engineStats?: Record<string, EngineStats> } }>;
  geomeanRatios?: Record<string, unknown>;
};

function numberFromEnv(name: string): number | null {
  const parsed = Number(process.env[name]);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : null;
}

function timestampFromEnv(name: string): string | null {
  const value = process.env[name]?.trim();
  if (!value) return null;
  const time = Date.parse(value);
  return Number.isNaN(time) ? null : new Date(time).toISOString();
}

function finiteRatio(value: unknown): number | null {
  return typeof value === "number" && Number.isFinite(value) && value > 0
    ? value
    : null;
}

export function summaryFromReport(report: JetStreamReport) {
  const targets = Array.isArray(report.targets) ? report.targets : [];
  const failedWorkloadCount = targets.filter((target) =>
    Object.values(target.summary?.engineStats ?? {}).some(
      (stats) =>
        (stats.timeout ?? 0) +
          (stats.crash ?? 0) +
          (stats.oom ?? 0) +
          (stats.verificationFailed ?? 0) +
          (stats.missingResult ?? 0) >
        0,
    ),
  ).length;
  const engineVersions = Object.fromEntries(
    (report.metadata?.engines ?? [])
      .filter(
        (engine): engine is { name: string; version: string } =>
          typeof engine.name === "string" && typeof engine.version === "string",
      )
      .map((engine) => [engine.name, engine.version]),
  );
  return {
    workloadCount: targets.length,
    failedWorkloadCount,
    repetitions:
      typeof report.metadata?.options?.repetitions === "number"
        ? report.metadata.options.repetitions
        : null,
    referenceRatios: {
      quickjs: finiteRatio(report.geomeanRatios?.goccia_over_qjs),
      node: finiteRatio(report.geomeanRatios?.goccia_over_node),
    },
    engineVersions,
    corpusCommit:
      typeof report.metadata?.corpus?.jetStream?.commit === "string"
        ? report.metadata.corpus.jetStream.commit
        : "unknown",
    driverVersion:
      typeof report.metadata?.driver?.version === "number"
        ? report.metadata.driver.version
        : null,
  };
}

async function readEntry(filePath: string): Promise<JetStreamBlobPublishEntry> {
  const reportJson = await readFile(filePath, "utf8");
  const report = JSON.parse(reportJson) as JetStreamReport;
  if (!Array.isArray(report.targets)) {
    throw new Error(`${filePath} is not a valid JetStream report`);
  }
  const runId = numberFromEnv("GITHUB_RUN_ID");
  const artifactId =
    numberFromEnv("JETSTREAM_ARTIFACT_ID") ?? runId ?? Date.now();
  const repository = process.env.GITHUB_REPOSITORY ?? "frostney/GocciaScript";
  const server = process.env.GITHUB_SERVER_URL ?? "https://github.com";
  const headSha = process.env.GITHUB_SHA ?? "unknown";
  const now = new Date().toISOString();
  const createdAt = timestampFromEnv("JETSTREAM_RUN_CREATED_AT") ?? now;
  return {
    runId: runId ?? artifactId,
    runNumber: numberFromEnv("GITHUB_RUN_NUMBER") ?? 0,
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
    summary: summaryFromReport(report),
    reportJson: `${JSON.stringify(report, null, 2)}\n`,
  };
}

async function main() {
  const filePath = process.argv.slice(2).find((arg) => !arg.startsWith("--"));
  if (!filePath)
    throw new Error(
      "Usage: bun scripts/publish-jetstream-report.ts jetstream-report.json",
    );
  if (!process.env.BLOB_READ_WRITE_TOKEN) {
    throw new Error("Set BLOB_READ_WRITE_TOKEN to publish JetStream data");
  }
  const entry = await readEntry(filePath);
  const prefix = jetStreamBlobPrefix();
  console.log(
    `[publish-jetstream] publishing ${jetStreamBlobReportPathForArtifactId(entry.artifactId, prefix)}`,
  );
  console.log(
    `[publish-jetstream] publishing ${jetStreamBlobDailyPathForDay(entry.createdAt.slice(0, 10), prefix)}`,
  );
  await publishJetStreamReportsToBlob([entry]);
}

if (import.meta.main) {
  main().catch((error) => {
    console.error(
      `[publish-jetstream] ${error instanceof Error ? error.message : String(error)}`,
    );
    process.exit(1);
  });
}
