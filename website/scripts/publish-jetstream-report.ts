#!/usr/bin/env bun

import { readFile } from "node:fs/promises";
import {
  type JetStreamBlobPublishEntry,
  jetStreamBlobDailyPathForRun,
  jetStreamBlobPrefix,
  jetStreamBlobReportPathForArtifactId,
  publishJetStreamReportsToBlob,
} from "../src/lib/jetstream-blob-store";
import { currentRunMetadata, timestampFromEnv } from "./lib/report-publishing";

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
    corpus?: {
      jetStream?: {
        commit?: unknown;
        benchmarks?: Record<string, unknown>;
      };
    };
    engines?: Array<{ name?: unknown; version?: unknown }>;
    options?: { repetitions?: unknown };
  };
  targets?: Array<{
    name?: unknown;
    summary?: {
      engineStats?: Record<string, EngineStats>;
      checksumAgreement?: { ok?: unknown };
    };
  }>;
  geomeanRatios?: Record<string, unknown>;
};

function finiteRatio(value: unknown): number | null {
  return typeof value === "number" && Number.isFinite(value) && value > 0
    ? value
    : null;
}

export function summaryFromReport(report: JetStreamReport) {
  const targets = Array.isArray(report.targets) ? report.targets : [];
  const reportedTargetNames = targets
    .map((target) => target.name)
    .filter((name): name is string => typeof name === "string");
  const configuredTargetNames = Object.keys(
    report.metadata?.corpus?.jetStream?.benchmarks ?? {},
  );
  const targetNames = (
    configuredTargetNames.length > 0
      ? configuredTargetNames
      : reportedTargetNames
  ).sort();
  const missingWorkloadCount = targetNames.filter(
    (name) => !reportedTargetNames.includes(name),
  ).length;
  const requiredEngines = ["goccia", "qjs", "node"];
  const failedWorkloadCount =
    missingWorkloadCount +
    targets.filter((target) => {
      const statsByEngine = target.summary?.engineStats ?? {};
      return (
        target.summary?.checksumAgreement?.ok === false ||
        requiredEngines.some((engine) => {
          const stats = statsByEngine[engine];
          return (
            !stats ||
            (stats.ok ?? 0) === 0 ||
            (stats.timeout ?? 0) +
              (stats.crash ?? 0) +
              (stats.oom ?? 0) +
              (stats.verificationFailed ?? 0) +
              (stats.missingResult ?? 0) >
              0
          );
        })
      );
    }).length;
  const engineVersions = Object.fromEntries(
    (report.metadata?.engines ?? [])
      .filter(
        (engine): engine is { name: string; version: string } =>
          typeof engine.name === "string" && typeof engine.version === "string",
      )
      .map((engine) => [engine.name, engine.version]),
  );
  return {
    workloadCount: targetNames.length,
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
    targetNames,
  };
}

async function readEntry(filePath: string): Promise<JetStreamBlobPublishEntry> {
  const reportJson = await readFile(filePath, "utf8");
  const report = JSON.parse(reportJson) as JetStreamReport;
  if (!Array.isArray(report.targets)) {
    throw new Error(`${filePath} is not a valid JetStream report`);
  }
  return {
    ...currentRunMetadata(
      "JETSTREAM_ARTIFACT_ID",
      timestampFromEnv("JETSTREAM_RUN_CREATED_AT", false),
    ),
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
    `[publish-jetstream] publishing ${jetStreamBlobDailyPathForRun(entry.createdAt.slice(0, 10), entry.runId, prefix)}`,
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
