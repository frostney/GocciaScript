#!/usr/bin/env bun
/**
 * Build-time test262 dashboard sync.
 *
 * Set SKIP_TEST262_SYNC=1 for local/offline builds that should not contact
 * external services. Set BLOB_READ_WRITE_TOKEN or attach a Vercel Blob store
 * during deployment so the website build can materialize the durable Vercel
 * Blob manifest into content/test262/.
 */

import { access, mkdir, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  createTest262DashboardData,
  createTest262DashboardFallback,
  normalizeTest262Report,
  type Test262Report,
  type Test262TimelinePoint,
  test262DataPaths,
} from "../src/lib/test262-dashboard";
import {
  loadTest262BlobManifest,
  readTest262BlobReportJson,
} from "./test262-blob-store";

const here = path.dirname(fileURLToPath(import.meta.url));
const websiteRoot = path.resolve(here, "..");
const dataPaths = test262DataPaths();

function log(message: string) {
  console.log(`[sync-test262] ${message}`);
}

function isVercelBuild(): boolean {
  return process.env.VERCEL === "1";
}

function hasBlobReadCredentials(): boolean {
  return !!(
    process.env.BLOB_READ_WRITE_TOKEN ||
    (process.env.BLOB_STORE_ID &&
      (process.env.VERCEL_OIDC_TOKEN || isVercelBuild()))
  );
}

function shouldFailOnSyncError(): boolean {
  return (
    process.env.SKIP_TEST262_SYNC !== "1" &&
    (isVercelBuild() || hasBlobReadCredentials())
  );
}

function blobCredentialMode(): string {
  if (process.env.BLOB_READ_WRITE_TOKEN) return "BLOB_READ_WRITE_TOKEN";
  if (process.env.BLOB_STORE_ID && process.env.VERCEL_OIDC_TOKEN) {
    return "Vercel OIDC + BLOB_STORE_ID";
  }
  if (process.env.BLOB_STORE_ID) return "BLOB_STORE_ID";
  if (process.env.VERCEL_OIDC_TOKEN) return "Vercel OIDC";
  return "none";
}

async function resetOutputDir() {
  log(
    `reset output directory ${path.relative(websiteRoot, dataPaths.dataDir)}`,
  );
  await rm(dataPaths.dataDir, { recursive: true, force: true });
  await mkdir(dataPaths.reportsDir, { recursive: true });
}

async function writeJson(filePath: string, value: unknown) {
  await writeFile(filePath, `${JSON.stringify(value, null, 2)}\n`);
}

async function writeFallback(
  status: "needs-build-token" | "empty" | "error",
  message: string,
) {
  await resetOutputDir();
  await writeJson(
    dataPaths.dashboardPath,
    createTest262DashboardFallback(status, message),
  );
}

async function snapshotExists(): Promise<boolean> {
  try {
    await access(dataPaths.dashboardPath);
    return true;
  } catch {
    return false;
  }
}

async function preserveSnapshotOrWriteFallback(
  status: "needs-build-token" | "empty" | "error",
  message: string,
): Promise<void> {
  if (await snapshotExists()) {
    log(
      `existing ${path.relative(
        websiteRoot,
        dataPaths.dashboardPath,
      )} found - leaving snapshot unchanged`,
    );
    return;
  }
  await writeFallback(status, message);
}

async function syncReports() {
  log("starting build-time test262 dashboard sync");
  if (process.env.SKIP_TEST262_SYNC === "1") {
    await preserveSnapshotOrWriteFallback(
      "empty",
      "test262 dashboard sync was skipped for this website build.",
    );
    log("SKIP_TEST262_SYNC=1");
    return;
  }

  if (hasBlobReadCredentials()) {
    log(
      `${blobCredentialMode()} present; reading Vercel Blob test262 manifest`,
    );
    if (await syncReportsFromBlob()) return;
    const message =
      "The Vercel Blob test262 manifest was missing or did not contain any readable reports.";
    await preserveSnapshotOrWriteFallback("empty", message);
    log("Vercel Blob sync found no usable reports");
    throw new Error(message);
  }

  const message =
    "Attach a Vercel Blob store to the website project or set BLOB_READ_WRITE_TOKEN during the website build to read the test262 manifest.";
  await preserveSnapshotOrWriteFallback("needs-build-token", message);
  log("Vercel Blob credentials missing");
  if (isVercelBuild()) throw new Error(message);
}

async function syncReportsFromBlob(): Promise<boolean> {
  const manifest = await loadTest262BlobManifest();
  if (!manifest || manifest.daily.length === 0) {
    log("Vercel Blob manifest missing or empty");
    return false;
  }

  await resetOutputDir();
  const entries: { point: Test262TimelinePoint; report: Test262Report }[] = [];
  for (const [index, point] of manifest.daily.entries()) {
    log(
      `reading Blob report ${index + 1}/${manifest.daily.length}: ${
        point.reportPath
      }`,
    );
    const json = await readTest262BlobReportJson(point);
    if (!json) {
      log(`Blob report unavailable: ${point.reportPath}`);
      continue;
    }
    const report = normalizeTest262Report(JSON.parse(json));
    if (!report) {
      log(`Blob report invalid: ${point.reportPath}`);
      continue;
    }
    await writeFile(
      path.join(dataPaths.reportsDir, `${point.artifactId}.json`),
      `${json.trimEnd()}\n`,
    );
    entries.push({ point, report });
    log(
      `stored Blob report ${index + 1}/${manifest.daily.length}: ${
        report.summary.passed
      }/${report.summary.totalRun} passing`,
    );
  }

  entries.sort(
    (a, b) => Date.parse(a.point.createdAt) - Date.parse(b.point.createdAt),
  );
  const latest = entries[entries.length - 1];
  if (!latest) return false;

  await writeFile(dataPaths.latestPath, `${JSON.stringify(latest.report)}\n`);
  await writeJson(
    dataPaths.dashboardPath,
    createTest262DashboardData({
      generatedAt: new Date().toISOString(),
      timeline: entries.map((entry) => entry.point),
      latestReport: latest.report,
    }),
  );
  log(
    `wrote ${entries.length} reports from Vercel Blob manifest to ${path.relative(
      websiteRoot,
      dataPaths.dataDir,
    )}`,
  );
  return true;
}

syncReports().catch(async (err) => {
  const message = err instanceof Error ? err.message : String(err);
  console.error(`[sync-test262] failed: ${message}`);
  try {
    await preserveSnapshotOrWriteFallback(
      "error",
      "test262 dashboard sync failed during the website build.",
    );
  } catch (fallbackErr) {
    const fallbackMessage =
      fallbackErr instanceof Error ? fallbackErr.message : String(fallbackErr);
    console.error(
      `[sync-test262] failed to write fallback: ${fallbackMessage}`,
    );
  }
  if (shouldFailOnSyncError()) process.exit(1);
});
