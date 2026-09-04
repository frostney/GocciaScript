import { list } from "@vercel/blob";
import {
  type BlobAccess,
  type BlobReport,
  blobAccess,
  byCreatedAtThenRunNumber,
  cleanBlobPrefix,
  putCompressedReportBlob,
  putDailyBlobPointer,
  readBlobBytes,
  readCompressedBlobText,
} from "./report-blob";

export type JetStreamBlobAccess = BlobAccess;

export type JetStreamReferenceRatios = {
  quickjs: number | null;
  node: number | null;
};

export type JetStreamBlobRunSummary = {
  workloadCount: number;
  failedWorkloadCount: number;
  repetitions: number | null;
  referenceRatios: JetStreamReferenceRatios;
  engineVersions: Record<string, string>;
  corpusCommit: string;
  driverVersion: number | null;
  targetNames: string[];
};

export type JetStreamBlobReport = BlobReport;

export type JetStreamBlobRun = {
  runId: number;
  runNumber: number;
  artifactId: number;
  title: string;
  headSha: string;
  shortSha: string;
  runUrl: string;
  createdAt: string;
  updatedAt: string;
  artifactCreatedAt: string;
  summary: JetStreamBlobRunSummary;
  report: JetStreamBlobReport;
  publishedAt: string;
};

export type JetStreamBlobPublishEntry = Omit<
  JetStreamBlobRun,
  "report" | "publishedAt"
> & { reportJson: string };

const DEFAULT_PREFIX = "jetstream";

export function jetStreamBlobPrefix(): string {
  return cleanBlobPrefix(process.env.JETSTREAM_BLOB_PREFIX, DEFAULT_PREFIX);
}

export function jetStreamBlobAccess(): JetStreamBlobAccess {
  return blobAccess(process.env.JETSTREAM_BLOB_ACCESS);
}

export function jetStreamBlobRunsPrefix(
  prefix = jetStreamBlobPrefix(),
): string {
  return `${prefix}/runs`;
}

export function jetStreamBlobDailyPrefix(
  prefix = jetStreamBlobPrefix(),
): string {
  return `${prefix}/daily`;
}

export function jetStreamBlobReportPathForArtifactId(
  artifactId: number,
  prefix = jetStreamBlobPrefix(),
): string {
  return `${jetStreamBlobRunsPrefix(prefix)}/${artifactId}/report.json.gz`;
}

export function jetStreamBlobDailyPathForRun(
  day: string,
  runId: number,
  prefix = jetStreamBlobPrefix(),
): string {
  return `${jetStreamBlobDailyPrefix(prefix)}/${day}/${runId}.json`;
}

function isJetStreamBlobRun(value: unknown): value is JetStreamBlobRun {
  if (!value || typeof value !== "object") return false;
  const run = value as Record<string, unknown>;
  return (
    Number.isSafeInteger(run.runId) &&
    Number.isSafeInteger(run.runNumber) &&
    Number.isSafeInteger(run.artifactId) &&
    typeof run.createdAt === "string" &&
    Number.isFinite(Date.parse(run.createdAt)) &&
    typeof run.summary === "object" &&
    run.summary !== null &&
    typeof run.report === "object" &&
    run.report !== null
  );
}

export async function readJetStreamBlobReportJson(
  run: Pick<JetStreamBlobRun, "report">,
): Promise<string | null> {
  return readCompressedBlobText(run.report.path, jetStreamBlobAccess());
}

export async function listJetStreamBlobDailyRuns(
  prefix = jetStreamBlobPrefix(),
): Promise<JetStreamBlobRun[]> {
  const runs: JetStreamBlobRun[] = [];
  let cursor: string | undefined;
  do {
    const page = await list({
      cursor,
      limit: 1000,
      prefix: `${jetStreamBlobDailyPrefix(prefix)}/`,
    });
    cursor = page.cursor;
    for (const blob of page.blobs) {
      const bytes = await readBlobBytes(blob.pathname, jetStreamBlobAccess());
      if (!bytes) continue;
      try {
        const parsed: unknown = JSON.parse(new TextDecoder().decode(bytes));
        if (isJetStreamBlobRun(parsed)) runs.push(parsed);
      } catch {
        // Ignore malformed historical pointers and retain the valid timeline.
      }
    }
    if (!page.hasMore) break;
  } while (cursor);
  return runs.sort(byCreatedAtThenRunNumber);
}

export async function publishJetStreamReportsToBlob(
  entries: JetStreamBlobPublishEntry[],
): Promise<JetStreamBlobRun[]> {
  const prefix = jetStreamBlobPrefix();
  const access = jetStreamBlobAccess();
  const publishedRuns: JetStreamBlobRun[] = [];
  for (const entry of entries) {
    const reportPath = jetStreamBlobReportPathForArtifactId(
      entry.artifactId,
      prefix,
    );
    const reportBlob = await putCompressedReportBlob(
      reportPath,
      entry.reportJson,
      access,
    );
    const published: JetStreamBlobRun = {
      ...entry,
      report: reportBlob,
      publishedAt: new Date().toISOString(),
    };
    await putDailyBlobPointer(
      jetStreamBlobDailyPathForRun(
        entry.createdAt.slice(0, 10),
        entry.runId,
        prefix,
      ),
      published,
      access,
    );
    publishedRuns.push(published);
  }
  return publishedRuns.sort(byCreatedAtThenRunNumber);
}
