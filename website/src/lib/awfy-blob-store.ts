import { list } from "@vercel/blob";
import {
  type BlobAccess,
  type BlobReport,
  blobAccess,
  byCreatedAtThenRunNumber,
  cleanBlobPrefix,
  putCompressedReportBlob,
  putDailyBlobPointer,
  readBlobText,
  readCompressedBlobText,
} from "./report-blob";

export type AwfyBlobAccess = BlobAccess;

export type AwfyBlobReport = BlobReport;

export type AwfyBlobRunSummary = {
  targetCount: number;
  awfyCount: number;
  probeCount: number;
  workloadCount: number;
  failedWorkloadCount: number;
  repetitions: number | null;
  referenceRatios: {
    quickjs: number | null;
    node: number | null;
  };
  engineVersions: Record<string, string>;
  corpusCommit: string;
  driverVersion: number | null;
  targetNames: string[];
};

export type AwfyBlobRun = {
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
  summary: AwfyBlobRunSummary;
  report: AwfyBlobReport;
  publishedAt: string;
};

export type AwfyBlobPublishEntry = Omit<
  AwfyBlobRun,
  "report" | "publishedAt"
> & {
  reportJson: string;
};

const DEFAULT_PREFIX = "awfy";

export function awfyBlobPrefix(): string {
  return cleanBlobPrefix(process.env.AWFY_BLOB_PREFIX, DEFAULT_PREFIX);
}

export function awfyBlobAccess(): AwfyBlobAccess {
  return blobAccess(process.env.AWFY_BLOB_ACCESS);
}

export function awfyBlobRunsPrefix(prefix = awfyBlobPrefix()): string {
  return `${prefix}/runs`;
}

export function awfyBlobDailyPrefix(prefix = awfyBlobPrefix()): string {
  return `${prefix}/daily`;
}

export function awfyBlobReportPathForArtifactId(
  artifactId: number,
  prefix = awfyBlobPrefix(),
): string {
  return `${awfyBlobRunsPrefix(prefix)}/${artifactId}/report.json.gz`;
}

export function awfyBlobDailyPathForDay(
  day: string,
  prefix = awfyBlobPrefix(),
): string {
  return `${awfyBlobDailyPrefix(prefix)}/${day}.json`;
}

function dailyPathForRun(
  run: Pick<AwfyBlobRun, "createdAt">,
  prefix = awfyBlobPrefix(),
): string {
  return awfyBlobDailyPathForDay(run.createdAt.slice(0, 10), prefix);
}

function isAwfyBlobRun(value: unknown): value is AwfyBlobRun {
  if (!value || typeof value !== "object") return false;
  const run = value as Record<string, unknown>;
  return (
    Number.isSafeInteger(run.runId) &&
    Number.isSafeInteger(run.runNumber) &&
    Number.isSafeInteger(run.artifactId) &&
    typeof run.createdAt === "string" &&
    Number.isFinite(Date.parse(run.createdAt)) &&
    typeof run.report === "object" &&
    run.report !== null
  );
}

export async function readAwfyBlobReportJson(
  run: Pick<AwfyBlobRun, "report">,
): Promise<string | null> {
  return readCompressedBlobText(run.report.path, awfyBlobAccess());
}

export async function listAwfyBlobDailyRuns(
  prefix = awfyBlobPrefix(),
): Promise<AwfyBlobRun[]> {
  const runs: AwfyBlobRun[] = [];
  let cursor: string | undefined;
  do {
    const page = await list({
      cursor,
      limit: 1000,
      prefix: `${awfyBlobDailyPrefix(prefix)}/`,
    });
    cursor = page.cursor;
    for (const blob of page.blobs) {
      const text = await readBlobText(blob.pathname, awfyBlobAccess());
      if (!text) continue;
      try {
        const parsed: unknown = JSON.parse(text);
        if (isAwfyBlobRun(parsed)) runs.push(parsed);
      } catch {
        // Ignore malformed historical pointers and retain the valid timeline.
      }
    }
    if (!page.hasMore) break;
  } while (cursor);
  return runs.sort(byCreatedAtThenRunNumber);
}

export async function publishAwfyReportsToBlob(
  entries: AwfyBlobPublishEntry[],
): Promise<AwfyBlobRun[]> {
  const prefix = awfyBlobPrefix();
  const access = awfyBlobAccess();
  const publishedRuns: AwfyBlobRun[] = [];

  for (const entry of entries) {
    const reportPath = awfyBlobReportPathForArtifactId(
      entry.artifactId,
      prefix,
    );
    const reportBlob = await putCompressedReportBlob(
      reportPath,
      entry.reportJson,
      access,
    );

    const published: AwfyBlobRun = {
      runId: entry.runId,
      runNumber: entry.runNumber,
      artifactId: entry.artifactId,
      title: entry.title,
      headSha: entry.headSha,
      shortSha: entry.shortSha,
      runUrl: entry.runUrl,
      createdAt: entry.createdAt,
      updatedAt: entry.updatedAt,
      artifactCreatedAt: entry.artifactCreatedAt,
      summary: entry.summary,
      report: reportBlob,
      publishedAt: new Date().toISOString(),
    };
    await putDailyBlobPointer(
      dailyPathForRun(published, prefix),
      published,
      access,
    );
    publishedRuns.push(published);
  }

  return publishedRuns.sort(byCreatedAtThenRunNumber);
}
