import { list } from "@vercel/blob";
import type {
  Test262Report,
  Test262TimelinePoint,
} from "@/lib/test262-dashboard";
import {
  type BlobAccess,
  type BlobReport,
  blobAccess,
  byCreatedAtThenRunNumber,
  cleanBlobPrefix,
  type ProfileReportKind,
  publishProfileArtifacts,
  putCompressedReportBlob,
  putDailyBlobPointer,
  readBlobText,
  readCompressedBlobText,
} from "./report-blob";

export type Test262BlobAccess = BlobAccess;
export type Test262ProfileBlobReportKind = ProfileReportKind;

export type Test262BlobRun = Test262TimelinePoint & {
  reportPath: string;
  reportUrl: string;
  reportDownloadUrl: string;
  reportCompressedSize: number;
  publishedAt: string;
};

export type Test262BlobPublishEntry = {
  point: Test262TimelinePoint;
  report: Test262Report;
  reportJson: string;
};

export type Test262ProfileBlobReport = BlobReport;

export type Test262ProfileBlobRun = {
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
  profileReports: {
    aggregate: Test262ProfileBlobReport;
    markdown?: Test262ProfileBlobReport;
    detailsArchive?: Test262ProfileBlobReport;
  };
  publishedAt: string;
};

export type Test262ProfileBlobPublishEntry = Omit<
  Test262ProfileBlobRun,
  "profileReports" | "publishedAt"
> & {
  aggregateJson: string;
  markdown?: Buffer;
  detailsArchive?: Buffer;
};

const DEFAULT_PREFIX = "test262";
const DEFAULT_PROFILE_PREFIX = "test262-profiles";

export function test262BlobPrefix(): string {
  return cleanBlobPrefix(process.env.TEST262_BLOB_PREFIX, DEFAULT_PREFIX);
}

export function test262ProfileBlobPrefix(): string {
  return cleanBlobPrefix(
    process.env.TEST262_PROFILE_BLOB_PREFIX,
    DEFAULT_PROFILE_PREFIX,
  );
}

export function test262BlobAccess(): Test262BlobAccess {
  return blobAccess(process.env.TEST262_BLOB_ACCESS);
}

export function test262BlobRunsPrefix(prefix = test262BlobPrefix()): string {
  return `${prefix}/runs`;
}

export function test262BlobDailyPrefix(prefix = test262BlobPrefix()): string {
  return `${prefix}/daily`;
}

export function test262ProfileBlobRunsPrefix(
  prefix = test262ProfileBlobPrefix(),
): string {
  return `${prefix}/runs`;
}

export function test262ProfileBlobDailyPrefix(
  prefix = test262ProfileBlobPrefix(),
): string {
  return `${prefix}/daily`;
}

export function test262BlobReportPathForArtifactId(
  artifactId: number,
  prefix = test262BlobPrefix(),
): string {
  return `${test262BlobRunsPrefix(prefix)}/${artifactId}.json.gz`;
}

export function test262BlobDailyPathForDay(
  day: string,
  prefix = test262BlobPrefix(),
): string {
  return `${test262BlobDailyPrefix(prefix)}/${day}.json`;
}

export function test262ProfileBlobReportPathForArtifactId(
  artifactId: number,
  kind: Test262ProfileBlobReportKind,
  prefix = test262ProfileBlobPrefix(),
): string {
  const base = `${test262ProfileBlobRunsPrefix(prefix)}/${artifactId}`;
  if (kind === "aggregate") return `${base}/aggregate.json.gz`;
  if (kind === "markdown") return `${base}/summary.md`;
  return `${base}/details.tar.gz`;
}

export function test262ProfileBlobDailyPathForDay(
  day: string,
  prefix = test262ProfileBlobPrefix(),
): string {
  return `${test262ProfileBlobDailyPrefix(prefix)}/${day}.json`;
}

function dailyPathForPoint(
  point: Test262TimelinePoint,
  prefix = test262BlobPrefix(),
): string {
  return test262BlobDailyPathForDay(point.createdAt.slice(0, 10), prefix);
}

function profileDailyPathForRun(
  run: Pick<Test262ProfileBlobRun, "createdAt">,
  prefix = test262ProfileBlobPrefix(),
): string {
  return test262ProfileBlobDailyPathForDay(run.createdAt.slice(0, 10), prefix);
}

function isBlobRun(value: unknown): value is Test262BlobRun {
  if (!value || typeof value !== "object") return false;
  const run = value as Record<string, unknown>;
  const isNonEmptyString = (entry: unknown): entry is string =>
    typeof entry === "string" && entry.length > 0;
  const isValidTimestamp = (entry: unknown): entry is string =>
    isNonEmptyString(entry) && Number.isFinite(Date.parse(entry));
  return (
    Number.isSafeInteger(run.runId) &&
    Number.isSafeInteger(run.runNumber) &&
    Number.isSafeInteger(run.artifactId) &&
    isValidTimestamp(run.createdAt) &&
    isNonEmptyString(run.reportPath) &&
    isNonEmptyString(run.reportUrl) &&
    isNonEmptyString(run.reportDownloadUrl) &&
    typeof run.reportCompressedSize === "number" &&
    Number.isSafeInteger(run.reportCompressedSize) &&
    run.reportCompressedSize >= 0 &&
    isValidTimestamp(run.publishedAt)
  );
}

export async function readTest262BlobReportJson(
  run: Pick<Test262BlobRun, "reportPath">,
): Promise<string | null> {
  return readCompressedBlobText(run.reportPath, test262BlobAccess());
}

export async function readTest262BlobReportJsonByArtifactId(
  artifactId: number,
): Promise<string | null> {
  if (!Number.isSafeInteger(artifactId) || artifactId <= 0) return null;
  return readCompressedBlobText(
    test262BlobReportPathForArtifactId(artifactId),
    test262BlobAccess(),
  );
}

export async function listTest262BlobDailyRuns(
  prefix = test262BlobPrefix(),
): Promise<Test262BlobRun[]> {
  const access = test262BlobAccess();
  const runs: Test262BlobRun[] = [];
  let cursor: string | undefined;
  do {
    const page = await list({
      cursor,
      limit: 1000,
      prefix: `${test262BlobDailyPrefix(prefix)}/`,
    });
    cursor = page.cursor;
    for (const blob of page.blobs) {
      const text = await readBlobText(blob.pathname, access);
      if (!text) continue;
      let parsed: unknown;
      try {
        parsed = JSON.parse(text);
      } catch {
        continue;
      }
      if (isBlobRun(parsed)) runs.push(parsed);
    }
    if (!page.hasMore) break;
  } while (cursor);

  return runs.sort(byCreatedAtThenRunNumber);
}

export async function publishTest262ReportsToBlob(
  entries: Test262BlobPublishEntry[],
): Promise<Test262BlobRun[]> {
  const prefix = test262BlobPrefix();
  const access = test262BlobAccess();
  const publishedRuns: Test262BlobRun[] = [];

  for (const entry of entries) {
    const reportPath = test262BlobReportPathForArtifactId(
      entry.point.artifactId,
      prefix,
    );
    const reportBlob = await putCompressedReportBlob(
      reportPath,
      entry.reportJson,
      access,
    );
    const published: Test262BlobRun = {
      ...entry.point,
      jsonUrl: `/api/test262/results/${entry.point.artifactId}`,
      reportPath,
      reportUrl: reportBlob.url,
      reportDownloadUrl: reportBlob.downloadUrl,
      reportCompressedSize: reportBlob.size,
      publishedAt: new Date().toISOString(),
    };
    await putDailyBlobPointer(
      dailyPathForPoint(published, prefix),
      published,
      access,
    );
    publishedRuns.push(published);
  }

  return publishedRuns.sort(byCreatedAtThenRunNumber);
}

export async function publishTest262ProfileReportsToBlob(
  entries: Test262ProfileBlobPublishEntry[],
): Promise<Test262ProfileBlobRun[]> {
  const prefix = test262ProfileBlobPrefix();
  const access = test262BlobAccess();
  const publishedRuns: Test262ProfileBlobRun[] = [];

  for (const entry of entries) {
    const profileReports = await publishProfileArtifacts(
      entry,
      (kind) =>
        test262ProfileBlobReportPathForArtifactId(
          entry.artifactId,
          kind,
          prefix,
        ),
      access,
    );

    const published: Test262ProfileBlobRun = {
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
      profileReports,
      publishedAt: new Date().toISOString(),
    };
    await putDailyBlobPointer(
      profileDailyPathForRun(published, prefix),
      published,
      access,
    );
    publishedRuns.push(published);
  }

  return publishedRuns.sort(byCreatedAtThenRunNumber);
}
