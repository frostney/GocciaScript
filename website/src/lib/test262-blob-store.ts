import { gunzipSync, gzipSync } from "node:zlib";
import { BlobNotFoundError, get, list, put } from "@vercel/blob";
import type {
  Test262Report,
  Test262TimelinePoint,
} from "@/lib/test262-dashboard";

export type Test262BlobAccess = "public" | "private";
export type Test262ProfileBlobReportKind =
  | "aggregate"
  | "markdown"
  | "detailsArchive";

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

export type Test262ProfileBlobReport = {
  path: string;
  url: string;
  downloadUrl: string;
  size: number;
};

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
const DEFAULT_ACCESS: Test262BlobAccess = "public";

function cleanPrefix(value: string | undefined, fallback: string): string {
  const trimmed = (value ?? fallback).trim().replace(/^\/+|\/+$/g, "");
  return trimmed || fallback;
}

export function test262BlobPrefix(): string {
  return cleanPrefix(process.env.TEST262_BLOB_PREFIX, DEFAULT_PREFIX);
}

export function test262ProfileBlobPrefix(): string {
  return cleanPrefix(
    process.env.TEST262_PROFILE_BLOB_PREFIX,
    DEFAULT_PROFILE_PREFIX,
  );
}

export function test262BlobAccess(): Test262BlobAccess {
  return process.env.TEST262_BLOB_ACCESS === "private"
    ? "private"
    : DEFAULT_ACCESS;
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

async function streamToBytes(stream: ReadableStream<Uint8Array>) {
  const reader = stream.getReader();
  const chunks: Uint8Array[] = [];
  let total = 0;
  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    chunks.push(value);
    total += value.byteLength;
  }
  const out = new Uint8Array(total);
  let offset = 0;
  for (const chunk of chunks) {
    out.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return out;
}

async function readBlobText(
  pathname: string,
  access = test262BlobAccess(),
): Promise<string | null> {
  try {
    const result = await get(pathname, { access });
    if (!result || result.statusCode !== 200 || !result.stream) return null;
    return new TextDecoder().decode(await streamToBytes(result.stream));
  } catch (err) {
    if (err instanceof BlobNotFoundError) return null;
    throw err;
  }
}

async function readBlobBytes(
  pathname: string,
  access = test262BlobAccess(),
): Promise<Uint8Array | null> {
  try {
    const result = await get(pathname, { access });
    if (!result || result.statusCode !== 200 || !result.stream) return null;
    return await streamToBytes(result.stream);
  } catch (err) {
    if (err instanceof BlobNotFoundError) return null;
    throw err;
  }
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

function byCreatedAtThenRunNumber(
  a: Pick<Test262TimelinePoint, "createdAt" | "runNumber">,
  b: Pick<Test262TimelinePoint, "createdAt" | "runNumber">,
): number {
  return (
    Date.parse(a.createdAt) - Date.parse(b.createdAt) ||
    a.runNumber - b.runNumber
  );
}

export async function readTest262BlobReportJson(
  run: Pick<Test262BlobRun, "reportPath">,
): Promise<string | null> {
  const bytes = await readBlobBytes(run.reportPath);
  return bytes ? gunzipSync(bytes).toString("utf8") : null;
}

export async function readTest262BlobReportJsonByArtifactId(
  artifactId: number,
): Promise<string | null> {
  if (!Number.isSafeInteger(artifactId) || artifactId <= 0) return null;
  const bytes = await readBlobBytes(
    test262BlobReportPathForArtifactId(artifactId),
  );
  return bytes ? gunzipSync(bytes).toString("utf8") : null;
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
    const compressed = gzipSync(`${entry.reportJson.trimEnd()}\n`);
    const reportBlob = await put(reportPath, compressed, {
      access,
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });
    const published: Test262BlobRun = {
      ...entry.point,
      jsonUrl: `/api/test262/results/${entry.point.artifactId}`,
      reportPath,
      reportUrl: reportBlob.url,
      reportDownloadUrl: reportBlob.downloadUrl,
      reportCompressedSize: compressed.byteLength,
      publishedAt: new Date().toISOString(),
    };
    await put(
      dailyPathForPoint(published, prefix),
      JSON.stringify(published, null, 2),
      {
        access,
        allowOverwrite: true,
        cacheControlMaxAge: 900,
        contentType: "application/json",
      },
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
    const aggregatePath = test262ProfileBlobReportPathForArtifactId(
      entry.artifactId,
      "aggregate",
      prefix,
    );
    const aggregateCompressed = gzipSync(`${entry.aggregateJson.trimEnd()}\n`);
    const aggregateBlob = await put(aggregatePath, aggregateCompressed, {
      access,
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });
    const profileReports: Test262ProfileBlobRun["profileReports"] = {
      aggregate: {
        path: aggregatePath,
        url: aggregateBlob.url,
        downloadUrl: aggregateBlob.downloadUrl,
        size: aggregateCompressed.byteLength,
      },
    };

    if (entry.markdown) {
      const markdownPath = test262ProfileBlobReportPathForArtifactId(
        entry.artifactId,
        "markdown",
        prefix,
      );
      const markdownBlob = await put(markdownPath, entry.markdown, {
        access,
        allowOverwrite: true,
        cacheControlMaxAge: 31_536_000,
        contentType: "text/markdown; charset=utf-8",
      });
      profileReports.markdown = {
        path: markdownPath,
        url: markdownBlob.url,
        downloadUrl: markdownBlob.downloadUrl,
        size: entry.markdown.byteLength,
      };
    }

    if (entry.detailsArchive) {
      const archivePath = test262ProfileBlobReportPathForArtifactId(
        entry.artifactId,
        "detailsArchive",
        prefix,
      );
      const archiveBlob = await put(archivePath, entry.detailsArchive, {
        access,
        allowOverwrite: true,
        cacheControlMaxAge: 31_536_000,
        contentType: "application/gzip",
      });
      profileReports.detailsArchive = {
        path: archivePath,
        url: archiveBlob.url,
        downloadUrl: archiveBlob.downloadUrl,
        size: entry.detailsArchive.byteLength,
      };
    }

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
    await put(
      profileDailyPathForRun(published, prefix),
      JSON.stringify(published, null, 2),
      {
        access,
        allowOverwrite: true,
        cacheControlMaxAge: 900,
        contentType: "application/json",
      },
    );
    publishedRuns.push(published);
  }

  return publishedRuns.sort(byCreatedAtThenRunNumber);
}
