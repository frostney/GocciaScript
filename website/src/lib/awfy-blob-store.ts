import { gunzipSync, gzipSync } from "node:zlib";
import { BlobNotFoundError, get, list, put } from "@vercel/blob";

export type AwfyBlobAccess = "public" | "private";

export type AwfyBlobReport = {
  path: string;
  url: string;
  downloadUrl: string;
  size: number;
};

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
const DEFAULT_ACCESS: AwfyBlobAccess = "public";

function cleanPrefix(value: string | undefined, fallback: string): string {
  const trimmed = (value ?? fallback).trim().replace(/^\/+|\/+$/g, "");
  return trimmed || fallback;
}

export function awfyBlobPrefix(): string {
  return cleanPrefix(process.env.AWFY_BLOB_PREFIX, DEFAULT_PREFIX);
}

export function awfyBlobAccess(): AwfyBlobAccess {
  return process.env.AWFY_BLOB_ACCESS === "private"
    ? "private"
    : DEFAULT_ACCESS;
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

function byCreatedAtThenRunNumber(
  a: Pick<AwfyBlobRun, "createdAt" | "runNumber">,
  b: Pick<AwfyBlobRun, "createdAt" | "runNumber">,
): number {
  return (
    Date.parse(a.createdAt) - Date.parse(b.createdAt) ||
    a.runNumber - b.runNumber
  );
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
  const bytes = new Uint8Array(total);
  let offset = 0;
  for (const chunk of chunks) {
    bytes.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return bytes;
}

async function readBlobBytes(pathname: string): Promise<Uint8Array | null> {
  try {
    const result = await get(pathname, { access: awfyBlobAccess() });
    if (!result || result.statusCode !== 200 || !result.stream) return null;
    return await streamToBytes(result.stream);
  } catch (error) {
    if (error instanceof BlobNotFoundError) return null;
    throw error;
  }
}

async function readBlobText(pathname: string): Promise<string | null> {
  const bytes = await readBlobBytes(pathname);
  return bytes ? new TextDecoder().decode(bytes) : null;
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
  const bytes = await readBlobBytes(run.report.path);
  return bytes ? gunzipSync(bytes).toString("utf8") : null;
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
      const text = await readBlobText(blob.pathname);
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
    const compressed = gzipSync(`${entry.reportJson.trimEnd()}\n`);
    const reportBlob = await put(reportPath, compressed, {
      access,
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });

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
      report: {
        path: reportPath,
        url: reportBlob.url,
        downloadUrl: reportBlob.downloadUrl,
        size: compressed.byteLength,
      },
      publishedAt: new Date().toISOString(),
    };
    await put(
      dailyPathForRun(published, prefix),
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
