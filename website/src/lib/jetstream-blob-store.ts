import { gunzipSync, gzipSync } from "node:zlib";
import { BlobNotFoundError, get, list, put } from "@vercel/blob";

export type JetStreamBlobAccess = "public" | "private";

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

export type JetStreamBlobReport = {
  path: string;
  url: string;
  downloadUrl: string;
  size: number;
};

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
const DEFAULT_ACCESS: JetStreamBlobAccess = "public";

function cleanPrefix(value: string | undefined, fallback: string): string {
  const trimmed = (value ?? fallback).trim().replace(/^\/+|\/+$/g, "");
  return trimmed || fallback;
}

export function jetStreamBlobPrefix(): string {
  return cleanPrefix(process.env.JETSTREAM_BLOB_PREFIX, DEFAULT_PREFIX);
}

export function jetStreamBlobAccess(): JetStreamBlobAccess {
  return process.env.JETSTREAM_BLOB_ACCESS === "private"
    ? "private"
    : DEFAULT_ACCESS;
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

function byCreatedAtThenRunNumber(
  left: Pick<JetStreamBlobRun, "createdAt" | "runNumber">,
  right: Pick<JetStreamBlobRun, "createdAt" | "runNumber">,
): number {
  return (
    Date.parse(left.createdAt) - Date.parse(right.createdAt) ||
    left.runNumber - right.runNumber
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
    const result = await get(pathname, { access: jetStreamBlobAccess() });
    if (!result || result.statusCode !== 200 || !result.stream) return null;
    return await streamToBytes(result.stream);
  } catch (error) {
    if (error instanceof BlobNotFoundError) return null;
    throw error;
  }
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
  const bytes = await readBlobBytes(run.report.path);
  return bytes ? gunzipSync(bytes).toString("utf8") : null;
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
      const bytes = await readBlobBytes(blob.pathname);
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
    const compressed = gzipSync(`${entry.reportJson.trimEnd()}\n`);
    const reportBlob = await put(reportPath, compressed, {
      access,
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });
    const published: JetStreamBlobRun = {
      ...entry,
      report: {
        path: reportPath,
        url: reportBlob.url,
        downloadUrl: reportBlob.downloadUrl,
        size: compressed.byteLength,
      },
      publishedAt: new Date().toISOString(),
    };
    await put(
      jetStreamBlobDailyPathForRun(
        entry.createdAt.slice(0, 10),
        entry.runId,
        prefix,
      ),
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
