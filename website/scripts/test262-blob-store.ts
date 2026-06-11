import { gunzipSync, gzipSync } from "node:zlib";
import {
  BlobNotFoundError,
  BlobPreconditionFailedError,
  get,
  put,
} from "@vercel/blob";
import {
  jsonUrlForArtifact,
  type Test262Report,
  type Test262TimelinePoint,
  test262DashboardSource,
} from "../src/lib/test262-dashboard";

export type Test262BlobAccess = "public" | "private";

export type Test262BlobRun = Test262TimelinePoint & {
  reportPath: string;
  reportUrl: string;
  reportDownloadUrl: string;
  reportCompressedSize: number;
  publishedAt: string;
};

export type Test262BlobManifest = {
  version: 1;
  generatedAt: string;
  source: ReturnType<typeof test262DashboardSource> & {
    storage: "vercel-blob";
    prefix: string;
    access: Test262BlobAccess;
  };
  latest: Test262BlobRun | null;
  daily: Test262BlobRun[];
  runs: Test262BlobRun[];
};

export type Test262BlobPublishEntry = {
  point: Test262TimelinePoint;
  report: Test262Report;
  reportJson: string;
};

const DEFAULT_PREFIX = "test262";
const DEFAULT_ACCESS: Test262BlobAccess = "public";
const MANIFEST_WRITE_ATTEMPTS = 3;

function cleanPrefix(value: string | undefined): string {
  const trimmed = (value ?? DEFAULT_PREFIX).trim().replace(/^\/+|\/+$/g, "");
  return trimmed || DEFAULT_PREFIX;
}

export function test262BlobPrefix(): string {
  return cleanPrefix(process.env.TEST262_BLOB_PREFIX);
}

export function test262BlobAccess(): Test262BlobAccess {
  return process.env.TEST262_BLOB_ACCESS === "private"
    ? "private"
    : DEFAULT_ACCESS;
}

export function test262BlobManifestPath(prefix = test262BlobPrefix()): string {
  return `${prefix}/manifest.json`;
}

function reportPathForPoint(
  point: Test262TimelinePoint,
  prefix = test262BlobPrefix(),
): string {
  return `${prefix}/runs/${point.artifactId}.json.gz`;
}

function dailyPathForPoint(
  point: Test262TimelinePoint,
  prefix = test262BlobPrefix(),
): string {
  return `${prefix}/daily/${point.createdAt.slice(0, 10)}.json`;
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

async function readBlobTextWithMeta(
  pathname: string,
  access = test262BlobAccess(),
): Promise<{ text: string; etag: string } | null> {
  try {
    const result = await get(pathname, { access });
    if (!result || result.statusCode !== 200 || !result.stream) return null;
    return {
      text: new TextDecoder().decode(await streamToBytes(result.stream)),
      etag: result.blob.etag,
    };
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
  return (
    typeof run.runId === "number" &&
    typeof run.runNumber === "number" &&
    typeof run.artifactId === "number" &&
    typeof run.createdAt === "string" &&
    typeof run.reportPath === "string" &&
    typeof run.reportUrl === "string" &&
    typeof run.reportDownloadUrl === "string" &&
    typeof run.reportCompressedSize === "number" &&
    typeof run.publishedAt === "string"
  );
}

function normalizeManifest(
  value: unknown,
  prefix = test262BlobPrefix(),
): Test262BlobManifest | null {
  if (!value || typeof value !== "object") return null;
  const manifest = value as Record<string, unknown>;
  const runs = Array.isArray(manifest.runs)
    ? manifest.runs.filter(isBlobRun)
    : [];
  return createManifest(runs, prefix);
}

export async function loadTest262BlobManifest(
  prefix = test262BlobPrefix(),
): Promise<Test262BlobManifest | null> {
  const snapshot = await loadTest262BlobManifestSnapshot(prefix);
  return snapshot?.manifest ?? null;
}

async function loadTest262BlobManifestSnapshot(
  prefix = test262BlobPrefix(),
  access = test262BlobAccess(),
): Promise<{ manifest: Test262BlobManifest; etag: string } | null> {
  const result = await readBlobTextWithMeta(
    test262BlobManifestPath(prefix),
    access,
  );
  if (!result) return null;
  const manifest = normalizeManifest(JSON.parse(result.text), prefix);
  return manifest ? { manifest, etag: result.etag } : null;
}

export async function readTest262BlobReportJson(
  run: Pick<Test262BlobRun, "reportPath">,
): Promise<string | null> {
  const bytes = await readBlobBytes(run.reportPath);
  return bytes ? gunzipSync(bytes).toString("utf8") : null;
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

function dailyRuns(runs: Test262BlobRun[]): Test262BlobRun[] {
  const byDay = new Map<string, Test262BlobRun>();
  for (const run of [...runs].sort(byCreatedAtThenRunNumber)) {
    byDay.set(run.createdAt.slice(0, 10), run);
  }
  return [...byDay.values()].sort(byCreatedAtThenRunNumber);
}

function createManifest(
  runs: Test262BlobRun[],
  prefix = test262BlobPrefix(),
): Test262BlobManifest {
  const sortedRuns = [...runs].sort(byCreatedAtThenRunNumber);
  const daily = dailyRuns(sortedRuns);
  return {
    version: 1,
    generatedAt: new Date().toISOString(),
    source: {
      ...test262DashboardSource(),
      storage: "vercel-blob",
      prefix,
      access: test262BlobAccess(),
    },
    latest: daily[daily.length - 1] ?? null,
    daily,
    runs: sortedRuns,
  };
}

export async function publishTest262ReportsToBlob(
  entries: Test262BlobPublishEntry[],
): Promise<Test262BlobManifest> {
  const prefix = test262BlobPrefix();
  const access = test262BlobAccess();
  const publishedRuns: Test262BlobRun[] = [];

  for (const entry of entries) {
    const reportPath = reportPathForPoint(entry.point, prefix);
    const compressed = gzipSync(`${entry.reportJson.trimEnd()}\n`);
    const reportBlob = await put(reportPath, compressed, {
      access,
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });
    const published: Test262BlobRun = {
      ...entry.point,
      jsonUrl: jsonUrlForArtifact(entry.point.artifactId),
      reportPath,
      reportUrl: reportBlob.url,
      reportDownloadUrl: reportBlob.downloadUrl,
      reportCompressedSize: compressed.byteLength,
      publishedAt: new Date().toISOString(),
    };
    publishedRuns.push(published);
  }

  return publishManifestWithRetry(publishedRuns, prefix, access);
}

function isManifestWriteConflict(err: unknown): boolean {
  return (
    err instanceof BlobPreconditionFailedError ||
    (err instanceof Error && /precondition|already exists/i.test(err.message))
  );
}

async function publishManifestWithRetry(
  publishedRuns: Test262BlobRun[],
  prefix: string,
  access: Test262BlobAccess,
): Promise<Test262BlobManifest> {
  const changedDays = new Set(
    publishedRuns.map((run) => run.createdAt.slice(0, 10)),
  );
  for (let attempt = 1; attempt <= MANIFEST_WRITE_ATTEMPTS; attempt++) {
    // The CDN may serve the manifest up to its cache TTL (~60s) stale; the
    // ifMatch precondition below turns a stale read into a loud write
    // conflict instead of a silently lost update.
    const existing = await loadTest262BlobManifestSnapshot(prefix, access);
    const merged = new Map<number, Test262BlobRun>();
    for (const run of existing?.manifest.runs ?? []) {
      merged.set(run.artifactId, run);
    }
    for (const run of publishedRuns) {
      merged.set(run.artifactId, run);
    }

    const manifest = createManifest([...merged.values()], prefix);
    try {
      await put(
        test262BlobManifestPath(prefix),
        JSON.stringify(manifest, null, 2),
        {
          access,
          allowOverwrite: !!existing,
          cacheControlMaxAge: 60,
          contentType: "application/json",
          ...(existing ? { ifMatch: existing.etag } : {}),
        },
      );
    } catch (err) {
      if (
        !isManifestWriteConflict(err) ||
        attempt === MANIFEST_WRITE_ATTEMPTS
      ) {
        throw err;
      }
      continue;
    }

    // Only materialize daily files for a manifest state that actually won
    // the conditional write.
    for (const run of manifest.daily) {
      if (!changedDays.has(run.createdAt.slice(0, 10))) continue;
      await put(dailyPathForPoint(run, prefix), JSON.stringify(run, null, 2), {
        access,
        allowOverwrite: true,
        cacheControlMaxAge: 60,
        contentType: "application/json",
      });
    }
    return manifest;
  }
  throw new Error("failed to publish test262 Blob manifest");
}
