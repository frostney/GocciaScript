import { gunzipSync, gzipSync } from "node:zlib";
import { BlobNotFoundError, get, put } from "@vercel/blob";

export type BlobAccess = "public" | "private";

export type BlobReport = {
  path: string;
  url: string;
  downloadUrl: string;
  size: number;
};

export type ProfileReportKind = "aggregate" | "markdown" | "detailsArchive";

export function cleanBlobPrefix(
  value: string | undefined,
  fallback: string,
): string {
  return (value ?? fallback).trim().replace(/^\/+|\/+$/g, "") || fallback;
}

export function blobAccess(value: string | undefined): BlobAccess {
  return value === "private" ? "private" : "public";
}

export function byCreatedAtThenRunNumber(
  a: { createdAt: string; runNumber: number },
  b: { createdAt: string; runNumber: number },
): number {
  return (
    Date.parse(a.createdAt) - Date.parse(b.createdAt) ||
    a.runNumber - b.runNumber
  );
}

async function streamToBytes(
  stream: ReadableStream<Uint8Array>,
): Promise<Uint8Array> {
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

export async function readBlobBytes(
  pathname: string,
  access: BlobAccess,
): Promise<Uint8Array | null> {
  try {
    const result = await get(pathname, { access });
    if (!result || result.statusCode !== 200 || !result.stream) return null;
    return await streamToBytes(result.stream);
  } catch (error) {
    if (error instanceof BlobNotFoundError) return null;
    throw error;
  }
}

export async function readBlobText(
  pathname: string,
  access: BlobAccess,
): Promise<string | null> {
  const bytes = await readBlobBytes(pathname, access);
  return bytes ? new TextDecoder().decode(bytes) : null;
}

export async function readCompressedBlobText(
  pathname: string,
  access: BlobAccess,
): Promise<string | null> {
  const bytes = await readBlobBytes(pathname, access);
  return bytes ? gunzipSync(bytes).toString("utf8") : null;
}

async function putReportBlob(
  path: string,
  bytes: Buffer,
  contentType: string,
  access: BlobAccess,
): Promise<BlobReport> {
  const blob = await put(path, bytes, {
    access,
    allowOverwrite: true,
    cacheControlMaxAge: 31_536_000,
    contentType,
  });
  return {
    path,
    url: blob.url,
    downloadUrl: blob.downloadUrl,
    size: bytes.byteLength,
  };
}

export function putCompressedReportBlob(
  path: string,
  json: string,
  access: BlobAccess,
): Promise<BlobReport> {
  return putReportBlob(
    path,
    gzipSync(`${json.trimEnd()}\n`),
    "application/gzip",
    access,
  );
}

export async function putDailyBlobPointer(
  path: string,
  value: unknown,
  access: BlobAccess,
): Promise<void> {
  await put(path, JSON.stringify(value, null, 2), {
    access,
    allowOverwrite: true,
    cacheControlMaxAge: 900,
    contentType: "application/json",
  });
}

export async function publishProfileArtifacts(
  entry: { aggregateJson: string; markdown?: Buffer; detailsArchive?: Buffer },
  pathForKind: (kind: ProfileReportKind) => string,
  access: BlobAccess,
): Promise<{
  aggregate: BlobReport;
  markdown?: BlobReport;
  detailsArchive?: BlobReport;
}> {
  const profileReports: {
    aggregate: BlobReport;
    markdown?: BlobReport;
    detailsArchive?: BlobReport;
  } = {
    aggregate: await putCompressedReportBlob(
      pathForKind("aggregate"),
      entry.aggregateJson,
      access,
    ),
  };
  if (entry.markdown) {
    profileReports.markdown = await putReportBlob(
      pathForKind("markdown"),
      entry.markdown,
      "text/markdown; charset=utf-8",
      access,
    );
  }
  if (entry.detailsArchive) {
    profileReports.detailsArchive = await putReportBlob(
      pathForKind("detailsArchive"),
      entry.detailsArchive,
      "application/gzip",
      access,
    );
  }
  return profileReports;
}
