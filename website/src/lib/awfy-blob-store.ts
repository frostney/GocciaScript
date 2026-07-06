import { gzipSync } from "node:zlib";
import { put } from "@vercel/blob";

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
  repetitions: number | null;
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
