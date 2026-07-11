import { gzipSync } from "node:zlib";
import { put } from "@vercel/blob";

export type WebToolingBlobAccess = "public" | "private";

export type WebToolingBlobReport = {
  path: string;
  url: string;
  downloadUrl: string;
  size: number;
};

export type WebToolingBlobRunSummary = {
  workloadCount: number;
  builtCount: number;
  completedCount: number;
  buildFailedCount: number;
  timeoutCount: number;
  crashCount: number;
  syntaxErrorCount: number;
  runtimeErrorCount: number;
  oomCount: number;
  missingResultCount: number;
  repetitions: number | null;
};

export type WebToolingBlobRun = {
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
  summary: WebToolingBlobRunSummary;
  report: WebToolingBlobReport;
  publishedAt: string;
};

export type WebToolingBlobPublishEntry = Omit<
  WebToolingBlobRun,
  "report" | "publishedAt"
> & {
  reportJson: string;
};

const DEFAULT_PREFIX = "web-tooling";
const DEFAULT_ACCESS: WebToolingBlobAccess = "public";

function cleanPrefix(value: string | undefined, fallback: string): string {
  const trimmed = (value ?? fallback).trim().replace(/^\/+|\/+$/g, "");
  return trimmed || fallback;
}

export function webToolingBlobPrefix(): string {
  return cleanPrefix(process.env.WEB_TOOLING_BLOB_PREFIX, DEFAULT_PREFIX);
}

export function webToolingBlobAccess(): WebToolingBlobAccess {
  return process.env.WEB_TOOLING_BLOB_ACCESS === "private"
    ? "private"
    : DEFAULT_ACCESS;
}

export function webToolingBlobRunsPrefix(
  prefix = webToolingBlobPrefix(),
): string {
  return `${prefix}/runs`;
}

export function webToolingBlobDailyPrefix(
  prefix = webToolingBlobPrefix(),
): string {
  return `${prefix}/daily`;
}

export function webToolingBlobReportPathForArtifactId(
  artifactId: number,
  prefix = webToolingBlobPrefix(),
): string {
  return `${webToolingBlobRunsPrefix(prefix)}/${artifactId}/report.json.gz`;
}

export function webToolingBlobDailyPathForDay(
  day: string,
  prefix = webToolingBlobPrefix(),
): string {
  return `${webToolingBlobDailyPrefix(prefix)}/${day}.json`;
}

function dailyPathForRun(
  run: Pick<WebToolingBlobRun, "createdAt">,
  prefix = webToolingBlobPrefix(),
): string {
  return webToolingBlobDailyPathForDay(run.createdAt.slice(0, 10), prefix);
}

function byCreatedAtThenRunNumber(
  a: Pick<WebToolingBlobRun, "createdAt" | "runNumber">,
  b: Pick<WebToolingBlobRun, "createdAt" | "runNumber">,
): number {
  return (
    Date.parse(a.createdAt) - Date.parse(b.createdAt) ||
    a.runNumber - b.runNumber
  );
}

export async function publishWebToolingReportsToBlob(
  entries: WebToolingBlobPublishEntry[],
): Promise<WebToolingBlobRun[]> {
  const prefix = webToolingBlobPrefix();
  const access = webToolingBlobAccess();
  const publishedRuns: WebToolingBlobRun[] = [];

  for (const entry of entries) {
    const reportPath = webToolingBlobReportPathForArtifactId(
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

    const published: WebToolingBlobRun = {
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
