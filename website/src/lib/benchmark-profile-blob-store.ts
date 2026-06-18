import { gzipSync } from "node:zlib";
import { put } from "@vercel/blob";

export type BenchmarkProfileBlobAccess = "public" | "private";
export type BenchmarkProfileBlobReportKind =
  | "aggregate"
  | "markdown"
  | "detailsArchive";

export type BenchmarkProfileBlobReport = {
  path: string;
  url: string;
  downloadUrl: string;
  size: number;
};

export type BenchmarkProfileBlobRun = {
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
    aggregate: BenchmarkProfileBlobReport;
    markdown?: BenchmarkProfileBlobReport;
    detailsArchive?: BenchmarkProfileBlobReport;
  };
  publishedAt: string;
};

export type BenchmarkProfileBlobPublishEntry = Omit<
  BenchmarkProfileBlobRun,
  "profileReports" | "publishedAt"
> & {
  aggregateJson: string;
  markdown?: Buffer;
  detailsArchive?: Buffer;
};

const DEFAULT_PREFIX = "benchmark-profiles";
const DEFAULT_ACCESS: BenchmarkProfileBlobAccess = "public";

function cleanPrefix(value: string | undefined, fallback: string): string {
  const trimmed = (value ?? fallback).trim().replace(/^\/+|\/+$/g, "");
  return trimmed || fallback;
}

export function benchmarkProfileBlobPrefix(): string {
  return cleanPrefix(process.env.BENCHMARK_PROFILE_BLOB_PREFIX, DEFAULT_PREFIX);
}

export function benchmarkProfileBlobAccess(): BenchmarkProfileBlobAccess {
  return process.env.BENCHMARK_PROFILE_BLOB_ACCESS === "private"
    ? "private"
    : DEFAULT_ACCESS;
}

export function benchmarkProfileBlobRunsPrefix(
  prefix = benchmarkProfileBlobPrefix(),
): string {
  return `${prefix}/runs`;
}

export function benchmarkProfileBlobDailyPrefix(
  prefix = benchmarkProfileBlobPrefix(),
): string {
  return `${prefix}/daily`;
}

export function benchmarkProfileBlobReportPathForArtifactId(
  artifactId: number,
  kind: BenchmarkProfileBlobReportKind,
  prefix = benchmarkProfileBlobPrefix(),
): string {
  const base = `${benchmarkProfileBlobRunsPrefix(prefix)}/${artifactId}`;
  if (kind === "aggregate") return `${base}/aggregate.json.gz`;
  if (kind === "markdown") return `${base}/summary.md`;
  return `${base}/details.tar.gz`;
}

export function benchmarkProfileBlobDailyPathForDay(
  day: string,
  prefix = benchmarkProfileBlobPrefix(),
): string {
  return `${benchmarkProfileBlobDailyPrefix(prefix)}/${day}.json`;
}

function dailyPathForRun(
  run: Pick<BenchmarkProfileBlobRun, "createdAt">,
  prefix = benchmarkProfileBlobPrefix(),
): string {
  return benchmarkProfileBlobDailyPathForDay(
    run.createdAt.slice(0, 10),
    prefix,
  );
}

function byCreatedAtThenRunNumber(
  a: Pick<BenchmarkProfileBlobRun, "createdAt" | "runNumber">,
  b: Pick<BenchmarkProfileBlobRun, "createdAt" | "runNumber">,
): number {
  return (
    Date.parse(a.createdAt) - Date.parse(b.createdAt) ||
    a.runNumber - b.runNumber
  );
}

export async function publishBenchmarkProfileReportsToBlob(
  entries: BenchmarkProfileBlobPublishEntry[],
): Promise<BenchmarkProfileBlobRun[]> {
  const prefix = benchmarkProfileBlobPrefix();
  const access = benchmarkProfileBlobAccess();
  const publishedRuns: BenchmarkProfileBlobRun[] = [];

  for (const entry of entries) {
    const aggregatePath = benchmarkProfileBlobReportPathForArtifactId(
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
    const profileReports: BenchmarkProfileBlobRun["profileReports"] = {
      aggregate: {
        path: aggregatePath,
        url: aggregateBlob.url,
        downloadUrl: aggregateBlob.downloadUrl,
        size: aggregateCompressed.byteLength,
      },
    };

    if (entry.markdown) {
      const markdownPath = benchmarkProfileBlobReportPathForArtifactId(
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
      const archivePath = benchmarkProfileBlobReportPathForArtifactId(
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

    const published: BenchmarkProfileBlobRun = {
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
