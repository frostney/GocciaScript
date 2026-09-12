import {
  type BlobAccess,
  type BlobReport,
  blobAccess,
  byCreatedAtThenRunNumber,
  cleanBlobPrefix,
  type ProfileReportKind,
  publishProfileArtifacts,
  putDailyBlobPointer,
} from "./report-blob";

export type BenchmarkProfileBlobAccess = BlobAccess;
export type BenchmarkProfileBlobReportKind = ProfileReportKind;

export type BenchmarkProfileBlobReport = BlobReport;

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

export function benchmarkProfileBlobPrefix(): string {
  return cleanBlobPrefix(
    process.env.BENCHMARK_PROFILE_BLOB_PREFIX,
    DEFAULT_PREFIX,
  );
}

export function benchmarkProfileBlobAccess(): BenchmarkProfileBlobAccess {
  return blobAccess(process.env.BENCHMARK_PROFILE_BLOB_ACCESS);
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

export async function publishBenchmarkProfileReportsToBlob(
  entries: BenchmarkProfileBlobPublishEntry[],
): Promise<BenchmarkProfileBlobRun[]> {
  const prefix = benchmarkProfileBlobPrefix();
  const access = benchmarkProfileBlobAccess();
  const publishedRuns: BenchmarkProfileBlobRun[] = [];

  for (const entry of entries) {
    const profileReports = await publishProfileArtifacts(
      entry,
      (kind) =>
        benchmarkProfileBlobReportPathForArtifactId(
          entry.artifactId,
          kind,
          prefix,
        ),
      access,
    );

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
    await putDailyBlobPointer(
      dailyPathForRun(published, prefix),
      published,
      access,
    );
    publishedRuns.push(published);
  }

  return publishedRuns.sort(byCreatedAtThenRunNumber);
}
