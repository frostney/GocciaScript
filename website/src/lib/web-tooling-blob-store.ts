import {
  type BlobAccess,
  type BlobReport,
  blobAccess,
  byCreatedAtThenRunNumber,
  cleanBlobPrefix,
  putCompressedReportBlob,
  putDailyBlobPointer,
} from "./report-blob";

export type WebToolingBlobAccess = BlobAccess;

export type WebToolingBlobReport = BlobReport;

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

export function webToolingBlobPrefix(): string {
  return cleanBlobPrefix(process.env.WEB_TOOLING_BLOB_PREFIX, DEFAULT_PREFIX);
}

export function webToolingBlobAccess(): WebToolingBlobAccess {
  return blobAccess(process.env.WEB_TOOLING_BLOB_ACCESS);
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
    const reportBlob = await putCompressedReportBlob(
      reportPath,
      entry.reportJson,
      access,
    );

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
      report: reportBlob,
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
