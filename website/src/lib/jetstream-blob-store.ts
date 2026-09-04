import {
  listBlobHistory,
  publishBlobHistorySnapshots,
  rebuildBlobHistorySnapshots,
} from "./blob-history";
import {
  type BlobAccess,
  type BlobReport,
  blobAccess,
  byCreatedAtThenRunNumber,
  cleanBlobPrefix,
  putCompressedReportBlob,
  putDailyBlobPointer,
  readCompressedBlobText,
} from "./report-blob";

export type JetStreamBlobAccess = BlobAccess;

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

export type JetStreamBlobReport = BlobReport;

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

export function jetStreamBlobPrefix(): string {
  return cleanBlobPrefix(process.env.JETSTREAM_BLOB_PREFIX, DEFAULT_PREFIX);
}

export function jetStreamBlobAccess(): JetStreamBlobAccess {
  return blobAccess(process.env.JETSTREAM_BLOB_ACCESS);
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
  return readCompressedBlobText(run.report.path, jetStreamBlobAccess());
}

function compactHistoryRun(run: JetStreamBlobRun): JetStreamBlobRun {
  // Legacy pointers embed the full report as an extra field. The report artifact
  // remains authoritative; the timeline needs only the declared run fields.
  const { reportJson: _reportJson, ...history } = run as JetStreamBlobRun & {
    reportJson?: unknown;
  };
  return history;
}

export async function rebuildJetStreamBlobHistory(): Promise<number> {
  return rebuildBlobHistorySnapshots(
    jetStreamBlobPrefix(),
    jetStreamBlobAccess(),
    isJetStreamBlobRun,
    compactHistoryRun,
  );
}

export async function listJetStreamBlobDailyRuns(
  prefix = jetStreamBlobPrefix(),
): Promise<JetStreamBlobRun[]> {
  return listBlobHistory(prefix, jetStreamBlobAccess(), isJetStreamBlobRun);
}

export async function publishJetStreamReportsToBlob(
  entries: JetStreamBlobPublishEntry[],
): Promise<JetStreamBlobRun[]> {
  const prefix = jetStreamBlobPrefix();
  const access = jetStreamBlobAccess();
  const publishedRuns: JetStreamBlobRun[] = [];
  const pointers: { pathname: string; etag: string; run: JetStreamBlobRun }[] =
    [];
  for (const entry of entries) {
    const reportPath = jetStreamBlobReportPathForArtifactId(
      entry.artifactId,
      prefix,
    );
    const reportBlob = await putCompressedReportBlob(
      reportPath,
      entry.reportJson,
      access,
    );
    const published: JetStreamBlobRun = {
      ...entry,
      report: reportBlob,
      publishedAt: new Date().toISOString(),
    };
    const pathname = jetStreamBlobDailyPathForRun(
      entry.createdAt.slice(0, 10),
      entry.runId,
      prefix,
    );
    const pointer = await putDailyBlobPointer(pathname, published, access);
    pointers.push({ pathname, etag: pointer.etag, run: published });
    publishedRuns.push(published);
  }
  await publishBlobHistorySnapshots(
    prefix,
    access,
    isJetStreamBlobRun,
    pointers,
    compactHistoryRun,
  );
  return publishedRuns.sort(byCreatedAtThenRunNumber);
}
