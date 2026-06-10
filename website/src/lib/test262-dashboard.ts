import { readFile } from "node:fs/promises";
import path from "node:path";
import { inflateRawSync } from "node:zlib";
import { GITHUB_REPO_URL } from "@/lib/github";

const TEST262_WORKFLOW = "ci.yml";
const TEST262_ARTIFACT_NAME = "test262-results";
const TEST262_REPORT_NAME = "test262-results.json";
const GROUP_SEGMENTS = 2;
const MIN_GROUP_TESTS = 25;
const TEST262_DATA_DIR = path.join(process.cwd(), "content", "test262");
const TEST262_DASHBOARD_PATH = path.join(TEST262_DATA_DIR, "dashboard.json");
const TEST262_LATEST_PATH = path.join(TEST262_DATA_DIR, "latest.json");
const TEST262_REPORTS_DIR = path.join(TEST262_DATA_DIR, "reports");

export type Test262Outcome = "PASS" | "FAIL" | "WRAPPER_INFRA" | "TIMEOUT";

export type Test262Record = {
  id: string;
  status: Test262Outcome;
  durationMs?: number;
  message?: string;
  diagnostic?: string;
};

export type Test262CategorySummary = {
  category: string;
  run: number;
  passed: number;
  failed: number;
  wrapperInfra: number;
  timeouts: number;
};

export type Test262SuiteSummary = {
  totalDiscovered: number;
  totalRun: number;
  passed: number;
  failed: number;
  wrapperInfraFailures: number;
  timeouts: number;
  durationSeconds: number;
  byCategory: Test262CategorySummary[];
};

export type Test262Report = {
  summary: Test262SuiteSummary;
  results: Test262Record[];
};

export type Test262RunMeta = {
  runId: number;
  runNumber: number;
  title: string;
  headSha: string;
  shortSha: string;
  runUrl: string;
  createdAt: string;
  updatedAt: string;
  artifactId: number;
  artifactCreatedAt: string;
  jsonUrl: string;
  reportDownloadUrl?: string;
  reportPath?: string;
  reportUrl?: string;
};

export type Test262TimelinePoint = Test262RunMeta & {
  summary: Test262SuiteSummary;
};

export type Test262GroupCoverage = {
  key: string;
  attempted: number;
  passed: number;
  failed: number;
  wrapperInfra: number;
  timeouts: number;
  passRate: number;
};

export type Test262DashboardData = {
  status: "ready" | "needs-build-token" | "empty" | "error";
  message?: string;
  generatedAt: string;
  source: {
    repositoryUrl: string;
    workflowUrl: string;
    artifactName: string;
    reportName: string;
    minGroupTests: number;
  };
  latest: Test262TimelinePoint | null;
  timeline: Test262TimelinePoint[];
  leastCovered: Test262GroupCoverage[];
  mostCovered: Test262GroupCoverage[];
};

export function jsonUrlForArtifact(artifactId: number): string {
  return `/api/test262/results/${artifactId}`;
}

export function test262DashboardSource() {
  return {
    repositoryUrl: GITHUB_REPO_URL,
    workflowUrl: `${GITHUB_REPO_URL}/actions/workflows/${TEST262_WORKFLOW}`,
    artifactName: TEST262_ARTIFACT_NAME,
    reportName: TEST262_REPORT_NAME,
    minGroupTests: MIN_GROUP_TESTS,
  };
}

function pct(passed: number, run: number): number {
  return run > 0 ? passed / run : 0;
}

function isOutcome(value: unknown): value is Test262Outcome {
  return (
    value === "PASS" ||
    value === "FAIL" ||
    value === "WRAPPER_INFRA" ||
    value === "TIMEOUT"
  );
}

function normalizeCategory(value: unknown): Test262CategorySummary | null {
  if (!value || typeof value !== "object") return null;
  const v = value as Record<string, unknown>;
  if (typeof v.category !== "string") return null;
  return {
    category: v.category,
    run: Number(v.run) || 0,
    passed: Number(v.passed) || 0,
    failed: Number(v.failed) || 0,
    wrapperInfra: Number(v.wrapperInfra) || 0,
    timeouts: Number(v.timeouts) || 0,
  };
}

export function normalizeTest262Report(value: unknown): Test262Report | null {
  if (!value || typeof value !== "object") return null;
  const v = value as Record<string, unknown>;
  const summary = v.summary as Record<string, unknown> | undefined;
  const results = v.results;
  if (!summary || !Array.isArray(results)) return null;
  const byCategory = Array.isArray(summary.byCategory)
    ? summary.byCategory
        .map(normalizeCategory)
        .filter((c): c is Test262CategorySummary => c !== null)
    : [];

  const normalizedResults = results
    .map((entry): Test262Record | null => {
      if (!entry || typeof entry !== "object") return null;
      const rec = entry as Record<string, unknown>;
      if (typeof rec.id !== "string" || !isOutcome(rec.status)) return null;
      return {
        id: rec.id,
        status: rec.status,
        durationMs:
          typeof rec.durationMs === "number" ? rec.durationMs : undefined,
        message: typeof rec.message === "string" ? rec.message : undefined,
        diagnostic:
          typeof rec.diagnostic === "string" ? rec.diagnostic : undefined,
      };
    })
    .filter((r): r is Test262Record => r !== null);

  return {
    summary: {
      totalDiscovered: Number(summary.totalDiscovered) || 0,
      totalRun: Number(summary.totalRun) || normalizedResults.length,
      passed: Number(summary.passed) || 0,
      failed: Number(summary.failed) || 0,
      wrapperInfraFailures: Number(summary.wrapperInfraFailures) || 0,
      timeouts: Number(summary.timeouts) || 0,
      durationSeconds: Number(summary.durationSeconds) || 0,
      byCategory,
    },
    results: normalizedResults,
  };
}

export function groupCoverage(
  report: Test262Report,
  minTests = MIN_GROUP_TESTS,
): Test262GroupCoverage[] {
  const groups = new Map<string, Test262GroupCoverage>();
  for (const result of report.results) {
    const parts = result.id.split("/");
    if (parts.length < GROUP_SEGMENTS) continue;
    const key = parts.slice(0, GROUP_SEGMENTS).join("/");
    const group =
      groups.get(key) ??
      ({
        key,
        attempted: 0,
        passed: 0,
        failed: 0,
        wrapperInfra: 0,
        timeouts: 0,
        passRate: 0,
      } satisfies Test262GroupCoverage);
    group.attempted++;
    if (result.status === "PASS") group.passed++;
    else if (result.status === "FAIL") group.failed++;
    else if (result.status === "WRAPPER_INFRA") group.wrapperInfra++;
    else if (result.status === "TIMEOUT") group.timeouts++;
    groups.set(key, group);
  }

  return Array.from(groups.values())
    .filter((group) => group.attempted >= minTests)
    .map((group) => ({
      ...group,
      passRate: pct(group.passed, group.attempted),
    }));
}

export function rankGroupCoverage(report: Test262Report) {
  const groups = groupCoverage(report);
  const leastCovered = [...groups]
    .sort(
      (a, b) =>
        a.passRate - b.passRate ||
        b.attempted - a.attempted ||
        a.key.localeCompare(b.key),
    )
    .slice(0, 5);
  const mostCovered = [...groups]
    .sort(
      (a, b) =>
        b.passRate - a.passRate ||
        b.attempted - a.attempted ||
        a.key.localeCompare(b.key),
    )
    .slice(0, 5);
  return { leastCovered, mostCovered };
}

function readUInt32LE(buf: Buffer, offset: number): number {
  return buf.readUInt32LE(offset);
}

function readUInt16LE(buf: Buffer, offset: number): number {
  return buf.readUInt16LE(offset);
}

export function extractJsonFromZip(
  bytes: Uint8Array,
  fileName: string,
): string {
  const buf = Buffer.from(bytes);
  const eocdSignature = 0x06054b50;
  let eocd = -1;
  for (let i = buf.length - 22; i >= Math.max(0, buf.length - 65_557); i--) {
    if (readUInt32LE(buf, i) === eocdSignature) {
      eocd = i;
      break;
    }
  }
  if (eocd < 0) throw new Error("ZIP end-of-central-directory not found");

  const entryCount = readUInt16LE(buf, eocd + 10);
  let cursor = readUInt32LE(buf, eocd + 16);
  for (let i = 0; i < entryCount; i++) {
    if (readUInt32LE(buf, cursor) !== 0x02014b50) {
      throw new Error("ZIP central directory is malformed");
    }
    const method = readUInt16LE(buf, cursor + 10);
    const compressedSize = readUInt32LE(buf, cursor + 20);
    const nameLength = readUInt16LE(buf, cursor + 28);
    const extraLength = readUInt16LE(buf, cursor + 30);
    const commentLength = readUInt16LE(buf, cursor + 32);
    const localHeaderOffset = readUInt32LE(buf, cursor + 42);
    const name = buf
      .subarray(cursor + 46, cursor + 46 + nameLength)
      .toString("utf8");

    if (name === fileName || name.endsWith(`/${fileName}`)) {
      if (readUInt32LE(buf, localHeaderOffset) !== 0x04034b50) {
        throw new Error("ZIP local file header is malformed");
      }
      const localNameLength = readUInt16LE(buf, localHeaderOffset + 26);
      const localExtraLength = readUInt16LE(buf, localHeaderOffset + 28);
      const dataStart =
        localHeaderOffset + 30 + localNameLength + localExtraLength;
      const compressed = buf.subarray(dataStart, dataStart + compressedSize);
      if (method === 0) return compressed.toString("utf8");
      if (method === 8) return inflateRawSync(compressed).toString("utf8");
      throw new Error(`Unsupported ZIP compression method: ${method}`);
    }

    cursor += 46 + nameLength + extraLength + commentLength;
  }
  throw new Error(`${fileName} not found in ZIP artifact`);
}

function fallbackDashboard(
  status: Test262DashboardData["status"],
  message: string,
): Test262DashboardData {
  return {
    status,
    message,
    generatedAt: new Date(0).toISOString(),
    source: test262DashboardSource(),
    latest: null,
    timeline: [],
    leastCovered: [],
    mostCovered: [],
  };
}

async function readJsonFile<T>(filePath: string): Promise<T | null> {
  try {
    return JSON.parse(await readFile(filePath, "utf8")) as T;
  } catch {
    return null;
  }
}

export async function readTest262ReportJsonByArtifactId(
  artifactId: number,
): Promise<string | null> {
  if (!Number.isSafeInteger(artifactId) || artifactId <= 0) return null;
  try {
    return await readFile(
      path.join(TEST262_REPORTS_DIR, `${artifactId}.json`),
      "utf8",
    );
  } catch {
    return null;
  }
}

export async function readLatestTest262ReportJson(): Promise<string | null> {
  try {
    return await readFile(TEST262_LATEST_PATH, "utf8");
  } catch {
    return null;
  }
}

export async function loadTest262DashboardData(): Promise<Test262DashboardData> {
  const data = await readJsonFile<Test262DashboardData>(TEST262_DASHBOARD_PATH);
  if (!data) {
    return fallbackDashboard(
      "empty",
      "No build-time test262 snapshot is available yet.",
    );
  }
  return data;
}

export function createTest262DashboardData({
  generatedAt,
  timeline,
  latestReport,
}: {
  generatedAt: string;
  timeline: Test262TimelinePoint[];
  latestReport: Test262Report | null;
}): Test262DashboardData {
  if (timeline.length === 0 || !latestReport) {
    return fallbackDashboard(
      "empty",
      "No test262 result reports were found during the website build.",
    );
  }

  const latest = timeline[timeline.length - 1];
  const { leastCovered, mostCovered } = rankGroupCoverage(latestReport);

  return {
    status: "ready",
    generatedAt,
    source: test262DashboardSource(),
    latest,
    timeline,
    leastCovered,
    mostCovered,
  };
}

export function createTest262DashboardFallback(
  status: Exclude<Test262DashboardData["status"], "ready">,
  message: string,
): Test262DashboardData {
  return {
    ...fallbackDashboard(status, message),
    generatedAt: new Date().toISOString(),
  };
}

export function test262ReportFileName(): string {
  return TEST262_REPORT_NAME;
}

export function test262DataPaths() {
  return {
    dataDir: TEST262_DATA_DIR,
    dashboardPath: TEST262_DASHBOARD_PATH,
    latestPath: TEST262_LATEST_PATH,
    reportsDir: TEST262_REPORTS_DIR,
  };
}
