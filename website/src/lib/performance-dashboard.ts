import { unstable_cache } from "next/cache";
import {
  type AwfyBlobRun,
  listAwfyBlobDailyRuns,
  readAwfyBlobReportJson,
} from "@/lib/awfy-blob-store";
import { GITHUB_REPO_URL } from "@/lib/github";
import {
  type JetStreamBlobRun,
  listJetStreamBlobDailyRuns,
  readJetStreamBlobReportJson,
} from "@/lib/jetstream-blob-store";

export const PERFORMANCE_DASHBOARD_CACHE_SECONDS = 900;
export const PERFORMANCE_DASHBOARD_CACHE_CONTROL =
  "public, max-age=0, s-maxage=900, stale-while-revalidate=3600";

export type PerformanceSuite = "awfy" | "jetstream";
export type PerformanceMeasurementUnit = "microseconds" | "score";

type EngineStats = {
  ok?: unknown;
  timeout?: unknown;
  crash?: unknown;
  oom?: unknown;
  verificationFailed?: unknown;
  missingResult?: unknown;
  medianMicros?: unknown;
  medianScore?: unknown;
};

type NormalizedReport = {
  metadata: {
    driverVersion: number | null;
    corpusCommit: string;
    engineVersions: Record<string, string>;
    repetitions: number | null;
  };
  geomeanRatios: Record<string, number>;
  targets: Array<{
    name: string;
    engineStats: Record<string, EngineStats>;
    ratios: Record<string, number>;
  }>;
};

export type PerformanceTarget = {
  name: string;
  status: "complete" | "degraded";
  failure: string | null;
  unit: PerformanceMeasurementUnit;
  goccia: number | null;
  quickjs: number | null;
  node: number | null;
  quickjsRatio: number | null;
  nodeRatio: number | null;
};

export type PerformanceTimelinePoint = {
  suite: PerformanceSuite;
  runId: number;
  runNumber: number;
  artifactId: number;
  runUrl: string;
  headSha: string;
  shortSha: string;
  createdAt: string;
  complete: boolean;
  stale: boolean;
  quickjsRatio: number | null;
  nodeRatio: number | null;
  failedWorkloadCount: number;
  workloadCount: number;
  repetitions: number | null;
  engineVersions: Record<string, string>;
  corpusCommit: string;
  driverVersion: number | null;
  compatibilityKey: string;
};

export type PerformanceSuiteData = {
  latest: PerformanceTimelinePoint | null;
  latestComplete: PerformanceTimelinePoint | null;
  timeline: PerformanceTimelinePoint[];
  targets: PerformanceTarget[];
};

export type PerformanceDashboardData = {
  status: "ready" | "needs-blob-credentials" | "empty" | "error";
  message?: string;
  generatedAt: string;
  source: { repositoryUrl: string; workflowUrl: string };
  awfy: PerformanceSuiteData;
  jetstream: PerformanceSuiteData;
};

type RunMeta = Pick<
  AwfyBlobRun | JetStreamBlobRun,
  | "runId"
  | "runNumber"
  | "artifactId"
  | "runUrl"
  | "headSha"
  | "shortSha"
  | "createdAt"
>;

type RunSummary = {
  workloadCount?: unknown;
  targetCount?: unknown;
  failedWorkloadCount?: unknown;
  repetitions?: unknown;
  referenceRatios?: {
    quickjs?: unknown;
    node?: unknown;
  };
  engineVersions?: Record<string, unknown>;
  corpusCommit?: unknown;
  driverVersion?: unknown;
  targetNames?: unknown;
};

type RunWithSummary = RunMeta & { summary: RunSummary };

function finitePositive(value: unknown): number | null {
  return typeof value === "number" && Number.isFinite(value) && value > 0
    ? value
    : null;
}

function nonNegativeInteger(value: unknown): number {
  return typeof value === "number" && Number.isSafeInteger(value) && value >= 0
    ? value
    : 0;
}

function normalizeReport(
  value: unknown,
  suite: PerformanceSuite,
): NormalizedReport | null {
  if (!value || typeof value !== "object") return null;
  const report = value as Record<string, unknown>;
  if (!Array.isArray(report.targets)) return null;
  const metadata =
    report.metadata && typeof report.metadata === "object"
      ? (report.metadata as Record<string, unknown>)
      : {};
  const driver =
    metadata.driver && typeof metadata.driver === "object"
      ? (metadata.driver as Record<string, unknown>)
      : {};
  const corpus =
    metadata.corpus && typeof metadata.corpus === "object"
      ? (metadata.corpus as Record<string, unknown>)
      : {};
  const corpusEntry = corpus[suite === "awfy" ? "awfy" : "jetStream"];
  const corpusObject =
    corpusEntry && typeof corpusEntry === "object"
      ? (corpusEntry as Record<string, unknown>)
      : {};
  const options =
    metadata.options && typeof metadata.options === "object"
      ? (metadata.options as Record<string, unknown>)
      : {};
  const engineVersions = Object.fromEntries(
    (Array.isArray(metadata.engines) ? metadata.engines : [])
      .filter(
        (engine): engine is Record<string, unknown> =>
          Boolean(engine) && typeof engine === "object",
      )
      .filter(
        (engine) =>
          typeof engine.name === "string" && typeof engine.version === "string",
      )
      .map((engine) => [engine.name as string, engine.version as string]),
  );
  const targets = report.targets
    .map((entry): NormalizedReport["targets"][number] | null => {
      if (!entry || typeof entry !== "object") return null;
      const target = entry as Record<string, unknown>;
      if (typeof target.name !== "string") return null;
      const summary =
        target.summary && typeof target.summary === "object"
          ? (target.summary as Record<string, unknown>)
          : {};
      const rawStats =
        summary.engineStats && typeof summary.engineStats === "object"
          ? (summary.engineStats as Record<string, unknown>)
          : {};
      const engineStats = Object.fromEntries(
        Object.entries(rawStats).filter(
          (entry): entry is [string, EngineStats] =>
            Boolean(entry[1]) && typeof entry[1] === "object",
        ),
      );
      const ratios = Object.fromEntries(
        Object.entries(
          summary.ratios && typeof summary.ratios === "object"
            ? (summary.ratios as Record<string, unknown>)
            : {},
        ).filter(
          (entry): entry is [string, number] =>
            finitePositive(entry[1]) !== null,
        ),
      );
      return { name: target.name, engineStats, ratios };
    })
    .filter(
      (target): target is NormalizedReport["targets"][number] =>
        target !== null,
    );
  const geomeanRatios = Object.fromEntries(
    Object.entries(
      report.geomeanRatios && typeof report.geomeanRatios === "object"
        ? (report.geomeanRatios as Record<string, unknown>)
        : {},
    ).filter(
      (entry): entry is [string, number] => finitePositive(entry[1]) !== null,
    ),
  );
  return {
    metadata: {
      driverVersion: typeof driver.version === "number" ? driver.version : null,
      corpusCommit:
        typeof corpusObject.commit === "string"
          ? corpusObject.commit
          : "unknown",
      engineVersions,
      repetitions:
        typeof options.repetitions === "number" ? options.repetitions : null,
    },
    targets,
    geomeanRatios,
  };
}

function targetFailure(
  target: NormalizedReport["targets"][number],
): string | null {
  const failures: string[] = [];
  for (const engine of ["goccia", "qjs", "node"]) {
    const stats = target.engineStats[engine];
    if (!stats) {
      failures.push(`${engine}: missing`);
      continue;
    }
    for (const [field, label] of [
      ["timeout", "timeout"],
      ["crash", "crash"],
      ["oom", "OOM"],
      ["verificationFailed", "verification failure"],
      ["missingResult", "missing result"],
    ] as const) {
      if (nonNegativeInteger(stats[field]) > 0)
        failures.push(`${engine}: ${label}`);
    }
  }
  return failures.length > 0 ? failures.join(", ") : null;
}

function targetsFromReport(
  report: NormalizedReport | null,
  suite: PerformanceSuite,
): PerformanceTarget[] {
  if (!report) return [];
  const field = suite === "awfy" ? "medianMicros" : "medianScore";
  return report.targets.map((target) => {
    const failure = targetFailure(target);
    return {
      name: target.name,
      status: failure ? "degraded" : "complete",
      failure,
      unit: suite === "awfy" ? "microseconds" : "score",
      goccia: finitePositive(target.engineStats.goccia?.[field]),
      quickjs: finitePositive(target.engineStats.qjs?.[field]),
      node: finitePositive(target.engineStats.node?.[field]),
      quickjsRatio: finitePositive(target.ratios.goccia_over_qjs),
      nodeRatio: finitePositive(target.ratios.goccia_over_node),
    };
  });
}

function timelinePoint(
  suite: PerformanceSuite,
  run: RunMeta,
  summary: RunSummary | null,
): PerformanceTimelinePoint {
  const workloadCount = nonNegativeInteger(
    summary?.workloadCount ?? summary?.targetCount,
  );
  const failedWorkloadCount = nonNegativeInteger(summary?.failedWorkloadCount);
  const quickjsRatio = finitePositive(summary?.referenceRatios?.quickjs);
  const nodeRatio = finitePositive(summary?.referenceRatios?.node);
  const engineVersions = Object.fromEntries(
    Object.entries(summary?.engineVersions ?? {}).filter(
      (entry): entry is [string, string] => typeof entry[1] === "string",
    ),
  );
  const targetNames = Array.isArray(summary?.targetNames)
    ? summary.targetNames.filter(
        (name): name is string => typeof name === "string",
      )
    : [];
  const complete =
    Boolean(summary) &&
    workloadCount > 0 &&
    failedWorkloadCount === 0 &&
    quickjsRatio !== null &&
    nodeRatio !== null;
  const compatibilityKey = JSON.stringify({
    suite,
    corpus:
      typeof summary?.corpusCommit === "string"
        ? summary.corpusCommit
        : "unknown",
    driver:
      typeof summary?.driverVersion === "number" ? summary.driverVersion : null,
    quickjs: engineVersions.qjs ?? "unknown",
    node: engineVersions.node ?? "unknown",
    targets: targetNames.sort(),
  });
  return {
    suite,
    ...run,
    complete,
    stale: false,
    quickjsRatio: complete ? quickjsRatio : null,
    nodeRatio: complete ? nodeRatio : null,
    failedWorkloadCount,
    workloadCount,
    repetitions:
      typeof summary?.repetitions === "number" ? summary.repetitions : null,
    engineVersions,
    corpusCommit:
      typeof summary?.corpusCommit === "string"
        ? summary.corpusCommit
        : "unknown",
    driverVersion:
      typeof summary?.driverVersion === "number" ? summary.driverVersion : null,
    compatibilityKey,
  };
}

async function parseReport(
  json: string | null,
  suite: PerformanceSuite,
): Promise<NormalizedReport | null> {
  if (!json) return null;
  try {
    return normalizeReport(JSON.parse(json), suite);
  } catch {
    return null;
  }
}

async function buildSuiteData<Run extends RunWithSummary>(
  suite: PerformanceSuite,
  runs: Run[],
  readReport: (run: Run) => Promise<string | null>,
): Promise<PerformanceSuiteData> {
  const timeline = runs.map((run) => timelinePoint(suite, run, run.summary));
  const latest = timeline.at(-1) ?? null;
  const latestComplete =
    [...timeline].reverse().find((point) => point.complete) ?? null;
  if (latest && !latest.complete && latestComplete) latestComplete.stale = true;
  const latestRun = runs.at(-1);
  let latestReport: NormalizedReport | null = null;
  if (latestRun) {
    try {
      latestReport = await parseReport(await readReport(latestRun), suite);
    } catch {
      latestReport = null;
    }
  }
  return {
    latest,
    latestComplete,
    timeline,
    targets: targetsFromReport(latestReport, suite),
  };
}

function emptySuite(): PerformanceSuiteData {
  return { latest: null, latestComplete: null, timeline: [], targets: [] };
}

function fallback(
  status: PerformanceDashboardData["status"],
  message: string,
): PerformanceDashboardData {
  return {
    status,
    message,
    generatedAt: new Date().toISOString(),
    source: {
      repositoryUrl: GITHUB_REPO_URL,
      workflowUrl: `${GITHUB_REPO_URL}/actions/workflows/ci.yml`,
    },
    awfy: emptySuite(),
    jetstream: emptySuite(),
  };
}

async function loadUncachedPerformanceDashboardData(): Promise<PerformanceDashboardData> {
  try {
    const [awfyRuns, jetStreamRuns] = await Promise.all([
      listAwfyBlobDailyRuns(),
      listJetStreamBlobDailyRuns(),
    ]);
    const [awfy, jetstream] = await Promise.all([
      buildSuiteData("awfy", awfyRuns, readAwfyBlobReportJson),
      buildSuiteData("jetstream", jetStreamRuns, readJetStreamBlobReportJson),
    ]);
    if (awfy.timeline.length === 0 && jetstream.timeline.length === 0) {
      return fallback("empty", "No performance barometer reports were found.");
    }
    return {
      status: "ready",
      generatedAt: new Date().toISOString(),
      source: {
        repositoryUrl: GITHUB_REPO_URL,
        workflowUrl: `${GITHUB_REPO_URL}/actions/workflows/ci.yml`,
      },
      awfy,
      jetstream,
    };
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    const status = /No blob credentials|No read-write token/i.test(message)
      ? "needs-blob-credentials"
      : "error";
    return fallback(status, `Failed to read performance reports: ${message}`);
  }
}

export const loadPerformanceDashboardData = unstable_cache(
  loadUncachedPerformanceDashboardData,
  ["performance-dashboard"],
  {
    revalidate: PERFORMANCE_DASHBOARD_CACHE_SECONDS,
    tags: ["performance-dashboard"],
  },
);

export const performanceDashboardTestApi = {
  normalizeReport,
  buildSuiteData,
  targetsFromReport,
  timelinePoint,
};
