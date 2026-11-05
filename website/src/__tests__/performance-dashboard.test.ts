import { describe, expect, mock, test } from "bun:test";
import { gunzipSync } from "node:zlib";
import { summaryFromReport as summaryFromAwfyReport } from "../../scripts/publish-awfy-report";
import { summaryFromReport } from "../../scripts/publish-jetstream-report";

type PutCall = {
  pathname: string;
  body: unknown;
  options: Record<string, unknown>;
};

const putCalls: PutCall[] = [];

mock.module("@vercel/blob", () => ({
  BlobNotFoundError: class BlobNotFoundError extends Error {},
  get: async () => null,
  list: async () => ({ blobs: [], cursor: undefined, hasMore: false }),
  put: async (
    pathname: string,
    body: unknown,
    options: Record<string, unknown>,
  ) => {
    putCalls.push({ pathname, body, options });
    return {
      url: `https://blob.test/${pathname}`,
      downloadUrl: `https://blob.test/${pathname}?download=1`,
      pathname,
      contentType: String(options.contentType ?? "application/octet-stream"),
      contentDisposition: "",
      etag: "etag",
    };
  },
}));

describe("Performance Barometer data", () => {
  test("normalizes lower-is-better AWFY ratios", async () => {
    const { performanceDashboardTestApi } = await import(
      "@/lib/performance-dashboard"
    );
    const report = performanceDashboardTestApi.normalizeReport(
      {
        metadata: {
          driver: { version: 1 },
          corpus: { awfy: { commit: "awfy-sha" } },
          engines: [
            { name: "goccia", version: "0.8" },
            { name: "qjs", version: "2026-06-04" },
            { name: "node", version: "v24" },
          ],
          options: { repetitions: 5 },
        },
        targets: [
          {
            name: "Tiny",
            summary: {
              engineStats: {
                goccia: { ok: 5, medianMicros: 200 },
                qjs: { ok: 5, medianMicros: 100 },
                node: { ok: 5, medianMicros: 50 },
              },
              ratios: { goccia_over_qjs: 2, goccia_over_node: 4 },
            },
          },
        ],
        geomeanRatios: { goccia_over_qjs: 2, goccia_over_node: 4 },
      },
      "awfy",
    );
    expect(report?.geomeanRatios).toEqual({
      goccia_over_qjs: 2,
      goccia_over_node: 4,
    });
    expect(
      performanceDashboardTestApi.targetsFromReport(report, "awfy")[0],
    ).toMatchObject({
      unit: "microseconds",
      goccia: 200,
      quickjsRatio: 2,
      nodeRatio: 4,
      status: "complete",
    });
  });

  test("keeps incomplete runs as gaps", async () => {
    const { performanceDashboardTestApi } = await import(
      "@/lib/performance-dashboard"
    );
    const point = performanceDashboardTestApi.timelinePoint(
      "jetstream",
      {
        runId: 1,
        runNumber: 1,
        artifactId: 1,
        runUrl: "https://example.test/run/1",
        headSha: "abcdef123456",
        shortSha: "abcdef12",
        createdAt: "2026-07-12T00:00:00.000Z",
      },
      null,
    );
    expect(point.complete).toBe(false);
    expect(point.quickjsRatio).toBeNull();
    expect(point.nodeRatio).toBeNull();
    expect(point.nodeJitlessRatio).toBeNull();
  });

  test("normalizes a driver 2 report with node-jitless", async () => {
    const { performanceDashboardTestApi } = await import(
      "@/lib/performance-dashboard"
    );
    const report = performanceDashboardTestApi.normalizeReport(
      {
        metadata: {
          driver: { version: 2 },
          corpus: { awfy: { commit: "awfy-sha" } },
          engines: [
            { name: "goccia", version: "0.8" },
            { name: "qjs", version: "2026-06-04" },
            { name: "node-jitless", version: "v24.1" },
            { name: "node", version: "v24.1" },
          ],
        },
        targets: [
          {
            name: "Tiny",
            summary: {
              engineStats: {
                goccia: { ok: 5, medianMicros: 200 },
                qjs: { ok: 5, medianMicros: 100 },
                "node-jitless": { ok: 5, medianMicros: 80 },
                node: { ok: 5, medianMicros: 50 },
              },
              ratios: {
                goccia_over_qjs: 2,
                "goccia_over_node-jitless": 2.5,
                goccia_over_node: 4,
              },
            },
          },
          {
            name: "Broken",
            summary: {
              engineStats: {
                goccia: { ok: 5, medianMicros: 200 },
                qjs: { ok: 5, medianMicros: 100 },
                node: { ok: 5, medianMicros: 50 },
              },
            },
          },
        ],
        geomeanRatios: {
          goccia_over_qjs: 2,
          "goccia_over_node-jitless": 2.5,
          goccia_over_node: 4,
        },
      },
      "awfy",
    );
    const [tiny, broken] = performanceDashboardTestApi.targetsFromReport(
      report,
      "awfy",
    );
    expect(tiny).toMatchObject({
      status: "complete",
      nodeJitless: 80,
      nodeJitlessRatio: 2.5,
    });
    // The report lists node-jitless, so a target without its stats degrades.
    expect(broken).toMatchObject({
      status: "degraded",
      failure: "node-jitless: missing",
      nodeJitless: null,
      nodeJitlessRatio: null,
    });
  });

  test("keeps driver 1 reports without node-jitless complete", async () => {
    const { performanceDashboardTestApi } = await import(
      "@/lib/performance-dashboard"
    );
    const report = performanceDashboardTestApi.normalizeReport(
      {
        metadata: {
          driver: { version: 1 },
          corpus: { awfy: { commit: "awfy-sha" } },
          engines: [
            { name: "goccia", version: "0.8" },
            { name: "qjs", version: "2026-06-04" },
            { name: "node", version: "v24" },
          ],
        },
        targets: [
          {
            name: "Tiny",
            summary: {
              engineStats: {
                goccia: { ok: 5, medianMicros: 200 },
                qjs: { ok: 5, medianMicros: 100 },
                node: { ok: 5, medianMicros: 50 },
              },
              ratios: { goccia_over_qjs: 2, goccia_over_node: 4 },
            },
          },
        ],
        geomeanRatios: { goccia_over_qjs: 2, goccia_over_node: 4 },
      },
      "awfy",
    );
    expect(
      performanceDashboardTestApi.targetsFromReport(report, "awfy")[0],
    ).toMatchObject({
      status: "complete",
      failure: null,
      nodeJitless: null,
      nodeJitlessRatio: null,
    });
    const point = performanceDashboardTestApi.timelinePoint(
      "awfy",
      {
        runId: 1,
        runNumber: 1,
        artifactId: 1,
        runUrl: "https://example.test/run/1",
        headSha: "abcdef123456",
        shortSha: "abcdef12",
        createdAt: "2026-07-12T00:00:00.000Z",
      },
      {
        workloadCount: 1,
        failedWorkloadCount: 0,
        referenceRatios: { quickjs: 2, node: 4 },
        driverVersion: 1,
        targetNames: ["Tiny"],
      },
    );
    expect(point).toMatchObject({
      complete: true,
      quickjsRatio: 2,
      nodeRatio: 4,
      nodeJitlessRatio: null,
    });
  });

  test("breaks the compatibility key when the protocol changes", async () => {
    const { performanceDashboardTestApi } = await import(
      "@/lib/performance-dashboard"
    );
    const run = {
      runId: 1,
      runNumber: 1,
      artifactId: 1,
      runUrl: "https://example.test/run/1",
      headSha: "abcdef123456",
      shortSha: "abcdef12",
      createdAt: "2026-07-12T00:00:00.000Z",
    };
    const summary = {
      workloadCount: 1,
      failedWorkloadCount: 0,
      referenceRatios: { quickjs: 2, node: 4, nodeJitless: 3 },
      driverVersion: 2,
      targetNames: ["Tiny"],
      protocolKey: "alternate|Tiny:10/3/5",
    };
    const key = (value: typeof summary | { protocolKey?: undefined }) =>
      performanceDashboardTestApi.timelinePoint(
        "awfy",
        run,
        value as typeof summary,
      ).compatibilityKey;
    const base = key(summary);
    expect(key({ ...summary, protocolKey: "alternate|Tiny:10/4/5" })).not.toBe(
      base,
    );
    expect(key({ ...summary, protocolKey: undefined })).not.toBe(base);
    expect(key(summary)).toBe(base);
    expect(
      performanceDashboardTestApi.timelinePoint("awfy", run, summary)
        .nodeJitlessRatio,
    ).toBe(3);
  });

  test("builds retained history from daily summaries and reads only the latest report", async () => {
    const { performanceDashboardTestApi } = await import(
      "@/lib/performance-dashboard"
    );
    const reads: number[] = [];
    const runs = [1, 2, 3].map((runId) => ({
      runId,
      runNumber: runId,
      artifactId: runId,
      runUrl: `https://example.test/run/${runId}`,
      headSha: `abcdef0${runId}`,
      shortSha: `abcdef0${runId}`,
      createdAt: `2026-07-${String(runId).padStart(2, "0")}T00:00:00.000Z`,
      summary: {
        workloadCount: 1,
        failedWorkloadCount: 0,
        repetitions: 5,
        referenceRatios: { quickjs: 2, node: 4 },
        engineVersions: { qjs: "2026-06-04", node: "v24" },
        corpusCommit: "release-sha",
        driverVersion: 1,
        targetNames: ["tiny"],
      },
    }));
    const latestReport = JSON.stringify({
      metadata: {
        driver: { version: 1 },
        corpus: { jetStream: { commit: "release-sha" } },
        engines: [
          { name: "goccia", version: "0.8" },
          { name: "qjs", version: "2026-06-04" },
          { name: "node", version: "v24" },
        ],
        options: { repetitions: 5 },
      },
      targets: [
        {
          name: "tiny",
          summary: {
            engineStats: {
              goccia: { ok: 5, medianScore: 10 },
              qjs: { ok: 5, medianScore: 20 },
              node: { ok: 5, medianScore: 40 },
            },
            ratios: { goccia_over_qjs: 2, goccia_over_node: 4 },
          },
        },
      ],
      geomeanRatios: { goccia_over_qjs: 2, goccia_over_node: 4 },
    });
    const data = await performanceDashboardTestApi.buildSuiteData(
      "jetstream",
      runs,
      async (run: (typeof runs)[number]) => {
        reads.push(run.runId);
        return latestReport;
      },
    );
    expect(reads).toEqual([3]);
    expect(data.timeline).toHaveLength(3);
    expect(data.timeline.every((point) => point.complete)).toBe(true);
    expect(data.targets[0]?.name).toBe("tiny");
  });

  test("summarizes JetStream failures and version boundaries", () => {
    expect(
      summaryFromReport({
        metadata: {
          driver: { version: 1 },
          corpus: {
            jetStream: {
              commit: "release-sha",
              benchmarks: { complete: {}, failed: {}, missing: {} },
            },
          },
          engines: [
            { name: "goccia", version: "0.8" },
            { name: "qjs", version: "2026-06-04" },
            { name: "node", version: "v24" },
          ],
          options: { repetitions: 5 },
        },
        targets: [
          {
            name: "complete",
            summary: {
              engineStats: {
                goccia: { ok: 5 },
                qjs: { ok: 5 },
                node: { ok: 5 },
              },
            },
          },
          {
            name: "failed",
            summary: {
              engineStats: {
                goccia: { timeout: 1 },
                qjs: { ok: 5 },
                node: { ok: 5 },
              },
            },
          },
        ],
        geomeanRatios: { goccia_over_qjs: 2, goccia_over_node: 4 },
      }),
    ).toEqual({
      workloadCount: 3,
      failedWorkloadCount: 2,
      repetitions: 5,
      referenceRatios: { quickjs: 2, node: 4, nodeJitless: null },
      engineVersions: {
        goccia: "0.8",
        qjs: "2026-06-04",
        node: "v24",
      },
      corpusCommit: "release-sha",
      driverVersion: 1,
      targetNames: ["complete", "failed", "missing"],
      protocolKey: null,
    });
  });

  test("summarizes a driver 2 JetStream report with node-jitless", () => {
    const report = {
      metadata: {
        driver: { version: 2 },
        corpus: { jetStream: { commit: "release-sha" } },
        engines: [
          { name: "goccia", version: "0.8" },
          { name: "qjs", version: "2026-06-04" },
          { name: "node-jitless", version: "v24.1" },
          { name: "node", version: "v24.1" },
        ],
        options: { repetitions: 5, engineOrder: "alternate" },
      },
      targets: [
        {
          name: "zeta",
          iterations: 3,
          summary: {
            engineStats: {
              goccia: { ok: 5 },
              qjs: { ok: 5 },
              "node-jitless": { ok: 5 },
              node: { ok: 5 },
            },
          },
        },
        {
          name: "alpha",
          iterations: 20,
          summary: {
            engineStats: {
              goccia: { ok: 5 },
              qjs: { ok: 5 },
              "node-jitless": { ok: 5 },
              node: { ok: 5 },
            },
          },
        },
      ],
      geomeanRatios: {
        goccia_over_qjs: 2,
        goccia_over_node: 4,
        "goccia_over_node-jitless": 3,
      },
    };
    expect(summaryFromReport(report)).toMatchObject({
      failedWorkloadCount: 0,
      referenceRatios: { quickjs: 2, node: 4, nodeJitless: 3 },
      driverVersion: 2,
      protocolKey: "alternate|alpha:20,zeta:3",
    });
    // A listed node-jitless engine is required once the report names it.
    const failing = structuredClone(report);
    const failingStats = (index: number) =>
      failing.targets[index]?.summary.engineStats as Record<string, unknown>;
    failingStats(0)["node-jitless"] = { timeout: 1 };
    expect(summaryFromReport(failing).failedWorkloadCount).toBe(1);
    delete failingStats(1)["node-jitless"];
    expect(summaryFromReport(failing).failedWorkloadCount).toBe(2);
  });

  test("summarizes a driver 2 AWFY report with node-jitless and protocol", () => {
    const stats = {
      goccia: { ok: 5 },
      qjs: { ok: 5 },
      "node-jitless": { ok: 5 },
      node: { ok: 5 },
    };
    const summary = summaryFromAwfyReport({
      metadata: {
        driver: { version: 2 },
        corpus: { awfy: { commit: "awfy-sha" } },
        engines: [
          { name: "goccia", version: "0.8" },
          { name: "qjs", version: "2026-06-04" },
          { name: "node-jitless", version: "v24.1" },
          { name: "node", version: "v24.1" },
        ],
        options: { repetitions: 5, engineOrder: "alternate" },
      },
      targets: [
        {
          name: "Richards",
          kind: "awfy",
          innerIterations: 10,
          warmupIterations: 3,
          measuredIterations: 5,
          summary: { engineStats: stats },
        },
        {
          name: "Bounce",
          kind: "awfy",
          innerIterations: 100,
          warmupIterations: 2,
          measuredIterations: 4,
          summary: {
            engineStats: { ...stats, "node-jitless": { crash: 1 } },
          },
        },
      ],
      geomeanRatios: {
        goccia_over_qjs: 2,
        goccia_over_node: 4,
        "goccia_over_node-jitless": 3,
      },
    });
    expect(summary).toMatchObject({
      failedWorkloadCount: 1,
      referenceRatios: { quickjs: 2, node: 4, nodeJitless: 3 },
      driverVersion: 2,
      protocolKey: "alternate|Bounce:100/2/4,Richards:10/3/5",
    });
  });

  test("marks missing AWFY workloads incomplete in daily summaries", () => {
    expect(
      summaryFromAwfyReport({
        metadata: {
          driver: { version: 1 },
          corpus: {
            awfy: {
              commit: "awfy-sha",
              benchmarks: { Bounce: "Bounce", Richards: "Richards" },
            },
          },
          engines: [
            { name: "goccia", version: "0.8" },
            { name: "qjs", version: "2026-06-04" },
            { name: "node", version: "v24" },
          ],
          options: { repetitions: 5 },
        },
        targets: [
          {
            name: "Bounce",
            kind: "awfy",
            summary: {
              engineStats: {
                goccia: { ok: 5 },
                qjs: { ok: 5 },
                node: { ok: 5 },
              },
            },
          },
          {
            name: "RuntimeProbe",
            kind: "probe",
            summary: {
              engineStats: {
                goccia: { crash: 1 },
                qjs: { ok: 5 },
                node: { ok: 5 },
              },
            },
          },
        ],
        geomeanRatios: { goccia_over_qjs: 2, goccia_over_node: 4 },
      }),
    ).toMatchObject({
      workloadCount: 2,
      failedWorkloadCount: 1,
      targetNames: ["Bounce", "Richards"],
      referenceRatios: { quickjs: 2, node: 4, nodeJitless: null },
      protocolKey: null,
    });
  });
});

describe("JetStream Blob store", () => {
  test("publishes compressed reports to distinct same-day run pointers", async () => {
    putCalls.length = 0;
    const { publishJetStreamReportsToBlob } = await import(
      "@/lib/jetstream-blob-store"
    );
    const reportJson = JSON.stringify({ schemaVersion: 1, targets: [] });
    const firstEntry = {
      runId: 101,
      runNumber: 7,
      artifactId: 1001,
      title: "CI",
      headSha: "abcdef123456",
      shortSha: "abcdef12",
      runUrl: "https://example.test/run/101",
      createdAt: "2026-07-12T10:00:00.000Z",
      updatedAt: "2026-07-12T10:05:00.000Z",
      artifactCreatedAt: "2026-07-12T10:05:00.000Z",
      summary: {
        workloadCount: 6,
        failedWorkloadCount: 0,
        repetitions: 5,
        referenceRatios: { quickjs: 2, node: 4, nodeJitless: null },
        engineVersions: { qjs: "2026-06-04", node: "v24" },
        corpusCommit: "release-sha",
        driverVersion: 1,
        targetNames: ["tiny"],
        protocolKey: null,
      },
      reportJson,
    };
    const runs = await publishJetStreamReportsToBlob([
      firstEntry,
      {
        ...firstEntry,
        runId: 102,
        runNumber: 8,
        artifactId: 1002,
        runUrl: "https://example.test/run/102",
        createdAt: "2026-07-12T12:00:00.000Z",
      },
    ]);
    expect(putCalls.map((call) => call.pathname)).toEqual([
      "jetstream/runs/1001/report.json.gz",
      "jetstream/daily/2026-07-12/101.json",
      "jetstream/runs/1002/report.json.gz",
      "jetstream/daily/2026-07-12/102.json",
    ]);
    expect(gunzipSync(putCalls[0]?.body as Uint8Array).toString("utf8")).toBe(
      `${reportJson}\n`,
    );
    expect(runs.map((run) => run.runId)).toEqual([101, 102]);
    expect(runs[0]?.summary.referenceRatios.node).toBe(4);
  });
});
