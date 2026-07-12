import { describe, expect, mock, test } from "bun:test";
import { gunzipSync } from "node:zlib";
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
  });

  test("summarizes JetStream failures and version boundaries", () => {
    expect(
      summaryFromReport({
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
          { summary: { engineStats: { goccia: { ok: 5 } } } },
          {
            summary: {
              engineStats: { goccia: { timeout: 1 }, qjs: { ok: 5 } },
            },
          },
        ],
        geomeanRatios: { goccia_over_qjs: 2, goccia_over_node: 4 },
      }),
    ).toEqual({
      workloadCount: 2,
      failedWorkloadCount: 1,
      repetitions: 5,
      referenceRatios: { quickjs: 2, node: 4 },
      engineVersions: {
        goccia: "0.8",
        qjs: "2026-06-04",
        node: "v24",
      },
      corpusCommit: "release-sha",
      driverVersion: 1,
    });
  });
});

describe("JetStream Blob store", () => {
  test("publishes compressed report and daily pointer", async () => {
    putCalls.length = 0;
    const { publishJetStreamReportsToBlob } = await import(
      "@/lib/jetstream-blob-store"
    );
    const reportJson = JSON.stringify({ schemaVersion: 1, targets: [] });
    const runs = await publishJetStreamReportsToBlob([
      {
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
          referenceRatios: { quickjs: 2, node: 4 },
          engineVersions: { qjs: "2026-06-04", node: "v24" },
          corpusCommit: "release-sha",
          driverVersion: 1,
        },
        reportJson,
      },
    ]);
    expect(putCalls.map((call) => call.pathname)).toEqual([
      "jetstream/runs/1001/report.json.gz",
      "jetstream/daily/2026-07-12.json",
    ]);
    expect(gunzipSync(putCalls[0]?.body as Uint8Array).toString("utf8")).toBe(
      `${reportJson}\n`,
    );
    expect(runs[0]?.summary.referenceRatios.node).toBe(4);
  });
});
