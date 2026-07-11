import { describe, expect, mock, test } from "bun:test";
import { gunzipSync } from "node:zlib";
import { summaryFromReport } from "../../scripts/publish-web-tooling-report";

type BlobPutCall = {
  pathname: string;
  body: unknown;
  options: Record<string, unknown>;
};

const putCalls: BlobPutCall[] = [];

mock.module("@vercel/blob", () => ({
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
      etag: "written-etag",
    };
  },
}));

function resetState() {
  putCalls.length = 0;
}

async function importBlobStore() {
  return await import("@/lib/web-tooling-blob-store");
}

describe("Web Tooling Blob store", () => {
  test("publishes compressed report and daily pointer", async () => {
    resetState();
    const { publishWebToolingReportsToBlob } = await importBlobStore();
    const report = {
      schemaVersion: 1,
      targets: [],
      summary: { workloadCount: 18 },
    };

    const runs = await publishWebToolingReportsToBlob([
      {
        runId: 202,
        runNumber: 2,
        artifactId: 2002,
        title: "CI",
        headSha: "abcdef123456",
        shortSha: "abcdef12",
        runUrl: "https://example.test/runs/202",
        createdAt: "2026-07-06T10:00:00.000Z",
        updatedAt: "2026-07-06T10:05:00.000Z",
        artifactCreatedAt: "2026-07-06T10:05:00.000Z",
        summary: {
          workloadCount: 18,
          builtCount: 18,
          completedCount: 3,
          buildFailedCount: 0,
          timeoutCount: 1,
          crashCount: 14,
          syntaxErrorCount: 0,
          runtimeErrorCount: 0,
          oomCount: 0,
          missingResultCount: 0,
          repetitions: 1,
        },
        reportJson: JSON.stringify(report),
      },
    ]);

    expect(putCalls.map((call) => call.pathname)).toEqual([
      "web-tooling/runs/2002/report.json.gz",
      "web-tooling/daily/2026-07-06.json",
    ]);
    expect(putCalls[0]?.options).toMatchObject({
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });
    expect(putCalls[1]?.options).toMatchObject({
      allowOverwrite: true,
      cacheControlMaxAge: 900,
      contentType: "application/json",
    });
    expect(gunzipSync(putCalls[0]?.body as Uint8Array).toString("utf8")).toBe(
      `${JSON.stringify(report)}\n`,
    );
    expect(runs.map((run) => run.artifactId)).toEqual([2002]);
    expect(runs[0]?.summary.workloadCount).toBe(18);
  });

  test("retains every report outcome in the daily summary", () => {
    expect(
      summaryFromReport({
        summary: {
          workloadCount: 4,
          builtCount: 4,
          completedCount: 2,
          buildFailedCount: 0,
          timeoutCount: 0,
          crashCount: 0,
          syntaxErrorCount: 1,
          runtimeErrorCount: 1,
          oomCount: 0,
          missingResultCount: 0,
        },
        metadata: { options: { repetitions: 1 } },
        targets: [{}, {}, {}, {}],
      }),
    ).toMatchObject({
      syntaxErrorCount: 1,
      runtimeErrorCount: 1,
    });
  });
});
