import { describe, expect, mock, test } from "bun:test";
import type {
  Test262Report,
  Test262SuiteSummary,
  Test262TimelinePoint,
} from "@/lib/test262-dashboard";
import type { Test262BlobRun } from "../../scripts/test262-blob-store";

type BlobGetCall = {
  pathname: string;
  options: Record<string, unknown>;
};

type BlobPutCall = {
  pathname: string;
  body: unknown;
  options: Record<string, unknown>;
};

const getCalls: BlobGetCall[] = [];
const putCalls: BlobPutCall[] = [];

class BlobPreconditionFailedError extends Error {
  constructor() {
    super("Vercel Blob: Precondition failed: ETag mismatch.");
  }
}

function textStream(text: string): ReadableStream<Uint8Array> {
  const stream = new Response(text).body;
  if (!stream) throw new Error("expected response body stream");
  return stream;
}

function summary(totalRun: number): Test262SuiteSummary {
  return {
    totalDiscovered: totalRun,
    totalRun,
    passed: totalRun,
    failed: 0,
    wrapperInfraFailures: 0,
    timeouts: 0,
    durationSeconds: 1,
    byCategory: [],
  };
}

const existingRun: Test262BlobRun = {
  runId: 101,
  runNumber: 1,
  title: "CI",
  headSha: "old-sha",
  shortSha: "old-sha",
  runUrl: "https://example.test/runs/101",
  createdAt: "2026-06-10T01:00:00.000Z",
  updatedAt: "2026-06-10T01:05:00.000Z",
  artifactId: 1001,
  artifactCreatedAt: "2026-06-10T01:05:00.000Z",
  jsonUrl: "/api/test262/results/1001",
  summary: summary(1),
  reportPath: "test262/runs/1001.json.gz",
  reportUrl: "https://blob.test/test262/runs/1001.json.gz",
  reportDownloadUrl: "https://blob.test/test262/runs/1001.json.gz?download=1",
  reportCompressedSize: 128,
  publishedAt: "2026-06-10T01:06:00.000Z",
};

mock.module("@vercel/blob", () => ({
  BlobPreconditionFailedError,
  get: async (pathname: string, options: Record<string, unknown>) => {
    getCalls.push({ pathname, options });
    return {
      statusCode: 200,
      stream: textStream(JSON.stringify({ version: 1, runs: [existingRun] })),
      blob: {
        etag: '"fresh-origin-etag"',
      },
    };
  },
  put: async (
    pathname: string,
    body: unknown,
    options: Record<string, unknown>,
  ) => {
    putCalls.push({ pathname, body, options });
    if (pathname === "test262/manifest.json") {
      if (options.ifMatch !== '"fresh-origin-etag"') {
        throw new BlobPreconditionFailedError();
      }
    }
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

describe("test262 Blob publishing", () => {
  test("bypasses manifest cache and preserves get ETags for conditional writes", async () => {
    getCalls.length = 0;
    putCalls.length = 0;

    const { publishTest262ReportsToBlob } = await import(
      "../../scripts/test262-blob-store"
    );
    const report: Test262Report = {
      summary: summary(2),
      results: [],
    };
    const point: Test262TimelinePoint = {
      runId: 202,
      runNumber: 2,
      title: "CI",
      headSha: "new-sha",
      shortSha: "new-sha",
      runUrl: "https://example.test/runs/202",
      createdAt: "2026-06-11T01:00:00.000Z",
      updatedAt: "2026-06-11T01:05:00.000Z",
      artifactId: 1002,
      artifactCreatedAt: "2026-06-11T01:05:00.000Z",
      jsonUrl: "/api/test262/results/1002",
      summary: report.summary,
    };

    const manifest = await publishTest262ReportsToBlob([
      {
        point,
        report,
        reportJson: JSON.stringify(report),
      },
    ]);

    const manifestRead = getCalls.find(
      (call) => call.pathname === "test262/manifest.json",
    );
    const manifestWrite = putCalls.find(
      (call) => call.pathname === "test262/manifest.json",
    );

    expect(manifestRead?.options).toMatchObject({ useCache: false });
    expect(manifestWrite?.options).toMatchObject({
      allowOverwrite: true,
      ifMatch: '"fresh-origin-etag"',
    });
    expect(manifest.runs.map((run) => run.artifactId)).toEqual([1001, 1002]);
  });
});
