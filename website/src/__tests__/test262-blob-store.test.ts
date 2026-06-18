import { describe, expect, mock, test } from "bun:test";
import { gunzipSync, gzipSync } from "node:zlib";
import type { Test262BlobRun } from "@/lib/test262-blob-store";
import type {
  Test262Report,
  Test262SuiteSummary,
  Test262TimelinePoint,
} from "@/lib/test262-dashboard";

type BlobGetCall = {
  pathname: string;
  options: Record<string, unknown>;
};

type BlobPutCall = {
  pathname: string;
  body: unknown;
  options: Record<string, unknown>;
};

type BlobListCall = {
  options: Record<string, unknown>;
};

const getCalls: BlobGetCall[] = [];
const putCalls: BlobPutCall[] = [];
const listCalls: BlobListCall[] = [];

class BlobNotFoundError extends Error {
  constructor() {
    super("The requested blob does not exist");
  }
}

function textStream(text: string): ReadableStream<Uint8Array> {
  const stream = new Response(text).body;
  if (!stream) throw new Error("expected response body stream");
  return stream;
}

function byteStream(bytes: Uint8Array): ReadableStream<Uint8Array> {
  const stream = new Response(Buffer.from(bytes)).body;
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

const dailyRun: Test262BlobRun = {
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

const reportJson = JSON.stringify({
  summary: summary(1),
  results: [{ id: "built-ins/Array/a.js", status: "PASS" }],
});

mock.module("@vercel/blob", () => ({
  BlobNotFoundError,
  get: async (pathname: string, options: Record<string, unknown>) => {
    getCalls.push({ pathname, options });
    if (pathname === "test262/daily/2026-06-10.json") {
      return {
        statusCode: 200,
        stream: textStream(JSON.stringify(dailyRun)),
        blob: { etag: "daily-etag" },
      };
    }
    if (pathname === "test262/daily/bad.json") {
      return {
        statusCode: 200,
        stream: textStream(JSON.stringify({ nope: true })),
        blob: { etag: "bad-etag" },
      };
    }
    if (pathname === "test262/daily/invalid-timestamp.json") {
      return {
        statusCode: 200,
        stream: textStream(
          JSON.stringify({ ...dailyRun, createdAt: "not-a-date" }),
        ),
        blob: { etag: "invalid-timestamp-etag" },
      };
    }
    if (pathname === "test262/daily/invalid-size.json") {
      return {
        statusCode: 200,
        stream: textStream(
          JSON.stringify({ ...dailyRun, reportCompressedSize: -1 }),
        ),
        blob: { etag: "invalid-size-etag" },
      };
    }
    if (pathname === "test262/daily/corrupt.json") {
      return {
        statusCode: 200,
        stream: textStream("{"),
        blob: { etag: "corrupt-etag" },
      };
    }
    if (pathname === "test262/runs/1001.json.gz") {
      return {
        statusCode: 200,
        stream: byteStream(gzipSync(reportJson)),
        blob: { etag: "report-etag" },
      };
    }
    return null;
  },
  list: async (options: Record<string, unknown>) => {
    listCalls.push({ options });
    if (!options.cursor) {
      return {
        blobs: [
          { pathname: "test262/daily/bad.json" },
          { pathname: "test262/daily/invalid-timestamp.json" },
          { pathname: "test262/daily/invalid-size.json" },
          { pathname: "test262/daily/corrupt.json" },
          { pathname: "test262/daily/2026-06-10.json" },
        ],
        cursor: "next",
        hasMore: true,
      };
    }
    return { blobs: [], hasMore: false };
  },
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
  getCalls.length = 0;
  putCalls.length = 0;
  listCalls.length = 0;
}

function publishEntry() {
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
  return { point, report, reportJson: JSON.stringify(report) };
}

async function importBlobStore() {
  return await import("@/lib/test262-blob-store");
}

async function importBenchmarkProfileBlobStore() {
  return await import("@/lib/benchmark-profile-blob-store");
}

describe("test262 Blob store", () => {
  test("publishes report and daily blobs without reading or writing a manifest", async () => {
    resetState();
    const { publishTest262ReportsToBlob } = await importBlobStore();

    const runs = await publishTest262ReportsToBlob([publishEntry()]);

    expect(getCalls).toHaveLength(0);
    expect(putCalls.map((call) => call.pathname)).toEqual([
      "test262/runs/1002.json.gz",
      "test262/daily/2026-06-11.json",
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
      `${JSON.stringify(publishEntry().report)}\n`,
    );
    expect(runs.map((run) => run.artifactId)).toEqual([1002]);
  });

  test("publishes profile reports under a separate namespace", async () => {
    resetState();
    const { publishTest262ProfileReportsToBlob } = await importBlobStore();
    const markdown = Buffer.from("# profile\n");
    const detailsArchive = Buffer.from([1, 2, 3]);

    const runs = await publishTest262ProfileReportsToBlob([
      {
        runId: 303,
        runNumber: 3,
        title: "CI",
        headSha: "profile-sha",
        shortSha: "profile-",
        runUrl: "https://example.test/runs/303",
        createdAt: "2026-06-12T01:00:00.000Z",
        updatedAt: "2026-06-12T01:05:00.000Z",
        artifactId: 3003,
        artifactCreatedAt: "2026-06-12T01:05:00.000Z",
        aggregateJson: JSON.stringify({ kind: "aggregate", total: 2 }),
        markdown,
        detailsArchive,
      },
    ]);

    expect(getCalls).toHaveLength(0);
    expect(putCalls.map((call) => call.pathname)).toEqual([
      "test262-profiles/runs/3003/aggregate.json.gz",
      "test262-profiles/runs/3003/summary.md",
      "test262-profiles/runs/3003/details.tar.gz",
      "test262-profiles/daily/2026-06-12.json",
    ]);
    expect(putCalls[0]?.options).toMatchObject({
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });
    expect(putCalls[1]?.options).toMatchObject({
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "text/markdown; charset=utf-8",
    });
    expect(putCalls[2]?.options).toMatchObject({
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });
    expect(putCalls[3]?.options).toMatchObject({
      allowOverwrite: true,
      cacheControlMaxAge: 900,
      contentType: "application/json",
    });
    expect(gunzipSync(putCalls[0]?.body as Uint8Array).toString("utf8")).toBe(
      `${JSON.stringify({ kind: "aggregate", total: 2 })}\n`,
    );
    expect(putCalls[1]?.body).toEqual(markdown);
    expect(putCalls[2]?.body).toEqual(detailsArchive);

    const pointer = JSON.parse(String(putCalls[3]?.body));
    expect(pointer.profileReports.aggregate.path).toBe(
      "test262-profiles/runs/3003/aggregate.json.gz",
    );
    expect(pointer.profileReports.markdown.path).toBe(
      "test262-profiles/runs/3003/summary.md",
    );
    expect(pointer.profileReports.detailsArchive.path).toBe(
      "test262-profiles/runs/3003/details.tar.gz",
    );
    expect(runs.map((run) => run.artifactId)).toEqual([3003]);
  });

  test("publishes benchmark profile reports under a separate namespace", async () => {
    resetState();
    const { publishBenchmarkProfileReportsToBlob } =
      await importBenchmarkProfileBlobStore();
    const markdown = Buffer.from("# benchmark profile\n");
    const detailsArchive = Buffer.from([4, 5, 6]);

    const runs = await publishBenchmarkProfileReportsToBlob([
      {
        runId: 404,
        runNumber: 4,
        title: "CI",
        headSha: "benchmark-sha",
        shortSha: "benchmar",
        runUrl: "https://example.test/runs/404",
        createdAt: "2026-06-13T01:00:00.000Z",
        updatedAt: "2026-06-13T01:05:00.000Z",
        artifactId: 4004,
        artifactCreatedAt: "2026-06-13T01:05:00.000Z",
        aggregateJson: JSON.stringify({ kind: "benchmark", total: 3 }),
        markdown,
        detailsArchive,
      },
    ]);

    expect(getCalls).toHaveLength(0);
    expect(putCalls.map((call) => call.pathname)).toEqual([
      "benchmark-profiles/runs/4004/aggregate.json.gz",
      "benchmark-profiles/runs/4004/summary.md",
      "benchmark-profiles/runs/4004/details.tar.gz",
      "benchmark-profiles/daily/2026-06-13.json",
    ]);
    expect(putCalls[0]?.options).toMatchObject({
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });
    expect(putCalls[1]?.options).toMatchObject({
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "text/markdown; charset=utf-8",
    });
    expect(putCalls[2]?.options).toMatchObject({
      allowOverwrite: true,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });
    expect(putCalls[3]?.options).toMatchObject({
      allowOverwrite: true,
      cacheControlMaxAge: 900,
      contentType: "application/json",
    });
    expect(gunzipSync(putCalls[0]?.body as Uint8Array).toString("utf8")).toBe(
      `${JSON.stringify({ kind: "benchmark", total: 3 })}\n`,
    );
    expect(putCalls[1]?.body).toEqual(markdown);
    expect(putCalls[2]?.body).toEqual(detailsArchive);

    const pointer = JSON.parse(String(putCalls[3]?.body));
    expect(pointer.profileReports.aggregate.path).toBe(
      "benchmark-profiles/runs/4004/aggregate.json.gz",
    );
    expect(pointer.profileReports.markdown.path).toBe(
      "benchmark-profiles/runs/4004/summary.md",
    );
    expect(pointer.profileReports.detailsArchive.path).toBe(
      "benchmark-profiles/runs/4004/details.tar.gz",
    );
    expect(runs.map((run) => run.artifactId)).toEqual([4004]);
  });

  test("lists daily run blobs and ignores malformed daily JSON", async () => {
    resetState();
    const { listTest262BlobDailyRuns } = await importBlobStore();

    const runs = await listTest262BlobDailyRuns();

    expect(listCalls).toHaveLength(2);
    expect(listCalls[0]?.options).toMatchObject({
      limit: 1000,
      prefix: "test262/daily/",
    });
    expect(getCalls.map((call) => call.pathname)).toEqual([
      "test262/daily/bad.json",
      "test262/daily/invalid-timestamp.json",
      "test262/daily/invalid-size.json",
      "test262/daily/corrupt.json",
      "test262/daily/2026-06-10.json",
    ]);
    expect(runs).toEqual([dailyRun]);
  });

  test("reads and decompresses a run report by artifact id", async () => {
    resetState();
    const { readTest262BlobReportJsonByArtifactId } = await importBlobStore();

    const json = await readTest262BlobReportJsonByArtifactId(1001);

    expect(json).toBe(reportJson);
    expect(getCalls).toEqual([
      {
        pathname: "test262/runs/1001.json.gz",
        options: { access: "public" },
      },
    ]);
  });
});
