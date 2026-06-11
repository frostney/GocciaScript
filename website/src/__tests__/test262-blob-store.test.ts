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

const MANIFEST_PATH = "test262/manifest.json";
const MANIFEST_ETAG = '"origin-etag"';

const getCalls: BlobGetCall[] = [];
const putCalls: BlobPutCall[] = [];
const state = {
  manifestExists: true,
  failManifestPuts: 0,
};

class BlobNotFoundError extends Error {
  constructor() {
    super("The requested blob does not exist");
  }
}

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
  BlobNotFoundError,
  BlobPreconditionFailedError,
  get: async (pathname: string, options: Record<string, unknown>) => {
    getCalls.push({ pathname, options });
    if (options.access === "public" && options.useCache === false) {
      // The real public CDN rejects the SDK's cache=0 parameter.
      throw new Error("Vercel Blob: Failed to fetch blob: 400 Bad Request");
    }
    // The real SDK signals a missing blob by returning null, not throwing.
    if (pathname === MANIFEST_PATH && !state.manifestExists) return null;
    return {
      statusCode: 200,
      stream: textStream(JSON.stringify({ version: 1, runs: [existingRun] })),
      blob: {
        etag: MANIFEST_ETAG,
      },
    };
  },
  put: async (
    pathname: string,
    body: unknown,
    options: Record<string, unknown>,
  ) => {
    putCalls.push({ pathname, body, options });
    if (pathname === MANIFEST_PATH) {
      if (state.failManifestPuts > 0) {
        state.failManifestPuts -= 1;
        throw new BlobPreconditionFailedError();
      }
      const expectedIfMatch = state.manifestExists ? MANIFEST_ETAG : undefined;
      if (options.ifMatch !== expectedIfMatch) {
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

function resetState() {
  getCalls.length = 0;
  putCalls.length = 0;
  state.manifestExists = true;
  state.failManifestPuts = 0;
}

function manifestGets() {
  return getCalls.filter((call) => call.pathname === MANIFEST_PATH);
}

function manifestPuts() {
  return putCalls.filter((call) => call.pathname === MANIFEST_PATH);
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

async function importPublish() {
  const { publishTest262ReportsToBlob } = await import(
    "../../scripts/test262-blob-store"
  );
  return publishTest262ReportsToBlob;
}

describe("test262 Blob publishing", () => {
  test("merges into the existing manifest with its ETag as ifMatch", async () => {
    resetState();
    const publish = await importPublish();

    const manifest = await publish([publishEntry()]);

    const [manifestRead] = manifestGets();
    expect(manifestRead?.options).toEqual({ access: "public" });
    expect(manifestPuts().at(-1)?.options).toMatchObject({
      allowOverwrite: true,
      ifMatch: MANIFEST_ETAG,
    });
    expect(manifest.runs.map((run) => run.artifactId)).toEqual([1001, 1002]);
  });

  test("creates the manifest without ifMatch when none exists", async () => {
    resetState();
    state.manifestExists = false;
    const publish = await importPublish();

    const manifest = await publish([publishEntry()]);

    const manifestWrite = manifestPuts().at(-1);
    expect(manifestWrite?.options).toMatchObject({ allowOverwrite: false });
    expect(manifestWrite?.options.ifMatch).toBeUndefined();
    expect(manifest.runs.map((run) => run.artifactId)).toEqual([1002]);
  });

  test("re-reads the manifest and retries when the conditional write conflicts", async () => {
    resetState();
    state.failManifestPuts = 1;
    const publish = await importPublish();

    const manifest = await publish([publishEntry()]);

    expect(manifestGets()).toHaveLength(2);
    expect(manifestPuts()).toHaveLength(2);
    expect(manifest.runs.map((run) => run.artifactId)).toEqual([1001, 1002]);
  });

  test("gives up after repeated write conflicts", async () => {
    resetState();
    state.failManifestPuts = Number.POSITIVE_INFINITY;
    const publish = await importPublish();

    await expect(publish([publishEntry()])).rejects.toThrow(
      /precondition failed/i,
    );
    expect(manifestGets()).toHaveLength(3);
    expect(manifestPuts()).toHaveLength(3);
  });
});
