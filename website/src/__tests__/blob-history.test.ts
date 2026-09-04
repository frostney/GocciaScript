import { afterAll, beforeEach, describe, expect, mock, test } from "bun:test";
import { createHash } from "node:crypto";
import { gunzipSync, gzipSync } from "node:zlib";

type Kind = "awfy" | "jetstream" | "test262";
type StoredBlob = { body: Buffer; etag: string };
const blobs = new Map<string, StoredBlob>();
const stale = new Map<string, StoredBlob>();
const gets: string[] = [];
const writes: string[] = [];
let active = 0;
let maximum = 0;
let pageSize = 1000;
let omitEtags = false;
let weakEtags = false;
let changedGeneration = false;
const accesses: string[] = [];
let rawFailure: string | null = null;
let snapshotFailure = false;
let beforeSnapshotWrite: (() => Promise<void>) | null = null;
const environment = { ...process.env };
const envNames = [
  "AWFY_BLOB_PREFIX",
  "JETSTREAM_BLOB_PREFIX",
  "TEST262_BLOB_PREFIX",
  "AWFY_BLOB_ACCESS",
  "JETSTREAM_BLOB_ACCESS",
  "TEST262_BLOB_ACCESS",
];

class BlobNotFoundError extends Error {}
function store(pathname: string, body: string | Buffer) {
  const bytes = Buffer.from(body);
  blobs.set(pathname, {
    body: bytes,
    etag: createHash("sha256").update(bytes).digest("hex"),
  });
}

mock.module("@vercel/blob", () => ({
  BlobNotFoundError,
  list: async ({ prefix, cursor }: { prefix: string; cursor?: string }) => {
    const entries = [...blobs.entries()]
      .filter(([pathname]) => pathname.startsWith(prefix))
      .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
    const offset = Number(cursor ?? 0);
    return {
      blobs: entries
        .slice(offset, offset + pageSize)
        .map(([pathname, value]) => ({
          pathname,
          etag: omitEtags
            ? undefined
            : weakEtags
              ? `W/"${value.etag}"`
              : value.etag,
        })),
      cursor: String(offset + pageSize),
      hasMore: offset + pageSize < entries.length,
    };
  },
  get: async (pathname: string, options: { access: string }) => {
    accesses.push(options.access);
    gets.push(pathname);
    active++;
    maximum = Math.max(maximum, active);
    try {
      await new Promise((resolve) => setTimeout(resolve, gets.length % 3));
      if (
        pathname === rawFailure ||
        (snapshotFailure && pathname.includes("/history/"))
      )
        throw new Error("transport failed");
      const blob = stale.get(pathname) ?? blobs.get(pathname);
      if (!blob) throw new BlobNotFoundError();
      return {
        statusCode: 200,
        stream: new Response(new Uint8Array(blob.body)).body,
        blob: { etag: `"${blob.etag}"`, size: blob.body.byteLength },
      };
    } finally {
      active--;
    }
  },
  put: async (
    pathname: string,
    body: string | Buffer,
    options: { access: string },
  ) => {
    accesses.push(options.access);
    if (pathname.includes("/history/")) {
      if (snapshotFailure) throw new Error("projection upload failed");
      await beforeSnapshotWrite?.();
      const previous = blobs.get(pathname);
      if (previous && !previous.body.equals(Buffer.from(body))) {
        changedGeneration = true;
        throw new Error("generation content changed");
      }
    }
    writes.push(pathname);
    store(pathname, body);
    return {
      pathname,
      url: `https://blob.test/${pathname}`,
      downloadUrl: `https://blob.test/${pathname}`,
      etag: blobs.get(pathname)?.etag,
    };
  },
}));

beforeEach(() => {
  blobs.clear();
  stale.clear();
  gets.length = 0;
  writes.length = 0;
  active = 0;
  maximum = 0;
  pageSize = 1000;
  omitEtags = false;
  weakEtags = false;
  changedGeneration = false;
  accesses.length = 0;
  rawFailure = null;
  snapshotFailure = false;
  beforeSnapshotWrite = null;
  for (const name of envNames) delete process.env[name];
});
afterAll(() => {
  for (const name of envNames) {
    if (environment[name] === undefined) delete process.env[name];
    else process.env[name] = environment[name];
  }
});

function metadata(
  id: number,
  createdAt = `2026-09-01T00:${String(id % 60).padStart(2, "0")}:00.000Z`,
) {
  return {
    runId: id,
    runNumber: id,
    artifactId: id,
    title: "CI",
    headSha: "sha",
    shortSha: "sha",
    runUrl: `https://example.test/${id}`,
    createdAt,
    updatedAt: createdAt,
    artifactCreatedAt: createdAt,
  };
}
const workloadSummary = {
  workloadCount: 1,
  failedWorkloadCount: 1,
  repetitions: 1,
  referenceRatios: { quickjs: null, node: null },
  engineVersions: {},
  corpusCommit: "corpus",
  driverVersion: 1,
  targetNames: ["failed"],
};
const conformanceSummary = {
  totalDiscovered: 1,
  totalRun: 1,
  passed: 0,
  failed: 1,
  wrapperInfraFailures: 0,
  timeouts: 0,
  durationSeconds: 1,
  byCategory: [],
};
function rawRun(kind: Kind, id: number, createdAt?: string) {
  const meta = metadata(id, createdAt);
  return {
    ...meta,
    summary: kind === "test262" ? conformanceSummary : workloadSummary,
    report: {
      path: `${kind}/runs/${id}/report.json.gz`,
      url: "https://blob.test/report",
      downloadUrl: "https://blob.test/report",
      size: 10,
    },
    reportPath: `${kind}/runs/${id}.json.gz`,
    reportUrl: "https://blob.test/report",
    reportDownloadUrl: "https://blob.test/report",
    reportCompressedSize: 10,
    publishedAt: meta.updatedAt,
    jsonUrl: `/api/test262/results/${id}`,
  };
}
function seed(
  kind: Kind,
  id: number,
  pathname = `${kind}/daily/2026-09-01/${id}.json`,
) {
  store(pathname, JSON.stringify(rawRun(kind, id)));
}
async function load(kind: Kind) {
  if (kind === "awfy")
    return (await import("@/lib/awfy-blob-store")).listAwfyBlobDailyRuns();
  if (kind === "jetstream")
    return (
      await import("@/lib/jetstream-blob-store")
    ).listJetStreamBlobDailyRuns();
  return (await import("@/lib/test262-blob-store")).listTest262BlobDailyRuns();
}
async function publish(kind: Kind, id: number, createdAt?: string) {
  const meta = metadata(id, createdAt);
  if (kind === "test262") {
    const report = { summary: conformanceSummary, results: [] };
    return (
      await import("@/lib/test262-blob-store")
    ).publishTest262ReportsToBlob([
      {
        point: {
          ...meta,
          summary: report.summary,
          jsonUrl: `/api/test262/results/${id}`,
        },
        report,
        reportJson: JSON.stringify(report),
      },
    ]);
  }
  const entry = {
    ...meta,
    summary: workloadSummary,
    reportJson: '{"targets":[]}',
  };
  if (kind === "jetstream")
    return (
      await import("@/lib/jetstream-blob-store")
    ).publishJetStreamReportsToBlob([entry]);
  return (await import("@/lib/awfy-blob-store")).publishAwfyReportsToBlob([
    {
      ...entry,
      summary: {
        ...workloadSummary,
        targetCount: 1,
        awfyCount: 1,
        probeCount: 0,
      },
    },
  ]);
}
async function rebuild(kind: Kind) {
  if (kind === "awfy")
    return (await import("@/lib/awfy-blob-store")).rebuildAwfyBlobHistory();
  if (kind === "jetstream")
    return (
      await import("@/lib/jetstream-blob-store")
    ).rebuildJetStreamBlobHistory();
  return (await import("@/lib/test262-blob-store")).rebuildTest262BlobHistory();
}
function snapshots() {
  return [...blobs.keys()].filter((path) => path.includes("/history/"));
}
function rawGets() {
  return gets.filter((path) => path.includes("/daily/"));
}

for (const kind of ["awfy", "jetstream", "test262"] as const) {
  describe(`${kind} history`, () => {
    test("bounds fallback reads across pages and preserves every degraded run", async () => {
      pageSize = 11;
      omitEtags = true;
      for (let id = 1; id <= 35; id++) seed(kind, id);
      store(`${kind}/daily/corrupt.json`, "{");
      const runs = await load(kind);
      expect(runs.map((run) => run.runId)).toEqual(
        Array.from({ length: 35 }, (_, index) => index + 1),
      );
      expect(
        runs.every((run) =>
          "failedWorkloadCount" in run.summary
            ? run.summary.failedWorkloadCount === 1
            : run.summary.failed === 1,
        ),
      ).toBe(true);
      expect(maximum).toBe(8);
      expect(rawGets()).toHaveLength(36);
    });

    test("serves an unchanged month from one immutable snapshot", async () => {
      for (let id = 1; id <= 24; id++) seed(kind, id);
      await publish(kind, 25);
      expect(snapshots()).toHaveLength(1);
      const path = snapshots()[0];
      expect(path).toMatch(
        new RegExp(`^${kind}/history/v1/2026-09/[a-f0-9]{64}\\.json\\.gz$`),
      );
      gets.length = 0;
      expect(await load(kind)).toHaveLength(25);
      expect(gets).toEqual([path]);
    });

    test("falls back for missing, corrupt, and mismatched snapshots", async () => {
      seed(kind, 1);
      await publish(kind, 2);
      const path = snapshots()[0];
      const original = blobs.get(path);
      if (!original) throw new Error("expected snapshot");
      const payload = JSON.parse(gunzipSync(original.body).toString());
      for (const body of [
        null,
        Buffer.from("corrupt"),
        gzipSync(JSON.stringify({ ...payload, fingerprint: "wrong" })),
        gzipSync(JSON.stringify({ ...payload, pointers: [] })),
        gzipSync(
          JSON.stringify({
            ...payload,
            pointers: payload.pointers.map((pointer: object) => ({
              ...pointer,
              run: {},
            })),
          }),
        ),
        gzipSync(Buffer.alloc(1024 * 1024 + 1)),
      ]) {
        if (body) store(path, body);
        else blobs.delete(path);
        gets.length = 0;
        expect((await load(kind)).map((run) => run.runId)).toEqual([1, 2]);
        expect(rawGets()).toHaveLength(2);
      }
    });

    test("rebuilds old months without changing raw data or the access boundary", async () => {
      process.env[`${kind.toUpperCase()}_BLOB_PREFIX`] = "custom";
      process.env[`${kind.toUpperCase()}_BLOB_ACCESS`] = "private";
      seed(kind, 1, "custom/daily/2026-08-01.json");
      seed(kind, 2, "custom/daily/2026-09-01.json");
      const original = [...blobs.entries()];
      expect(await rebuild(kind)).toBe(2);
      expect(await rebuild(kind)).toBe(0);
      expect(writes).toHaveLength(2);
      expect(
        writes.every((path) => path.startsWith("custom/history/v1/")),
      ).toBe(true);
      for (const [path, blob] of original)
        expect(blobs.get(path)).toEqual(blob);
      gets.length = 0;
      expect((await load(kind)).map((run) => run.runId)).toEqual([1, 2]);
      expect(gets).toHaveLength(2);
      expect(accesses.every((access) => access === "private")).toBe(true);
    });

    test("propagates raw transport errors without starting another batch", async () => {
      omitEtags = true;
      for (let id = 1; id <= 20; id++) seed(kind, id);
      rawFailure = `${kind}/daily/2026-09-01/1.json`;
      await expect(load(kind)).rejects.toThrow("transport failed");
      await new Promise((resolve) => setTimeout(resolve, 5));
      expect(rawGets()).toHaveLength(8);
      expect(maximum).toBe(8);
    });
  });
}

test("late concurrent snapshots cannot replace the current generation", async () => {
  let release = () => {};
  let started = () => {};
  const gate = new Promise<void>((resolve) => {
    release = resolve;
  });
  const ready = new Promise<void>((resolve) => {
    started = resolve;
  });
  beforeSnapshotWrite = async () => {
    beforeSnapshotWrite = null;
    started();
    await gate;
  };
  const older = publish("jetstream", 1, "2026-09-02T00:00:00.000Z");
  await ready;
  await publish("jetstream", 2, "2026-09-01T00:00:00.000Z");
  release();
  await older;
  expect(snapshots()).toHaveLength(2);
  gets.length = 0;
  expect((await load("jetstream")).map((run) => run.runId)).toEqual([2, 1]);
  expect(rawGets()).toHaveLength(0);
});

test("does not cache stale public pointer bytes under a new fingerprint", async () => {
  await publish("jetstream", 1);
  const path = "jetstream/daily/2026-09-01/1.json";
  const old = blobs.get(path);
  if (!old) throw new Error("expected raw pointer");
  stale.set(path, old);
  store(path, JSON.stringify(rawRun("jetstream", 10)));
  await publish("jetstream", 2);
  expect(snapshots()).toHaveLength(1);
  stale.clear();
  await publish("jetstream", 3);
  gets.length = 0;
  expect((await load("jetstream")).map((run) => run.runId)).toEqual([2, 3, 10]);
  expect(rawGets()).toHaveLength(0);
});

test("bounds snapshot partitions to 128 pointers", async () => {
  for (let id = 1; id <= 129; id++) seed("jetstream", id);
  await publish("jetstream", 130);
  expect(snapshots()).toHaveLength(2);
  expect(
    snapshots()
      .map(
        (path) =>
          JSON.parse(
            gunzipSync(blobs.get(path)?.body ?? Buffer.alloc(0)).toString(),
          ).pointers.length,
      )
      .sort((a, b) => a - b),
  ).toEqual([2, 128]);
  gets.length = 0;
  expect(await load("jetstream")).toHaveLength(130);
  expect(gets).toHaveLength(2);
});

test("projection failures leave successful raw publication available", async () => {
  snapshotFailure = true;
  const warning = console.warn;
  console.warn = () => {};
  try {
    expect(await publish("jetstream", 1)).toHaveLength(1);
  } finally {
    console.warn = warning;
  }
  expect(blobs.has("jetstream/runs/1/report.json.gz")).toBe(true);
  expect((await load("jetstream")).map((run) => run.runId)).toEqual([1]);
});

test("keeps established daily overwrite and per-run publication shapes", async () => {
  for (const kind of ["awfy", "test262", "jetstream"] as const) {
    await publish(kind, 1);
    await publish(kind, 2);
    expect((await load(kind)).map((run) => run.runId)).toEqual(
      kind === "jetstream" ? [1, 2] : [2],
    );
    expect(
      [...blobs.keys()].filter((path) => path.startsWith(`${kind}/runs/`)),
    ).toHaveLength(2);
  }
});

test("preserves listing order for equal timestamps and run numbers across cached and legacy pointers", async () => {
  for (const [id, pathname] of [
    [3, "jetstream/daily/000-legacy.json"],
    [1, "jetstream/daily/2026-09-01/1.json"],
    [2, "jetstream/daily/2026-09-01/2.json"],
  ] as const) {
    store(
      pathname,
      JSON.stringify({
        ...rawRun("jetstream", id, "2026-09-01T00:00:00Z"),
        runNumber: 1,
      }),
    );
  }
  await rebuild("jetstream");
  gets.length = 0;
  expect((await load("jetstream")).map((run) => run.runId)).toEqual([3, 1, 2]);
  expect(rawGets()).toEqual(["jetstream/daily/000-legacy.json"]);
});

test("does not build or select a snapshot from weak ETags", async () => {
  weakEtags = true;
  seed("jetstream", 1);
  expect(await rebuild("jetstream")).toBe(0);
  expect((await load("jetstream")).map((run) => run.runId)).toEqual([1]);
  expect(gets).toEqual(["jetstream/daily/2026-09-01/1.json"]);
});

test("simultaneous rebuilds of one generation write identical bytes", async () => {
  seed("jetstream", 1);
  let arrivals = 0;
  let release = () => {};
  const gate = new Promise<void>((resolve) => {
    release = resolve;
  });
  beforeSnapshotWrite = async () => {
    if (++arrivals === 2) release();
    await gate;
  };
  const results = await Promise.all([
    rebuild("jetstream"),
    rebuild("jetstream"),
  ]);
  expect(results).toEqual([1, 1]);
  expect(snapshots()).toHaveLength(1);
  expect(changedGeneration).toBe(false);
});

test("oversized generations remain available through raw reads", async () => {
  store(
    "jetstream/daily/2026-09-01/1.json",
    JSON.stringify({
      ...rawRun("jetstream", 1),
      title: "x".repeat(1024 * 1024),
    }),
  );
  expect(await rebuild("jetstream")).toBe(0);
  expect(snapshots()).toHaveLength(0);
  expect((await load("jetstream")).map((run) => run.runId)).toEqual([1]);
});
