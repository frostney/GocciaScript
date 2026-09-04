import { createHash } from "node:crypto";
import { gunzipSync, gzipSync } from "node:zlib";
import { BlobNotFoundError, get, list, put } from "@vercel/blob";

const READ_CONCURRENCY = 8;
const SNAPSHOT_POINTERS = 128;
const SNAPSHOT_BYTES = 1024 * 1024;
// Bump when projection semantics change, including which raw records qualify.
const SNAPSHOT_VERSION = 1;

type Access = "public" | "private";
type RunOrder = { createdAt: string; runNumber: number };
type Pointer = { pathname: string; etag?: string; order: number };
type LoadedPointer<Run> = {
  pathname: string;
  etag: string | null;
  run: Run | null;
};
type Partition = {
  path: string;
  fingerprint: string;
  pointers: { pathname: string; etag: string; order: number }[];
};

function strongEtag(etag: unknown): string | null {
  if (typeof etag !== "string" || !etag || etag.startsWith("W/")) return null;
  return etag.startsWith('"') && etag.endsWith('"')
    ? etag.slice(1, -1) || null
    : etag;
}

function pointerMonth(prefix: string, pathname: string): string | null {
  if (!pathname.startsWith(`${prefix}/daily/`)) return null;
  const relative = pathname.slice(`${prefix}/daily/`.length);
  return (
    /^(\d{4}-(?:0[1-9]|1[0-2]))-\d{2}(?:\.json$|\/)/.exec(relative)?.[1] ?? null
  );
}

async function readInBatches<Input, Output>(
  items: readonly Input[],
  read: (item: Input) => Promise<Output>,
): Promise<Output[]> {
  const results: Output[] = [];
  for (let offset = 0; offset < items.length; offset += READ_CONCURRENCY) {
    results.push(
      ...(await Promise.all(
        items.slice(offset, offset + READ_CONCURRENCY).map(read),
      )),
    );
  }
  return results;
}

async function listPointers(prefix: string): Promise<Pointer[]> {
  const pointers: Pointer[] = [];
  let cursor: string | undefined;
  do {
    const page = await list({ cursor, limit: 1000, prefix });
    for (const blob of page.blobs)
      pointers.push({
        pathname: blob.pathname,
        etag: blob.etag,
        order: pointers.length,
      });
    cursor = page.cursor;
    if (!page.hasMore) break;
  } while (cursor);
  return pointers;
}

function partitionPointers(prefix: string, pointers: Pointer[]) {
  const months = new Map<
    string,
    { pathname: string; etag: string; order: number }[]
  >();
  const uncached: Pointer[] = [];
  for (const pointer of pointers) {
    const month = pointerMonth(prefix, pointer.pathname);
    const etag = strongEtag(pointer.etag);
    if (!month || !etag) {
      uncached.push(pointer);
      continue;
    }
    const entries = months.get(month) ?? [];
    entries.push({ pathname: pointer.pathname, etag, order: pointer.order });
    months.set(month, entries);
  }
  const partitions: Partition[] = [];
  for (const [month, entries] of months) {
    entries.sort((a, b) =>
      a.pathname < b.pathname
        ? -1
        : a.pathname > b.pathname
          ? 1
          : a.etag < b.etag
            ? -1
            : a.etag > b.etag
              ? 1
              : 0,
    );
    for (let offset = 0; offset < entries.length; offset += SNAPSHOT_POINTERS) {
      const pointers = entries.slice(offset, offset + SNAPSHOT_POINTERS);
      const fingerprint = createHash("sha256")
        .update(
          JSON.stringify(
            pointers.map(({ pathname, etag }) => [pathname, etag]),
          ),
        )
        .digest("hex");
      partitions.push({
        path: `${prefix}/history/v${SNAPSHOT_VERSION}/${month}/${fingerprint}.json.gz`,
        fingerprint,
        pointers,
      });
    }
  }
  return { partitions, uncached };
}

async function readPointer<Run>(
  pointer: Pointer,
  access: Access,
  isRun: (value: unknown) => value is Run,
): Promise<LoadedPointer<Run>> {
  try {
    const response = await get(pointer.pathname, { access });
    if (!response || response.statusCode !== 200 || !response.stream)
      return { pathname: pointer.pathname, etag: null, run: null };
    const text = await new Response(response.stream).text();
    let run: Run | null = null;
    try {
      const value: unknown = JSON.parse(text);
      if (isRun(value)) run = value;
    } catch {
      // Malformed historical pointers have never been part of the timeline.
    }
    return {
      pathname: pointer.pathname,
      etag: strongEtag(response.blob.etag),
      run,
    };
  } catch (error) {
    if (error instanceof BlobNotFoundError)
      return { pathname: pointer.pathname, etag: null, run: null };
    throw error;
  }
}

async function readSnapshot<Run>(
  partition: Partition,
  access: Access,
  isRun: (value: unknown) => value is Run,
): Promise<(Run | null)[] | null> {
  try {
    const response = await get(partition.path, { access });
    if (!response || response.statusCode !== 200 || !response.stream)
      return null;
    if (response.blob.size > SNAPSHOT_BYTES) {
      await response.stream.cancel();
      return null;
    }
    const bytes = await new Response(response.stream).arrayBuffer();
    if (bytes.byteLength > SNAPSHOT_BYTES) return null;
    const snapshot = JSON.parse(
      gunzipSync(bytes, { maxOutputLength: SNAPSHOT_BYTES }).toString("utf8"),
    );
    if (
      snapshot?.version !== SNAPSHOT_VERSION ||
      snapshot.fingerprint !== partition.fingerprint ||
      !Array.isArray(snapshot.pointers) ||
      snapshot.pointers.length !== partition.pointers.length
    )
      return null;
    const runs: (Run | null)[] = [];
    for (const [index, expected] of partition.pointers.entries()) {
      const pointer = snapshot.pointers[index];
      if (
        pointer?.pathname !== expected.pathname ||
        pointer.etag !== expected.etag
      )
        return null;
      if (pointer.run !== null && !isRun(pointer.run)) return null;
      runs.push(pointer.run);
    }
    return runs;
  } catch {
    // Snapshots are optional projections; the raw pointers remain authoritative.
    return null;
  }
}

export async function listBlobHistory<Run extends RunOrder>(
  prefix: string,
  access: Access,
  isRun: (value: unknown) => value is Run,
): Promise<Run[]> {
  const pointers = await listPointers(`${prefix}/daily/`);
  const { partitions, uncached } = partitionPointers(prefix, pointers);
  const snapshots = await readInBatches(partitions, (partition) =>
    readSnapshot(partition, access, isRun),
  );
  const runs: (Run | null)[] = Array(pointers.length).fill(null);
  const fallback = [...uncached];
  for (const [index, snapshot] of snapshots.entries()) {
    if (snapshot) {
      for (const [row, run] of snapshot.entries())
        runs[partitions[index].pointers[row].order] = run;
    } else fallback.push(...partitions[index].pointers);
  }
  const raw = await readInBatches(fallback, (pointer) =>
    readPointer(pointer, access, isRun),
  );
  for (const [index, pointer] of raw.entries())
    runs[fallback[index].order] = pointer.run;
  return runs
    .filter((run): run is Run => run !== null)
    .sort(
      (a, b) =>
        Date.parse(a.createdAt) - Date.parse(b.createdAt) ||
        a.runNumber - b.runNumber,
    );
}

async function writeSnapshots<Run>(
  partitions: Partition[],
  access: Access,
  isRun: (value: unknown) => value is Run,
  known = new Map<string, LoadedPointer<Run>>(),
  projectRun: (run: Run) => Run = (run) => run,
): Promise<number> {
  let written = 0;
  for (const partition of partitions) {
    if (await readSnapshot(partition, access, isRun)) continue;
    const pointers = await readInBatches(
      partition.pointers,
      async (pointer) => {
        const current = known.get(pointer.pathname);
        return current?.etag === pointer.etag
          ? current
          : readPointer(pointer, access, isRun);
      },
    );
    // Public Blob reads may still serve a previous pointer through the CDN.
    // Only materialize the generation whose exact ETags were listed.
    if (
      pointers.some(
        (pointer, index) => pointer.etag !== partition.pointers[index].etag,
      )
    )
      continue;
    const json = JSON.stringify({
      version: SNAPSHOT_VERSION,
      fingerprint: partition.fingerprint,
      pointers: pointers.map(({ pathname, etag, run }) => ({
        pathname,
        etag,
        run: run === null ? null : projectRun(run),
      })),
    });
    if (Buffer.byteLength(json) > SNAPSHOT_BYTES) continue;
    const compressed = gzipSync(json);
    if (compressed.byteLength > SNAPSHOT_BYTES) continue;
    // Equal fingerprints describe equal raw generations, so simultaneous
    // writers produce the same content at this immutable pathname.
    await put(partition.path, compressed, {
      access,
      allowOverwrite: true,
      addRandomSuffix: false,
      cacheControlMaxAge: 31_536_000,
      contentType: "application/gzip",
    });
    written++;
  }
  return written;
}

export async function rebuildBlobHistorySnapshots<Run>(
  prefix: string,
  access: Access,
  isRun: (value: unknown) => value is Run,
  projectRun?: (run: Run) => Run,
): Promise<number> {
  const { partitions } = partitionPointers(
    prefix,
    await listPointers(`${prefix}/daily/`),
  );
  return writeSnapshots(partitions, access, isRun, undefined, projectRun);
}

export async function publishBlobHistorySnapshots<Run>(
  prefix: string,
  access: Access,
  isRun: (value: unknown) => value is Run,
  published: LoadedPointer<Run>[],
  projectRun?: (run: Run) => Run,
): Promise<void> {
  try {
    const known = new Map(
      published.map((pointer) => [
        pointer.pathname,
        { ...pointer, etag: strongEtag(pointer.etag) },
      ]),
    );
    const months = new Set(
      published.map((pointer) => pointerMonth(prefix, pointer.pathname)),
    );
    for (const month of months) {
      if (!month) continue;
      const { partitions } = partitionPointers(
        prefix,
        await listPointers(`${prefix}/daily/${month}-`),
      );
      await writeSnapshots(partitions, access, isRun, known, projectRun);
    }
  } catch (error) {
    console.warn(
      "Blob history snapshot publication failed; raw reports remain available:",
      error,
    );
  }
}
