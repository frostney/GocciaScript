# Immutable Blob history snapshots

**Date:** 2026-09-04
**Area:** `website`, `report-publishing`

AWFY, JetStream, and test262 dashboards keep raw daily pointers as their source
of truth and use immutable monthly snapshots to reduce history downloads.
[The shared history helper](../../website/src/lib/blob-history.ts) owns bounded
reads and projections; each Blob store keeps its report schema and validation.

## Context

The previous readers listed every daily pointer and fetched them serially.
That made request latency grow with the number of retained runs. Simply keeping
only recent or successful runs would lose useful history. A mutable summary
index would need coordination between concurrent and out-of-order publishers.

The installed `@vercel/blob` 2.4.0 source exposes ETags in
`ListBlobResultBlob`, PUT results, and GET response metadata. Its `get` API
only bypasses caching for private blobs: `useCache: false` does not make public
pointer reads fresh. Consequently, an index writer must not associate an old
CDN response with a newer listed pointer version.

## Decision

Readers still list all raw pointers under each report's `daily/` prefix. Strong
ETags and pathnames are sorted lexically within each month and divided into
chunks of at most 128 pointers. The SHA-256 digest of each chunk's ordered
`[pathname, etag]` pairs names its snapshot:

```text
<prefix>/history/v1/<YYYY-MM>/<sha256>.json.gz
```

Snapshots contain the fingerprint and the corresponding parsed pointer
records. The JetStream adapter omits the duplicate `reportJson` payload that
legacy daily pointers embed; full reports and raw pointers remain unchanged. Their serialized content is deterministic and bounded to 1 MiB before
compression. Readers check the version, fingerprint, exact descriptor sequence,
and report-specific schemas, and bound gzip expansion. Missing, invalid, or
unreadable snapshots fall back to raw pointers. Unknown historical path shapes,
missing ETags, and weak ETags also use raw reads. Malformed raw pointers remain
excluded as before; transport failures on raw reads still propagate.

Changes to projection semantics, including which raw records qualify, require
a new snapshot version.

Snapshot and raw reads each run in batches of at most eight per report family.
Results preserve original listing order before the existing timestamp and run
number sort, including ties. No retained run is removed for age or failure.
A snapshot hit needs no per-pointer GETs; listing remains linear in raw pointer
count, while downloads become one per valid chunk.

CI publishers write the full reports and existing daily pointers first, then
attempt snapshots only for affected months. They verify every fetched ETag
against its listed descriptor. Just-written pointer bytes can be reused when
the PUT ETag matches the listing. A stale public response leaves that generation
uncached. Snapshot errors cannot turn a successful raw publication into failure.
The explicit [rebuild command](../../website/scripts/rebuild-history-snapshots.ts)
uses the same checks to populate older months and repair unavailable snapshots;
request handling never writes projections.

Distinct generations have distinct paths, so late or concurrent writers cannot
overwrite the current generation with an older one. Simultaneous writes to the
same path describe identical pointer versions and serialize identically. There
is no mutable latest-index pointer or compare-and-swap loop. Existing AWFY and
test262 daily overwrite behavior, JetStream's per-run daily paths, profile
publication, and each store's access settings remain unchanged.

## Consequences

Every raw report and pointer remains available under its existing contract.
Snapshot failure costs bounded fallback reads, with no new availability
requirement. Blob listing and raw reads retain their existing consistency
properties; this projection does not promise a transactional cross-page view.

All snapshot generations are retained. Each chunk is bounded, but total storage
is not: publishing repeatedly within a month retains prior generations, and
out-of-order inserts may change several sorted chunks. In the worst case, bytes
retained across a month's publications grow quadratically with its pointer
count. This is a deliberate storage-for-request-latency tradeoff. There is no
automatic garbage collection or raw-data deletion. Operators can observe growth
under `history/v1/`; any future retention policy needs a separate decision that
accounts for readers using an earlier listing and concurrent publishers.
