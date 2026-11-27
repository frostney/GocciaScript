# 0192 - History reads ask for the strong ETag and project raw pointers

**Date:** 2026-11-26
**Area:** `website`, `report-publishing`
**Related:** [ADR 0115](0115-immutable-blob-history-snapshots.md)

## Context

[ADR 0115](0115-immutable-blob-history-snapshots.md) builds a snapshot only
when every fetched pointer's ETag matches its listing, and treats weak ETags
as unverifiable. `fetch` in Node and Bun asks for compressed bodies by
default, and the Blob CDN answers a compressed response with a weak ETag
(`W/"…"`); only the identity encoding returns the strong ETag the listing
reports. No snapshot was ever written, so every dashboard read fell back to
every raw pointer.

JetStream daily pointers also embedded the whole report, because the
publisher spread the publish entry into each pointer. Snapshots dropped that
payload, but raw reads kept it. The Performance dashboard grew to 44 MB,
past the 2 MB limit of Next's data cache, so every request read every
pointer again and any single failed read showed the error page.

## Decision

Pointer reads send `Accept-Encoding: identity`, so a fetched ETag is the
strong one and can be matched against the listing. The weak-ETag rule of
ADR 0115 stays: a weak ETag still cannot select or build a snapshot.

A raw read applies the same projection a snapshot stores, so a read returns
the same runs with or without its month's snapshot. JetStream's projection
drops the embedded report, and new JetStream pointers no longer embed it;
the report blob stays authoritative. Existing raw pointers are not
rewritten.

Dashboard loaders reject on a failed read instead of returning an error
result from inside `unstable_cache`, which stores nothing for a rejected cold
read and keeps serving its last good entry when a revalidation rejects. The
error result is built outside the cache.

## Consequences

CI publishers and the rebuild command write snapshots again. Months that no
publish touches after this change keep reading raw pointers until the
rebuild command runs. Pointer reads transfer uncompressed bytes, which is
small for the compact pointers published from now on. A transient Blob
failure no longer replaces a cached dashboard with an error page.
