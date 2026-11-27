# 0193 - fetch requests past the worker cap wait their turn

**Date:** 2026-11-27
**Area:** `runtime`
**Related:** [ADR 0031](0031-fetch-only-async-io.md), [ADR 0146](0146-embedder-fetch-transport.md)

## Context

Each host thread runs at most 16 fetch requests at once, one worker thread
and socket each. The 17th concurrent call rejected with
`TypeError: fetch worker limit exceeded`, so
`Promise.all(urls.map((u) => fetch(u)))` failed for any list longer than 16
and every script that fetched a list needed its own pool. Browsers and Node
queue requests past their connection limits instead.

## Decision

A request made while all 16 slots are taken waits in a first-in, first-out
queue on its thread and starts when a worker finishes. A waiting request has
no thread or socket, so the cap still bounds both.

A waiting request is the run's work like a running one:

- Its host-side copy of the URL and headers counts against the memory limit
  from the call until it ends, and a request that does not fit rejects with
  the memory `RangeError`. The memory limit bounds the queue, so it has no
  separate depth cap that would fail long lists again.
- Its `AbortSignal.timeout()` deadline runs from the `fetch()` call. A
  request's socket gets the run's and the signal's time left when it starts.
- The run waits for it, and the run's deadline still ends the run.
- An abort, a timed-out test, or an aborted run removes it before it starts.

The cap stays fixed at 16 and gets no flag. With the queue, a script no
longer sees the cap except as throughput, and a script that wants fewer
requests in flight can limit itself. A flag would add an option to every
binary, config file, and the embedding API without a host that needs it.
`--help` and the permissions limits section name the cap and the queue.

## Consequences

Lists of any length can be fetched with `Promise.all`. Throughput is bounded
by 16 requests in flight per thread. An aborted request still keeps its slot
until its socket call returns, because an abort does not interrupt a blocked
socket, so requests aborted against a server that never answers delay the
ones queued behind them until a deadline ends those sockets.
