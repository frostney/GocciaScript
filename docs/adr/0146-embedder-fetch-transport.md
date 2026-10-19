# 0146 - An embedder can answer an engine's fetch requests through its own transport

**Date:** 2026-10-18
**Area:** `runtime`, `embedding`
**Related:** [ADR 0031](0031-fetch-only-async-io.md), [ADR 0122](0122-unified-capability-model.md), [ADR 0142](0142-the-run-waits-until-no-extension-has-pending-work.md)

## Context

`fetch()` always went to the network. Each thread's fetch manager started a
worker thread per request, which ran the built-in HTTP client, and nothing
between `fetch()` and that client could be replaced. A host that wants to
record the responses a run received and later run the same script again
against those recordings, without the network, had no seam to do it: it
could only stand up a proxy and point the script at it, which changes the
URLs the script sees and still needs a socket.

The same seam serves hosts that route requests through their own client or
policy, and test hosts that want deterministic answers.

## Decision

**An engine carries an optional transport, `TGocciaEngine.FetchTransport`
(`IGocciaFetchTransport`, unit `Goccia.FetchTransport`).** With none, the
default, nothing changes. With one, each request the engine's `fetch()`
starts is handed to the transport instead of a worker thread.

**The transport travels with the request, as the capability set does.** The
fetch builtin reads it from its engine when a request starts and puts it in
the request's `TGocciaFetchPolicy`, beside the capabilities and the response
ceiling. The thread's manager is shared by every engine on the thread, so it
holds no transport of its own; engines nested on one thread and engines on
other threads each use their own. Children a host creates, such as a sandbox
`runScript` child, get no transport unless the host gives them one.

**Everything `fetch()` decides before network work still happens first.** The
URL is parsed, the host is checked against the `net` capability by name and
audited, the method must be `GET` or `HEAD`, an aborted signal rejects at
once, and the per-thread cap of 16 requests in flight applies. A request the
built-in client would refuse to send, for an unsupported scheme or a control
character in the request target or a header, fails with that client's
`TypeError` through the promise and never reaches the transport. The transport
receives the method, the URL, the header list, an empty body (no supported
method has one), the time left before the run's deadline or the signal's
timeout, the resolved response ceiling, and the capability set.

**The answer goes through a responder the engine owns
(`IGocciaFetchResponder`).** `Respond` gives the final response, `Fail` a
network error (`TypeError`) or, with a denied scope, a refusal by the
capability (`PermissionDenied`), as the built-in client reports them. Either
may be called inside `Fetch` or later from any thread; the first answer wins.
The answer becomes a completion on the manager's queue, the same queue a
worker posts to, and the promise settles at the engine's next completion
pump on its own thread. So `fetch()` never settles inside the call, and the
request stays in the engine's pending set until it is answered: the run waits
for it as for a network request, until the run deadline or the request's
signal ends the wait. As for a network request, the wait runs no script and
consumes no instruction budget, so an instruction limit stops it only when
the budget was already exhausted.

**The engine holds a transport's response to the rules a network response
meets.** WHATWG Fetch gives a response a status from 200 to 599, status text
that is a reason phrase, header names that are tokens and values without NUL,
CR or LF, and a null body for `HEAD` and for a null body status. Header
values and status text are byte sequences, so a character past U+00FF is
refused, and header values are normalized. A status, status text or header
that breaks those rules, a final URL that is not an absolute http(s) URL, or
a body the response keeps that is over the response ceiling, rejects the
fetch with `TypeError`, the way a malformed or oversized network response
does.

**`Response.url` reads as the built-in client reports it.** The final URL a
transport gives is exposed as given, and the request URL as `fetch()`
received it when it gives none, as the built-in client exposes a redirect's
`Location` and the request URL. WHATWG Fetch serializes the URL without its
fragment; the built-in client does not yet
([#1786](https://github.com/frostney/GocciaScript/issues/1786)), and the
transport path follows it, so a script reads the same `url` with or without
a transport and a recorded run replays to the same value. Fixing both paths
together is that issue's work.

**A request leaves the pending set the same way whatever answers it.**
Settling, an abort and a discard (the engine's run failed or it ended) remove
it and release its slot in the cap; a later answer is refused. An abort or a
discard also cancels the responder, even when an answer was already taken
but not yet settled: `IsCancelled` turns true, so a recording host can tell
an answer the script never saw. A request that settled is not cancelled.
The responder keeps the completion queue alive by reference, so an answer
that arrives after its engine is gone is dropped, not delivered to freed
state. A transport that raises from `Fetch` cancels its request and the fetch
rejects with `TypeError`, as when a worker could not start; the run's own
timeout, instruction-limit, memory-limit and engine-integrity errors still
propagate and end the run, as from any `fetch()` call.

**`FetchOverNetwork` performs a request as the built-in client would.** A
recording transport forwards to it from a thread of its own, wraps the
responder to record what it answers, and so applies the same redirect-hop and
resolved-address checks, ceiling and timeout as a run without a transport.
The worker uses the same routine. What it answers then meets the response
rules above; a conforming server's response passes them unchanged, so a
forwarded response reads as a direct one does. Only what a non-conforming
server sends, such as a status outside 200 to 599, is refused where the
built-in client would pass it on.

## Considered options

- **A global or per-thread transport.** Rejected: engines on one thread may
  belong to different runs, and a pooled worker thread runs many. The
  transport is run state, so it lives with the engine, as the capability set
  does.
- **A synchronous callback returning the response.** Simpler for replay, but
  a recording host must then block the engine thread on real I/O, and the
  run could not apply its signal or deadline while it waited. The responder
  serves both.
- **Settling the promise inside the call when the transport answers
  synchronously.** Rejected: WHATWG Fetch never settles a fetch inside the
  `fetch()` call, and a script would observe a different order with and
  without a transport.
- **A transport that may decline a request back to the network.** Rejected:
  a run would then depend on which requests a host forgot to answer.
  Forwarding through `FetchOverNetwork` makes it explicit.
- **Serializing a transport's final URL to the WHATWG form while the
  built-in client keeps reporting the raw string.** Rejected: `url` would
  then depend on whether a transport is installed, and a run recorded
  through the network would read differently from the same run without a
  transport. Both paths change together under #1786.
- **Releasing a transport request's slot when it is answered.** Rejected: an
  answer given inside `Fetch` would free the slot before the script could
  observe it, so a replayed run could pass the cap where its recorded run
  was refused. The slot is released when the request leaves the pending set,
  on the engine's thread.

## Consequences

- A transport request records no redirect-hop or address decisions in the
  capability audit; its trail is the dispatch and the name check `fetch()`
  makes before the hand-over. A transport that does its own I/O is
  responsible for those checks; `FetchOverNetwork` makes them but does not
  audit them.
- An abort cannot interrupt a transport's work; it sees the cancellation
  through `IsCancelled`.
- A transport's request URL is the string `fetch()` received, unnormalized.
  A host that keys recordings by it chooses its own normalization.
- The WASM lane still has no fetch manager, so `fetch()` stays unavailable
  there with or without a transport, and `FetchOverNetwork` answers with a
  network error.
