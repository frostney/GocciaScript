# 0196 - An aborted fetch stops at its next socket poll

**Date:** 2026-11-27
**Area:** `runtime`, `embedding`
**Related:** [ADR 0193](0193-fetch-requests-past-the-worker-cap-wait-their-turn.md), [ADR 0190](0190-fetch-connects-across-address-families.md)

## Context

ADR 0193's queue made a stuck worker visible: an aborted request kept its
slot until its blocked socket call returned. With no deadline, that call has
no timeout, so 16 requests aborted against a server that never answers held
every slot. Every later `fetch()` on the thread then waited for its own
deadline, or for ever. An aborted run, a timed-out test and a freed engine
left the same workers behind, and so broke fetch for the later runs of a
long-lived embedder or the REPL (#2216).

The worker owns its socket and blocks in it, through plain reads, the
connection race or a TLS backend's reads (OpenSSL, SecureTransport,
SChannel). Two other designs could not stop it on every platform. A
`shutdown()` from another thread wakes a blocked receive on Linux and
macOS. On Windows it does not, against a peer that never answers. Closing
the socket from another thread races with the reuse of its handle. A
worker released from its slot without being stopped keeps a thread and a
socket open for every request aborted that way, and the 16-request bound
on threads and sockets then no longer holds.

## Decision

Every request carries a cancellation in its HTTP policy. The engine
cancels it when the request is aborted, dropped by an aborted run or a
timed-out test, or detached by a freed engine. The HTTP client checks it
between its steps: the lookup, the connection, the handshake, the send and
each read. With a cancellation, no socket wait lasts longer than one
50 ms poll before it checks again. The client makes these waits itself,
for plain reads, the connection race and each TLS backend's reads.

A cancelled request closes its sockets and ends with
`HTTP request cancelled`, which the script never sees, because its
promise has already settled. Its worker then finishes and frees its slot,
so the next waiting request starts within one poll. A connection attempt
on its own thread (a request with a deadline) holds its own reference to
the cancellation and ends within the same poll.

## Consequences

An aborted request frees its slot at once, whatever the server does, on
every platform and over HTTP and HTTPS. The 16-request cap still bounds
threads and sockets. A waiting socket wakes 20 times a second while it
blocks, which costs nothing measurable against a network wait.

Two waits are still not cut short. The system resolver (`getaddrinfo`)
cannot be interrupted, so a request cancelled during a lookup ends when the
lookup returns, which the resolver's own timeouts bound. A send blocks only
while the peer has stopped reading and the socket buffers are full, which
takes request headers larger than those buffers.
