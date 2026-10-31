# 0170 - fetch reuses the connection within an origin and shares one OpenSSL context

**Date:** 2026-10-31
**Area:** `runtime`, `embedding`
**Related:** [ADR 0146](0146-embedder-fetch-transport.md), [ADR 0156](0156-fetch-honours-the-redirect-mode.md), [#1991](https://github.com/frostney/GocciaScript/issues/1991)

## Context

The built-in HTTP client opened a new connection, and on HTTPS a new TLS
session, for every request, each redirect hop included, and sent
`Connection: close` on all of them. On Linux each connection also built its
own OpenSSL `SSL_CTX` and parsed the whole default CA bundle into it, about
30 ms of CPU per HTTPS request. The client socket never set `TCP_NODELAY`, so
on TLS 1.3 the request written after the handshake waited for a delayed ACK.

## Decision

**A redirect within the origin rides the connection that answered it,** when
that response ended cleanly: HTTP/1.1 without `Connection: close`, and a body
framed by `Content-Length` with nothing after it, by `chunked` (the final
coding of a single `Transfer-Encoding` field, no `Content-Length` beside it)
read to its end with nothing after it, or no body. Every hop still runs the
allow-list and host checks; a reused hop checks the address the connection
was pinned to and does no lookup. Credential stripping is unchanged, and no
connection outlives the request.

**A hop that finds a reused connection dead is made once more on a new one:**
a failed send, a read that fails or ends before the first byte, or a first
line that is no HTTP/1.x status line, checked before any body is read. A
response that did arrive is never requested twice. Only `GET` and `HEAD` reach
the client, so the repeated request is idempotent.

**`Connection: close` is sent only when no reuse can follow:** redirect mode
`"manual"` or `"error"`, or the request that reaches the redirect limit.

**On OpenSSL the process shares one client `SSL_CTX`,** created on first use
under the existing load lock, with `SSL_new` under the same lock taking its
own reference. It holds configuration only, with session caching off. It is
built again when `SSL_CERT_FILE` or `SSL_CERT_DIR` change, read through libc
`getenv` because the RTL's environment copy does not see `setenv`.

**The Unix client socket sets `TCP_NODELAY`.**

## Consequences

- A 20-hop same-origin redirect chain over local HTTPS went from about 1.7 s
  to about 15 ms, measured before the change on Linux x86_64 (#1991).
- Stray bytes a server sends after a response, if they form a complete valid
  response, are taken as the next hop's answer, as in any HTTP/1.1
  keep-alive client; reuse stays within one origin, so they come from the same
  server.
- In `"follow"` mode the client closes first and holds each connection's
  TIME-WAIT, which bounds new connections to one destination at roughly 470
  per second; `"manual"` and `"error"` leave that to the server as before.
- A server that answers a request without a length and keeps the connection
  open now holds the fetch until its timeout, since the request no longer
  asks it to close.
- The CA file is read once per process for each value of the two variables,
  so a file replaced at the same path is seen after a restart.
- Reuse across separate `fetch()` calls and TLS session resumption for new
  connections are left for later.
