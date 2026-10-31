# Fetch Runtime APIs

*The loader runtime's bounded WHATWG fetch and cancellation surface.*

## Executive Summary

- **Network authority stays explicit** — `fetch` supports only `GET` and `HEAD`, and every destination needs the engine's `net` capability; a refusal throws `PermissionDenied`.
- **Cancellation is fetch-scoped** — `AbortController`, `AbortSignal.abort`, and `AbortSignal.timeout` reject associated fetch promises with the signal's exact reason.
- **No general event loop** — timeout state is observed at signal access and fetch-completion pump checkpoints.
- **Real EventTarget base** — `EventTarget` and `Event` are runtime globals, and `AbortSignal` inherits from `EventTarget` for `addEventListener`, `onabort`, and the `abort` event.
- **Focused object model** — `Request`, streaming bodies, CORS, event propagation, and `AbortSignal.any` are not implemented.

## fetch

`fetch(url, options?)` implements a subset of the [WHATWG Fetch Standard](https://fetch.spec.whatwg.org/#fetch-method) and returns `Promise<Response>`.

The `options` object supports:

- `method`: `"GET"` or `"HEAD"` (case-insensitive);
- `headers`: a plain object or `Headers` instance;
- `redirect`: `"follow"` (the default), `"error"`, or `"manual"`; any other value throws `TypeError`;
- `signal`: an `AbortSignal`, `null`, or `undefined`.

### Redirects

A redirect status is 301, 302, 303, 307 or 308; any other status, a 300 or 306 with a `Location` included, is the response. What a redirect does depends on the `redirect` option ([WHATWG Fetch, HTTP fetch](https://fetch.spec.whatwg.org/#http-fetch); [ADR 0156](adr/0156-fetch-honours-the-redirect-mode.md)):

- `"follow"` follows up to 20 redirects. A twenty-first rejects with `TypeError: redirect limit exceeded: more than 20 redirects`. A redirect response without a `Location` is the response. The method stays as it was on every hop, a `HEAD` after a 303 included.
- `"error"` rejects any redirect, one without a `Location` included, with `TypeError: redirect refused: the request's redirect mode is "error"`, as soon as its status line and headers arrive, without reading its body.
- `"manual"` resolves with the redirect response itself: its status, `statusText`, headers (`Location` included) and body, `redirected` false, and `url` the request's URL. A browser answers with an opaque-redirect response (status 0, no headers) here; GocciaScript has no origin model to protect, so it answers as Node.js and Bun do.

`Authorization`, `Cookie` and `Proxy-Authorization` headers the script sets are sent only while the redirects stay on the request's origin (scheme, host and port). The first redirect to another origin drops them for that hop and every later one, even one that returns to the original origin. An HTTPS-to-HTTP redirect changes the scheme, so the credentials never travel without TLS. Other headers are sent on every hop.

A redirect's `Location` is parsed by the URL Standard's parser with the redirecting response's URL as the base (relative, `./`, `../`, absolute-path, query-only and scheme-relative forms), and the URL sent, for the first request and every hop, has its dot segments removed (`https://h/a/./b/../c` is requested as `https://h/a/c`); a `Location` that does not parse is a network error.

A redirect to the same origin is sent over the connection that carried the redirect when that response ended cleanly ([ADR 0170](adr/0170-fetch-reuses-the-connection-within-an-origin.md)): it is HTTP/1.1 without `Connection: close`, and its body is framed by `Content-Length` with nothing after it, by `chunked` read to its end with nothing after it, or it has none (a `HEAD` response, a 204 or a 304). `chunked` counts only as the final coding of a single `Transfer-Encoding` field with no `Content-Length` beside it. The hop's checks run against the address that connection was opened to, without a second lookup. A hop on a reused connection is sent once more, on a new connection, when its request cannot be written, when the connection ends or fails before the first byte of the response, or when what arrives first is not an HTTP/1.x status line (bytes the previous response left behind); a response that did arrive is never requested again. Any other hop opens a connection of its own, and no connection outlives the `fetch` call.

Under `"manual"` and `"error"`, and on the request that reaches the redirect limit, the request asks the server to close the connection. Every other request leaves it open for a redirect, so a server that answers with neither `Content-Length` nor `chunked` and keeps the connection open holds the fetch until its timeout.

### Transport security

HTTPS uses SecureTransport on macOS, SChannel on Windows, and OpenSSL on Linux. On Linux the first HTTPS request loads OpenSSL once for the whole process, so concurrent first requests share that load. If OpenSSL is missing, every HTTPS request rejects with a `TypeError` saying that HTTPS requires OpenSSL.

With OpenSSL the process keeps one client context, so the trust store is read once rather than for every connection. The context carries configuration only: no TLS session is cached in it. A change to `SSL_CERT_FILE` or `SSL_CERT_DIR` makes the next connection read the trust store again; a CA file replaced at the same path is read after a restart.

### Network capability

Every destination needs the engine's `net` capability; see
[Permissions](permissions.md#net-scopes-and-private-ranges) for the scopes and
how they combine. A refused request throws a `PermissionDenied` whose message
names the host (and a non-default port), never the request path:

```js
try {
  fetch("http://blocked.example/data");
} catch (error) {
  error instanceof PermissionDenied; // true
  error.message;                     // "net: blocked.example"
}
```

The host name is checked synchronously, before any lookup, so a refused
request has no observable side effect. The request then checks the address the
name resolved to, and repeats both checks on **every redirect hop**; a hop the
capability refuses rejects the returned promise with `PermissionDenied`.

On the command line `--allow-net` and `--deny-net` fill the capability, and a
config file's `permissions` block does the same once the config is trusted
(`--trust`) or accepted for the run (`-P`); see
[Config trust](permissions.md#config-trust). Private, loopback, and
link-local destinations are refused unless the grant names them: with
`private`, or with an IP or CIDR scope covering the address. A host-name grant,
even an unscoped `--allow-net`, does not reach a name that resolves privately.
`--max-fetch-bytes` sets the response-body ceiling (default `8MiB`); it is a
setting, not a capability. See [Permissions](permissions.md#command-line) for
the grammar and [Limits and units](permissions.md#limits-and-units) for the
accepted sizes:

```bash
./build/GocciaRunner example.js \
  --allow-net=api.example.com,127.0.0.1:8080 \
  --max-fetch-bytes=1MiB
```

A response over the ceiling rejects with `TypeError`.

### Response bodies and the memory limit

A response body is fully buffered, and its bytes count against the engine's
memory limit (`--max-memory`) like any other allocation the script makes. The
bytes are reserved when the fetch settles, so a body that does not fit the
room left rejects the fetch with the catchable
`RangeError: Allocation failed: GC heap size exceeds the configured memory limit`,
whatever `--max-fetch-bytes` allows. Responses a script keeps hold of keep
their bodies reserved, so bodies that each fit can together pass the limit.
`text()`, `json()` and `arrayBuffer()` give the body's bytes back once they
have read it (`arrayBuffer()` hands them to the buffer it returns, without a
copy), and a response that is collected unread gives them back too. A
`new Response(body)` reserves its encoded body the same way and throws the
`RangeError` when it does not fit. See [Garbage Collector — what `BytesAllocated`
tracks](garbage-collector.md#what-bytesallocated-tracks).

The capability set and the ceiling belong to one engine: embedders fix the set
at engine construction and set `FetchMaxResponseBytes` on the engine. Each
request carries them to the worker or embedder transport that performs it, so
engines that share a thread — a sandbox `runScript` child inside its parent —
neither see nor change each other's policy. An embedder can also give an engine
its own transport, which answers that engine's requests instead of the network
after these checks; see [Embedding — Fetch
Transport](embedding.md#fetch-transport).

### Threat model notes

These are documented candidly, in the spirit of [VISION.md](../VISION.md): the
sandbox is a reduced attack surface, not a verified security boundary.

**DNS rebinding is addressed.** The host is resolved **once**, the resolved
address is validated, and the connection is pinned to that address, so an
allowed name under attacker DNS control cannot answer the check with a public
address and the connect with an internal one. Resolution, validation, and
pinning are re-applied on every redirect hop.

TLS verification still runs against the **hostname**, never the pinned
literal. Pinning changes which address is dialed, not which identity the peer
must prove.

**SSRF is reduced, not eliminated.** Private ranges are refused unless a grant
names them, which blocks the common targets — cloud instance metadata at `169.254.169.254`, loopback services,
RFC1918 hosts. It cannot stop a *public* address that proxies to an internal
one.

Address classification is deliberately deny-biased: anything that is not
exactly four decimal octets or a recognizable IPv6 form is treated as private
rather than reinterpreted. Shortened and hex forms (`127.1`, `0x7f.0.0.1`)
are a standard way to smuggle loopback past a textual filter, so they are
refused rather than parsed.

### Runtime behavior and limits

Requests run on fetch-specific background workers, or go to the engine's transport when an embedder installed one, and settle promises on the owning runtime thread. `await fetch(...)` synchronously waits by pumping fetch completions; the Promise microtask queue is not a general I/O event loop.

Host names resolve through the platform resolver (`getaddrinfo`), so the hosts file, nsswitch, stub resolvers, and IPv6 nameservers apply as they do for any other program on the machine. Connections are IPv4 only: the first IPv4 address is used, and a name with no IPv4 address rejects with `TypeError: Failed to resolve host: <name>`, followed by the resolver's reason when it gives one.

Each host thread caps requests in flight at 16, and runtimes on the same thread share that cap. Additional calls reject their returned promise with `TypeError` until a slot frees: a worker frees its slot when it finishes, and a request an embedder's transport handles frees it when the request settles, aborts or is discarded. An abort rejects the pending promise and discards any late completion. Controller-driven abort does not interrupt an already-blocking platform socket call; `AbortSignal.timeout()` also supplies its deadline to the HTTP worker so platform I/O is bounded by that timeout. An engine waits for, and on failure discards, only the requests it started itself, so a nested engine ending leaves its parent's in-flight requests running. The run's own deadline is not a fetch error: when `--timeout` elapses while requests are pending, the run ends with the uncatchable [`TimeoutError`](json-output.md#timeouterror-in-json), and under `--test-timeout` the test that was waiting is marked as timed out, instead of a fetch promise rejecting with a network error. A request is given the deadline in force when it started, so one that outlives it — a fetch a test left behind — rejects later with `TypeError: HTTP request timed out`. That message applies to connecting and to reads over plain HTTP and HTTPS alike: a TLS handshake or read ends at the request's deadline however the server paces its bytes, whether it goes silent or trickles a record one byte at a time, and the worker ends with the request.

A response body that ends early is a network error, as the Fetch Standard requires for an incomplete message: when the connection closes before the `Content-Length` bytes, or before a `chunked` body's last chunk, the fetch rejects with `TypeError: HTTP response body ended before its declared length`, and a `chunked` body that does not follow the chunked coding (a chunk-size line that is not hexadecimal, or chunk data not followed by CRLF) rejects with `TypeError: Invalid HTTP response: malformed chunked body`; the trailer section after the last chunk is read through its closing empty line and its fields are dropped. A `Content-Length` that is not decimal digits rejects with `TypeError: Invalid HTTP response: malformed Content-Length`. A body with neither header ends when the connection closes, and a connection that fails rather than closes rejects.

A peer that resets the connection never ends the process with `SIGPIPE`: a reset that lands on the request or the response rejects the one fetch with a `TypeError` (unless it lands on a redirect hop's reused connection before any of the response arrives, which sends the hop again; see [Redirects](#redirects)), and one that lands only on the `close_notify` alert that closing a TLS connection writes, after the response is complete, leaves the fetch's result alone. Sockets send with `MSG_NOSIGNAL` on Linux and carry `SO_NOSIGPIPE` on macOS and FreeBSD; OpenSSL's socket BIO writes without either, so on Linux each OpenSSL call that can write runs with `SIGPIPE` blocked on the calling thread and takes back a `SIGPIPE` it raised. The process-wide disposition of `SIGPIPE`, which an embedder may have set, is never changed.

The focused subset has no `Request` object, streaming request or response body, or CORS processing.

## AbortController and AbortSignal

The cancellation values follow [WHATWG DOM §3, Aborting ongoing activities](https://dom.spec.whatwg.org/#aborting-ongoing-activities) for the supported surface.

| API | Behavior |
|-----|----------|
| `new AbortController()` | Creates a controller with one stable `signal` object |
| `controller.signal` | Returns the controller's `AbortSignal` |
| `controller.abort(reason?)` | Aborts once; defaults to an `AbortError` `DOMException` |
| `signal.aborted` | Reports whether the signal has an abort reason |
| `signal.reason` | Returns the exact supplied reason or default `DOMException` |
| `signal.throwIfAborted()` | Throws the exact reason when aborted |
| `AbortSignal.abort(reason?)` | Returns an already-aborted signal |
| `AbortSignal.timeout(milliseconds)` | Returns a signal that aborts with a `TimeoutError` `DOMException` |

`AbortSignal` cannot be constructed directly. Abort is one-shot: later calls to `controller.abort()` do not replace the first reason. `fetch` rejects with the exact signal reason, including non-Error JavaScript values.

GocciaScript does not provide a general timer task queue. Timeout signals therefore transition when the host reaches an observable checkpoint: reading signal state, calling `throwIfAborted()`, or pumping an associated fetch. This preserves bounded fetch cancellation without adding an unrelated browser event loop.

`AbortSignal.any()` is not implemented.

### The abort event

`AbortSignal` inherits from [`EventTarget`](#eventtarget-and-event), so an abort is observable as an event as well as a state change.

| API | Behavior |
|-----|----------|
| `signal.addEventListener("abort", listener)` | Registers a listener for the one-shot `abort` event |
| `signal.onabort` | Event handler attribute for the `abort` event; `null` when unset |
| `event.type` / `event.target` | `"abort"` and the signal itself |

Per [WHATWG DOM §3.2](https://dom.spec.whatwg.org/#abortsignal-signal-abort), a signal aborts at most once, so the `abort` event fires at most once and a listener registered after the abort never runs. `controller.abort()` dispatches synchronously, and it runs the signal's abort algorithms before firing the event: an in-flight `fetch` registers one, so its promise is already rejected and the abort reason already readable by the time any listener runs.

```js
const controller = new AbortController();
controller.signal.addEventListener("abort", (event) => {
  console.log(event.type, event.target.reason.name); // "abort" "AbortError"
});
controller.abort();
```

Because there is no timer task queue, an `AbortSignal.timeout()` signal aborts at the moment the host observes its expiry, and its `abort` event is delivered at that same point — reading `.aborted` after the deadline both flips the state and dispatches the event, exactly once. See [ADR 0104](adr/0104-whatwg-eventtarget-base.md).

## EventTarget and Event

`EventTarget` implements the [WHATWG DOM `EventTarget` interface](https://dom.spec.whatwg.org/#interface-eventtarget) for single-target dispatch.

| API | Behavior |
|-----|----------|
| `new EventTarget()` | Creates a dispatch target with an empty listener list |
| `target.addEventListener(type, callback, options?)` | Registers a listener; `options` is a boolean capture flag or `{ capture, once }` |
| `target.removeEventListener(type, callback, options?)` | Removes the listener matching `(type, callback, capture)` |
| `target.dispatchEvent(event)` | Invokes matching listeners in registration order; returns `false` when a cancelable event was canceled |
| `new Event(type, eventInit?)` | Creates an event; `eventInit` accepts `bubbles` and `cancelable` |
| `event.type`, `event.target`, `event.currentTarget` | Event identity and dispatch state |
| `event.bubbles`, `event.cancelable`, `event.defaultPrevented`, `event.preventDefault()` | Cancellation surface |

A listener is identified by `(type, callback, capture)` and is never added twice. `once` removes the listener before invoking it, removal during a dispatch is respected, and listeners added during a dispatch are not invoked for that event. A non-callable object listener is invoked through its `handleEvent` method. `dispatchEvent` throws an `InvalidStateError` `DOMException` when the event is already being dispatched.

```js
const target = new EventTarget();
target.addEventListener("ready", (event) => console.log(event.type), { once: true });
target.dispatchEvent(new Event("ready")); // logs "ready"
target.dispatchEvent(new Event("ready")); // listener already removed
```

An exception thrown by a listener propagates out of `dispatchEvent` (and out of `controller.abort()`, or out of an `await` when a timeout signal is observed by the fetch pump). The WHATWG algorithm instead reports the exception to a global error handler, which GocciaScript does not have; propagating keeps the failure observable rather than discarding it. The dispatch state is still unwound, so the event and the target stay usable, already-rejected requests stay rejected, and any signal not yet reached settles at the next pump.

GocciaScript has no node tree, so `bubbles` and `capture` are recorded and reported but cause no propagation. `eventPhase`, `stopPropagation`, `stopImmediatePropagation`, `composed`, `composedPath`, `timeStamp`, `isTrusted`, and `CustomEvent` are not implemented; the `passive` listener option is accepted and ignored, and the `signal` member of `AddEventListenerOptions` is not supported.

## Headers

`Headers` implements the [WHATWG Fetch `Headers` interface](https://fetch.spec.whatwg.org/#headers-class) with `get`, `has`, `forEach`, `entries`, `keys`, `values`, and `[Symbol.iterator]`. Construction accepts no argument, a plain object, or another `Headers` instance.

Response headers are read-only. Mutation methods (`append`, `set`, and `delete`) are not implemented.

## Response

`Response` implements the focused [WHATWG Fetch `Response` interface](https://fetch.spec.whatwg.org/#response-class):

- metadata: `status`, `statusText`, `ok`, `url`, `headers`, `type`, and `redirected`;
- body state: `bodyUsed`;
- buffered body methods: `text()`, `json()`, and `arrayBuffer()`.

There is no `Response.body` `ReadableStream`, `blob()`, `formData()`, or `clone()`. The body is fully buffered and can be consumed once.
