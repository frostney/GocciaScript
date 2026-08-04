# Fetch Runtime APIs

*The loader runtime's bounded WHATWG fetch and cancellation surface.*

## Executive Summary

- **Network authority stays explicit** — `fetch` supports only `GET` and `HEAD` and requires a host allowlist.
- **Cancellation is fetch-scoped** — `AbortController`, `AbortSignal.abort`, and `AbortSignal.timeout` reject associated fetch promises with the signal's exact reason.
- **No general event loop** — timeout state is observed at signal access and fetch-completion pump checkpoints.
- **Focused object model** — `Request`, streaming bodies, CORS, EventTarget listeners, `AbortSignal.any`, and `onabort` are not implemented.

## fetch

`fetch(url, options?)` implements a subset of the [WHATWG Fetch Standard](https://fetch.spec.whatwg.org/#fetch-method) and returns `Promise<Response>`.

The `options` object supports:

- `method`: `"GET"` or `"HEAD"` (case-insensitive);
- `headers`: a plain object or `Headers` instance;
- `signal`: an `AbortSignal`, `null`, or `undefined`.

Redirects (301/302/303/307/308) are followed automatically up to 20 hops. HTTPS uses SecureTransport on macOS, SChannel on Windows, and OpenSSL on Linux.

### Allowed hosts

Without `--allowed-host` or an `"allowed-hosts"` config key in `goccia.json`, any call to `fetch` throws `TypeError`.

```bash
./build/GocciaScriptLoader example.js \
  --allowed-host=api.example.com \
  --allowed-host=cdn.example.com
```

```json
{ "allowed-hosts": ["api.example.com", "cdn.example.com"] }
```

Host matching is case-insensitive and ignores port, path, and userinfo.

### Runtime behavior and limits

Requests run on fetch-specific background workers and settle promises on the owning runtime thread. `await fetch(...)` synchronously waits by pumping fetch completions; the Promise microtask queue is not a general I/O event loop.

Each host thread caps active fetch workers at 16; runtimes on the same thread share that cap. Additional calls reject their returned promise with `TypeError` until a worker finishes. An abort rejects the pending promise and discards any late completion. Controller-driven abort does not interrupt an already-blocking platform socket call; `AbortSignal.timeout()` also supplies its deadline to the HTTP worker so platform I/O is bounded by that timeout.

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

EventTarget behavior (`addEventListener`, `removeEventListener`, `onabort`, and the `abort` event) and `AbortSignal.any()` are not implemented.

## Headers

`Headers` implements the [WHATWG Fetch `Headers` interface](https://fetch.spec.whatwg.org/#headers-class) with `get`, `has`, `forEach`, `entries`, `keys`, `values`, and `[Symbol.iterator]`. Construction accepts no argument, a plain object, or another `Headers` instance.

Response headers are read-only. Mutation methods (`append`, `set`, and `delete`) are not implemented.

## Response

`Response` implements the focused [WHATWG Fetch `Response` interface](https://fetch.spec.whatwg.org/#response-class):

- metadata: `status`, `statusText`, `ok`, `url`, `headers`, `type`, and `redirected`;
- body state: `bodyUsed`;
- buffered body methods: `text()`, `json()`, and `arrayBuffer()`.

There is no `Response.body` `ReadableStream`, `blob()`, `formData()`, or `clone()`. The body is fully buffered and can be consumed once.
