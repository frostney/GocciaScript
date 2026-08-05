# Fetch Runtime APIs

*The loader runtime's bounded WHATWG fetch and cancellation surface.*

## Executive Summary

- **Network authority stays explicit** — `fetch` supports only `GET` and `HEAD` and requires a host allowlist.
- **Cancellation is fetch-scoped** — `AbortController`, `AbortSignal.abort`, and `AbortSignal.timeout` reject associated fetch promises with the signal's exact reason.
- **No general event loop** — timeout state is observed at signal access and fetch-completion pump checkpoints.
- **Real EventTarget base** — `EventTarget` and `Event` are runtime globals, and `AbortSignal` inherits from `EventTarget` for `addEventListener`, `onabort`, and the `abort` event.
- **Focused object model** — `Request`, streaming bodies, CORS, event propagation, and `AbortSignal.any` are not implemented.

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

`AbortSignal.any()` is not implemented.

### The abort event

`AbortSignal` inherits from [`EventTarget`](#eventtarget-and-event), so an abort is observable as an event as well as a state change.

| API | Behavior |
|-----|----------|
| `signal.addEventListener("abort", listener)` | Registers a listener for the one-shot `abort` event |
| `signal.onabort` | Event handler attribute for the `abort` event; `null` when unset |
| `event.type` / `event.target` | `"abort"` and the signal itself |

Per [WHATWG DOM §3.2](https://dom.spec.whatwg.org/#abortsignal-signal-abort), a signal aborts at most once, so the `abort` event fires at most once and a listener registered after the abort never runs. The abort reason is already readable when listeners run, and `controller.abort()` dispatches synchronously.

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

An exception thrown by a listener propagates out of `dispatchEvent` (and out of `controller.abort()`). The WHATWG algorithm instead reports the exception to a global error handler, which GocciaScript does not have; propagating keeps the failure observable rather than discarding it. The dispatch state is still unwound, so the event and the target stay usable.

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
