# Async Context

*The `node:async_hooks` surface: stores that travel with a continuation instead of with a call.*

## Executive Summary

- **Node's module, Node's address** — `import { AsyncLocalStorage, AsyncResource } from "node:async_hooks"`, with a default export carrying both, exactly as Node spells it.
- **The engine propagates the context** — a snapshot of every bound store is captured when a continuation is created and installed when it runs, so a store survives `await`, `.then`, `.catch`, and `.finally`.
- **Concurrent chains stay separate** — the snapshot holds all instances at once, so interleaved chains and several `AsyncLocalStorage` instances never see each other's stores.
- **Available everywhere the loader profile is** — it carries no capability, so there is no opt-in flag.
- **No observer API** — `createHook`, `executionAsyncId`, and the resource lifecycle callbacks are not provided; see [ADR 0112](adr/0112-native-async-local-storage.md).

## AsyncLocalStorage

```javascript
import { AsyncLocalStorage } from "node:async_hooks";

const requestContext = new AsyncLocalStorage();

const handle = async (request) =>
  requestContext.run({ id: request.id }, async () => {
    await loadUser();
    // Still the same store, on the other side of the await.
    return requestContext.getStore().id;
  });
```

`new AsyncLocalStorage(options?)` accepts an options object with two members:

| Option | Effect |
|--------|--------|
| `defaultValue` | What `getStore()` reports when no store is bound. Defaults to `undefined`. |
| `name` | Read back through the `name` property. Defaults to the empty string. |

| Method | Behavior |
|--------|----------|
| `run(store, callback, ...args)` | Binds `store`, calls `callback(...args)`, and restores the previous binding when the synchronous part of `callback` returns. Returns the callback's result. Continuations created while it runs keep the store. Re-enables a disabled instance. |
| `getStore()` | The bound store, or the instance's `defaultValue` when nothing is bound. A store bound as `undefined` wins over the default value. |
| `enterWith(store)` | Binds `store` for the rest of the current execution and for continuations created from it. There is no scope to leave, so the binding lasts until whatever installed the surrounding context restores it. Re-enables a disabled instance. |
| `exit(callback, ...args)` | Calls `callback(...args)` with `undefined` bound — not with the default value — and restores the previous binding afterwards. |
| `disable()` | Deletes the binding from the current context. `getStore()` then reports the `defaultValue` until `run` or `enterWith` binds again. A continuation captured *before* the `disable` still carries the store it captured. |

| Static | Behavior |
|--------|----------|
| `AsyncLocalStorage.bind(fn)` | Returns `fn` pinned to the context current at the `bind` call. Throws `TypeError` at the `bind` call if `fn` is not callable. |
| `AsyncLocalStorage.snapshot()` | Returns `(fn, ...args) => fn(...args)`, run under the context current at the `snapshot` call. It has no callback to validate, so a non-callable is a `TypeError` at the runner's call instead. |

Every function these return is named `bound`, as Node's are. A `bind` wrapper
reports its target's `length`; a `snapshot` runner has no target and reports
`1`, the arity of the `(fn, ...args)` runner itself.

## AsyncResource

An `AsyncResource` captures the async context once, at construction, and replays
it on demand. It is the mechanism a library uses to run a stored callback under
the context it was registered in.

```javascript
import { AsyncResource } from "node:async_hooks";

const resource = new AsyncResource("db-query");
const later = resource.bind(() => requestContext.getStore());
```

| Member | Behavior |
|--------|----------|
| `new AsyncResource(type, options?)` | Captures the current context. `type` and `options` are accepted and unused. |
| `runInAsyncScope(fn, thisArg, ...args)` | Calls `fn` under the captured context and returns its result. Unlike `bind`, an omitted `thisArg` means `this` is `undefined` rather than the call site's receiver. |
| `bind(fn, thisArg?)` | Returns `fn` pinned to the captured context. An undefined `thisArg` leaves the call site's own receiver in place, so a bound function installed as an object method still sees that object as `this`. Throws `TypeError` at the `bind` call if `fn` is not callable. |
| `AsyncResource.bind(fn, type?, thisArg?)` | Returns `fn` pinned to the context current at the `bind` call, with the same receiver and validation rules. |
| `asyncId()` / `triggerAsyncId()` | A number unique to the resource. Nothing else in the engine relates to it, and `triggerAsyncId` reports the resource's own id. |
| `emitDestroy()` | Returns the resource. There are no destroy hooks to emit to. |

## What propagates, and what does not

The context travels with every continuation the engine creates: `await`
resumptions, `.then` / `.catch` / `.finally` handlers, callbacks passed to
`queueMicrotask`, and promise reactions registered inside a scope but settled
outside it. A reaction records the context where it was *registered*, so

```javascript
storage.run("registered", () => pending.then(handler));
```

reaches `handler` with `"registered"` bound however `pending` is eventually
settled.

An async generator body observes the context of whichever call resumed it, in
both executors and as in Node — a `for await` inside a `run` sees that run's
store, and a generator resumed outside one sees no store.

It travels into timer callbacks too. A `setTimeout` scheduled inside a `run`
captures the snapshot at registration and runs under it, even though the `run`
returned long before the timer fired — see [Fake
timers](testing-api.md#fake-timers) for the queue itself. That is the third
propagation seam, and it needed nothing new from the snapshot mechanism.

The `async_hooks` observer API (`createHook`, `executionAsyncId`, and the `init`
/ `before` / `after` / `destroy` callbacks) is not provided — it describes an
async-resource lifecycle this engine does not have. [ADR
0112](adr/0112-native-async-local-storage.md) records that cut and the snapshot
mechanism behind the propagation; [ADR
0113](adr/0113-deterministic-virtual-timer-queue.md) records the timer seam it
predicted.

## Availability

`node:async_hooks` is installed by the loader runtime profile, so it resolves in
`GocciaScriptLoader`, `GocciaTestRunner`, `GocciaREPL`,
`GocciaBenchmarkRunner`, and `GocciaSandboxRunner` (which applies that profile
before installing its own sandbox extension) without a flag. It grants no capability — no I/O, no
clock, no ambient authority — so nothing about it is gated. `GocciaScriptLoaderBare`
attaches no runtime and therefore does not resolve it.
