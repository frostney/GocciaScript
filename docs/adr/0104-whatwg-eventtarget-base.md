# WHATWG EventTarget base for AbortSignal

**Date:** 2026-08-05
**Area:** `runtime`

GocciaScript provides a real `EventTarget` base class and a minimal `Event`
value, and `AbortSignal` genuinely inherits from it. This supersedes the
narrower non-goal recorded in [ADR 0031](0031-fetch-only-async-io.md), which
stated that EventTarget listeners and the `abort` event were out of scope for
the fetch-only cancellation surface. The rest of ADR 0031 stands unchanged: no
general event loop, no timer task queue, and fetch completions still drain
through the existing pump.

`EventTarget` is constructible (`new EventTarget()`), exposes
`addEventListener`, `removeEventListener`, and `dispatchEvent`, and follows
[WHATWG DOM](https://dom.spec.whatwg.org/) § 2.7 listener-list semantics: a
listener is identified by the `(type, callback, capture)` triple and is not
added twice, `once` removes the listener before invoking it, removal during a
dispatch is respected, and listeners appended during a dispatch are not invoked
for that event. Non-callable object listeners are invoked through `handleEvent`
with the listener object as the receiver. `Event` (§ 2.2) carries `type`,
`target`, `currentTarget`, `bubbles`, `cancelable`, `defaultPrevented`, and
`preventDefault()`. `dispatchEvent` throws an `InvalidStateError` `DOMException`
when the event's dispatch flag is already set.

The prototype chain is real rather than simulated: `AbortSignal.prototype`'s
`[[Prototype]]` is `EventTarget.prototype` and `AbortSignal`'s `[[Prototype]]`
is `EventTarget`, so `signal instanceof EventTarget` holds and the shared
listener machinery is inherited rather than duplicated. `AbortSignal` follows
§ 3.2 signal abort ordering, and does so through the spec's own mechanism: the
signal carries an abort-algorithms set that hosts register into. An
already-aborted signal returns early, the abort reason is set, the registered
algorithms run and the set is emptied, and the `abort` event is fired last.
`fetch` is the one host that registers an algorithm today — it adds one per
in-flight request that rejects that request's promise and drops its pending
entry — so `controller.abort()` settles the fetch synchronously, before any
listener observes the abort, rather than deferring the rejection to the next
completion pump. Registering an algorithm on an already-aborted signal is
refused, the same rule that governs listeners, and the host removes its
algorithm when a request completes normally. A signal aborts at most once, so
the `abort` event fires at most once and a listener registered after the abort
never runs. `onabort` is an event handler IDL attribute (§ 8.1.5.1): its listener is
registered when a non-null handler is first assigned and keeps that registration
position, so assigning `null` clears the handler without reordering the
remaining listeners.

Because there is no timer task queue, an `AbortSignal.timeout()` signal aborts
at the moment the host observes its expiry, and the `abort` event is delivered
at that same observation point — reading `.aborted` or `.reason` after expiry
both flips the state and dispatches the event, exactly once. Timeout expiry is
the one case where the state flip is separated from the algorithms and the
event: it is detected while the fetch pump walks its pending-request list, and
an abort algorithm mutates that list, so the pump flips the state during the
walk and then runs the algorithms and fires the event once the walk is over. No
script executes in that window, so a listener still cannot be registered after a
signal aborted but before its event fires.

A listener that throws during that pump-driven dispatch propagates out of the
pumping call (typically an `await` on a fetch). The containment is deliberate
and bounded: each signal's algorithms and event are processed as a unit, so the
requests already rejected stay rejected and the signals not yet reached keep
their pending state and are settled by the next pump. Completions already
queued by worker threads are not lost either — they remain queued and settle at
the next pump rather than being discarded.

`EventTarget` and `Event` ship with the same runtime extension as
`AbortController`, `AbortSignal`, `Headers`, and `Response` rather than as core
realm globals, because the core GocciaScript realm is ECMA-262 only and every
WHATWG surface is opt-in through a runtime profile.

One deliberate deviation concerns listener exceptions. Under WHATWG DOM § 2.9,
inner invoke *reports* an exception thrown by a listener rather than propagating
it, which is why `dispatchEvent` and `controller.abort()` never throw on a
listener's behalf in a browser. GocciaScript deviates: it has no global
error-reporting channel to report into, so an exception thrown by a listener
propagates out of `dispatchEvent` (and out of `controller.abort()`) instead of
being swallowed. Making it observable is preferred over discarding it; the
dispatch state is still unwound, so the event and the target remain usable
afterwards, and a signal's `abort` event is still marked as fired and never
re-dispatched.

A second deliberate deviation: the accessors this runtime installs on
`Event.prototype` — and `onabort` on `AbortSignal` — are non-enumerable, where
WebIDL specifies interface attributes as enumerable. This follows the house
convention already set by the pre-existing `aborted` and `reason` accessors, so
`Object.keys(Event.prototype)` is empty and every accessor's descriptor reports
`enumerable: false`. Consistency within the runtime is preferred over matching
WebIDL enumerability for one interface.

Deliberately out of scope: there is no node tree, so `bubbles` and `capture` are
recorded and reported faithfully but produce no propagation — `eventPhase`,
`stopPropagation`, `stopImmediatePropagation`, `composed`, and `composedPath`
are absent. `Event.timeStamp` and `isTrusted` are omitted rather than backed by
an invented time origin. The `passive` listener option is accepted and ignored,
the `signal` member of `AddEventListenerOptions` is not supported, and
`AbortSignal.any()` and its dependent-signal propagation (§ 3.2 signal abort
step 5) remain unimplemented. `CustomEvent` is not provided.
