// Event, EventTarget, AbortController and AbortSignal (DOM Standard) for the CLI host, as GocciaScript has
// them: accessors are not enumerable (ADR 0104), and an exception a listener throws propagates out of
// dispatchEvent. Called once per realm with the global object, the host's natives (n) and the preludes' shared
// internals (DOMException, timers).
(function (G, n, shared) {
  'use strict';
  const DOMException = shared.DOMException;
  // what the functions below call, captured before any script runs: a script that replaces a prototype method or a
  // global does not reach into these interfaces
  const { TypeError, String, Number } = G;
  const defineProperty = Object.defineProperty, ReflectApply = Reflect.apply, MathMax = Math.max;
  const NumberIsFinite = Number.isFinite, MAX_SAFE_INTEGER = Number.MAX_SAFE_INTEGER;
  const hidden = (o, k, v) => defineProperty(o, k, { __proto__: null, value: v, writable: true, enumerable: false, configurable: true });
  // a list append that defines the index (a setter on Array.prototype must not see it)
  const append = (list, v) => { defineProperty(list, list.length, { __proto__: null, value: v, writable: true, enumerable: true, configurable: true }); };
  const copyList = (list) => { const r = []; for (let i = 0; i < list.length; i++) append(r, list[i]); return r; };
  const isObject = (v) => v !== null && (typeof v === 'object' || typeof v === 'function');

  // ---------------- Event ----------------
  let eventState;   // the event's internal record, for EventTarget
  class Event {
    #s;
    constructor(type, eventInitDict = undefined) {
      if (arguments.length < 1) throw new TypeError("Failed to construct 'Event': 1 argument required, but only 0 present.");
      if (eventInitDict !== undefined && eventInitDict !== null && !isObject(eventInitDict))
        throw new TypeError("Failed to construct 'Event': The provided value is not of type 'EventInit'.");
      const init = eventInitDict === undefined || eventInitDict === null ? {} : eventInitDict;
      const bubbles = !!init.bubbles, cancelable = !!init.cancelable, composed = !!init.composed;
      this.#s = { type: String(type), bubbles, cancelable, composed, target: null, currentTarget: null, phase: 0,
        canceled: false, stop: false, stopNow: false, dispatching: false, inPassive: false, timeStamp: n.hrNow() };
    }
    static #of(e, what) {
      if (!isObject(e) || !(#s in e)) throw new TypeError(`Event.prototype.${what}: Illegal invocation`);
      return e.#s;
    }
    get type() { return Event.#of(this, 'type').type; }
    get target() { return Event.#of(this, 'target').target; }
    get srcElement() { return Event.#of(this, 'srcElement').target; }
    get currentTarget() { return Event.#of(this, 'currentTarget').currentTarget; }
    get eventPhase() { return Event.#of(this, 'eventPhase').phase; }
    get bubbles() { return Event.#of(this, 'bubbles').bubbles; }
    get cancelable() { return Event.#of(this, 'cancelable').cancelable; }
    get composed() { return Event.#of(this, 'composed').composed; }
    get defaultPrevented() { return Event.#of(this, 'defaultPrevented').canceled; }
    get returnValue() { return !Event.#of(this, 'returnValue').canceled; }
    set returnValue(v) { const s = Event.#of(this, 'returnValue'); if (!v && s.cancelable && !s.inPassive) s.canceled = true; }
    get cancelBubble() { return Event.#of(this, 'cancelBubble').stop; }
    set cancelBubble(v) { if (v) Event.#of(this, 'cancelBubble').stop = true; }
    get isTrusted() { Event.#of(this, 'isTrusted'); return false; }
    get timeStamp() { return Event.#of(this, 'timeStamp').timeStamp; }
    composedPath() { const s = Event.#of(this, 'composedPath'); return s.dispatching && s.currentTarget !== null ? [s.currentTarget] : []; }
    stopPropagation() { Event.#of(this, 'stopPropagation').stop = true; }
    stopImmediatePropagation() { const s = Event.#of(this, 'stopImmediatePropagation'); s.stop = true; s.stopNow = true; }
    preventDefault() { const s = Event.#of(this, 'preventDefault'); if (s.cancelable && !s.inPassive) s.canceled = true; }
    static { eventState = (e) => (isObject(e) && #s in e ? e.#s : undefined); }
  }
  Object.defineProperty(Event.prototype, Symbol.toStringTag, { __proto__: null, value: 'Event', configurable: true });

  // ---------------- EventTarget ----------------
  let listenersOf;   // the target's listener list, for AbortSignal
  function flatten(options, withPassive) {
    if (!isObject(options)) return { capture: !!options, once: false, passive: false, signal: null };
    const capture = !!options.capture;
    if (!withPassive) return { capture };
    const once = !!options.once, passive = !!options.passive;
    const signal = options.signal;
    if (signal !== undefined && !shared.isAbortSignal(signal))
      throw new TypeError("Failed to execute 'addEventListener' on 'EventTarget': member signal is not of type 'AbortSignal'.");
    return { capture, once, passive, signal: signal === undefined ? null : signal };
  }
  class EventTarget {
    #listeners = [];
    constructor() {}
    static #check(t, what) {
      if (!isObject(t) || !(#listeners in t)) throw new TypeError(`EventTarget.prototype.${what}: Illegal invocation`);
    }
    addEventListener(type, callback, options = undefined) {
      EventTarget.#check(this, 'addEventListener');
      if (arguments.length < 2) throw new TypeError("Failed to execute 'addEventListener' on 'EventTarget': 2 arguments required.");
      if (typeof type === 'symbol') throw new TypeError('Cannot convert a Symbol value to a string');
      type = String(type);
      if (callback !== null && callback !== undefined && !isObject(callback))
        throw new TypeError("Failed to execute 'addEventListener' on 'EventTarget': parameter 2 is not of type 'Object'.");
      const o = flatten(options, true);
      if (callback === null || callback === undefined) return;
      if (o.signal !== null && shared.isAborted(o.signal)) return;
      if (this.#find(type, callback, o.capture) !== undefined) return;
      const l = { type, callback, capture: o.capture, once: o.once, passive: o.passive, signal: o.signal, removed: false };
      append(this.#listeners, l);
      if (o.signal !== null) shared.onAbort(o.signal, () => this.#remove(l));
    }
    removeEventListener(type, callback, options = undefined) {
      EventTarget.#check(this, 'removeEventListener');
      if (arguments.length < 2) throw new TypeError("Failed to execute 'removeEventListener' on 'EventTarget': 2 arguments required.");
      if (typeof type === 'symbol') throw new TypeError('Cannot convert a Symbol value to a string');
      type = String(type);
      if (callback !== null && callback !== undefined && !isObject(callback))
        throw new TypeError("Failed to execute 'removeEventListener' on 'EventTarget': parameter 2 is not of type 'Object'.");
      const o = flatten(options, false);
      const l = this.#find(type, callback, o.capture);
      if (l !== undefined) this.#remove(l);
    }
    #find(type, callback, capture) {
      const list = this.#listeners;
      for (let i = 0; i < list.length; i++) {
        const x = list[i];
        if (!x.removed && x.type === type && x.callback === callback && x.capture === capture) return x;
      }
      return undefined;
    }
    #remove(l) {
      l.removed = true;
      const list = this.#listeners;
      let i = 0;
      while (i < list.length && list[i] !== l) i++;
      if (i === list.length) return;
      for (; i + 1 < list.length; i++) list[i] = list[i + 1];
      list.length--;
    }
    dispatchEvent(event) {
      EventTarget.#check(this, 'dispatchEvent');
      const s = eventState(event);
      if (s === undefined) throw new TypeError("Failed to execute 'dispatchEvent' on 'EventTarget': parameter 1 is not of type 'Event'.");
      if (s.dispatching) throw new DOMException("Failed to execute 'dispatchEvent' on 'EventTarget': The event is already being dispatched.", 'InvalidStateError');
      s.dispatching = true;
      s.target = this;
      s.currentTarget = this;
      s.phase = 2;
      try {
        // the listeners when the dispatch started; one removed meanwhile does not run
        const listeners = copyList(this.#listeners);
        for (let i = 0; i < listeners.length; i++) {
          const l = listeners[i];
          if (l.removed || l.type !== s.type) continue;
          if (s.stopNow) break;
          if (l.once) this.#remove(l);
          if (l.passive) s.inPassive = true;
          try {
            if (typeof l.callback === 'function') ReflectApply(l.callback, this, [event]);
            else {
              const handle = l.callback.handleEvent;
              if (typeof handle !== 'function') throw new TypeError('handleEvent is not a function');
              ReflectApply(handle, l.callback, [event]);
            }
          } finally {
            s.inPassive = false;
          }
        }
      } finally {
        s.dispatching = false;
        s.currentTarget = null;
        s.phase = 0;
        s.stop = false;
        s.stopNow = false;
      }
      return !s.canceled;
    }
    static { listenersOf = (t) => t.#listeners; }
  }
  Object.defineProperty(EventTarget.prototype, Symbol.toStringTag, { __proto__: null, value: 'EventTarget', configurable: true });
  // EventTarget's own methods, for AbortSignal's internal steps (a script may replace the prototype's)
  const addListener = EventTarget.prototype.addEventListener, removeListener = EventTarget.prototype.removeEventListener;
  const dispatch = EventTarget.prototype.dispatchEvent;
  void listenersOf;

  // ---------------- AbortSignal and AbortController ----------------
  let creating = false;
  let signalAbort;
  class AbortSignal extends EventTarget {
    #aborted = false;
    #reason = undefined;
    #algorithms = [];
    #onabort = null;
    #onabortListener = null;
    #deadline = -1;   // AbortSignal.timeout's: the hrNow time it aborts at, observed when its state is read
    constructor() {
      if (!creating) throw new TypeError('Illegal constructor');
      super();
    }
    static #check(s, what) {
      if (!isObject(s) || !(#aborted in s)) throw new TypeError(`AbortSignal.prototype.${what}: Illegal invocation`);
    }
    static #create() {
      creating = true;
      try { return new AbortSignal(); } finally { creating = false; }
    }
    // a timeout signal whose time has passed aborts when its state is first observed
    #observe() {
      if (this.#deadline >= 0 && !this.#aborted && n.hrNow() >= this.#deadline) this.#timeOut();
    }
    #timeOut() {
      this.#deadline = -1;
      this.#signalAbort(new DOMException('The operation timed out.', 'TimeoutError'));
    }
    get aborted() { AbortSignal.#check(this, 'aborted'); this.#observe(); return this.#aborted; }
    get reason() { AbortSignal.#check(this, 'reason'); this.#observe(); return this.#reason; }
    throwIfAborted() { AbortSignal.#check(this, 'throwIfAborted'); this.#observe(); if (this.#aborted) throw this.#reason; }
    get onabort() { AbortSignal.#check(this, 'onabort'); return this.#onabort; }
    set onabort(v) {
      AbortSignal.#check(this, 'onabort');
      const fn = typeof v === 'function' ? v : null;
      if (fn === null) {
        if (v !== null && v !== undefined && !isObject(v)) return;
        if (this.#onabortListener !== null) { ReflectApply(removeListener, this, ['abort', this.#onabortListener]); this.#onabortListener = null; }
        this.#onabort = null;
        return;
      }
      this.#onabort = fn;
      if (this.#onabortListener === null) {
        // one listener for the attribute keeps its place when the handler is replaced
        this.#onabortListener = (event) => { const h = this.#onabort; if (h !== null) return ReflectApply(h, this, [event]); };
        ReflectApply(addListener, this, ['abort', this.#onabortListener]);
      }
    }
    #signalAbort(reason) {
      if (this.#aborted) return;
      this.#aborted = true;
      this.#reason = reason === undefined ? new DOMException('This operation was aborted', 'AbortError') : reason;
      const algorithms = this.#algorithms;
      this.#algorithms = [];
      for (let i = 0; i < algorithms.length; i++) algorithms[i]();
      ReflectApply(dispatch, this, [new Event('abort')]);
    }
    static abort(reason = undefined) {
      const s = AbortSignal.#create();
      s.#aborted = true;
      s.#reason = reason === undefined ? new DOMException('This operation was aborted', 'AbortError') : reason;
      return s;
    }
    static timeout(milliseconds) {
      if (arguments.length < 1) throw new TypeError("Failed to execute 'timeout' on 'AbortSignal': 1 argument required, but only 0 present.");
      const ms = Number(milliseconds);
      if (!NumberIsFinite(ms) || ms < 0 || ms > MAX_SAFE_INTEGER)
        throw new TypeError("Failed to execute 'timeout' on 'AbortSignal': Value is outside the 'unsigned long long' value range.");
      const s = AbortSignal.#create();
      s.#deadline = n.hrNow() + ms;
      shared.timers.add(() => { if (!s.#aborted) s.#timeOut(); }, ms, [], false);
      return s;
    }
    static any(signals) {
      // WebIDL sequence<AbortSignal>: the argument's own iterator
      const list = [...signals];
      for (let i = 0; i < list.length; i++) if (!isObject(list[i]) || !(#aborted in list[i])) throw new TypeError("Failed to execute 'any' on 'AbortSignal': member is not of type 'AbortSignal'.");
      const s = AbortSignal.#create();
      for (let i = 0; i < list.length; i++) if (list[i].#aborted) { s.#aborted = true; s.#reason = list[i].#reason; return s; }
      for (let i = 0; i < list.length; i++) { const x = list[i]; append(x.#algorithms, () => s.#signalAbort(x.#reason)); }
      return s;
    }
    static {
      signalAbort = (s, reason) => s.#signalAbort(reason);
      shared.isAbortSignal = (v) => isObject(v) && #aborted in v;
      shared.onAbort = (s, fn) => { if (!s.#aborted) append(s.#algorithms, fn); };
      shared.isAborted = (s) => { s.#observe(); return s.#aborted; };
      shared.abortReason = (s) => s.#reason;
      // the milliseconds left on an AbortSignal.timeout signal (-1 for any other), for fetch's worker
      shared.signalRemaining = (s) => s.#deadline < 0 ? -1 : MathMax(0, s.#deadline - n.hrNow());
    }
  }
  Object.defineProperty(AbortSignal.prototype, Symbol.toStringTag, { __proto__: null, value: 'AbortSignal', configurable: true });

  class AbortController {
    #signal;
    constructor() {
      creating = true;
      try { this.#signal = new AbortSignal(); } finally { creating = false; }
    }
    static #check(c, what) {
      if (!isObject(c) || !(#signal in c)) throw new TypeError(`AbortController.prototype.${what}: Illegal invocation`);
    }
    get signal() { AbortController.#check(this, 'signal'); return this.#signal; }
    abort(reason = undefined) { AbortController.#check(this, 'abort'); signalAbort(this.#signal, reason); }
  }
  Object.defineProperty(AbortController.prototype, Symbol.toStringTag, { __proto__: null, value: 'AbortController', configurable: true });

  hidden(G, 'Event', Event);
  hidden(G, 'EventTarget', EventTarget);
  hidden(G, 'AbortController', AbortController);
  hidden(G, 'AbortSignal', AbortSignal);
  shared.Event = Event;
  shared.EventTarget = EventTarget;
  shared.runtimeGlobals.push('EventTarget', 'Event', 'AbortController', 'AbortSignal');
})
