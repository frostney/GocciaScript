// node:async_hooks for the CLI host on G2: AsyncLocalStorage and AsyncResource over the engine's async context
// (G2.Promise: a promise reaction made by then or await runs in the context current when it was made; a timer runs
// in the one current when it was scheduled). A context is an immutable Map from AsyncLocalStorage instances to
// their stores, replaced, never changed, so a continuation keeps the one it captured. Called once per realm with
// the global object, the host's natives (n) and the preludes' shared internals; registers node:async_hooks.
(function (G, n, shared) {
  'use strict';
  const EMPTY = new Map();
  const DISABLED = Symbol('disabled');
  const current = () => { const c = n.asyncGet(); return c === undefined ? EMPTY : c; };
  const enter = (c) => { n.asyncSet(c === EMPTY ? undefined : c); };
  const isCallable = (f) => typeof f === 'function';
  // a context with instance's entry set (undefined: removed)
  function withEntry(ctx, instance, value, remove) {
    const next = new Map(ctx);
    if (remove) next.delete(instance); else next.set(instance, value);
    return next;
  }
  // calls fn in context ctx, putting back the context it was called in afterwards
  function runIn(ctx, fn, thisArg, args) {
    const saved = n.asyncGet();
    enter(ctx);
    try { return Reflect.apply(fn, thisArg, args); } finally { n.asyncSet(saved); }
  }
  // a function that runs fn in ctx, named "bound" as Node's are, with fn's length; an undefined thisArg leaves the
  // call site's receiver in place
  function boundTo(ctx, fn, thisArg) {
    const bound = {
      bound(...args) { return runIn(ctx, fn, thisArg === undefined ? this : thisArg, args); }
    }.bound;
    Object.defineProperty(bound, 'length', { value: fn.length, configurable: true });
    return bound;
  }

  class AsyncLocalStorage {
    #defaultValue;
    #name;
    constructor(options = undefined) {
      const o = options === undefined || options === null ? {} : options;
      this.#defaultValue = o.defaultValue;
      this.#name = o.name === undefined ? '' : String(o.name);
    }
    static #check(s, what) {
      if (s === null || typeof s !== 'object' || !(#name in s))
        throw new TypeError(`AsyncLocalStorage.prototype.${what}: not an AsyncLocalStorage`);
    }
    get name() { AsyncLocalStorage.#check(this, 'name'); return this.#name; }
    static bind(fn) {
      if (!isCallable(fn)) throw new TypeError('The "fn" argument must be of type function');
      return boundTo(current(), fn, undefined);
    }
    static snapshot() {
      const ctx = current();
      const runner = {
        // the receiver of the runner is not forwarded: fn runs with undefined this
        snapshot(fn, ...args) {
          if (!isCallable(fn)) throw new TypeError('The "fn" argument must be of type function');
          return runIn(ctx, fn, undefined, args);
        }
      }.snapshot;
      Object.defineProperty(runner, 'name', { value: 'bound', configurable: true });
      return runner;
    }
    run(store, callback, ...args) {
      AsyncLocalStorage.#check(this, 'run');
      if (!isCallable(callback)) throw new TypeError('The "callback" argument must be of type function');
      return runIn(withEntry(current(), this, store, false), callback, undefined, args);
    }
    exit(callback, ...args) {
      AsyncLocalStorage.#check(this, 'exit');
      if (!isCallable(callback)) throw new TypeError('The "callback" argument must be of type function');
      return runIn(withEntry(current(), this, DISABLED, false), callback, undefined, args);
    }
    enterWith(store) {
      AsyncLocalStorage.#check(this, 'enterWith');
      enter(withEntry(current(), this, store, false));
    }
    getStore() {
      AsyncLocalStorage.#check(this, 'getStore');
      const ctx = current();
      if (!ctx.has(this)) return this.#defaultValue;
      const v = ctx.get(this);
      return v === DISABLED ? undefined : v;
    }
    // a frame edit: this instance's binding leaves the current context (getStore then reports the default value)
    disable() {
      AsyncLocalStorage.#check(this, 'disable');
      const ctx = current();
      if (ctx.has(this)) enter(withEntry(ctx, this, undefined, true));
    }
  }
  Object.defineProperty(AsyncLocalStorage.prototype, Symbol.toStringTag, { value: 'AsyncLocalStorage', configurable: true });

  let nextAsyncId = 1;
  class AsyncResource {
    #ctx;
    #type;
    #asyncId;
    #triggerId;
    constructor(type, options = undefined) {
      if (typeof type !== 'string') throw new TypeError('The "type" argument must be of type string');
      this.#type = type;
      this.#ctx = current();
      this.#asyncId = ++nextAsyncId;
      const o = options === undefined || options === null ? {} : (typeof options === 'number' ? { triggerAsyncId: options } : options);
      this.#triggerId = typeof o.triggerAsyncId === 'number' ? o.triggerAsyncId : 1;
    }
    static #check(r, what) {
      if (r === null || typeof r !== 'object' || !(#ctx in r))
        throw new TypeError(`AsyncResource.prototype.${what}: not an AsyncResource`);
    }
    runInAsyncScope(fn, thisArg = undefined, ...args) {
      AsyncResource.#check(this, 'runInAsyncScope');
      if (!isCallable(fn)) throw new TypeError('The "fn" argument must be of type function');
      return runIn(this.#ctx, fn, thisArg, args);
    }
    bind(fn, thisArg = undefined) {
      AsyncResource.#check(this, 'bind');
      if (!isCallable(fn)) throw new TypeError('The "fn" argument must be of type function');
      return boundTo(this.#ctx, fn, thisArg);
    }
    static bind(fn, type = undefined, thisArg = undefined) {
      if (!isCallable(fn)) throw new TypeError('The "fn" argument must be of type function');
      const r = new AsyncResource(typeof type === 'string' ? type : 'bound-anonymous-fn');
      return r.bind(fn, thisArg);
    }
    asyncId() { AsyncResource.#check(this, 'asyncId'); return this.#asyncId; }
    triggerAsyncId() { AsyncResource.#check(this, 'triggerAsyncId'); return this.#triggerId; }
    emitDestroy() { AsyncResource.#check(this, 'emitDestroy'); return this; }
    get type() { AsyncResource.#check(this, 'type'); return this.#type; }
  }
  Object.defineProperty(AsyncResource.prototype, Symbol.toStringTag, { value: 'AsyncResource', configurable: true });

  const asyncHooks = { AsyncLocalStorage, AsyncResource };
  shared.hostModules['node:async_hooks'] = () => ({ AsyncLocalStorage, AsyncResource, default: asyncHooks });
})
