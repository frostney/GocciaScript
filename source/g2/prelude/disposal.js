(function (G, isRetComp, markIntrinsic) {
  "use strict";
  // Explicit resource management: SuppressedError, DisposableStack, AsyncDisposableStack, and the helpers the
  // compiler calls for using / await using blocks (AddDisposableResource, DisposeResources). A dispose
  // capability is an array of [value, method, async] triples.
  var apply = Reflect.apply, defineProperty = Object.defineProperty, getProto = Object.getPrototypeOf;
  var setProto = Object.setPrototypeOf;
  var SymDispose = Symbol.dispose, SymAsyncDispose = Symbol.asyncDispose, SymToStringTag = Symbol.toStringTag;
  var TypeErr = TypeError, RefErr = ReferenceError, Err = Error, Str = String;
  var PromiseResolve = Promise.resolve.bind(Promise);

  function isObject(v) { return (typeof v === "object" && v !== null) || typeof v === "function"; }
  function def(o, k, v) { defineProperty(o, k, { value: v, writable: true, enumerable: false, configurable: true }); }

  // SuppressedError ( error, suppressed, message )
  var SEP;
  function SuppressedError(error, suppressed, message) {
    var e = new Err();
    var p = new.target === undefined ? SEP : new.target.prototype;
    setProto(e, isObject(p) ? p : markIntrinsic(SEP, new.target));
    if (message !== undefined) def(e, "message", Str(message));
    def(e, "error", error);
    def(e, "suppressed", suppressed);
    return e;
  }
  SEP = Object.create(Err.prototype);
  defineProperty(SuppressedError, "prototype", { value: SEP, writable: false, enumerable: false, configurable: false });
  def(SEP, "constructor", SuppressedError);
  def(SEP, "name", "SuppressedError");
  def(SEP, "message", "");
  setProto(SuppressedError, Err);
  def(G, "SuppressedError", SuppressedError);
  markIntrinsic(SuppressedError);
  function suppressed(error, previous) {
    var e = new Err();
    setProto(e, SEP);
    def(e, "error", error);
    def(e, "suppressed", previous);
    return e;
  }

  // GetDisposeMethod
  function disposeMethod(v, async) {
    var m;
    if (async) {
      m = v[SymAsyncDispose];
      if (m === undefined || m === null) {
        m = v[SymDispose];
        if (m === undefined || m === null) return undefined;
        if (typeof m !== "function") throw new TypeErr("[Symbol.dispose] is not a function");
        var sync = m;
        return function () { apply(sync, this, []); };
      }
    } else {
      m = v[SymDispose];
      if (m === undefined || m === null) return undefined;
    }
    if (typeof m !== "function") throw new TypeErr("the dispose method is not a function");
    return m;
  }
  // AddDisposableResource(capability, V, hint)
  function add(cap, v, async) {
    if (v === null || v === undefined) {
      if (async) { var n = cap.length; cap[n] = undefined; cap[n + 1] = undefined; cap[n + 2] = true; }
      return;
    }
    if (!isObject(v)) throw new TypeErr("a using declaration's value must be an object, null or undefined");
    var m = disposeMethod(v, async);
    if (m === undefined) throw new TypeErr(async ? "the value has no [Symbol.asyncDispose] or [Symbol.dispose] method" : "the value has no [Symbol.dispose] method");
    var k = cap.length;
    cap[k] = v; cap[k + 1] = m; cap[k + 2] = async;
  }
  function addMethod(cap, v, m, async) { var k = cap.length; cap[k] = v; cap[k + 1] = m; cap[k + 2] = async; }

  // DisposeResources for a block without await using: exc / hasExc the block's completion (a return completion
  // in flight through the block is rethrown as it is)
  function dispose(cap, exc, hasExc) {
    var retComp = hasExc && isRetComp(exc), errored = hasExc && !retComp, err = exc;
    for (var i = cap.length - 3; i >= 0; i -= 3) {
      var m = cap[i + 1];
      if (m === undefined) continue;
      try { apply(m, cap[i], []); } catch (e) { if (errored) err = suppressed(e, err); else { errored = true; err = e; } }
    }
    cap.length = 0;
    if (errored) throw err;
    if (retComp) throw exc;
  }
  async function disposeAsync(cap, exc, hasExc) {
    var retComp = hasExc && isRetComp(exc), errored = hasExc && !retComp, err = exc;
    var needsAwait = false, hasAwaited = false;
    for (var i = cap.length - 3; i >= 0; i -= 3) {
      var v = cap[i], m = cap[i + 1], async = cap[i + 2];
      if (!async && needsAwait && !hasAwaited) { await undefined; needsAwait = false; }
      if (m !== undefined) {
        try {
          var r = apply(m, v, []);
          if (async) { hasAwaited = true; await r; }
        } catch (e) { if (errored) err = suppressed(e, err); else { errored = true; err = e; } }
      } else needsAwait = true;
    }
    if (needsAwait && !hasAwaited) await undefined;
    cap.length = 0;
    if (errored) throw err;
    if (retComp) throw exc;
  }

  class Stamp { constructor(o) { return o; } }

  // 27.3 DisposableStack
  class DisposableStack {
    #state = "pending"; #cap = [];
    constructor() {}
    static #check(s, name) {
      if (!isObject(s) || !(#state in s)) throw new TypeErr("DisposableStack.prototype." + name + " called on an incompatible receiver");
    }
    get disposed() { DisposableStack.#check(this, "disposed"); return this.#state === "disposed"; }
    dispose() {
      DisposableStack.#check(this, "dispose");
      if (this.#state === "disposed") return undefined;
      this.#state = "disposed";
      dispose(this.#cap, undefined, false);
      return undefined;
    }
    use(value) {
      DisposableStack.#check(this, "use");
      if (this.#state === "disposed") throw new RefErr("the DisposableStack is already disposed");
      add(this.#cap, value, false);
      return value;
    }
    adopt(value, onDispose) {
      DisposableStack.#check(this, "adopt");
      if (this.#state === "disposed") throw new RefErr("the DisposableStack is already disposed");
      if (typeof onDispose !== "function") throw new TypeErr("onDispose is not a function");
      addMethod(this.#cap, undefined, function () { return onDispose(value); }, false);
      return value;
    }
    defer(onDispose) {
      DisposableStack.#check(this, "defer");
      if (this.#state === "disposed") throw new RefErr("the DisposableStack is already disposed");
      if (typeof onDispose !== "function") throw new TypeErr("onDispose is not a function");
      addMethod(this.#cap, undefined, onDispose, false);
      return undefined;
    }
    move() {
      DisposableStack.#check(this, "move");
      if (this.#state === "disposed") throw new RefErr("the DisposableStack is already disposed");
      var n = new DisposableStack();
      n.#cap = this.#cap;
      this.#cap = [];
      this.#state = "disposed";
      return n;
    }
  }

  // 27.4 AsyncDisposableStack
  class AsyncDisposableStack {
    #state = "pending"; #cap = [];
    constructor() {}
    static #check(s, name) {
      if (!isObject(s) || !(#state in s)) throw new TypeErr("AsyncDisposableStack.prototype." + name + " called on an incompatible receiver");
    }
    static #has(s) { return isObject(s) && #state in s; }
    get disposed() { AsyncDisposableStack.#check(this, "disposed"); return this.#state === "disposed"; }
    disposeAsync() {
      if (!AsyncDisposableStack.#has(this)) {
        try { throw new TypeErr("AsyncDisposableStack.prototype.disposeAsync called on an incompatible receiver"); }
        catch (e) { return Promise.reject(e); }
      }
      if (this.#state === "disposed") return PromiseResolve(undefined);
      this.#state = "disposed";
      return disposeAsync(this.#cap, undefined, false);
    }
    use(value) {
      AsyncDisposableStack.#check(this, "use");
      if (this.#state === "disposed") throw new RefErr("the AsyncDisposableStack is already disposed");
      add(this.#cap, value, true);
      return value;
    }
    adopt(value, onDisposeAsync) {
      AsyncDisposableStack.#check(this, "adopt");
      if (this.#state === "disposed") throw new RefErr("the AsyncDisposableStack is already disposed");
      if (typeof onDisposeAsync !== "function") throw new TypeErr("onDisposeAsync is not a function");
      addMethod(this.#cap, undefined, function () { return onDisposeAsync(value); }, true);
      return value;
    }
    defer(onDisposeAsync) {
      AsyncDisposableStack.#check(this, "defer");
      if (this.#state === "disposed") throw new RefErr("the AsyncDisposableStack is already disposed");
      if (typeof onDisposeAsync !== "function") throw new TypeErr("onDisposeAsync is not a function");
      addMethod(this.#cap, undefined, onDisposeAsync, true);
      return undefined;
    }
    move() {
      AsyncDisposableStack.#check(this, "move");
      if (this.#state === "disposed") throw new RefErr("the AsyncDisposableStack is already disposed");
      var n = new AsyncDisposableStack();
      n.#cap = this.#cap;
      this.#cap = [];
      this.#state = "disposed";
      return n;
    }
  }

  var DP = DisposableStack.prototype, AP = AsyncDisposableStack.prototype;
  def(DP, SymDispose, DP.dispose);
  defineProperty(DP, SymToStringTag, { value: "DisposableStack", writable: false, enumerable: false, configurable: true });
  def(AP, SymAsyncDispose, AP.disposeAsync);
  defineProperty(AP, SymToStringTag, { value: "AsyncDisposableStack", writable: false, enumerable: false, configurable: true });
  def(G, "DisposableStack", DisposableStack);
  markIntrinsic(DisposableStack);
  def(G, "AsyncDisposableStack", AsyncDisposableStack);
  markIntrinsic(AsyncDisposableStack);

  // %AsyncIteratorPrototype% [ @@asyncDispose ] (27.1.4.1)
  var AIP = getProto(getProto(async function* () {}.prototype));
  var asyncIterDispose = {
    [SymAsyncDispose]() {
      var O = this;
      return new Promise(function (resolve, reject) {
        var ret;
        try { ret = O.return; } catch (e) { reject(e); return; }
        if (ret === undefined || ret === null) { resolve(undefined); return; }
        if (typeof ret !== "function") { reject(new TypeErr("return is not a function")); return; }
        var r;
        try { r = apply(ret, O, [undefined]); } catch (e) { reject(e); return; }
        PromiseResolve(r).then(function () { resolve(undefined); }, reject);
      });
    }
  }[SymAsyncDispose];
  def(AIP, SymAsyncDispose, asyncIterDispose);

  return { add: add, dispose: dispose, disposeAsync: disposeAsync };
})
