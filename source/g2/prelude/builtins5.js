(function (G) {
  "use strict";
  // Array.fromAsync, Object.groupBy / Map.groupBy, Map / WeakMap getOrInsert and getOrInsertComputed, by the spec's
  // steps; the intrinsics used are captured here, so later changes to the globals are not observed.
  var apply = Reflect.apply, construct = Reflect.construct, defineProperty = Object.defineProperty;
  var create = Object.create, isArray = Array.isArray;
  var SymIterator = Symbol.iterator, SymAsyncIterator = Symbol.asyncIterator;
  var TypeErr = TypeError, PromiseCtor = Promise;
  var MapCtor = Map, MP = Map.prototype, mapHas = MP.has, mapGet = MP.get, mapSet = MP.set;

  function def(o, k, v) { defineProperty(o, k, { value: v, writable: true, enumerable: false, configurable: true }); }
  function isObject(v) { return (typeof v === "object" && v !== null) || typeof v === "function"; }
  function isConstructor(f) {
    if (typeof f !== "function") return false;
    try { construct(String, [], f); return true; } catch (e) { return false; }
  }
  function getMethod(v, k) {
    var f = v[k];
    if (f === undefined || f === null) return undefined;
    if (typeof f !== "function") throw new TypeErr("not a function");
    return f;
  }
  function createDataProperty(o, k, v) {
    defineProperty(o, k, { value: v, writable: true, enumerable: true, configurable: true });
  }
  function closeAsync(it) {
    var r;
    try { r = it.return; } catch (e) { return; }
    if (r === undefined || r === null) return;
    try { return apply(r, it, []); } catch (e) {}
  }

  // Array.fromAsync ( asyncItems [ , mapfn [ , thisArg ] ] )
  var fromAsync = {
    async fromAsync(asyncItems) {
      var C = this, mapfn = arguments.length > 1 ? arguments[1] : undefined, thisArg = arguments.length > 2 ? arguments[2] : undefined;
      var mapping = mapfn !== undefined;
      if (mapping && typeof mapfn !== "function") throw new TypeErr("Array.fromAsync: the mapper is not a function");
      var usingAsync = getMethod(asyncItems, SymAsyncIterator), usingSync;
      if (usingAsync === undefined) usingSync = getMethod(asyncItems, SymIterator);
      var A, k = 0, v;
      if (usingAsync !== undefined || usingSync !== undefined) {
        A = isConstructor(C) ? construct(C, []) : [];
        var it = apply(usingAsync !== undefined ? usingAsync : usingSync, asyncItems, []);
        if (!isObject(it)) throw new TypeErr("the iterator is not an object");
        var next = it.next, sync = usingAsync === undefined;
        for (;;) {
          var r = apply(next, it, []);
          if (!sync) r = await r;
          if (!isObject(r)) throw new TypeErr("the iterator result is not an object");
          if (r.done) { A.length = k; return A; }
          v = r.value;
          if (sync) {
            try { v = await v; } catch (e) { var rf = it.return; if (rf !== undefined && rf !== null) { try { apply(rf, it, []); } catch (e2) {} } throw e; }
          }
          if (mapping) {
            try { v = await apply(mapfn, thisArg, [v, k]); }
            catch (e) { if (sync) { var rs = it.return; if (rs !== undefined && rs !== null) { try { apply(rs, it, []); } catch (e3) {} } } else await closeAsync(it); throw e; }
          }
          try { createDataProperty(A, k, v); }
          catch (e) { if (sync) { var rt = it.return; if (rt !== undefined && rt !== null) { try { apply(rt, it, []); } catch (e4) {} } } else await closeAsync(it); throw e; }
          k++;
        }
      }
      var arrayLike = Object(asyncItems), len = arrayLike.length;
      len = +len; len = len !== len || len <= 0 ? 0 : (len === Infinity ? 9007199254740991 : Math.min(Math.floor(len), 9007199254740991));
      A = isConstructor(C) ? construct(C, [len]) : new Array(len);
      for (; k < len; k++) {
        v = await arrayLike[k];
        if (mapping) v = await apply(mapfn, thisArg, [v, k]);
        createDataProperty(A, k, v);
      }
      A.length = len;
      return A;
    }
  }.fromAsync;
  def(Array, "fromAsync", fromAsync);

  // 7.3.35 GroupBy ( items, callback, keyCoercion )
  function groupBy(items, callback, property) {
    if (items === undefined || items === null) throw new TypeErr("groupBy called on null or undefined");
    if (typeof callback !== "function") throw new TypeErr("groupBy: the callback is not a function");
    var keys = [], groups = [];
    var m = getMethod(items, SymIterator);
    if (m === undefined) throw new TypeErr("groupBy: items is not iterable");
    var it = apply(m, items, []);
    if (!isObject(it)) throw new TypeErr("the iterator is not an object");
    var next = it.next;
    for (var k = 0; ; k++) {
      var r = apply(next, it, []);
      if (!isObject(r)) throw new TypeErr("the iterator result is not an object");
      if (r.done) break;
      var value = r.value, key;
      try {
        key = callback(value, k);
        if (property) key = typeof key === "symbol" ? key : (isObject(key) ? toPropertyKey(key) : String(key));
        else if (key === 0) key = 0;   // -0 to +0
      } catch (e) {
        var rf = it.return;
        if (rf !== undefined && rf !== null) { try { apply(rf, it, []); } catch (e2) {} }
        throw e;
      }
      var found = -1;
      for (var i = 0; i < keys.length; i++) if (keys[i] === key || (key !== key && keys[i] !== keys[i])) { found = i; break; }
      if (found < 0) { keys[keys.length] = key; groups[groups.length] = [value]; }
      else { var g = groups[found]; g[g.length] = value; }
    }
    return { keys: keys, groups: groups };
  }
  function toPropertyKey(v) { var s = { [v]: 0 }; return Object.getOwnPropertySymbols(s).length ? Object.getOwnPropertySymbols(s)[0] : Object.keys(s)[0]; }
  var statics = {
    groupBy(items, callback) {
      var r = groupBy(items, callback, true), o = create(null);
      for (var i = 0; i < r.keys.length; i++) createDataProperty(o, r.keys[i], r.groups[i]);
      return o;
    }
  };
  def(Object, "groupBy", statics.groupBy);
  var mapStatics = {
    groupBy(items, callback) {
      var r = groupBy(items, callback, false), m = new MapCtor();
      for (var i = 0; i < r.keys.length; i++) apply(mapSet, m, [r.keys[i], r.groups[i]]);
      return m;
    }
  };
  def(Map, "groupBy", mapStatics.groupBy);

  // Map.prototype.getOrInsert / getOrInsertComputed, and the WeakMap ones
  var mapMethods = {
    getOrInsert(key, value) {
      apply(mapHas, this, [key]);   // RequireInternalSlot [[MapData]]
      if (key === 0) key = 0;
      if (apply(mapHas, this, [key])) return apply(mapGet, this, [key]);
      apply(mapSet, this, [key, value]);
      return value;
    },
    getOrInsertComputed(key, callbackfn) {
      apply(mapHas, this, [key]);
      if (typeof callbackfn !== "function") throw new TypeErr("getOrInsertComputed: the callback is not a function");
      if (key === 0) key = 0;
      if (apply(mapHas, this, [key])) return apply(mapGet, this, [key]);
      var value = callbackfn(key);
      apply(mapSet, this, [key, value]);
      return value;
    }
  };
  def(MP, "getOrInsert", mapMethods.getOrInsert);
  def(MP, "getOrInsertComputed", mapMethods.getOrInsertComputed);
  // 24.3 WeakMap and 24.4 WeakSet (entries held strongly: G2 has no ephemerons yet), over captured Map / Set
  var SetCtor = Set, SP = Set.prototype, setHas = SP.has, setAdd = SP.add, setDelete = SP.delete, mapDelete = MP.delete;
  function canHoldWeakly(v) { return isObject(v) || (typeof v === "symbol" && Symbol.keyFor(v) === undefined); }
  // 24.1.1.2 AddEntriesFromIterable / the WeakSet constructor's loop
  function addEntries(target, iterable, adder, pairs) {
    var m = getMethod(iterable, SymIterator);
    if (m === undefined) throw new TypeErr("object is not iterable");
    var it = apply(m, iterable, []);
    if (!isObject(it)) throw new TypeErr("the iterator is not an object");
    var next = it.next;
    for (;;) {
      var r = apply(next, it, []);
      if (!isObject(r)) throw new TypeErr("the iterator result is not an object");
      if (r.done) return target;
      var v = r.value;
      try {
        if (pairs) {
          if (!isObject(v)) throw new TypeErr("Iterator value " + String(v) + " is not an entry object");
          apply(adder, target, [v[0], v[1]]);
        } else apply(adder, target, [v]);
      } catch (e) {
        var rf = it.return;
        if (rf !== undefined && rf !== null) { try { apply(rf, it, []); } catch (e2) {} }
        throw e;
      }
    }
  }
  class WeakMap {
    #m = new MapCtor();
    constructor() {
      var iterable = arguments.length > 0 ? arguments[0] : undefined;
      if (iterable === undefined || iterable === null) return;
      var adder = this.set;
      if (typeof adder !== "function") throw new TypeErr("WeakMap.prototype.set is not a function");
      addEntries(this, iterable, adder, true);
    }
    static #check(o, name) { if (!isObject(o) || !(#m in o)) throw new TypeErr("WeakMap.prototype." + name + " called on an incompatible receiver"); }
    delete(key) { WeakMap.#check(this, "delete"); if (!canHoldWeakly(key)) return false; return apply(mapDelete, this.#m, [key]); }
    get(key) { WeakMap.#check(this, "get"); if (!canHoldWeakly(key)) return undefined; return apply(mapGet, this.#m, [key]); }
    has(key) { WeakMap.#check(this, "has"); if (!canHoldWeakly(key)) return false; return apply(mapHas, this.#m, [key]); }
    set(key, value) {
      WeakMap.#check(this, "set");
      if (!canHoldWeakly(key)) throw new TypeErr("Invalid value used as weak map key");
      apply(mapSet, this.#m, [key, value]);
      return this;
    }
    getOrInsert(key, value) {
      WeakMap.#check(this, "getOrInsert");
      if (!canHoldWeakly(key)) throw new TypeErr("Invalid value used as weak map key");
      if (apply(mapHas, this.#m, [key])) return apply(mapGet, this.#m, [key]);
      apply(mapSet, this.#m, [key, value]);
      return value;
    }
    getOrInsertComputed(key, callbackfn) {
      WeakMap.#check(this, "getOrInsertComputed");
      if (!canHoldWeakly(key)) throw new TypeErr("Invalid value used as weak map key");
      if (typeof callbackfn !== "function") throw new TypeErr("getOrInsertComputed: the callback is not a function");
      if (apply(mapHas, this.#m, [key])) return apply(mapGet, this.#m, [key]);
      var value = callbackfn(key);
      apply(mapSet, this.#m, [key, value]);
      return value;
    }
  }
  class WeakSet {
    #s = new SetCtor();
    constructor() {
      var iterable = arguments.length > 0 ? arguments[0] : undefined;
      if (iterable === undefined || iterable === null) return;
      var adder = this.add;
      if (typeof adder !== "function") throw new TypeErr("WeakSet.prototype.add is not a function");
      addEntries(this, iterable, adder, false);
    }
    static #check(o, name) { if (!isObject(o) || !(#s in o)) throw new TypeErr("WeakSet.prototype." + name + " called on an incompatible receiver"); }
    add(value) {
      WeakSet.#check(this, "add");
      if (!canHoldWeakly(value)) throw new TypeErr("Invalid value used in weak set");
      apply(setAdd, this.#s, [value]);
      return this;
    }
    delete(value) { WeakSet.#check(this, "delete"); if (!canHoldWeakly(value)) return false; return apply(setDelete, this.#s, [value]); }
    has(value) { WeakSet.#check(this, "has"); if (!canHoldWeakly(value)) return false; return apply(setHas, this.#s, [value]); }
  }
  // 24.2.4 Set.prototype union / intersection / difference / symmetricDifference / isSubsetOf / isSupersetOf /
  // isDisjointFrom, with GetSetRecord (24.2.1.2)
  var setSize = Object.getOwnPropertyDescriptor(SP, "size").get, setValues = SP.values;
  function thisSet(o) { apply(setSize, o, []); return o; }   // RequireInternalSlot [[SetData]]
  function setRecord(obj) {
    if (!isObject(obj)) throw new TypeErr("the argument is not an object");
    var raw = obj.size, num = +raw;
    if (num !== num) throw new TypeErr("the argument's size is not a number");
    var size = num === Infinity || num === -Infinity ? num : (num < 0 ? -Math.floor(-num) : Math.floor(num));
    if (size === 0) size = 0;
    if (size < 0) throw new RangeError("the argument's size is negative");
    var has = obj.has;
    if (typeof has !== "function") throw new TypeErr("the argument's has is not a function");
    var keys = obj.keys;
    if (typeof keys !== "function") throw new TypeErr("the argument's keys is not a function");
    return { set: obj, size: size, has: has, keys: keys };
  }
  function keysIter(rec) {
    var it = apply(rec.keys, rec.set, []);
    if (!isObject(it)) throw new TypeErr("keys() did not return an object");
    return { it: it, next: it.next };
  }
  function stepValue(k) {
    var r = apply(k.next, k.it, []);
    if (!isObject(r)) throw new TypeErr("the iterator result is not an object");
    if (r.done) return DONE;
    return r.value;
  }
  var DONE = {};
  function canon(v) { return v === 0 ? 0 : v; }
  function closeIt(k) { var f = k.it.return; if (f !== undefined && f !== null) { var r = apply(f, k.it, []); if (!isObject(r)) throw new TypeErr("return() result is not an object"); } }
  function copySet(o) { var r = new SetCtor(), it = apply(setValues, o, []), n; while (!(n = it.next()).done) apply(setAdd, r, [n.value]); return r; }
  function eachOf(o, f) { var it = apply(setValues, o, []), n; while (!(n = it.next()).done) if (f(n.value) === false) return false; return true; }
  var setMethods = {
    union(other) {
      var O = thisSet(this), rec = setRecord(other), k = keysIter(rec), r = copySet(O), v;
      while ((v = stepValue(k)) !== DONE) apply(setAdd, r, [canon(v)]);
      return r;
    },
    intersection(other) {
      var O = thisSet(this), rec = setRecord(other), r = new SetCtor(), v;
      if (apply(setSize, O, []) <= rec.size) {
        eachOf(O, function (e) { if (apply(rec.has, rec.set, [e])) apply(setAdd, r, [e]); });
      } else {
        var k = keysIter(rec);
        while ((v = stepValue(k)) !== DONE) { v = canon(v); if (apply(setHas, O, [v])) apply(setAdd, r, [v]); }
      }
      return r;
    },
    difference(other) {
      var O = thisSet(this), rec = setRecord(other), r = copySet(O), v;
      if (apply(setSize, O, []) <= rec.size) {
        eachOf(O, function (e) { if (apply(rec.has, rec.set, [e])) apply(setDelete, r, [e]); });
      } else {
        var k = keysIter(rec);
        while ((v = stepValue(k)) !== DONE) apply(setDelete, r, [canon(v)]);
      }
      return r;
    },
    symmetricDifference(other) {
      var O = thisSet(this), rec = setRecord(other), k = keysIter(rec), r = copySet(O), v;
      while ((v = stepValue(k)) !== DONE) {
        v = canon(v);
        var already = apply(setHas, r, [v]);
        if (apply(setHas, O, [v])) { if (already) apply(setDelete, r, [v]); }
        else if (!already) apply(setAdd, r, [v]);
      }
      return r;
    },
    isSubsetOf(other) {
      var O = thisSet(this), rec = setRecord(other);
      if (apply(setSize, O, []) > rec.size) return false;
      return eachOf(O, function (e) { if (!apply(rec.has, rec.set, [e])) return false; });
    },
    isSupersetOf(other) {
      var O = thisSet(this), rec = setRecord(other), v;
      if (apply(setSize, O, []) < rec.size) return false;
      var k = keysIter(rec);
      while ((v = stepValue(k)) !== DONE) if (!apply(setHas, O, [v])) { closeIt(k); return false; }
      return true;
    },
    isDisjointFrom(other) {
      var O = thisSet(this), rec = setRecord(other), v;
      if (apply(setSize, O, []) <= rec.size) {
        return eachOf(O, function (e) { if (apply(rec.has, rec.set, [e])) return false; });
      }
      var k = keysIter(rec);
      while ((v = stepValue(k)) !== DONE) if (apply(setHas, O, [v])) { closeIt(k); return false; }
      return true;
    }
  };
  var smk = Object.keys(setMethods);
  for (var si = 0; si < smk.length; si++) def(SP, smk[si], setMethods[smk[si]]);

  defineProperty(WeakMap.prototype, Symbol.toStringTag, { value: "WeakMap", writable: false, enumerable: false, configurable: true });
  defineProperty(WeakSet.prototype, Symbol.toStringTag, { value: "WeakSet", writable: false, enumerable: false, configurable: true });
  def(G, "WeakMap", WeakMap);
  def(G, "WeakSet", WeakSet);

  // Await Dictionary (stage 3): Promise.allKeyed / Promise.allSettledKeyed ( promises )
  var ownKeys = Reflect.ownKeys, getOwnPD = Reflect.getOwnPropertyDescriptor;
  // 27.2.1.5 NewPromiseCapability (the executor is an anonymous built-in-like function of length 2)
  function newPromiseCapability(C) {
    if (!isConstructor(C)) throw new TypeErr("Promise capability constructor is not a constructor");
    var resolve, reject;
    var executor = (0, function (res, rej) {
      if (resolve !== undefined) throw new TypeErr("Promise capability resolve already set");
      if (reject !== undefined) throw new TypeErr("Promise capability reject already set");
      resolve = res;
      reject = rej;
    });
    var promise = construct(C, [executor]);
    if (typeof resolve !== "function") throw new TypeErr("Promise capability resolve is not callable");
    if (typeof reject !== "function") throw new TypeErr("Promise capability reject is not callable");
    return { promise: promise, resolve: resolve, reject: reject };
  }
  function keyedResult(keys, values) {
    var r = create(null);
    for (var i = 0; i < keys.length; i++) createDataProperty(r, keys[i], values[i]);
    return r;
  }
  function keyedCombinator(C, promises, settled) {
    var cap = newPromiseCapability(C);
    var promiseResolve;
    try {
      promiseResolve = C.resolve;
      if (typeof promiseResolve !== "function") throw new TypeErr("Promise resolve is not callable");
    } catch (e) {
      apply(cap.reject, undefined, [e]);
      return cap.promise;
    }
    if (!isObject(promises)) {
      apply(cap.reject, undefined, [new TypeErr("Promise." + (settled ? "allSettledKeyed" : "allKeyed") + " requires an object")]);
      return cap.promise;
    }
    try {
      var allKeys = ownKeys(promises);
      var keys = [], values = [], remaining = { n: 1 }, index = 0;
      for (var i = 0; i < allKeys.length; i++) {
        var key = allKeys[i];
        var desc = getOwnPD(promises, key);
        if (desc === undefined || !desc.enumerable) continue;
        var value = promises[key];
        keys[index] = key;
        values[index] = undefined;
        var nextPromise = apply(promiseResolve, C, [value]);
        var onFulfilled, onRejected;
        if (settled) {
          var state = { called: false };   // [[AlreadyCalled]], shared by the element's two functions
          onFulfilled = settleElement(index, values, keys, cap, remaining, "fulfilled", state);
          onRejected = settleElement(index, values, keys, cap, remaining, "rejected", state);
        } else {
          onFulfilled = resolveElement(index, values, keys, cap, remaining);
          onRejected = cap.reject;
        }
        remaining.n++;
        var then = nextPromise.then;
        apply(then, nextPromise, [onFulfilled, onRejected]);
        index++;
      }
      if (--remaining.n === 0) apply(cap.resolve, undefined, [keyedResult(keys, values)]);
    } catch (e) {
      apply(cap.reject, undefined, [e]);
    }
    return cap.promise;
  }
  function resolveElement(index, values, keys, cap, remaining) {
    var alreadyCalled = false;
    return (0, (x) => {
      if (alreadyCalled) return undefined;
      alreadyCalled = true;
      values[index] = x;
      if (--remaining.n === 0) return apply(cap.resolve, undefined, [keyedResult(keys, values)]);
      return undefined;
    });
  }
  function settleElement(index, values, keys, cap, remaining, status, state) {
    return (0, (x) => {
      if (state.called) return undefined;
      state.called = true;
      var obj = {};
      createDataProperty(obj, "status", status);
      createDataProperty(obj, status === "fulfilled" ? "value" : "reason", x);
      values[index] = obj;
      if (--remaining.n === 0) return apply(cap.resolve, undefined, [keyedResult(keys, values)]);
      return undefined;
    });
  }
  var promiseStatics = {
    allKeyed(promises) { return keyedCombinator(this, promises, false); },
    allSettledKeyed(promises) { return keyedCombinator(this, promises, true); }
  };
  def(PromiseCtor, "allKeyed", promiseStatics.allKeyed);
  def(PromiseCtor, "allSettledKeyed", promiseStatics.allSettledKeyed);

  // 23.3 Uint8Array base64 / hex (ES2026)
  var U8 = Uint8Array, TAP = Object.getPrototypeOf(U8.prototype);
  var getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  var taTag = getOwnPropertyDescriptor(TAP, Symbol.toStringTag).get;
  var taLength = getOwnPropertyDescriptor(TAP, "length").get;
  var taBuffer = getOwnPropertyDescriptor(TAP, "buffer").get;
  var abDetached = getOwnPropertyDescriptor(ArrayBuffer.prototype, "detached").get;
  var B64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  var B64URL = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
  var B64VAL = [];
  for (var bi = 0; bi < 64; bi++) B64VAL[B64.charCodeAt(bi)] = bi;
  var SyntaxErr = SyntaxError;
  // 23.3.3.1 ValidateUint8Array, ValidateTypedArrayBounds (a detached buffer)
  function validateU8(ta) {
    if (apply(taTag, ta, []) !== "Uint8Array") throw new TypeErr("not a Uint8Array");
  }
  function boundedLength(ta) {
    if (apply(abDetached, apply(taBuffer, ta, []), [])) throw new TypeErr("the Uint8Array's buffer is detached");
    return apply(taLength, ta, []);
  }
  // 14.? GetOptionsObject
  function optionsObject(o) {
    if (o === undefined) return create(null);
    if (isObject(o)) return o;
    throw new TypeErr("options must be an object");
  }
  function alphabetOf(opts) {
    var a = opts.alphabet;
    if (a === undefined) a = "base64";
    if (a !== "base64" && a !== "base64url") throw new TypeErr("alphabet must be \"base64\" or \"base64url\"");
    return a;
  }
  function lastChunkOf(opts) {
    var l = opts.lastChunkHandling;
    if (l === undefined) l = "loose";
    if (l !== "loose" && l !== "strict" && l !== "stop-before-partial") throw new TypeErr("invalid lastChunkHandling");
    return l;
  }
  function isAsciiWs(c) { return c === 0x09 || c === 0x0A || c === 0x0C || c === 0x0D || c === 0x20; }
  // 23.3.3.7 FromBase64: { read, bytes, error }
  function fromBase64(string, alphabet, lastChunkHandling, maxLength) {
    if (maxLength === undefined) maxLength = 9007199254740991;
    var bytes = [];
    if (maxLength === 0) return { read: 0, bytes: bytes, error: null };
    var read = 0, chunk = [], index = 0, length = string.length;
    function fail() { return { read: read, bytes: bytes, error: new SyntaxErr("invalid base64 string") }; }
    function decodeChunk(c, throwOnExtraBits) {
      var n = c.length;
      var v = (c[0] << 18) | (c[1] << 12) | ((n > 2 ? c[2] : 0) << 6) | (n > 3 ? c[3] : 0);
      var b0 = (v >> 16) & 255, b1 = (v >> 8) & 255, b2 = v & 255;
      if (n === 4) { bytes.push(b0, b1, b2); return true; }
      if (n === 2) { if (throwOnExtraBits && b1 !== 0) return false; bytes.push(b0); return true; }
      if (throwOnExtraBits && b2 !== 0) return false;
      bytes.push(b0, b1);
      return true;
    }
    for (;;) {
      while (index < length && isAsciiWs(string.charCodeAt(index))) index++;
      if (index === length) {
        if (chunk.length > 0) {
          if (lastChunkHandling === "stop-before-partial") return { read: read, bytes: bytes, error: null };
          if (lastChunkHandling === "strict") return fail();
          if (chunk.length === 1) return fail();
          decodeChunk(chunk, false);
        }
        return { read: length, bytes: bytes, error: null };
      }
      var c = string.charCodeAt(index);
      index++;
      if (c === 0x3D) {
        if (chunk.length < 2) return fail();
        while (index < length && isAsciiWs(string.charCodeAt(index))) index++;
        if (chunk.length === 2) {
          if (index === length) {
            if (lastChunkHandling === "stop-before-partial") return { read: read, bytes: bytes, error: null };
            return fail();
          }
          if (string.charCodeAt(index) === 0x3D) {
            index++;
            while (index < length && isAsciiWs(string.charCodeAt(index))) index++;
          }
        }
        if (index < length) return fail();
        if (!decodeChunk(chunk, lastChunkHandling === "strict")) return fail();
        return { read: length, bytes: bytes, error: null };
      }
      if (alphabet === "base64url") {
        if (c === 0x2B || c === 0x2F) return fail();
        if (c === 0x2D) c = 0x2B; else if (c === 0x5F) c = 0x2F;
      }
      var v = B64VAL[c];
      if (v === undefined) return fail();
      var remaining = maxLength - bytes.length;
      if ((remaining === 1 && chunk.length === 2) || (remaining === 2 && chunk.length === 3))
        return { read: read, bytes: bytes, error: null };
      chunk.push(v);
      if (chunk.length === 4) {
        decodeChunk(chunk, false);
        chunk = [];
        read = index;
        if (bytes.length === maxLength) return { read: read, bytes: bytes, error: null };
      }
    }
  }
  // 23.3.3.8 FromHex
  function hexVal(c) {
    if (c >= 0x30 && c <= 0x39) return c - 0x30;
    if (c >= 0x61 && c <= 0x66) return c - 0x57;
    if (c >= 0x41 && c <= 0x46) return c - 0x37;
    return -1;
  }
  function fromHex(string, maxLength) {
    if (maxLength === undefined) maxLength = 9007199254740991;
    var length = string.length, bytes = [], read = 0;
    if (length % 2 !== 0) return { read: 0, bytes: bytes, error: new SyntaxErr("odd number of hex digits") };
    while (read < length && bytes.length < maxLength) {
      var h = hexVal(string.charCodeAt(read)), l = hexVal(string.charCodeAt(read + 1));
      if (h < 0 || l < 0) return { read: read, bytes: bytes, error: new SyntaxErr("invalid hex digit") };
      read += 2;
      bytes.push(h * 16 + l);
    }
    return { read: read, bytes: bytes, error: null };
  }
  function newU8(bytes) {
    var ta = new U8(bytes.length);
    for (var i = 0; i < bytes.length; i++) ta[i] = bytes[i];
    return ta;
  }
  function resultObj(read, written) { var o = {}; o.read = read; o.written = written; return o; }
  var u8Statics = {
    fromBase64(string, options) {
      if (typeof string !== "string") throw new TypeErr("Uint8Array.fromBase64 requires a string");
      var opts = optionsObject(options);
      var alphabet = alphabetOf(opts), lch = lastChunkOf(opts);
      var r = fromBase64(string, alphabet, lch);
      if (r.error) throw r.error;
      return newU8(r.bytes);
    },
    fromHex(string) {
      if (typeof string !== "string") throw new TypeErr("Uint8Array.fromHex requires a string");
      var r = fromHex(string);
      if (r.error) throw r.error;
      return newU8(r.bytes);
    }
  };
  var u8Methods = {
    setFromBase64(string, options) {
      var into = this;
      validateU8(into);
      if (typeof string !== "string") throw new TypeErr("setFromBase64 requires a string");
      var opts = optionsObject(options);
      var alphabet = alphabetOf(opts), lch = lastChunkOf(opts);
      var byteLength = boundedLength(into);
      var r = fromBase64(string, alphabet, lch, byteLength);
      for (var i = 0; i < r.bytes.length; i++) into[i] = r.bytes[i];
      if (r.error) throw r.error;
      return resultObj(r.read, r.bytes.length);
    },
    setFromHex(string) {
      var into = this;
      validateU8(into);
      if (typeof string !== "string") throw new TypeErr("setFromHex requires a string");
      var byteLength = boundedLength(into);
      var r = fromHex(string, byteLength);
      for (var i = 0; i < r.bytes.length; i++) into[i] = r.bytes[i];
      if (r.error) throw r.error;
      return resultObj(r.read, r.bytes.length);
    },
    toBase64(options) {
      var o = this;
      validateU8(o);
      var opts = optionsObject(options);
      var alphabet = alphabetOf(opts);
      var omitPadding = !!opts.omitPadding;
      var n = boundedLength(o), A = alphabet === "base64" ? B64 : B64URL, out = "", i = 0;
      for (; i + 2 < n; i += 3) {
        var v = (o[i] << 16) | (o[i + 1] << 8) | o[i + 2];
        out += A[(v >> 18) & 63] + A[(v >> 12) & 63] + A[(v >> 6) & 63] + A[v & 63];
      }
      if (n - i === 1) {
        var v1 = o[i] << 16;
        out += A[(v1 >> 18) & 63] + A[(v1 >> 12) & 63] + (omitPadding ? "" : "==");
      } else if (n - i === 2) {
        var v2 = (o[i] << 16) | (o[i + 1] << 8);
        out += A[(v2 >> 18) & 63] + A[(v2 >> 12) & 63] + A[(v2 >> 6) & 63] + (omitPadding ? "" : "=");
      }
      return out;
    },
    toHex() {
      var o = this;
      validateU8(o);
      var n = boundedLength(o), out = "", H = "0123456789abcdef";
      for (var i = 0; i < n; i++) out += H[o[i] >> 4] + H[o[i] & 15];
      return out;
    }
  };
  // the optional options parameter is not counted in length
  defineProperty(u8Statics.fromBase64, "length", { value: 1, writable: false, enumerable: false, configurable: true });
  defineProperty(u8Methods.setFromBase64, "length", { value: 1, writable: false, enumerable: false, configurable: true });
  defineProperty(u8Methods.toBase64, "length", { value: 0, writable: false, enumerable: false, configurable: true });
  def(U8, "fromBase64", u8Statics.fromBase64);
  def(U8, "fromHex", u8Statics.fromHex);
  var u8k = ["setFromBase64", "setFromHex", "toBase64", "toHex"];
  for (var ui = 0; ui < u8k.length; ui++) def(U8.prototype, u8k[ui], u8Methods[u8k[ui]]);

})
