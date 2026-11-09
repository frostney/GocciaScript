(function (G) {
  "use strict";
  // ECMA-262 27.1 Iterator, %Iterator.prototype% helpers, %IteratorHelperPrototype%, %WrapForValidIteratorPrototype%,
  // Iterator.from / concat / zip / zipKeyed. A helper is a generator (the spec's CreateIteratorFromClosure) behind an
  // %IteratorHelperPrototype% object whose internal slots are private fields; the intrinsics used are captured here,
  // so later changes to the globals are not observed.
  var apply = Reflect.apply, defineProperty = Object.defineProperty, getProto = Object.getPrototypeOf;
  var create = Object.create, ownKeys = Reflect.ownKeys, getOwnDesc = Reflect.getOwnPropertyDescriptor;
  var isProtoOf = Object.prototype.isPrototypeOf, isInteger = Number.isInteger, trunc = Math.trunc;
  var TypeErr = TypeError, RangeErr = RangeError, Str = String;
  var SymIterator = Symbol.iterator, SymToStringTag = Symbol.toStringTag, SymDispose = Symbol.dispose;
  var IterProto = getProto(getProto([][SymIterator]()));
  var GenProto = getProto(function* () {}).prototype;
  var genNext = GenProto.next, genReturn = GenProto.return;
  var DONE = {};

  function isObject(v) { return (typeof v === "object" && v !== null) || typeof v === "function"; }
  function def(o, k, v) { defineProperty(o, k, { value: v, writable: true, enumerable: false, configurable: true }); }
  function sameValueZero(a, b) { return a === b || (a !== a && b !== b); }

  // 7.3.11 GetMethod
  function getMethod(v, k) {
    var f = v[k];
    if (f === undefined || f === null) return undefined;
    if (typeof f !== "function") throw new TypeErr("not a function");
    return f;
  }
  // 7.4.2 GetIteratorDirect
  function direct(obj) { return { it: obj, next: obj.next, done: false }; }
  // 7.4.10 IteratorStepValue (and IteratorStep when wantValue is false)
  function step(rec, wantValue) {
    var r, d;
    try { r = apply(rec.next, rec.it, []); } catch (e) { rec.done = true; throw e; }
    if (!isObject(r)) { rec.done = true; throw new TypeErr("iterator result is not an object"); }
    try { d = r.done; } catch (e) { rec.done = true; throw e; }
    if (d) { rec.done = true; return DONE; }
    if (!wantValue) return true;
    try { return r.value; } catch (e) { rec.done = true; throw e; }
  }
  function stepValue(rec) { return step(rec, true); }
  // 7.4.11 IteratorClose with a throw completion: errors of return() are dropped
  function closeThrow(it) {
    try { var f = getMethod(it, "return"); if (f !== undefined) apply(f, it, []); } catch (e) {}
  }
  // 7.4.11 IteratorClose with a normal or return completion
  function closeNormal(it) {
    var f = getMethod(it, "return");
    if (f === undefined) return;
    var r = apply(f, it, []);
    if (!isObject(r)) throw new TypeErr("iterator return() result is not an object");
  }
  // 7.4.12 IteratorCloseAll (reverse order); err holds a pending throw as { e }
  function closeAll(recs, err) {
    for (var i = recs.length - 1; i >= 0; i--) {
      if (err) closeThrow(recs[i].it);
      else { try { closeNormal(recs[i].it); } catch (e) { err = { e: e }; } }
    }
    if (err) throw err.e;
  }
  function removeRec(list, rec) {
    for (var i = 0; i < list.length; i++) if (list[i] === rec) { for (var j = i; j < list.length - 1; j++) list[j] = list[j + 1]; list.length--; return; }
  }
  // 7.4.3 GetIteratorFlattenable
  function flattenable(obj, allowStrings) {
    if (!isObject(obj) && !(allowStrings && typeof obj === "string")) throw new TypeErr("not an iterable or iterator object");
    var m = getMethod(obj, SymIterator), it;
    if (m === undefined) it = obj;
    else it = apply(m, obj, []);
    if (!isObject(it)) throw new TypeErr("iterator is not an object");
    return direct(it);
  }
  // 7.4.2 GetIterator(obj, sync)
  function getIterator(obj) {
    var m = getMethod(obj, SymIterator);
    if (m === undefined) throw new TypeErr("object is not iterable");
    var it = apply(m, obj, []);
    if (!isObject(it)) throw new TypeErr("iterator is not an object");
    return direct(it);
  }
  function copyList(list, first) { var a = [], n = 0; if (first !== undefined) a[n++] = first; for (var i = 0; i < list.length; i++) a[n++] = list[i]; return a; }
  function arrayOf(list) { var a = [], n = list.length; for (var i = 0; i < n; i++) defineProperty(a, i, { value: list[i], writable: true, enumerable: true, configurable: true }); return a; }

  // %IteratorHelperPrototype% objects and %WrapForValidIteratorPrototype% objects: internal slots as private fields
  class Stamp { constructor(o) { return o; } }
  class Helper extends Stamp {
    #gen; #under; #started = false;
    constructor(o, gen, under) { super(o); this.#gen = gen; this.#under = under; }
    // 27.1.2.1.1
    static next() {
      if (!isObject(this) || !(#gen in this)) throw new TypeErr("next method called on an incompatible receiver");
      this.#started = true;
      return apply(genNext, this.#gen, []);
    }
    // 27.1.2.1.2
    static return() {
      if (!isObject(this) || !(#gen in this)) throw new TypeErr("return method called on an incompatible receiver");
      if (!this.#started) {
        this.#started = true;
        apply(genReturn, this.#gen, [undefined]);   // suspended-start: completed without running
        closeAll(this.#under, undefined);
        return { value: undefined, done: true };
      }
      return apply(genReturn, this.#gen, [undefined]);
    }
  }
  var HelperProto = create(IterProto);
  def(HelperProto, "next", Helper.next);
  def(HelperProto, "return", Helper.return);
  defineProperty(HelperProto, SymToStringTag, { value: "Iterator Helper", writable: false, enumerable: false, configurable: true });
  function helper(genFn, args, under) { var h = create(HelperProto); new Helper(h, apply(genFn, undefined, args), under); return h; }

  class Wrap extends Stamp {
    #rec;
    constructor(o, rec) { super(o); this.#rec = rec; }
    // 27.1.3.2.2.1.1
    static next() {
      if (!isObject(this) || !(#rec in this)) throw new TypeErr("next method called on an incompatible receiver");
      var rec = this.#rec;
      return apply(rec.next, rec.it, []);
    }
    // 27.1.3.2.2.1.2
    static return() {
      if (!isObject(this) || !(#rec in this)) throw new TypeErr("return method called on an incompatible receiver");
      var it = this.#rec.it, f = getMethod(it, "return");
      if (f === undefined) return { value: undefined, done: true };
      return apply(f, it, []);
    }
  }
  var WrapProto = create(IterProto);
  def(WrapProto, "next", Wrap.next);
  def(WrapProto, "return", Wrap.return);

  // the helper closures (a return() at a yield runs the finally, which closes the underlying iterator)
  function* mapGen(rec, fn) {
    for (var counter = 0; ; counter++) {
      var v = stepValue(rec);
      if (v === DONE) return;
      var m;
      try { m = fn(v, counter); } catch (e) { closeThrow(rec.it); throw e; }
      var ok = false;
      try { yield m; ok = true; } finally { if (!ok) closeNormal(rec.it); }
    }
  }
  function* filterGen(rec, fn) {
    for (var counter = 0; ; counter++) {
      var v = stepValue(rec);
      if (v === DONE) return;
      var sel;
      try { sel = fn(v, counter); } catch (e) { closeThrow(rec.it); throw e; }
      if (sel) {
        var ok = false;
        try { yield v; ok = true; } finally { if (!ok) closeNormal(rec.it); }
      }
    }
  }
  function* takeGen(rec, remaining) {
    for (;;) {
      if (remaining === 0) { closeNormal(rec.it); return; }
      if (remaining !== Infinity) remaining--;
      var v = stepValue(rec);
      if (v === DONE) return;
      var ok = false;
      try { yield v; ok = true; } finally { if (!ok) closeNormal(rec.it); }
    }
  }
  function* dropGen(rec, remaining) {
    while (remaining > 0) {
      if (remaining !== Infinity) remaining--;
      if (step(rec, false) === DONE) return;
    }
    for (;;) {
      var v = stepValue(rec);
      if (v === DONE) return;
      var ok = false;
      try { yield v; ok = true; } finally { if (!ok) closeNormal(rec.it); }
    }
  }
  function* flatMapGen(rec, fn) {
    for (var counter = 0; ; counter++) {
      var v = stepValue(rec);
      if (v === DONE) return;
      var inner;
      try { inner = flattenable(fn(v, counter), false); } catch (e) { closeThrow(rec.it); throw e; }
      for (;;) {
        var iv;
        try { iv = stepValue(inner); } catch (e) { closeThrow(rec.it); throw e; }
        if (iv === DONE) break;
        var ok = false;
        try { yield iv; ok = true; } finally {
          if (!ok) {
            try { closeNormal(inner.it); } catch (e) { closeThrow(rec.it); throw e; }
            closeNormal(rec.it);
          }
        }
      }
    }
  }
  function* chunksGen(rec, size) {
    var buf = [];
    for (;;) {
      var v = stepValue(rec);
      if (v === DONE) { if (buf.length > 0) yield arrayOf(buf); return; }
      buf[buf.length] = v;
      if (buf.length === size) {
        var ok = false, out = arrayOf(buf);
        buf = [];
        try { yield out; ok = true; } finally { if (!ok) closeNormal(rec.it); }
      }
    }
  }
  function* windowsGen(rec, size, partial) {
    var buf = [];
    for (;;) {
      var v = stepValue(rec);
      if (v === DONE) { if (partial && buf.length > 0 && buf.length < size) yield arrayOf(buf); return; }
      if (buf.length === size) { for (var i = 0; i < size - 1; i++) buf[i] = buf[i + 1]; buf.length = size - 1; }
      buf[buf.length] = v;
      if (buf.length === size) {
        var ok = false;
        try { yield arrayOf(buf); ok = true; } finally { if (!ok) closeNormal(rec.it); }
      }
    }
  }
  function* concatGen(iterables) {
    for (var i = 0; i < iterables.length; i++) {
      var it = apply(iterables[i].open, iterables[i].iterable, []);
      if (!isObject(it)) throw new TypeErr("iterator is not an object");
      var rec = direct(it);
      for (;;) {
        var v = stepValue(rec);
        if (v === DONE) break;
        var ok = false;
        try { yield v; ok = true; } finally { if (!ok) closeNormal(rec.it); }
      }
    }
  }
  // 27.1.3.4.1 IteratorZip
  function* zipGen(iters, mode, padding, finish, open) {
    var n = iters.length;
    if (n === 0) return;
    for (;;) {
      var results = [];
      for (var i = 0; i < n; i++) {
        var rec = iters[i], r;
        if (rec === null) r = padding[i];
        else {
          try { r = stepValue(rec); } catch (e) { removeRec(open, rec); closeAll(open, { e: e }); }
          if (r === DONE) {
            removeRec(open, rec);
            if (mode === "shortest") { closeAll(open, undefined); return; }
            if (mode === "strict") {
              if (i !== 0) closeAll(open, { e: new TypeErr("Iterator.zip: iterators of different lengths in strict mode") });
              for (var k = 1; k < n; k++) {
                var o;
                try { o = step(iters[k], false); } catch (e) { removeRec(open, iters[k]); closeAll(open, { e: e }); }
                if (o === DONE) removeRec(open, iters[k]);
                else closeAll(open, { e: new TypeErr("Iterator.zip: iterators of different lengths in strict mode") });
              }
              return;
            }
            if (open.length === 0) return;
            iters[i] = null;
            r = padding[i];
          }
        }
        results[i] = r;
      }
      var ok = false;
      try { yield finish(results); ok = true; } finally { if (!ok) closeAll(open, undefined); }
    }
  }

  function thisObj(o) { if (!isObject(o)) throw new TypeErr("Iterator.prototype method called on a non-object"); return o; }
  function fail(o, E, msg) { closeThrow(o); throw new E(msg); }
  function needFn(o, f) { if (typeof f !== "function") fail(o, TypeErr, "callback is not a function"); }
  function limitOf(o, limit) {
    var n;
    try { n = +limit; } catch (e) { closeThrow(o); throw e; }
    if (n !== n) fail(o, RangeErr, "limit is NaN");
    if (n !== Infinity && n > 9007199254740991) fail(o, RangeErr, "limit is too large");
    n = trunc(n);
    if (n < 0) fail(o, RangeErr, "limit must be non-negative");
    return n;
  }
  function sizeOf(o, size) {
    if (typeof size !== "number" || !isInteger(size)) fail(o, TypeErr, "size is not an integral Number");
    if (size < 1 || size > 4294967295) fail(o, RangeErr, "size out of range");
    return size;
  }

  // 27.1.3.3 %Iterator.prototype% methods
  var methods = {
    map(mapper) { var o = thisObj(this); needFn(o, mapper); var rec = direct(o); return helper(mapGen, [rec, mapper], [rec]); },
    filter(predicate) { var o = thisObj(this); needFn(o, predicate); var rec = direct(o); return helper(filterGen, [rec, predicate], [rec]); },
    take(limit) { var o = thisObj(this), n = limitOf(o, limit), rec = direct(o); return helper(takeGen, [rec, n], [rec]); },
    drop(limit) { var o = thisObj(this), n = limitOf(o, limit), rec = direct(o); return helper(dropGen, [rec, n], [rec]); },
    flatMap(mapper) { var o = thisObj(this); needFn(o, mapper); var rec = direct(o); return helper(flatMapGen, [rec, mapper], [rec]); },
    reduce(reducer) {
      var o = thisObj(this); needFn(o, reducer);
      var rec = direct(o), acc, counter;
      if (arguments.length < 2) {
        acc = stepValue(rec);
        if (acc === DONE) throw new TypeErr("Reduce of empty iterator with no initial value");
        counter = 1;
      } else { acc = arguments[1]; counter = 0; }
      for (;; counter++) {
        var v = stepValue(rec);
        if (v === DONE) return acc;
        try { acc = reducer(acc, v, counter); } catch (e) { closeThrow(rec.it); throw e; }
      }
    },
    toArray() {
      var rec = direct(thisObj(this)), items = [];
      for (;;) { var v = stepValue(rec); if (v === DONE) return arrayOf(items); items[items.length] = v; }
    },
    forEach(fn) {
      var o = thisObj(this); needFn(o, fn);
      var rec = direct(o);
      for (var counter = 0; ; counter++) {
        var v = stepValue(rec);
        if (v === DONE) return undefined;
        try { fn(v, counter); } catch (e) { closeThrow(rec.it); throw e; }
      }
    },
    some(predicate) {
      var o = thisObj(this); needFn(o, predicate);
      var rec = direct(o), r;
      for (var counter = 0; ; counter++) {
        var v = stepValue(rec);
        if (v === DONE) return false;
        try { r = predicate(v, counter); } catch (e) { closeThrow(rec.it); throw e; }
        if (r) { closeNormal(rec.it); return true; }
      }
    },
    every(predicate) {
      var o = thisObj(this); needFn(o, predicate);
      var rec = direct(o), r;
      for (var counter = 0; ; counter++) {
        var v = stepValue(rec);
        if (v === DONE) return true;
        try { r = predicate(v, counter); } catch (e) { closeThrow(rec.it); throw e; }
        if (!r) { closeNormal(rec.it); return false; }
      }
    },
    find(predicate) {
      var o = thisObj(this); needFn(o, predicate);
      var rec = direct(o), r;
      for (var counter = 0; ; counter++) {
        var v = stepValue(rec);
        if (v === DONE) return undefined;
        try { r = predicate(v, counter); } catch (e) { closeThrow(rec.it); throw e; }
        if (r) { closeNormal(rec.it); return v; }
      }
    },
    chunks(chunkSize) { var o = thisObj(this), n = sizeOf(o, chunkSize), rec = direct(o); return helper(chunksGen, [rec, n], [rec]); },
    windows(windowSize) {
      var o = thisObj(this), n = sizeOf(o, windowSize), u = arguments.length > 1 ? arguments[1] : undefined;
      if (u === undefined) u = "only-full";
      if (u !== "only-full" && u !== "allow-partial") fail(o, TypeErr, "undersized must be \"only-full\" or \"allow-partial\"");
      var rec = direct(o);
      return helper(windowsGen, [rec, n, u === "allow-partial"], [rec]);
    },
    includes(searchElement) {
      var o = thisObj(this), skip = arguments.length > 1 ? arguments[1] : undefined, toSkip = 0;
      if (skip !== undefined) {
        if (typeof skip !== "number" || !(skip === Infinity || skip === -Infinity || isInteger(skip))) fail(o, TypeErr, "skippedElements is not an integral Number");
        toSkip = skip;
      }
      if (toSkip < 0) fail(o, RangeErr, "skippedElements must be non-negative");
      if (toSkip !== Infinity && toSkip > 9007199254740991) fail(o, RangeErr, "skippedElements is too large");
      var rec = direct(o), skipped = 0;
      for (;;) {
        var v = stepValue(rec);
        if (v === DONE) return false;
        if (skipped < toSkip) skipped++;
        else if (sameValueZero(v, searchElement)) { closeNormal(rec.it); return true; }
      }
    },
    join(separator) {
      var o = thisObj(this), sep = ",";
      if (separator !== undefined) { try { sep = Str(separator); } catch (e) { closeThrow(o); throw e; } }
      var rec = direct(o), out = "", first = true;
      for (;;) {
        var v = stepValue(rec);
        if (v === DONE) return out;
        if (first) first = false; else out += sep;
        if (v !== undefined && v !== null) {
          if (typeof v === "symbol") { closeThrow(rec.it); throw new TypeErr("Cannot convert a Symbol value to a string"); }
          try { out += Str(v); } catch (e) { closeThrow(rec.it); throw e; }
        }
      }
    },
    // 27.1.3.3.17
    [SymDispose]() { var f = getMethod(this, "return"); if (f !== undefined) apply(f, this, []); }
  };
  var keys = ownKeys(methods);
  for (var i = 0; i < keys.length; i++) def(IterProto, keys[i], methods[keys[i]]);

  // 27.1.3.3.2 / .19 accessors with SetterThatIgnoresPrototypeProperties
  function ignoringSetter(thisV, key, v) {
    if (!isObject(thisV)) throw new TypeErr("setter called on a non-object");
    if (thisV === IterProto) throw new TypeErr("Cannot assign to read only property of Iterator.prototype");
    if (getOwnDesc(thisV, key) === undefined) defineProperty(thisV, key, { value: v, writable: true, enumerable: true, configurable: true });
    else thisV[key] = v;
  }
  var Iterator;
  var acc = {
    get constructor() { return Iterator; },
    set constructor(v) { ignoringSetter(this, "constructor", v); },
    get [SymToStringTag]() { return "Iterator"; },
    set [SymToStringTag](v) { ignoringSetter(this, SymToStringTag, v); }
  };
  var dc = getOwnDesc(acc, "constructor"), dt = getOwnDesc(acc, SymToStringTag);
  defineProperty(IterProto, "constructor", { get: dc.get, set: dc.set, enumerable: false, configurable: true });
  defineProperty(IterProto, SymToStringTag, { get: dt.get, set: dt.set, enumerable: false, configurable: true });

  // 27.1.3.1 the Iterator constructor
  Iterator = function Iterator() {
    if (new.target === undefined || new.target === Iterator) throw new TypeErr("Iterator is an abstract class and cannot be constructed directly");
  };
  defineProperty(Iterator, "prototype", { value: IterProto, writable: false, enumerable: false, configurable: false });

  function getOptions(options) {
    if (options === undefined) return create(null);
    if (!isObject(options)) throw new TypeErr("options must be an object");
    return options;
  }
  function zipMode(options) {
    var mode = options.mode;
    if (mode === undefined) mode = "shortest";
    if (mode !== "shortest" && mode !== "longest" && mode !== "strict") throw new TypeErr("mode must be \"shortest\", \"longest\" or \"strict\"");
    return mode;
  }
  var statics = {
    // 27.1.3.2.2
    from(O) {
      var rec = flattenable(O, true);
      if (apply(isProtoOf, IterProto, [rec.it])) return rec.it;
      var w = create(WrapProto);
      new Wrap(w, rec);
      return w;
    },
    // 27.1.3.2.1
    concat(...items) {
      var iterables = [];
      for (var i = 0; i < items.length; i++) {
        var item = items[i];
        if (!isObject(item)) throw new TypeErr("Iterator.concat: not an object");
        var m = getMethod(item, SymIterator);
        if (m === undefined) throw new TypeErr("Iterator.concat: not iterable");
        iterables[iterables.length] = { open: m, iterable: item };
      }
      return helper(concatGen, [iterables], []);
    },
    // 27.1.3.2.4
    zip(iterables) {
      if (!isObject(iterables)) throw new TypeErr("Iterator.zip: iterables must be an object");
      var options = getOptions(arguments.length > 1 ? arguments[1] : undefined), mode = zipMode(options), padOpt;
      if (mode === "longest") {
        padOpt = options.padding;
        if (padOpt !== undefined && !isObject(padOpt)) throw new TypeErr("padding must be an object");
      }
      var iters = [], padding = [], input = getIterator(iterables), next;
      for (;;) {
        try { next = stepValue(input); } catch (e) { closeAll(iters, { e: e }); }
        if (next === DONE) break;
        var it;
        try { it = flattenable(next, false); } catch (e) { closeAll(copyList(iters, input), { e: e }); }
        iters[iters.length] = it;
      }
      var n = iters.length;
      if (mode === "longest") {
        if (padOpt === undefined) { for (var i = 0; i < n; i++) padding[i] = undefined; }
        else {
          var pi;
          try { pi = getIterator(padOpt); } catch (e) { closeAll(iters, { e: e }); }
          var using = true;
          for (var j = 0; j < n; j++) {
            if (using) {
              var pv;
              try { pv = stepValue(pi); } catch (e) { closeAll(iters, { e: e }); }
              if (pv === DONE) using = false; else padding[j] = pv;
            }
            if (!using) padding[j] = undefined;
          }
          if (using) { try { closeNormal(pi.it); } catch (e) { closeAll(iters, { e: e }); } }
        }
      }
      var open = copyList(iters);
      return helper(zipGen, [iters, mode, padding, arrayOf, open], open);
    },
    // 27.1.3.2.5
    zipKeyed(iterables) {
      if (!isObject(iterables)) throw new TypeErr("Iterator.zipKeyed: iterables must be an object");
      var options = getOptions(arguments.length > 1 ? arguments[1] : undefined), mode = zipMode(options), padOpt;
      if (mode === "longest") {
        padOpt = options.padding;
        if (padOpt !== undefined && !isObject(padOpt)) throw new TypeErr("padding must be an object");
      }
      var iters = [], padding = [], keys = [], all = ownKeys(iterables);
      for (var i = 0; i < all.length; i++) {
        var key = all[i], d, v;
        try { d = getOwnDesc(iterables, key); } catch (e) { closeAll(iters, { e: e }); }
        if (d !== undefined && d.enumerable) {
          try { v = iterables[key]; } catch (e) { closeAll(iters, { e: e }); }
          if (v !== undefined) {
            keys[keys.length] = key;
            var it;
            try { it = flattenable(v, false); } catch (e) { closeAll(iters, { e: e }); }
            iters[iters.length] = it;
          }
        }
      }
      var n = iters.length;
      if (mode === "longest") {
        if (padOpt === undefined) { for (var j = 0; j < n; j++) padding[j] = undefined; }
        else for (var k = 0; k < keys.length; k++) { try { padding[k] = padOpt[keys[k]]; } catch (e) { closeAll(iters, { e: e }); } }
      }
      var finish = function (results) {
        var o = create(null);
        for (var m = 0; m < n; m++) defineProperty(o, keys[m], { value: results[m], writable: true, enumerable: true, configurable: true });
        return o;
      };
      var open = copyList(iters);
      return helper(zipGen, [iters, mode, padding, finish, open], open);
    }
  };
  var sk = ownKeys(statics);
  for (var s = 0; s < sk.length; s++) def(Iterator, sk[s], statics[sk[s]]);
  def(G, "Iterator", Iterator);
})
