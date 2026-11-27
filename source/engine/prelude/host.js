// The CLI host's globals (docs/built-ins.md): the Goccia object, and the global object as the CLIs leave it.
// Goccia.HostAPI calls this function once per realm with the global object, the host's natives (n), the preludes'
// shared internals, the build facts as JSON, and whether the host runs tests and allows ShadowRealm; it returns the Goccia object, which the host binds as
// the const Goccia.
(function (G, n, shared, infoJson, testMode, shadowRealm, configJson) {
  'use strict';
  const gc = n.gc, gcBytes = n.gcBytes, gcMaxBytes = n.gcMaxBytes, gcSuggestedMaxBytes = n.gcSuggestedMaxBytes;
  const defineProperty = Object.defineProperty;
  // what the functions below call, captured before any script runs: a script that replaces a prototype method or a
  // global does not reach into the host's built-ins
  const ObjectCreate = Object.create, ObjectKeys = Object.keys, getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  const ArrayIsArray = Array.isArray, JSONParse = JSON.parse, ReflectApply = Reflect.apply;
  const StringCtor = String, NumberCtor = Number, TypeErrorCtor = TypeError, ErrorCtor = Error;
  const uncurryThis = Function.prototype.bind.bind(Function.prototype.call);
  const StringPrototypeCharCodeAt = uncurryThis(String.prototype.charCodeAt);
  const StringPrototypeSlice = uncurryThis(String.prototype.slice);
  const StringPrototypeToUpperCase = uncurryThis(String.prototype.toUpperCase);
  const StringPrototypePadStart = uncurryThis(String.prototype.padStart);
  const NumberPrototypeToString = uncurryThis(Number.prototype.toString);
  const ArrayPrototypeJoin = uncurryThis(Array.prototype.join);
  const WeakSetPrototypeHas = uncurryThis(WeakSet.prototype.has);
  // a list append that defines the index (a setter on Array.prototype must not see it)
  const append = (list, v) => { defineProperty(list, list.length, { __proto__: null, value: v, writable: true, enumerable: true, configurable: true }); };
  const info = JSON.parse(infoJson);
  // the host's configuration: the goccia.json keys and their values (strings, or arrays of strings)
  shared.config = JSON.parse(configJson);
  const configFlag = (k) => shared.config[k] === 'true';

  // the conformance host's extras the CLIs do not have: print, eval, and the Annex B escape and unescape, which
  // GocciaScript keeps only under compat-annex-b (docs/language.md, Annex B)
  if (!shared.bare) delete G.print;
  delete G.eval;
  if (!shared.annexB) {
    delete G.escape;
    delete G.unescape;
  }
  if (!shadowRealm) delete G.ShadowRealm;
  // GocciaScriptLoaderBare: the core engine without a runtime keeps print and none of the runtime's globals (the
  // conformance host's console, performance and queueMicrotask, and web.js's encoders and structuredClone)
  if (shared.bare) {
    for (const k of ['console', 'performance', 'queueMicrotask', 'TextEncoder', 'TextDecoder', 'structuredClone'])
      delete G[k];
  }

  // a read-only, enumerable, non-configurable data property (the Pascal engines' DefineProperty with [pfEnumerable])
  function fixed(o, k, v) { defineProperty(o, k, { __proto__: null, value: v, writable: false, enumerable: true, configurable: false }); }
  // a getter only, neither enumerable nor configurable, named "get k"
  function getter(o, k, fn) {
    const get = { get [k]() { return fn(); } };
    defineProperty(o, k, { __proto__: null, get: Object.getOwnPropertyDescriptor(get, k).get, enumerable: false, configurable: false });
  }
  function groups(list) {
    const o = {};
    for (const g of list) fixed(o, g.key, g.entries.map(e => ({ name: e.name, link: e.link })));
    return o;
  }

  const build = {};
  fixed(build, 'os', info.os);
  fixed(build, 'arch', info.arch);
  fixed(build, 'date', info.date);
  fixed(build, 'gcStress', info.gcStress);

  getter(gc, 'maxBytes', gcMaxBytes);
  getter(gc, 'suggestedMaxBytes', gcSuggestedMaxBytes);
  getter(gc, 'bytesAllocated', gcBytes);

  // the names the host's runtime extensions install, in their order; the preludes after this one add theirs
  const runtimeGlobals = shared.runtimeGlobals;
  if (testMode) runtimeGlobals.push('TestAssertions');
  // goccia:ast (experimental-ast): the parse tree as Goccia.AST.Report reports it, its objects with null
  // prototypes and its tokens ordinary objects, as the Pascal engines materialize them
  if (configFlag('experimental-ast')) {
    const rebuild = (v, plain) => {
      if (ArrayIsArray(v)) {
        const a = [];
        for (let i = 0; i < v.length; i++) append(a, rebuild(v[i], plain));
        return a;
      }
      if (v === null || typeof v !== 'object') return v;
      const o = plain ? {} : ObjectCreate(null);
      const keys = ObjectKeys(v);
      for (let i = 0; i < keys.length; i++) defineProperty(o, keys[i], { __proto__: null, value: rebuild(v[keys[i]], plain), writable: true, enumerable: true, configurable: true });
      return o;
    };
    const parse = {
      parse(source, options = undefined) {
        if (typeof source !== 'string') throw new TypeErrorCtor('parse expects the source text as a string');
        const o = options !== null && typeof options === 'object' ? options : { __proto__: null };
        const flag = (k, d) => (o[k] === undefined ? d : !!o[k]);
        const fileName = typeof o.fileName === 'string' ? o.fileName : '<source>';
        const raw = JSONParse(n.astParse(source, flag('jsx', false), flag('module', true), flag('tokens', false),
          flag('expressions', false), fileName));
        const result = ObjectCreate(null);
        result.source = raw.source;
        result.comments = rebuild(raw.comments, false);
        result.root = rebuild(raw.root, false);
        if (raw.tokens !== undefined) result.tokens = rebuild(raw.tokens, true);
        return result;
      }
    }.parse;
    shared.hostModules['goccia:ast'] = () => ({ parse });
  }

  // goccia:test outside the test runner: the testing API installed module-only on its first import (the test
  // runner's install registers its own)
  if (!testMode && !shared.bare) shared.hostModules['goccia:test'] = () => n.testingModule();

  // the test runner's vitest compatibility shim, over goccia:test and goccia:timers (--no-vitest-compat: none)
  if (testMode && !configFlag('no-vitest-compat')) shared.hostModules.vitest = () => n.vitestSource();

  // the REPL's value display (Goccia.REPL.Formatter): primitives as literals, functions, promises, errors, regexps,
  // Maps and Sets by their kind, anything else as JSON with a two-space indent; color: ANSI colors
  {
    const MAX_ITEMS = 100;
    const paint = (text, code, color) => (color ? `\x1b[${code}m${text}\x1b[0m` : text);
    const escapeText = (s) => {
      let out = '';
      // by code unit: a surrogate is never escaped, so a pair comes out as it went in
      for (let i = 0; i < s.length; i++) {
        const c = s[i], k = StringPrototypeCharCodeAt(s, i);
        if (c === "'") out += "\\'";
        else if (c === '\\') out += '\\\\';
        else if (k === 8) out += '\\b';
        else if (k === 9) out += '\\t';
        else if (k === 10) out += '\\n';
        else if (k === 13) out += '\\r';
        else if (k < 32) out += '\\x' + StringPrototypePadStart(StringPrototypeToUpperCase(NumberPrototypeToString(k, 16)), 2, '0');
        else out += c;
      }
      return out;
    };
    // fn: an uncurried getter, called with its receiver
    const tryCall = (fn, v) => { try { fn(v); return true; } catch { return false; } };
    const mapSize = uncurryThis(getOwnPropertyDescriptor(Map.prototype, 'size').get);
    const setSize = uncurryThis(getOwnPropertyDescriptor(Set.prototype, 'size').get);
    const regexpSource = uncurryThis(getOwnPropertyDescriptor(RegExp.prototype, 'source').get);
    const RegExpProto = RegExp.prototype, RegExpPrototypeToString = uncurryThis(RegExp.prototype.toString);
    const MapPrototypeEntries = uncurryThis(Map.prototype.entries), SetPrototypeValues = uncurryThis(Set.prototype.values);
    const MapIteratorNext = uncurryThis(Object.getPrototypeOf(new Map().entries()).next);
    const SetIteratorNext = uncurryThis(Object.getPrototypeOf(new Set().values()).next);
    const isError = typeof Error.isError === 'function' ? Error.isError : (v) => v instanceof ErrorCtor;
    const stringify = JSON.stringify;
    const format = (v, color) => {
      if (v === undefined) return paint('undefined', '90', color);
      if (v === null) return paint('null', '1', color);
      switch (typeof v) {
        case 'string': return paint("'" + escapeText(v) + "'", '32', color);
        case 'number': case 'boolean': return paint(StringCtor(v), '33', color);
        case 'bigint': return paint(StringCtor(v) + 'n', '33', color);
        case 'symbol': return paint(StringCtor(v), '32', color);
        case 'function': {
          const name = StringCtor(v.name);
          return paint(name !== '' ? `[Function: ${name}]` : '[Function (anonymous)]', '36', color);
        }
      }
      const state = n.promiseState(v);
      if (state !== undefined) {
        if (state[0] === 0) return 'Promise { ' + paint('<pending>', '36', color) + ' }';
        const inner = state[1] === v ? paint('[Circular]', '36', color) : format(state[1], color);
        if (state[0] === 1) return 'Promise { ' + inner + ' }';
        return 'Promise { ' + paint('<rejected>', '31', color) + ' ' + inner + ' }';
      }
      if (isError(v)) return paint(StringCtor(v.name) + ': ' + StringCtor(v.message), '31', color);
      if (v !== RegExpProto && tryCall(regexpSource, v)) return paint(RegExpPrototypeToString(v), '31', color);
      if (tryCall(mapSize, v)) {
        const size = mapSize(v);
        if (size === 0) return `Map(0) {}`;
        const parts = [], it = MapPrototypeEntries(v);
        for (let r = MapIteratorNext(it), i = 0; !r.done; r = MapIteratorNext(it), i++) {
          if (i >= MAX_ITEMS) { append(parts, `... ${size - MAX_ITEMS} more`); break; }
          append(parts, format(r.value[0], color) + ' => ' + format(r.value[1], color));
        }
        return `Map(${size}) { ` + ArrayPrototypeJoin(parts, ', ') + ' }';
      }
      if (tryCall(setSize, v)) {
        const size = setSize(v);
        if (size === 0) return `Set(0) {}`;
        const parts = [], it = SetPrototypeValues(v);
        for (let r = SetIteratorNext(it), i = 0; !r.done; r = SetIteratorNext(it), i++) {
          if (i >= MAX_ITEMS) { append(parts, `... ${size - MAX_ITEMS} more`); break; }
          append(parts, format(r.value, color));
        }
        return `Set(${size}) { ` + ArrayPrototypeJoin(parts, ', ') + ' }';
      }
      return StringCtor(stringify(v, null, '  '));
    };
    shared.replFormat = format;
  }

  // Performance (High Resolution Time, as GocciaScript has it): an interface script cannot construct, whose
  // prototype holds now(), timeOrigin (the epoch time the realm started at), toJSON() and the toStringTag, its
  // members enumerable as WebIDL makes them; the global performance is its one instance
  if (typeof G.performance === 'object' && G.performance !== null) {
    const host = G.performance, nowFn = host.now;
    // the engine's time origin (the epoch time it started at, taken again once a host's clock is in place: a
    // deterministic run's is 0); every realm of the engine shares it
    const brand = new WeakSet();
    const check = (o) => { if (!WeakSetPrototypeHas(brand, o)) throw new TypeErrorCtor('Illegal invocation'); };
    const Performance = { Performance() { throw new TypeErrorCtor('Illegal constructor'); } }.Performance;
    const proto = {};
    const members = {
      now() { check(this); return ReflectApply(nowFn, host, []); },
      get timeOrigin() { check(this); return n.timeOrigin(); },
      toJSON() { check(this); return { timeOrigin: n.timeOrigin() }; }
    };
    defineProperty(proto, 'constructor', { __proto__: null, value: Performance, writable: true, enumerable: false, configurable: true });
    for (const k of ['now', 'toJSON']) defineProperty(proto, k, { __proto__: null, value: members[k], writable: true, enumerable: true, configurable: true });
    defineProperty(proto, 'timeOrigin', { __proto__: null, get: Object.getOwnPropertyDescriptor(members, 'timeOrigin').get, set: undefined, enumerable: true, configurable: true });
    defineProperty(proto, Symbol.toStringTag, { __proto__: null, value: 'Performance', writable: false, enumerable: false, configurable: true });
    defineProperty(Performance, 'prototype', { __proto__: null, value: proto, writable: false, enumerable: false, configurable: false });
    const perf = Object.create(proto);
    brand.add(perf);
    const d = Object.getOwnPropertyDescriptor(G, 'performance');
    defineProperty(G, 'performance', { __proto__: null, value: perf, writable: d.writable, enumerable: d.enumerable, configurable: d.configurable });
    defineProperty(G, 'Performance', { __proto__: null, value: Performance, writable: true, enumerable: false, configurable: true });
  }
  // GocciaScript's Temporal.PlainMonthDay.compare (not in Temporal): by month, then day, each operand converted as
  // PlainMonthDay.from converts it
  const Temporal = G.Temporal;
  if (Temporal && Temporal.PlainMonthDay && !Object.prototype.hasOwnProperty.call(Temporal.PlainMonthDay, 'compare')) {
    const PMD = Temporal.PlainMonthDay, PMDFrom = PMD.from;
    const monthCodeOf = uncurryThis(getOwnPropertyDescriptor(PMD.prototype, 'monthCode').get);
    const dayOf = uncurryThis(getOwnPropertyDescriptor(PMD.prototype, 'day').get);
    const monthOf = (md) => NumberCtor(StringPrototypeSlice(monthCodeOf(md), 1, 3));
    const compare = {
      compare(one, two) {
        const a = ReflectApply(PMDFrom, PMD, [one]), b = ReflectApply(PMDFrom, PMD, [two]);
        const ma = monthOf(a), mb = monthOf(b);
        if (ma !== mb) return ma < mb ? -1 : 1;
        const da = dayOf(a), db = dayOf(b);
        return da === db ? 0 : da < db ? -1 : 1;
      }
    }.compare;
    defineProperty(PMD, 'compare', { __proto__: null, value: compare, writable: true, enumerable: false, configurable: true });
  }
  // GocciaScript's globalThis cannot be reassigned
  defineProperty(G, 'globalThis', { __proto__: null, writable: false });

  // PermissionDenied (docs/permissions.md): the error a permission check throws, an Error subclass callable without
  // new, like the native error constructors (its instances have [[ErrorData]], so Error.isError holds)
  const reflectConstruct = Reflect.construct;
  const PermissionDenied = function PermissionDenied(message) {
    const args = arguments.length > 1 ? [message, arguments[1]] : [message];
    return reflectConstruct(ErrorCtor, args, new.target === undefined ? PermissionDenied : new.target);
  };
  Object.setPrototypeOf(PermissionDenied, ErrorCtor);
  const permissionDeniedProto = Object.create(ErrorCtor.prototype);
  defineProperty(permissionDeniedProto, 'constructor', { __proto__: null, value: PermissionDenied, writable: true, enumerable: false, configurable: true });
  defineProperty(permissionDeniedProto, 'name', { __proto__: null, value: 'PermissionDenied', writable: true, enumerable: false, configurable: true });
  defineProperty(permissionDeniedProto, 'message', { __proto__: null, value: '', writable: true, enumerable: false, configurable: true });
  defineProperty(PermissionDenied, 'prototype', { __proto__: null, value: permissionDeniedProto, writable: false, enumerable: false, configurable: false });
  defineProperty(G, 'PermissionDenied', { __proto__: null, value: PermissionDenied, writable: true, enumerable: false, configurable: true });
  shared.PermissionDenied = PermissionDenied;

  // the prototypes structuredClone refuses, now that every prelude class exists and before any script runs
  if (typeof shared.collectCloneProtos === 'function') shared.collectCloneProtos();

  const Goccia = {
    version: info.version,
    commit: info.commit,
    runtimeGlobals,
    build
  };
  fixed(Goccia, 'spec', groups(info.spec));
  fixed(Goccia, 'proposal', groups(info.proposal));
  fixed(Goccia, 'shims', ['btoa', 'atob', 'parseInt', 'parseFloat', 'isNaN', 'isFinite', 'Date',
    'numberArrayToLocaleString', 'hasOwnProperty', '__proto__', 'defineGetter', 'defineSetter', 'lookupGetter',
    'lookupSetter']);
  Goccia.gc = gc;
  return Goccia;
})
