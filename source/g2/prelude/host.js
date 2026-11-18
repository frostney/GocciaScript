// The CLI host's globals for G2 (docs/built-ins.md): the Goccia object, and the global object as the CLIs leave it.
// G2.HostAPI calls this function once per realm with the global object, the host's natives (n), the preludes'
// shared internals, the build facts as JSON, and whether the host runs tests and allows ShadowRealm; it returns the Goccia object, which the host binds as
// the const Goccia.
(function (G, n, shared, infoJson, testMode, shadowRealm, configJson) {
  'use strict';
  const gc = n.gc, gcBytes = n.gcBytes, gcMaxBytes = n.gcMaxBytes, gcSuggestedMaxBytes = n.gcSuggestedMaxBytes;
  const defineProperty = Object.defineProperty;
  const info = JSON.parse(infoJson);
  // the host's configuration: the goccia.json keys and their values (strings, or arrays of strings)
  shared.config = JSON.parse(configJson);
  const configFlag = (k) => shared.config[k] === 'true';

  // the conformance host's extras the CLIs do not have: print, eval, and the Annex B escape and unescape, which
  // GocciaScript leaves out (docs/language.md, Annex B)
  delete G.print;
  delete G.eval;
  delete G.escape;
  delete G.unescape;
  if (!shadowRealm) delete G.ShadowRealm;

  // a read-only, enumerable, non-configurable data property (the Pascal engines' DefineProperty with [pfEnumerable])
  function fixed(o, k, v) { defineProperty(o, k, { value: v, writable: false, enumerable: true, configurable: false }); }
  // a getter only, neither enumerable nor configurable, named "get k"
  function getter(o, k, fn) {
    const get = { get [k]() { return fn(); } };
    defineProperty(o, k, { get: Object.getOwnPropertyDescriptor(get, k).get, enumerable: false, configurable: false });
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
      if (Array.isArray(v)) return v.map((x) => rebuild(x, plain));
      if (v === null || typeof v !== 'object') return v;
      const o = plain ? {} : Object.create(null);
      for (const k of Object.keys(v)) o[k] = rebuild(v[k], plain);
      return o;
    };
    const parse = {
      parse(source, options = undefined) {
        if (typeof source !== 'string') throw new TypeError('parse expects the source text as a string');
        const o = options !== null && typeof options === 'object' ? options : {};
        const flag = (k, d) => (o[k] === undefined ? d : !!o[k]);
        const fileName = typeof o.fileName === 'string' ? o.fileName : '<source>';
        const raw = JSON.parse(n.astParse(source, flag('jsx', false), flag('module', true), flag('tokens', false),
          flag('expressions', false), fileName));
        const result = Object.create(null);
        result.source = raw.source;
        result.comments = rebuild(raw.comments, false);
        result.root = rebuild(raw.root, false);
        if (raw.tokens !== undefined) result.tokens = rebuild(raw.tokens, true);
        return result;
      }
    }.parse;
    shared.hostModules['goccia:ast'] = () => ({ parse });
  }

  // the test runner's vitest compatibility shim, over goccia:test and goccia:timers
  if (testMode) shared.hostModules.vitest = () => n.vitestSource();

  // Performance (High Resolution Time, as GocciaScript has it): an interface script cannot construct, whose
  // prototype holds now(), timeOrigin (the epoch time the realm started at), toJSON() and the toStringTag, its
  // members enumerable as WebIDL makes them; the global performance is its one instance
  if (typeof G.performance === 'object' && G.performance !== null) {
    const host = G.performance, nowFn = host.now;
    const origin = n.realNow() - Reflect.apply(nowFn, host, []);
    const brand = new WeakSet();
    const check = (o) => { if (!brand.has(o)) throw new TypeError('Illegal invocation'); };
    const Performance = { Performance() { throw new TypeError('Illegal constructor'); } }.Performance;
    const proto = {};
    const members = {
      now() { check(this); return Reflect.apply(nowFn, host, []); },
      get timeOrigin() { check(this); return origin; },
      toJSON() { check(this); return { timeOrigin: origin }; }
    };
    defineProperty(proto, 'constructor', { value: Performance, writable: true, enumerable: false, configurable: true });
    for (const k of ['now', 'toJSON']) defineProperty(proto, k, { value: members[k], writable: true, enumerable: true, configurable: true });
    defineProperty(proto, 'timeOrigin', { get: Object.getOwnPropertyDescriptor(members, 'timeOrigin').get, set: undefined, enumerable: true, configurable: true });
    defineProperty(proto, Symbol.toStringTag, { value: 'Performance', writable: false, enumerable: false, configurable: true });
    defineProperty(Performance, 'prototype', { value: proto, writable: false, enumerable: false, configurable: false });
    const perf = Object.create(proto);
    brand.add(perf);
    const d = Object.getOwnPropertyDescriptor(G, 'performance');
    defineProperty(G, 'performance', { value: perf, writable: d.writable, enumerable: d.enumerable, configurable: d.configurable });
    defineProperty(G, 'Performance', { value: Performance, writable: true, enumerable: false, configurable: true });
  }
  // GocciaScript's Temporal.PlainMonthDay.compare (not in Temporal): by month, then day, each operand converted as
  // PlainMonthDay.from converts it
  const Temporal = G.Temporal;
  if (Temporal && Temporal.PlainMonthDay && !Object.prototype.hasOwnProperty.call(Temporal.PlainMonthDay, 'compare')) {
    const PMD = Temporal.PlainMonthDay;
    const monthOf = (md) => Number(md.monthCode.slice(1, 3));
    const compare = {
      compare(one, two) {
        const a = PMD.from(one), b = PMD.from(two);
        const ma = monthOf(a), mb = monthOf(b);
        if (ma !== mb) return ma < mb ? -1 : 1;
        return a.day === b.day ? 0 : a.day < b.day ? -1 : 1;
      }
    }.compare;
    defineProperty(PMD, 'compare', { value: compare, writable: true, enumerable: false, configurable: true });
  }
  // GocciaScript's globalThis cannot be reassigned
  defineProperty(G, 'globalThis', { writable: false });

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
