// The CLI host's globals for G2 (docs/built-ins.md): the Goccia object, and the global object as the CLIs leave it.
// G2.HostAPI calls this function once per realm with the global object, the host's natives (n), the preludes'
// shared internals, the build facts as JSON, and whether the host runs tests and allows ShadowRealm; it returns the Goccia object, which the host binds as
// the const Goccia.
(function (G, n, shared, infoJson, testMode, shadowRealm, configJson) {
  'use strict';
  const gc = n.gc, gcBytes = n.gcBytes, gcMaxBytes = n.gcMaxBytes;
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
  getter(gc, 'suggestedMaxBytes', gcMaxBytes);
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

  // performance.timeOrigin: the epoch time the realm started at
  if (typeof G.performance === 'object' && G.performance !== null && !('timeOrigin' in G.performance)) {
    const origin = n.realNow() - G.performance.now();
    Object.defineProperty(G.performance, 'timeOrigin', { get: Object.getOwnPropertyDescriptor({ get timeOrigin() { return origin; } }, 'timeOrigin').get, enumerable: true, configurable: true });
  }

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
