// The CLI host's globals for G2 (docs/built-ins.md): the Goccia object, and the global object as the CLIs leave it.
// G2.HostAPI calls this function once per realm with the global object, the host's natives (n), the build facts as
// JSON, and whether the host runs tests and allows ShadowRealm; it returns the Goccia object, which the host binds as
// the const Goccia.
(function (G, n, infoJson, testMode, shadowRealm) {
  'use strict';
  const gc = n.gc, gcBytes = n.gcBytes, gcMaxBytes = n.gcMaxBytes;
  const defineProperty = Object.defineProperty;
  const info = JSON.parse(infoJson);

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

  const runtimeGlobals = [];
  if (testMode) runtimeGlobals.push('TestAssertions');

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
