// The CLI host's modules for G2: goccia:* and the other host specifiers, as the preludes register them in
// shared.hostModules (key -> a function returning the module's exports object, or its JavaScript source text).
// G2.Modules asks G2.HostAPI's hook, which calls shared.loadHostModule(key): undefined for no host module, the
// source text, or the export names and values interleaved. Called first, with the global object, the host's natives
// (n) and the preludes' shared internals.
(function (G, n, shared) {
  'use strict';
  const registry = Object.create(null);
  shared.hostModules = registry;
  shared.loadHostModule = (key) => {
    const make = registry[key];
    if (make === undefined) return undefined;
    const exports = make();
    if (typeof exports === 'string') return exports;
    const out = [];
    for (const k of Object.keys(exports)) out.push(k, exports[k]);
    return out;
  };
})
