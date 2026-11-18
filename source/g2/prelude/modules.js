// The CLI host's modules for G2: goccia:* and the other host specifiers, as the preludes register them in
// shared.hostModules (key -> a function returning the module's exports object, or its JavaScript source text).
// G2.Modules asks G2.HostAPI's hook, which calls shared.loadHostModule(key): undefined for no host module, the
// source text, or the export names and values interleaved. Called first, with the global object, the host's natives
// (n) and the preludes' shared internals.
(function (G, n, shared) {
  'use strict';
  const registry = Object.create(null);
  shared.hostModules = registry;
  // the data and text files a module may import, by extension: each a function from the file's text (and path) to
  // the value the file holds. An object (not an array) exports its properties by name; anything else is the
  // default export. A format may instead return the export names and values itself, interleaved, as an array
  // tagged with shared.EXPORT_LIST.
  const formats = Object.create(null);
  shared.dataFormats = formats;
  const EXPORT_LIST = Symbol('export list');
  shared.EXPORT_LIST = EXPORT_LIST;
  const extensionOf = (path) => {
    const slash = path.lastIndexOf('/'), dot = path.lastIndexOf('.');
    return dot > slash ? path.slice(dot).toLowerCase() : '';
  };
  shared.hasHostModule = (key) => registry[key] !== undefined;
  shared.loadHostModule = (key) => {
    const make = registry[key];
    let exports;
    if (make !== undefined) exports = make();
    else {
      const format = key[0] === '/' ? formats[extensionOf(key)] : undefined;
      if (format === undefined) return undefined;
      const text = n.readText(key);
      if (text === undefined) return undefined;
      const value = format(text, key);
      if (Array.isArray(value) && value[EXPORT_LIST] === true) return value;
      if (value !== null && typeof value === 'object' && !Array.isArray(value)) exports = value;
      else return ['default', value];
    }
    if (typeof exports === 'string') return exports;
    const out = [];
    for (const k of Object.keys(exports)) out.push(k, exports[k]);
    return out;
  };
  const exportList = (pairs) => { pairs[EXPORT_LIST] = true; return pairs; };
  shared.exportList = exportList;

  // JSON (without a type attribute: its top-level object's properties) and text assets (content, default and
  // frozen metadata)
  formats['.json'] = (text) => JSON.parse(text);
  const textAsset = (text, path) => {
    const slash = path.lastIndexOf('/');
    const fileName = path.slice(slash + 1);
    const metadata = Object.freeze({ kind: 'text', path, fileName, extension: extensionOf(path),
      byteLength: n.fileSize(path) });
    return exportList(['content', text, 'default', text, 'metadata', metadata]);
  };
  formats['.txt'] = textAsset;
  formats['.md'] = textAsset;
})
