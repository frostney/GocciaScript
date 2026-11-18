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
  // import.meta of a module at path: url (a file: URL) and resolve(specifier), as GocciaScript has them
  shared.initImportMeta = (meta, path) => {
    meta.url = n.fileURL(path);
    meta.resolve = { resolve(...args) { return n.metaResolve(path, args.length, args[0]); } }.resolve;
  };
  const EXPORT_LIST = Symbol('export list');
  shared.EXPORT_LIST = EXPORT_LIST;
  const extensionOf = (path) => {
    const slash = path.lastIndexOf('/'), dot = path.lastIndexOf('.');
    return dot > slash ? path.slice(dot).toLowerCase() : '';
  };
  const FORMAT_NAMES = { '.json': 'JSON', '.json5': 'JSON5', '.jsonc': 'JSON5', '.jsonl': 'JSONL', '.csv': 'CSV',
    '.tsv': 'TSV', '.yaml': 'YAML', '.yml': 'YAML', '.toml': 'TOML' };
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
      // a parse failure names the specifier the module was imported by, not the host path
      const specifier = n.specifierOf(key);
      let value;
      try {
        value = format(text, key, specifier);
      } catch (e) {
        const kind = FORMAT_NAMES[extensionOf(key)];
        if (kind !== undefined && e instanceof SyntaxError)
          throw new SyntaxError(`Failed to parse ${kind} module "${specifier}": ${e.message}`);
        throw e;
      }
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
  // a JSON module, with or without type: 'json': its top-level object's properties, and default the value (unless
  // the object has a default property of its own, which is then the default export)
  shared.jsonExports = (value) => {
    const out = [];
    let ownDefault = false;
    if (value !== null && typeof value === 'object' && !Array.isArray(value))
      for (const k of Object.keys(value)) { out.push(k, value[k]); if (k === 'default') ownDefault = true; }
    if (!ownDefault) out.push('default', value);
    return exportList(out);
  };
  formats['.json'] = (text) => shared.jsonExports(JSON.parse(text));
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
