// The CLI host's modules: goccia:* and the other host specifiers, as the preludes register them in
// shared.hostModules (key -> a function returning the module's exports object, or its JavaScript source text).
// Goccia.Modules asks Goccia.HostAPI's hook, which calls shared.loadHostModule(key, filePath): undefined for no host
// module, the source text, or the export names and values interleaved. Called first, with the global object, the
// host's natives (n) and the preludes' shared internals.
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
  // a path's last separator: a slash, or on Windows a backslash
  const lastSlash = (path) => Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
  const extensionOf = (path) => {
    const slash = lastSlash(path), dot = path.lastIndexOf('.');
    return dot > slash ? path.slice(dot).toLowerCase() : '';
  };
  const FORMAT_NAMES = { '.json': 'JSON', '.json5': 'JSON5', '.jsonc': 'JSON5', '.jsonl': 'JSONL', '.csv': 'CSV',
    '.tsv': 'TSV', '.yaml': 'YAML', '.yml': 'YAML', '.toml': 'TOML' };
  shared.hasHostModule = (key) => registry[key] !== undefined;
  shared.loadHostModule = (key, filePath) => {
    const make = filePath === '' ? registry[key] : undefined;
    let exports;
    if (make !== undefined) {
      exports = make();
      if (Array.isArray(exports) && exports[EXPORT_LIST] === true) return exports;
    } else {
      if (filePath === '') return undefined;
      const extension = extensionOf(key);
      const format = n.isAbsolutePath(filePath) ? formats[extension] : undefined;
      if (format === undefined) return undefined;
      const text = n.readText(key, filePath);
      if (text === undefined) return undefined;
      // a parse failure names the specifier the module was imported by, not the host path
      const specifier = n.specifierOf(key);
      let value;
      try {
        value = format(text, key, specifier, filePath);
      } catch (e) {
        const kind = FORMAT_NAMES[extension];
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
  // a text asset's line breaks as LF (a CRLF or a lone CR), as GocciaScript normalizes them: a checkout on
  // Windows writes CRLF
  const toLF = (text) => {
    if (text.indexOf('\r') < 0) return text;
    let out = '';
    for (let i = 0; i < text.length; i++) {
      const c = text[i];
      if (c !== '\r') out += c;
      else {
        out += '\n';
        if (text[i + 1] === '\n') i++;
      }
    }
    return out;
  };
  const textAsset = (raw, path, _specifier, readPath) => {
    const text = toLF(raw);
    const slash = lastSlash(path);
    const fileName = path.slice(slash + 1);
    const metadata = Object.freeze({ kind: 'text', path, fileName, extension: extensionOf(path),
      byteLength: n.fileSize(readPath) });
    return exportList(['content', text, 'default', text, 'metadata', metadata]);
  };
  formats['.txt'] = textAsset;
  formats['.md'] = textAsset;

  // ---------------- virtual modules (docs/virtual-modules.md) ----------------
  // a host's module map: { address: { type?, content } }, type javascript (default) or typescript (source), json
  // (any value, exported as a JSON module is), text (the default export) or bytes (base64, an immutable
  // Uint8Array as the default export). A relative address names a file next to base (the directory of the config
  // or manifest that declares it); a definition may replace an earlier virtual one, but not a host module.
  const virtualKeys = Object.create(null);
  const isRelative = (a) => a.startsWith('./') || a.startsWith('../');
  const joinPath = (dir, rel) => {
    // a Windows directory (C:\dir, \\server\share): joined as the host's resolver spells the paths it resolves,
    // so that an import of the file meets this key
    if (dir.includes('\\') || /^[A-Za-z]:/.test(dir)) {
      const expanded = n.expandPath(dir, rel);
      if (expanded !== '') return expanded;
    }
    const parts = dir.split('/');
    for (const seg of rel.split('/')) {
      if (seg === '..') parts.pop();
      else if (seg !== '.' && seg !== '') parts.push(seg);
    }
    return parts.join('/') || '/';
  };
  const decodeBase64 = (text, address) => {
    let binary;
    try {
      binary = G.atob(text);
    } catch {
      throw new TypeError(`Virtual module "${address}" has invalid base64 content`);
    }
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
    return new Uint8Array(bytes.buffer.transferToImmutable());
  };
  shared.defineVirtualModules = (map, base) => {
    if (map === null || typeof map !== 'object' || Array.isArray(map))
      throw new TypeError('A virtual module map must be an object of module definitions');
    for (const address of Object.keys(map)) {
      const descriptor = map[address];
      if (descriptor === null || typeof descriptor !== 'object' || Array.isArray(descriptor) ||
          !Object.hasOwn(descriptor, 'content'))
        throw new TypeError(`Virtual module "${address}" must be an object with content`);
      const type = descriptor.type === undefined ? 'javascript' : descriptor.type;
      const content = descriptor.content;
      const key = isRelative(address) && base ? joinPath(base, address) : address;
      if (n.moduleLoaded(key))
        throw new TypeError(`Virtual module "${key}" cannot be replaced after it has been loaded.`);
      if (registry[key] !== undefined && virtualKeys[key] === undefined)
        throw new TypeError(`Virtual module "${address}" conflicts with a registered ${key.startsWith('goccia:') ? 'runtime' : 'host'} module.`);
      let make;
      switch (type) {
        case 'javascript':
        case 'typescript':
          if (typeof content !== 'string')
            throw new TypeError(`Virtual module "${address}" of type ${type} must have string content`);
          make = () => content;
          break;
        case 'json':
          make = () => shared.jsonExports(content);
          break;
        case 'text':
          if (typeof content !== 'string')
            throw new TypeError(`Virtual module "${address}" of type text must have string content`);
          make = () => exportList(['default', content]);
          break;
        case 'bytes': {
          if (typeof content !== 'string')
            throw new TypeError(`Virtual module "${address}" of type bytes must have base64 string content`);
          const bytes = decodeBase64(content, address);
          make = () => exportList(['default', bytes]);
          break;
        }
        default:
          throw new TypeError(`Virtual module "${address}" has an unknown type "${type}"`);
      }
      registry[key] = make;
      virtualKeys[key] = true;
    }
  };
})
