// goccia:csv and goccia:tsv (parse, parseChunk, stringify) and .csv / .tsv module imports for the CLI host, as
// Goccia.CSV and Goccia.TSV define them: CSV by RFC 4180 (comma-delimited by default, double-quote escaping), TSV by
// IANA text/tab-separated-values (tab-delimited, backslash escaping); both read a header row by default, keep every
// value a string, and pad short rows. Called once per realm with the global object, the host's natives (n) and the
// preludes' shared internals; registers the modules and the file formats.
(function (G, n, shared) {
  'use strict';
  const apply = Reflect.apply;
  // what the functions below call, captured before any script runs: a script that replaces a prototype method or a
  // global does not reach into the parser or the serializer
  const { TypeError, SyntaxError, String, Number } = G;
  const MathTrunc = Math.trunc, NumberIsNaN = Number.isNaN, getOwnPropertyNames = Object.getOwnPropertyNames;
  const uncurryThis = Function.prototype.bind.bind(Function.prototype.call);
  const StringPrototypeSlice = uncurryThis(String.prototype.slice), StringPrototypeIncludes = uncurryThis(String.prototype.includes);
  const StringPrototypeSplit = uncurryThis(String.prototype.split), ArrayPrototypeJoin = uncurryThis(Array.prototype.join);
  // lists and records as push and assignment would build them (a property a script adds to Array.prototype or
  // Object.prototype is #2218's)
  const append = (list, v) => { list[list.length] = v; };
  // the values of a row's fields
  const valuesOf = (fields) => { const r = []; for (let i = 0; i < fields.length; i++) append(r, fields[i].value); return r; };
  // every occurrence of a in s replaced by b
  const replaceAll = (s, a, b) => ArrayPrototypeJoin(StringPrototypeSplit(s, a), b);
  const isArray = Array.isArray;
  const isPlainRecord = (v) => v !== null && typeof v === 'object' && !isArray(v);
  const BOM = '\uFEFF';

  function readOptions(options, csv) {
    const o = { delimiter: ',', headers: true, skipEmptyLines: false };
    if (options === null || typeof options !== 'object') return o;
    if (csv) {
      const d = options.delimiter;
      if (typeof d === 'string' && d.length > 0) o.delimiter = d[0];
    }
    const h = options.headers;
    if (typeof h === 'boolean') o.headers = h;
    const s = options.skipEmptyLines;
    if (typeof s === 'boolean') o.skipEmptyLines = s;
    return o;
  }
  const callback = (args, i) => (args.length > i && typeof args[i] === 'function' ? args[i] : undefined);
  const toInteger = (v) => { const x = MathTrunc(Number(v)); return NumberIsNaN(x) ? 0 : x; };
  const clamp = (v, limit) => (v < 0 ? 0 : v > limit ? limit : v);

  // ---------------- rows ----------------
  // one CSV row from pos (end exclusive): { fields: [{ value, quoted }], pos, consumed }, or null for an
  // unterminated quoted field
  function csvRow(text, delimiter, pos, end) {
    const fields = [];
    let builder = '', quoted = false, inQuotes = false;
    while (pos < end) {
      const ch = text[pos];
      if (inQuotes) {
        if (ch === '"') {
          if (pos + 1 < end && text[pos + 1] === '"') { builder += '"'; pos += 2; }
          else { inQuotes = false; pos++; }
        } else { builder += ch; pos++; }
      } else if (ch === '"') {
        if (builder === '') quoted = true;
        inQuotes = true;
        pos++;
      } else if (ch === delimiter) {
        append(fields, { value: builder, quoted });
        builder = ''; quoted = false;
        pos++;
      } else if (ch === '\r' || ch === '\n') {
        append(fields, { value: builder, quoted });
        pos++;
        if (ch === '\r' && pos < end && text[pos] === '\n') pos++;
        return { fields, pos, consumed: true };
      } else { builder += ch; pos++; }
    }
    if (inQuotes) return null;
    append(fields, { value: builder, quoted });
    return { fields, pos, consumed: true };
  }
  const csvEmpty = (fields) => fields.length === 1 && fields[0].value === '' && !fields[0].quoted;

  function tsvUnescape(field) {
    let out = '';
    for (let i = 0; i < field.length; i++) {
      const c = field[i];
      if (c === '\\' && i + 1 < field.length) {
        const d = field[i + 1];
        if (d === 't') { out += '\t'; i++; continue; }
        if (d === 'n') { out += '\n'; i++; continue; }
        if (d === 'r') { out += '\r'; i++; continue; }
        if (d === '\\') { out += '\\'; i++; continue; }
      }
      out += c;
    }
    return out;
  }
  // one TSV row: { fields: [{ value }], pos, consumed } (consumed false: the text ended before a line break)
  function tsvRow(text, pos, end) {
    const fields = [];
    let start = pos;
    while (pos < end) {
      const ch = text[pos];
      if (ch === '\t') {
        append(fields, { value: tsvUnescape(StringPrototypeSlice(text, start, pos)), quoted: false });
        pos++;
        start = pos;
      } else if (ch === '\r' || ch === '\n') {
        append(fields, { value: tsvUnescape(StringPrototypeSlice(text, start, pos)), quoted: false });
        pos++;
        if (ch === '\r' && pos < end && text[pos] === '\n') pos++;
        return { fields, pos, consumed: true };
      } else pos++;
    }
    append(fields, { value: tsvUnescape(StringPrototypeSlice(text, start, pos)), quoted: false });
    return { fields, pos, consumed: false };
  }
  const tsvEmpty = (fields) => fields.length === 0 || (fields.length === 1 && fields[0].value === '');

  // ---------------- parse ----------------
  function makeFormat(csv) {
    const name = csv ? 'CSV' : 'TSV';
    const rowAt = (text, o, pos, end) => (csv ? csvRow(text, o.delimiter, pos, end) : tsvRow(text, pos, end));
    const isEmpty = csv ? csvEmpty : tsvEmpty;
    const record = (header, fields) => {
      const obj = {};
      for (let i = 0; i < header.length; i++) obj[header[i].value] = i < fields.length ? fields[i].value : '';
      return obj;
    };

    // every row's fields (the header row first, when present)
    function rows(text, o) {
      const out = [];
      if (text.length === 0) return out;
      let pos = text[0] === BOM ? 1 : 0;
      const end = text.length;
      let rowNumber = 0;
      while (pos < end) {
        const r = rowAt(text, o, pos, end);
        if (r === null) throw new SyntaxError(`Unterminated quoted field at row ${rowNumber + 1}`);
        rowNumber++;
        pos = r.pos;
        if (o.skipEmptyLines && isEmpty(r.fields)) continue;
        append(out, r.fields);
      }
      return out;
    }

    function parse(text, options = undefined, reviver = undefined) {
      if (arguments.length < 1) throw new TypeError(`${name}.parse: 1 argument required`);
      if (typeof text !== 'string') throw new TypeError(`${name}.parse: first argument must be a string`);
      const o = readOptions(options, csv);
      const fn = callback(arguments, 2);
      const result = [];
      if (text.length === 0) return result;
      let pos = text[0] === BOM ? 1 : 0;
      const end = text.length;
      let rowNumber = 0, header;
      if (o.headers) {
        if (fn !== undefined) {
          const all = rows(text, { ...o });
          if (all.length === 0) return result;
          header = all[0];
          for (let i = 1; i < all.length; i++) {
            const obj = {};
            for (let j = 0; j < header.length; j++) {
              const f = all[i][j];
              const context = csv ? { quoted: f !== undefined && f.quoted, row: i - 1, column: j } : { row: i - 1, column: j };
              obj[header[j].value] = apply(fn, undefined, [header[j].value, f !== undefined ? f.value : '', context]);
            }
            append(result, obj);
          }
          return result;
        }
        for (;;) {
          const r = rowAt(text, o, pos, end);
          if (r === null) throw new SyntaxError('Unterminated quoted field in header row');
          rowNumber++;
          pos = r.pos;
          header = r.fields;
          if (!(csv && o.skipEmptyLines && isEmpty(header))) break;
          if (pos >= end) break;
        }
        if (!csv && !o.headers) header = [];
      }
      if (fn !== undefined) {
        const all = rows(text, o);
        for (let i = 0; i < all.length; i++) {
          const fields = all[i], values = [];
          for (let j = 0; j < fields.length; j++) {
            const f = fields[j];
            append(values, apply(fn, undefined, [j, f.value, csv ? { quoted: f.quoted, row: i, column: j } : { row: i, column: j }]));
          }
          append(result, values);
        }
        return result;
      }
      while (pos < end) {
        const r = rowAt(text, o, pos, end);
        if (r === null) throw new SyntaxError(`Unterminated quoted field at row ${rowNumber + 1}`);
        rowNumber++;
        pos = r.pos;
        if (o.skipEmptyLines && isEmpty(r.fields)) continue;
        append(result, o.headers ? record(header, r.fields) : valuesOf(r.fields));
      }
      return result;
    }

    function parseChunk(text, options = undefined, start = undefined, endOffset = undefined) {
      if (arguments.length < 1) throw new TypeError(`${name}.parseChunk: 1 argument required`);
      if (typeof text !== 'string') throw new TypeError(`${name}.parseChunk: first argument must be a string`);
      const o = readOptions(options, csv);
      const startArg = arguments.length >= 3 ? toInteger(start) : 0;
      // a negative end reads to the end of the text, as Goccia.CSV's ParseChunk takes it
      const endArg = arguments.length >= 4 ? toInteger(endOffset) : text.length;
      const end = endArg < 0 ? text.length : clamp(endArg, text.length);
      let from = clamp(startArg, text.length);
      if (from === 0 && text[0] === BOM) from = 1;
      const values = [];
      const result = (read, done, error) => ({ values: values, read, done, error: error === undefined ? null : new SyntaxError(error) });
      if (from >= end) return result(from, true);
      let pos = from, resume = from, rowNumber = 0, header;
      if (o.headers && startArg === 0) {
        const r = rowAt(text, o, pos, end);
        if (r === null) return result(resume, false, 'Unterminated quoted field in header row');
        if (!csv && !r.consumed) return result(resume, false);
        header = r.fields;
        pos = r.pos;
        resume = pos;
        rowNumber++;
      }
      while (pos < end) {
        const r = rowAt(text, o, pos, end);
        if (r === null) return result(resume, false, `Unterminated quoted field at row ${rowNumber + 1}`);
        if (!csv && !r.consumed) return result(resume, false);
        rowNumber++;
        pos = r.pos;
        if (o.skipEmptyLines && isEmpty(r.fields)) { resume = pos; continue; }
        append(values, o.headers && header !== undefined ? record(header, r.fields) : valuesOf(r.fields));
        resume = pos;
      }
      return result(end, true);
    }

    // ---------------- stringify ----------------
    const escape = csv
      ? (s, d) => (StringPrototypeIncludes(s, d) || StringPrototypeIncludes(s, '"') || StringPrototypeIncludes(s, '\n') || StringPrototypeIncludes(s, '\r') ? '"' + replaceAll(s, '"', '""') + '"' : s)
      : (s) => replaceAll(replaceAll(replaceAll(replaceAll(s, '\\', '\\\\'), '\t', '\\t'), '\n', '\\n'), '\r', '\\r');
    function replaceRows(data, fn) {
      const out = [];
      if (data.length > 0 && isPlainRecord(data[0])) {
        const keys = getOwnPropertyNames(data[0]);
        for (let i = 0; i < data.length; i++) {
          const item = data[i];
          if (item === null || typeof item !== 'object') continue;
          const o = {};
          for (let k = 0; k < keys.length; k++) o[keys[k]] = apply(fn, undefined, [keys[k], item[keys[k]]]);
          append(out, o);
        }
      } else {
        for (let i = 0; i < data.length; i++) {
          const row = data[i];
          if (!isArray(row)) continue;
          const r = [];
          for (let j = 0; j < row.length; j++) append(r, apply(fn, undefined, [j, row[j]]));
          append(out, r);
        }
      }
      return out;
    }
    function stringify(data, options = undefined, replacer = undefined) {
      if (arguments.length < 1) throw new TypeError(`${name}.stringify: 1 argument required`);
      const o = readOptions(options, csv);
      const fn = callback(arguments, 2);
      if (!isArray(data)) return '';
      const rows = fn !== undefined ? replaceRows(data, fn) : data;
      if (rows.length === 0) return '';
      const d = csv ? o.delimiter : '\t';
      const lines = [];
      if (isPlainRecord(rows[0]) && o.headers) {
        const keys = getOwnPropertyNames(rows[0]);
        const line = (cell, length) => { let s = ''; for (let j = 0; j < length; j++) s += (j > 0 ? d : '') + cell(j); return s; };
        append(lines, line((j) => escape(keys[j], d), keys.length));
        for (let i = 0; i < rows.length; i++) {
          const item = rows[i];
          if (item === null || typeof item !== 'object') continue;
          append(lines, line((j) => { const v = item[keys[j]]; return v === undefined ? '' : escape(String(v), d); }, keys.length));
        }
      } else {
        for (let i = 0; i < rows.length; i++) {
          const row = rows[i];
          if (!isArray(row)) continue;
          let s = '';
          for (let j = 0; j < row.length; j++) s += (j > 0 ? d : '') + escape(String(row[j]), d);
          append(lines, s);
        }
      }
      return ArrayPrototypeJoin(lines, '\n');
    }
    return { parse, parseChunk, stringify };
  }

  const csvModule = makeFormat(true), tsvModule = makeFormat(false);
  shared.hostModules['goccia:csv'] = () => csvModule;
  shared.hostModules['goccia:tsv'] = () => tsvModule;
  // a .csv or .tsv file imports each record under its index ("0", "1", ...), its header row read
  const indexed = (rowsOf) => (text) => {
    const records = rowsOf(text);
    const out = [];
    for (let i = 0; i < records.length; i++) { append(out, String(i)); append(out, records[i]); }
    return shared.exportList(out);
  };
  shared.dataFormats['.csv'] = indexed((t) => csvModule.parse(t));
  shared.dataFormats['.tsv'] = indexed((t) => tsvModule.parse(t));
})
