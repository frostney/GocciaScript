// goccia:jsonl (parse, parseChunk) and .jsonl module imports for the CLI host on G2, as Goccia.JSONL defines them:
// each non-empty line is one JSON text; a string's offsets are code units, a Uint8Array's bytes (decoded as UTF-8,
// a leading BOM skipped). parseChunk stops before an incomplete final record and reports where it got to. Called
// once per realm with the global object, the host's natives (n) and the preludes' shared internals; registers the
// module and the file format.
(function (G, n, shared) {
  'use strict';
  const decoder = new G.TextDecoder();

  // whether text could still become a JSON text by appending to it (it ends inside a value)
  function isJSONPrefix(text) {
    let i = 0;
    const ws = () => { while (i < text.length && ' \t\n\r'.includes(text[i])) i++; };
    const EOF = {};
    function value() {
      ws();
      if (i >= text.length) throw EOF;
      const c = text[i];
      if (c === '{') {
        i++; ws();
        if (i >= text.length) throw EOF;
        if (text[i] === '}') { i++; return; }
        for (;;) {
          ws();
          if (i >= text.length) throw EOF;
          if (text[i] !== '"') return false;
          string();
          ws();
          if (i >= text.length) throw EOF;
          if (text[i] !== ':') return false;
          i++;
          if (value() === false) return false;
          ws();
          if (i >= text.length) throw EOF;
          if (text[i] === ',') { i++; continue; }
          if (text[i] === '}') { i++; return; }
          return false;
        }
      }
      if (c === '[') {
        i++; ws();
        if (i >= text.length) throw EOF;
        if (text[i] === ']') { i++; return; }
        for (;;) {
          if (value() === false) return false;
          ws();
          if (i >= text.length) throw EOF;
          if (text[i] === ',') { i++; continue; }
          if (text[i] === ']') { i++; return; }
          return false;
        }
      }
      if (c === '"') { string(); return; }
      for (const w of ['true', 'false', 'null']) {
        if (w[0] === c) {
          for (let k = 0; k < w.length; k++) {
            if (i >= text.length) throw EOF;
            if (text[i] !== w[k]) return false;
            i++;
          }
          return;
        }
      }
      if (c === '-' || (c >= '0' && c <= '9')) {
        const m = /^-?(0|[1-9][0-9]*)?(\.[0-9]*)?([eE][+-]?[0-9]*)?/.exec(text.slice(i));
        i += m[0].length;
        if (i >= text.length) throw EOF;
        return;
      }
      return false;
    }
    function string() {
      i++;
      while (i < text.length) {
        const c = text[i];
        if (c === '"') { i++; return; }
        if (c === '\\') i++;
        i++;
      }
      throw EOF;
    }
    try { return value() !== false && (ws(), i >= text.length); } catch (e) { if (e === EOF) return true; throw e; }
  }

  // the input as units: a string's code units or a Uint8Array's bytes
  function source(input, what) {
    if (typeof input === 'string') {
      return { length: input.length, at: (i) => input.charCodeAt(i), slice: (a, b) => input.slice(a, b),
        bom: input.charCodeAt(0) === 0xFEFF ? 1 : 0 };
    }
    if (input instanceof Uint8Array) {
      const bom = input.length >= 3 && input[0] === 0xEF && input[1] === 0xBB && input[2] === 0xBF ? 3 : 0;
      return { length: input.length, at: (i) => input[i], slice: (a, b) => decoder.decode(input.subarray(a, b)), bom };
    }
    throw new TypeError(`JSONL.${what}: first argument must be a string or a Uint8Array`);
  }
  const offset = (v, len) => {
    const x = Number(v);
    if (Number.isNaN(x) || x === -Infinity) return 0;
    if (x === Infinity) return len;
    const t = Math.trunc(x);
    return t < 0 ? 0 : t > len ? len : t;
  };

  // the lines in [start, end): each { from, to (content end), next (after the line break), last (no break) }
  function* lines(src, start, end) {
    let pos = start;
    while (pos < end) {
      let e = pos;
      while (e < end && src.at(e) !== 10 && src.at(e) !== 13) e++;
      let next = e;
      if (e < end) { next = e + 1; if (src.at(e) === 13 && next < end && src.at(next) === 10) next++; }
      yield { from: pos, to: e, next, last: e >= end };
      pos = next;
    }
  }
  function lineError(lineNumber, e) { return new SyntaxError(`JSONL line ${lineNumber}: ${e.message}`); }

  function parse(input) {
    if (arguments.length < 1) throw new TypeError('JSONL.parse: 1 argument required');
    const src = source(input, 'parse');
    const out = [];
    let lineNumber = 0;
    for (const l of lines(src, src.bom, src.length)) {
      lineNumber++;
      const text = src.slice(l.from, l.to);
      if (text.trim() === '') continue;
      try { out.push(JSON.parse(text)); } catch (e) { throw lineError(lineNumber, e); }
    }
    return out;
  }

  function parseChunk(input, start = undefined, end = undefined) {
    if (arguments.length < 1) throw new TypeError('JSONL.parseChunk: 1 argument required');
    const src = source(input, 'parseChunk');
    let from = arguments.length >= 2 ? offset(start, src.length) : 0;
    let to = arguments.length >= 3 ? offset(end, src.length) : src.length;
    if (to < from) to = from;
    if (from === 0) from = src.bom;
    const values = [];
    let read = from, lineNumber = 0;
    for (const l of lines(src, from, to)) {
      lineNumber++;
      const text = src.slice(l.from, l.to);
      if (text.trim() === '') { read = l.to; continue; }
      let v;
      try { v = JSON.parse(text); }
      catch (e) {
        if (l.last && isJSONPrefix(text.trim())) return { values, read, done: false, error: null };
        return { values, read, done: false, error: lineError(lineNumber, e) };
      }
      values.push(v);
      read = l.to;
    }
    return { values, read: to, done: true, error: null };
  }

  const jsonl = { parse, parseChunk };
  shared.hostModules['goccia:jsonl'] = () => jsonl;
  // a .jsonl file imports each record under its index ("0", "1", ...)
  shared.dataFormats['.jsonl'] = (text) => {
    const records = parse(text);
    const out = [];
    for (let i = 0; i < records.length; i++) out.push(String(i), records[i]);
    return shared.exportList(out);
  };
})
