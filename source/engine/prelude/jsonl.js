// goccia:jsonl (parse, parseChunk) and .jsonl module imports for the CLI host, as Goccia.JSONL defines them:
// each non-empty line is one JSON text; a string's offsets are code units, a Uint8Array's bytes (decoded as UTF-8,
// a leading BOM skipped). parseChunk stops before an incomplete final record and reports where it got to. Called
// once per realm with the global object, the host's natives (n) and the preludes' shared internals; registers the
// module and the file format.
(function (G, n, shared) {
  'use strict';
  // what the functions below call, captured before any script runs: a script that replaces a prototype method or a
  // global does not reach into the parser
  const { TypeError, SyntaxError, String, Number, Uint8Array } = G;
  const NumberIsNaN = Number.isNaN, MathTrunc = Math.trunc, JSONParse = JSON.parse, defineProperty = Object.defineProperty;
  const uncurryThis = Function.prototype.bind.bind(Function.prototype.call);
  const StringPrototypeIncludes = uncurryThis(String.prototype.includes), StringPrototypeSlice = uncurryThis(String.prototype.slice);
  const StringPrototypeCharCodeAt = uncurryThis(String.prototype.charCodeAt), StringPrototypeTrim = uncurryThis(String.prototype.trim);
  const RegExpPrototypeExec = uncurryThis(RegExp.prototype.exec);
  const TAP = Object.getPrototypeOf(Uint8Array.prototype);
  const taBuffer = uncurryThis(Object.getOwnPropertyDescriptor(TAP, 'buffer').get);
  const taByteOffset = uncurryThis(Object.getOwnPropertyDescriptor(TAP, 'byteOffset').get);
  const taLength = uncurryThis(Object.getOwnPropertyDescriptor(TAP, 'length').get);
  // a list append that defines the index (a setter on Array.prototype must not see it)
  const append = (list, v) => { defineProperty(list, list.length, { value: v, writable: true, enumerable: true, configurable: true }); };
  const decoder = new G.TextDecoder(), decode = uncurryThis(G.TextDecoder.prototype.decode);

  // whether text could still become a JSON text by appending to it (it ends inside a value)
  function isJSONPrefix(text) {
    let i = 0;
    const ws = () => { while (i < text.length && StringPrototypeIncludes(' \t\n\r', text[i])) i++; };
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
      const words = ['true', 'false', 'null'];
      for (let wi = 0; wi < words.length; wi++) {
        const w = words[wi];
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
        const m = RegExpPrototypeExec(/^-?(0|[1-9][0-9]*)?(\.[0-9]*)?([eE][+-]?[0-9]*)?/, StringPrototypeSlice(text, i));
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
      return { length: input.length, at: (i) => StringPrototypeCharCodeAt(input, i), slice: (a, b) => StringPrototypeSlice(input, a, b),
        bom: StringPrototypeCharCodeAt(input, 0) === 0xFEFF ? 1 : 0 };
    }
    if (input instanceof Uint8Array) {
      const length = taLength(input);
      const bom = length >= 3 && input[0] === 0xEF && input[1] === 0xBB && input[2] === 0xBF ? 3 : 0;
      return { length, at: (i) => input[i], slice: (a, b) => decode(decoder, new Uint8Array(taBuffer(input), taByteOffset(input) + a, b - a)), bom };
    }
    throw new TypeError(`JSONL.${what}: first argument must be a string or a Uint8Array`);
  }
  const offset = (v, len) => {
    const x = Number(v);
    if (NumberIsNaN(x) || x === -Infinity) return 0;
    if (x === Infinity) return len;
    const t = MathTrunc(x);
    return t < 0 ? 0 : t > len ? len : t;
  };

  // the lines in [start, end): each { from, to (content end), next (after the line break), last (no break) }
  // (a list, not a generator: iterating one would look up the generator prototype's next)
  function lines(src, start, end) {
    const out = [];
    let pos = start;
    while (pos < end) {
      let e = pos;
      while (e < end && src.at(e) !== 10 && src.at(e) !== 13) e++;
      let next = e;
      if (e < end) { next = e + 1; if (src.at(e) === 13 && next < end && src.at(next) === 10) next++; }
      append(out, { from: pos, to: e, next, last: e >= end });
      pos = next;
    }
    return out;
  }
  function lineError(lineNumber, e) { return new SyntaxError(`JSONL line ${lineNumber}: ${e.message}`); }

  function parse(input) {
    if (arguments.length < 1) throw new TypeError('JSONL.parse: 1 argument required');
    const src = source(input, 'parse');
    const out = [];
    let lineNumber = 0;
    const all = lines(src, src.bom, src.length);
    for (let k = 0; k < all.length; k++) {
      const l = all[k];
      lineNumber++;
      const text = src.slice(l.from, l.to);
      if (StringPrototypeTrim(text) === '') continue;
      try { append(out, JSONParse(text)); } catch (e) { throw lineError(lineNumber, e); }
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
    const all = lines(src, from, to);
    for (let k = 0; k < all.length; k++) {
      const l = all[k];
      lineNumber++;
      const text = src.slice(l.from, l.to);
      if (StringPrototypeTrim(text) === '') { read = l.to; continue; }
      let v;
      try { v = JSONParse(text); }
      catch (e) {
        if (l.last && isJSONPrefix(StringPrototypeTrim(text))) return { values, read, done: false, error: null };
        return { values, read, done: false, error: lineError(lineNumber, e) };
      }
      append(values, v);
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
    for (let i = 0; i < records.length; i++) { append(out, String(i)); append(out, records[i]); }
    return shared.exportList(out);
  };
})
