// goccia:json5 (parse, stringify) and .json5 module imports for the CLI host: the JSON5 data interchange
// format (json5.org), with the reference library's stringify (quote choice, identifier keys, a trailing comma when
// pretty-printing) and JSON.parse's reviver protocol, its context carrying a primitive's source text. Called once
// per realm with the global object, the host's natives (n) and the preludes' shared internals; registers the module
// and the .json5 file format.
(function (G, n, shared) {
  'use strict';
  const apply = Reflect.apply, defineProperty = Object.defineProperty;
  const isArray = Array.isArray, fromCodePoint = String.fromCodePoint;
  const ID_START = /[\p{L}\p{Nl}$_]/u;
  const ID_CONTINUE = /[\p{L}\p{Nl}$_\p{Mn}\p{Mc}\p{Nd}\p{Pc}\u200C\u200D]/u;
  const SPACE = /[\t\v\f \u00A0\uFEFF\n\r\u2028\u2029\p{Zs}]/u;

  // ---------------- parse ----------------
  function syntaxError(message, text, pos) {
    let line = 1, column = 1;
    for (let i = 0; i < pos && i < text.length; i++) {
      if (text[i] === '\n') { line++; column = 1; } else column++;
    }
    const e = new SyntaxError(`JSON5: ${message} at ${line}:${column}`);
    e.lineNumber = line;
    e.columnNumber = column;
    return e;
  }
  function describe(c) {
    if (c === undefined) return 'end of input';
    const code = c.codePointAt(0);
    if (code < 0x20 || code === 0x7F) return `character '\\x${code.toString(16).padStart(2, '0')}'`;
    return `character '${c}'`;
  }

  function parseText(text, wantSources) {
    let pos = 0;
    const sources = [];   // the source text of each primitive, in the order a reviver walk visits them
    const peek = () => text[pos];
    const fail = (what) => { throw syntaxError(what !== undefined ? what : `invalid ${describe(text[pos])}`, text, pos); };

    function skipSpace() {
      for (;;) {
        const c = text[pos];
        if (c === undefined) return;
        if (c === '/') {
          const d = text[pos + 1];
          if (d === '/') { pos += 2; while (pos < text.length && !/[\n\r\u2028\u2029]/.test(text[pos])) pos++; continue; }
          if (d === '*') {
            const end = text.indexOf('*/', pos + 2);
            if (end < 0) { pos = text.length; fail('invalid end of input'); }
            pos = end + 2;
            continue;
          }
          fail();
        }
        const cp = String.fromCodePoint(text.codePointAt(pos));
        if (SPACE.test(cp)) { pos += cp.length; continue; }
        return;
      }
    }
    function hexDigits(count) {
      let v = 0;
      for (let i = 0; i < count; i++) {
        const c = text[pos];
        if (c === undefined || !/[0-9a-fA-F]/.test(c)) fail();
        v = v * 16 + parseInt(c, 16);
        pos++;
      }
      return v;
    }
    function escape() {
      // after the backslash
      const c = text[pos];
      if (c === undefined) fail();
      pos++;
      switch (c) {
        case 'b': return '\b';
        case 'f': return '\f';
        case 'n': return '\n';
        case 'r': return '\r';
        case 't': return '\t';
        case 'v': return '\v';
        case '0':
          if (/[0-9]/.test(text[pos] || '')) { pos--; fail(); }
          return '\0';
        case 'x': return String.fromCharCode(hexDigits(2));
        case 'u': return String.fromCharCode(hexDigits(4));
        case '\n': case '\u2028': case '\u2029': return '';
        case '\r': if (text[pos] === '\n') pos++; return '';
      }
      if (/[1-9]/.test(c)) { pos--; fail(); }
      return c;
    }
    function string() {
      const quote = text[pos];
      const start = pos;
      pos++;
      let s = '';
      for (;;) {
        const c = text[pos];
        if (c === undefined) fail();
        if (c === quote) { pos++; break; }
        if (c === '\\') { pos++; s += escape(); continue; }
        if (c === '\n' || c === '\r') fail();
        s += c;
        pos++;
      }
      return { value: s, source: text.slice(start, pos) };
    }
    function identifierName() {
      let name = '';
      let first = true;
      for (;;) {
        let ch;
        const c = text[pos];
        if (c === '\\') {
          if (text[pos + 1] !== 'u') { pos++; fail(); }
          pos += 2;
          ch = String.fromCharCode(hexDigits(4));
        } else {
          if (c === undefined) break;
          ch = String.fromCodePoint(text.codePointAt(pos));
          if (!(first ? ID_START : ID_CONTINUE).test(ch)) break;
          pos += ch.length;
        }
        if (!(first ? ID_START : ID_CONTINUE).test(ch)) fail(`invalid identifier character`);
        name += ch;
        first = false;
      }
      if (first) fail();
      return name;
    }
    function word(w, value) {
      if (text.startsWith(w, pos)) { pos += w.length; return value; }
      // the first character that differs
      for (let i = 0; i < w.length; i++) if (text[pos + i] !== w[i]) { pos += i; break; }
      return fail();
    }
    function number() {
      const start = pos;
      let sign = 1;
      if (text[pos] === '+' || text[pos] === '-') { if (text[pos] === '-') sign = -1; pos++; }
      const c = text[pos];
      if (c === 'I') return { value: sign * word('Infinity', Infinity), source: text.slice(start, pos) };
      if (c === 'N') { word('NaN', NaN); return { value: NaN, source: text.slice(start, pos) }; }
      if (c === '0' && (text[pos + 1] === 'x' || text[pos + 1] === 'X')) {
        pos += 2;
        const h = pos;
        while (/[0-9a-fA-F]/.test(text[pos] || '')) pos++;
        if (pos === h) fail();
        return { value: sign * Number('0x' + text.slice(h, pos)), source: text.slice(start, pos) };
      }
      const digits = () => { const d = pos; while (/[0-9]/.test(text[pos] || '')) pos++; return pos - d; };
      let intDigits = 0;
      if (c === '0') { pos++; intDigits = 1; }
      else if (/[1-9]/.test(c || '')) intDigits = digits();
      let fracDigits = 0;
      if (text[pos] === '.') { pos++; fracDigits = digits(); }
      if (intDigits === 0 && fracDigits === 0) fail();
      if (text[pos] === 'e' || text[pos] === 'E') {
        pos++;
        if (text[pos] === '+' || text[pos] === '-') pos++;
        if (digits() === 0) fail();
      }
      const raw = text.slice(start, pos);
      return { value: sign * Number(raw.replace(/^[+-]/, '')), source: raw };
    }
    function value() {
      skipSpace();
      const c = text[pos];
      if (c === undefined) fail('invalid end of input');
      if (c === '{') {
        pos++;
        const o = {};
        skipSpace();
        if (text[pos] === '}') { pos++; return o; }
        for (;;) {
          skipSpace();
          if (text[pos] === undefined) fail('invalid end of input');
          let key;
          if (text[pos] === '"' || text[pos] === "'") key = string().value;
          else key = identifierName();
          skipSpace();
          if (text[pos] !== ':') { if (text[pos] === undefined) fail('invalid end of input'); fail(); }
          pos++;
          const v = value();
          defineProperty(o, key, { value: v, writable: true, enumerable: true, configurable: true });
          skipSpace();
          if (text[pos] === ',') {
            pos++;
            skipSpace();
            if (text[pos] === '}') { pos++; return o; }
            continue;
          }
          if (text[pos] === '}') { pos++; return o; }
          if (text[pos] === undefined) fail('invalid end of input');
          fail();
        }
      }
      if (c === '[') {
        pos++;
        const a = [];
        skipSpace();
        if (text[pos] === ']') { pos++; return a; }
        for (;;) {
          a.push(value());
          skipSpace();
          if (text[pos] === ',') {
            pos++;
            skipSpace();
            if (text[pos] === ']') { pos++; return a; }
            continue;
          }
          if (text[pos] === ']') { pos++; return a; }
          if (text[pos] === undefined) fail('invalid end of input');
          fail();
        }
      }
      let r;
      if (c === '"' || c === "'") r = string();
      else if (c === 'n') { const s = pos; word('null', null); r = { value: null, source: text.slice(s, pos) }; }
      else if (c === 't') { const s = pos; word('true', true); r = { value: true, source: text.slice(s, pos) }; }
      else if (c === 'f') { const s = pos; word('false', false); r = { value: false, source: text.slice(s, pos) }; }
      else if (/[-+.0-9IN]/.test(c)) r = number();
      else fail();
      if (wantSources) sources.push(r.source);
      return r.value;
    }
    const result = value();
    skipSpace();
    if (pos < text.length) fail();
    return { result, sources };
  }

  function parse(text, reviver = undefined) {
    const s = String(text);
    const wantSources = typeof reviver === 'function';
    const { result, sources } = parseText(s, wantSources);
    if (!wantSources) return result;
    // InternalizeJSONProperty, the primitives' source texts handed out in parse order
    let next = 0;
    const internalize = (holder, key) => {
      const v = holder[key];
      if (v !== null && typeof v === 'object') {
        if (isArray(v)) {
          for (let i = 0; i < v.length; i++) {
            const nv = internalize(v, String(i));
            if (nv === undefined) delete v[i]; else v[i] = nv;
          }
        } else {
          for (const k of Object.keys(v)) {
            const nv = internalize(v, k);
            if (nv === undefined) delete v[k]; else defineProperty(v, k, { value: nv, writable: true, enumerable: true, configurable: true });
          }
        }
        return apply(reviver, holder, [key, v, {}]);
      }
      const context = {};
      if (next < sources.length) context.source = sources[next++];
      return apply(reviver, holder, [key, v, context]);
    };
    return internalize({ '': result }, '');
  }

  // ---------------- stringify ----------------
  const isNumberObject = (v) => { try { Number.prototype.valueOf.call(v); return typeof v === 'object'; } catch { return false; } };
  const isStringObject = (v) => { try { String.prototype.valueOf.call(v); return typeof v === 'object'; } catch { return false; } };
  const isBooleanObject = (v) => { try { Boolean.prototype.valueOf.call(v); return typeof v === 'object'; } catch { return false; } };
  const isBigIntObject = (v) => { try { BigInt.prototype.valueOf.call(v); return typeof v === 'object'; } catch { return false; } };
  const booleanValue = Boolean.prototype.valueOf;
  const firstCodePoints = (s, count) => { let out = '', i = 0; for (const ch of s) { if (i++ >= count) break; out += ch; } return out; };

  function stringify(value, replacer = undefined, space = undefined) {
    const stack = [];
    let indent = '', gap = '', quote, propertyList, replacerFunc;
    if (replacer !== null && typeof replacer === 'object' && !isArray(replacer)) {
      space = replacer.space;
      quote = replacer.quote;
      replacer = replacer.replacer;
    }
    if (quote !== undefined && quote !== null) {
      quote = String(quote);
      if (quote !== "'" && quote !== '"') quote = undefined;
    } else quote = undefined;
    if (typeof replacer === 'function') replacerFunc = replacer;
    else if (isArray(replacer)) {
      propertyList = [];
      const len = replacer.length;
      for (let i = 0; i < len; i++) {
        const v = replacer[i];
        let item;
        if (typeof v === 'string') item = v;
        else if (typeof v === 'number' || isStringObject(v) || isNumberObject(v)) item = String(v);
        if (item !== undefined && !propertyList.includes(item)) propertyList.push(item);
      }
    }
    if (isNumberObject(space)) space = Number(space);
    else if (isStringObject(space)) space = String(space);
    if (typeof space === 'number') {
      if (space >= 1) gap = ' '.repeat(Math.min(10, Math.floor(space)));
    } else if (typeof space === 'string') gap = firstCodePoints(space, 10);

    function quoteString(s) {
      const quotes = { "'": 0.1, '"': 0.2 };
      const replacements = { "'": "\\'", '"': '\\"', '\\': '\\\\', '\b': '\\b', '\f': '\\f', '\n': '\\n', '\r': '\\r',
        '\t': '\\t', '\v': '\\v', '\0': '\\0', '\u2028': '\\u2028', '\u2029': '\\u2029' };
      let product = '';
      for (let i = 0; i < s.length; i++) {
        const c = s[i];
        if (c === "'" || c === '"') { quotes[c]++; product += c; continue; }
        if (c === '\0' && /[0-9]/.test(s[i + 1] || '')) { product += '\\x00'; continue; }
        if (replacements[c] !== undefined) { product += replacements[c]; continue; }
        if (c < ' ') { product += '\\x' + c.charCodeAt(0).toString(16).toUpperCase().padStart(2, '0'); continue; }
        product += c;
      }
      const quoteChar = quote !== undefined ? quote : (quotes["'"] <= quotes['"'] ? "'" : '"');
      product = product.split(quoteChar).join(replacements[quoteChar]);
      return quoteChar + product + quoteChar;
    }
    function serializeKey(key) {
      if (key.length === 0) return quoteString(key);
      const first = fromCodePoint(key.codePointAt(0));
      if (!ID_START.test(first)) return quoteString(key);
      for (let i = first.length; i < key.length;) {
        const ch = fromCodePoint(key.codePointAt(i));
        if (!ID_CONTINUE.test(ch)) return quoteString(key);
        i += ch.length;
      }
      return key;
    }
    function serializeProperty(key, holder) {
      let v = holder[key];
      if (v !== null && (typeof v === 'object' || typeof v === 'bigint')) {
        const m5 = v.toJSON5;
        if (typeof m5 === 'function') v = apply(m5, v, [key]);
        else {
          const m = v.toJSON;
          if (typeof m === 'function') v = apply(m, v, [key]);
        }
      }
      if (replacerFunc) v = apply(replacerFunc, holder, [key, v]);
      if (typeof v === 'object' && v !== null) {
        if (isNumberObject(v)) v = Number(v);
        else if (isStringObject(v)) v = String(v);
        else if (isBooleanObject(v)) v = apply(booleanValue, v, []);
        else if (isBigIntObject(v)) throw new TypeError('BigInt value can\'t be serialized in JSON5');
      }
      if (v === null) return 'null';
      if (v === true) return 'true';
      if (v === false) return 'false';
      if (typeof v === 'string') return quoteString(v);
      if (typeof v === 'number') return String(v);
      if (typeof v === 'bigint') throw new TypeError('BigInt value can\'t be serialized in JSON5');
      if (typeof v === 'object') return isArray(v) ? serializeArray(v) : serializeObject(v);
      return undefined;
    }
    function enter(v) {
      if (stack.includes(v)) throw new TypeError('Converting circular structure to JSON5');
      stack.push(v);
    }
    function serializeObject(v) {
      enter(v);
      const stepback = indent;
      indent += gap;
      const keys = propertyList !== undefined ? propertyList : Object.keys(v);
      const partial = [];
      for (const key of keys) {
        const ps = serializeProperty(key, v);
        if (ps !== undefined) partial.push(serializeKey(key) + ':' + (gap !== '' ? ' ' : '') + ps);
      }
      let out;
      if (partial.length === 0) out = '{}';
      else if (gap === '') out = '{' + partial.join(',') + '}';
      else out = '{\n' + indent + partial.join(',\n' + indent) + ',\n' + stepback + '}';
      stack.pop();
      indent = stepback;
      return out;
    }
    function serializeArray(v) {
      enter(v);
      const stepback = indent;
      indent += gap;
      const partial = [];
      const len = v.length;
      for (let i = 0; i < len; i++) {
        const ps = serializeProperty(String(i), v);
        partial.push(ps !== undefined ? ps : 'null');
      }
      let out;
      if (partial.length === 0) out = '[]';
      else if (gap === '') out = '[' + partial.join(',') + ']';
      else out = '[\n' + indent + partial.join(',\n' + indent) + ',\n' + stepback + ']';
      stack.pop();
      indent = stepback;
      return out;
    }
    return serializeProperty('', { '': value });
  }

  shared.hostModules['goccia:json5'] = () => ({ parse, stringify });
  // a .json5 file imports as its top-level object's properties
  shared.dataFormats['.json5'] = (text) => parse(text);
  // GocciaScript reads .jsonc (JSON with comments) with the JSON5 parser, as it does .json5
  shared.dataFormats['.jsonc'] = (text) => parse(text);
})
