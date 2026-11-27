// Web platform globals for the CLI host: DOMException (Web IDL), atob and btoa (HTML forgiving-base64),
// and structuredClone (HTML StructuredSerialize / StructuredDeserialize within one realm). Called once per realm
// with the global object, the host's natives (n) and the preludes' shared internals; installs the globals and sets
// shared.DOMException for the other host preludes.
(function (G, n, shared) {
  'use strict';
  const defineProperty = Object.defineProperty;
  const getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  const getPrototypeOf = Object.getPrototypeOf;
  const ownKeys = Reflect.ownKeys;
  const apply = Reflect.apply;
  const hidden = (o, k, v) => defineProperty(o, k, { value: v, writable: true, enumerable: false, configurable: true });
  // what the functions below call, captured before any script runs: a script that replaces a prototype method or a
  // global does not reach into these built-ins
  const { TypeError, RangeError, String, Uint8Array, ArrayBuffer, SharedArrayBuffer, DataView, Map, Date, RegExp, Error } = G;
  const ToObject = Object, isView = ArrayBuffer.isView, SetCtor = Set, ArrayCtor = Array;
  const uncurryThis = Function.prototype.bind.bind(Function.prototype.call);
  const StringPrototypeCharCodeAt = uncurryThis(String.prototype.charCodeAt), StringPrototypeSlice = uncurryThis(String.prototype.slice);
  const StringPrototypeToLowerCase = uncurryThis(String.prototype.toLowerCase);
  const ArrayPrototypeIncludes = uncurryThis(Array.prototype.includes);
  const RegExpPrototypeExec = uncurryThis(RegExp.prototype.exec);
  const ObjectPrototypeHasOwnProperty = uncurryThis(Object.prototype.hasOwnProperty);
  const ObjectPrototypeToString = uncurryThis(Object.prototype.toString);
  const MapPrototypeHas = uncurryThis(Map.prototype.has), MapPrototypeGet = uncurryThis(Map.prototype.get);
  const MapPrototypeSet = uncurryThis(Map.prototype.set), SetPrototypeHas = uncurryThis(Set.prototype.has);
  const TAP = getPrototypeOf(Uint8Array.prototype);
  const TypedArrayPrototypeSet = uncurryThis(TAP.set);
  const taTagOf = uncurryThis(getOwnPropertyDescriptor(TAP, Symbol.toStringTag).get);
  const taLengthOf = uncurryThis(getOwnPropertyDescriptor(TAP, 'length').get);
  const taBufferOf = uncurryThis(getOwnPropertyDescriptor(TAP, 'buffer').get);
  const taOffsetOf = uncurryThis(getOwnPropertyDescriptor(TAP, 'byteOffset').get);
  const taByteLengthOf = uncurryThis(getOwnPropertyDescriptor(TAP, 'byteLength').get);
  const dvBufferOf = uncurryThis(getOwnPropertyDescriptor(DataView.prototype, 'buffer').get);
  const dvOffsetOf = uncurryThis(getOwnPropertyDescriptor(DataView.prototype, 'byteOffset').get);
  const dvByteLengthOf = uncurryThis(getOwnPropertyDescriptor(DataView.prototype, 'byteLength').get);
  // a typed array's or DataView's buffer, offset and length, read from its internal slots
  const viewBuffer = (v) => (taTagOf(v) !== undefined ? taBufferOf(v) : dvBufferOf(v));
  const viewOffset = (v) => (taTagOf(v) !== undefined ? taOffsetOf(v) : dvOffsetOf(v));
  const viewLength = (v) => (taTagOf(v) !== undefined ? taByteLengthOf(v) : dvByteLengthOf(v));
  // a list append that defines the index (a setter on Array.prototype must not see it)
  const append = (list, v) => { defineProperty(list, list.length, { value: v, writable: true, enumerable: true, configurable: true }); };
  // ASCII whitespace (the Infra standard's: tab, LF, FF, CR, space)
  const isASCIIWhitespace = (c) => c === ' ' || c === '\t' || c === '\n' || c === '\f' || c === '\r';
  // s without ASCII whitespace at its ends, or anywhere (everywhere)
  const strip = (s, everywhere) => {
    let a = 0, b = s.length;
    if (!everywhere) {
      while (a < b && isASCIIWhitespace(s[a])) a++;
      while (b > a && isASCIIWhitespace(s[b - 1])) b--;
    }
    let r = '';
    for (let i = a; i < b; i++) if (!everywhere || !isASCIIWhitespace(s[i])) r += s[i];
    return r;
  };

  // ---------------- DOMException ----------------
  // the legacy codes of the names that have one (Web IDL, DOMException names table)
  const CODES = {
    IndexSizeError: 1, HierarchyRequestError: 3, WrongDocumentError: 4, InvalidCharacterError: 5,
    NoModificationAllowedError: 7, NotFoundError: 8, NotSupportedError: 9, InvalidStateError: 11, SyntaxError: 12,
    InvalidModificationError: 13, NamespaceError: 14, InvalidAccessError: 15, TypeMismatchError: 17,
    SecurityError: 18, NetworkError: 19, AbortError: 20, URLMismatchError: 21, QuotaExceededError: 22,
    TimeoutError: 23, InvalidNodeTypeError: 24, DataCloneError: 25
  };
  // an Error by its prototype chain but not by Error.isError (Web IDL: no [[ErrorData]]), with a message and a
  // stack of its own
  const ErrorCtor = Error;
  class DOMException {
    #name;
    constructor(message = '', name = 'Error') {
      this.#name = String(name);
      // GocciaScript keeps name, message and code as the instance's own enumerable fields (so deep equality tells
      // DOMExceptions apart), besides the prototype's getters
      const code = CODES[this.#name];
      defineProperty(this, 'name', { value: this.#name, writable: true, enumerable: true, configurable: true });
      defineProperty(this, 'message', { value: String(message), writable: true, enumerable: true, configurable: true });
      defineProperty(this, 'code', { value: code === undefined ? 0 : code, writable: true, enumerable: true, configurable: true });
      const stack = new ErrorCtor(String(message)).stack;
      if (typeof stack === 'string')
        defineProperty(this, 'stack', { value: stack, writable: true, enumerable: false, configurable: true });
    }
    get name() {
      if (this === null || typeof this !== 'object' || !(#name in this)) throw new TypeError('DOMException.prototype.name: not a DOMException');
      return this.#name;
    }
    get code() {
      if (this === null || typeof this !== 'object' || !(#name in this)) throw new TypeError('DOMException.prototype.code: not a DOMException');
      const c = CODES[this.#name];
      return c === undefined ? 0 : c;
    }
    static isDOMException(v) { return v !== null && typeof v === 'object' && #name in v; }
  }
  Object.setPrototypeOf(DOMException.prototype, ErrorCtor.prototype);
  Object.setPrototypeOf(DOMException, ErrorCtor);
  const isDOMException = DOMException.isDOMException;
  delete DOMException.isDOMException;
  for (const k of ['name', 'code']) {
    const d = getOwnPropertyDescriptor(DOMException.prototype, k);
    d.enumerable = true;
    defineProperty(DOMException.prototype, k, d);
  }
  defineProperty(DOMException.prototype, Symbol.toStringTag, { value: 'DOMException', configurable: true });
  for (const [name, code] of Object.entries(CODES)) {
    const constName = name.replace(/([a-z])([A-Z])/g, '$1_$2').toUpperCase().replace(/_ERROR$/, '_ERR');
    const desc = { value: code, writable: false, enumerable: true, configurable: false };
    defineProperty(DOMException, constName, desc);
    defineProperty(DOMException.prototype, constName, desc);
  }

  // ---------------- atob / btoa ----------------
  const fromBase64 = Uint8Array.fromBase64;
  const toBase64 = Uint8Array.prototype.toBase64;
  const fromCharCode = String.fromCharCode;
  function atob(data) {
    if (arguments.length < 1) throw new TypeError("Failed to execute 'atob': 1 argument required, but only 0 present.");
    let s = String(data);
    if (RegExpPrototypeExec(/[\t\n\f\r ]/, s) !== null) s = strip(s, true);
    if (s.length % 4 === 0) s = s[s.length - 1] !== '=' ? s : StringPrototypeSlice(s, 0, s[s.length - 2] === '=' ? -2 : -1);
    if (s.length % 4 === 1 || RegExpPrototypeExec(/[^A-Za-z0-9+/]/, s) !== null)
      throw new DOMException("Failed to execute 'atob': The string to be decoded is not correctly encoded.", 'InvalidCharacterError');
    const bytes = fromBase64(s, { lastChunkHandling: 'loose' });
    let out = '';
    // by chunks, each a view of the bytes with an own length (subarray would look up the species, and apply would
    // read the length through %TypedArray%.prototype's getter)
    const length = taLengthOf(bytes), buffer = taBufferOf(bytes), offset = taOffsetOf(bytes);
    for (let i = 0; i < length; i += 8192) {
      const n = length < i + 8192 ? length - i : 8192, chunk = new Uint8Array(buffer, offset + i, n);
      defineProperty(chunk, 'length', { value: n });
      out += apply(fromCharCode, null, chunk);
    }
    return out;
  }
  function btoa(data) {
    if (arguments.length < 1) throw new TypeError("Failed to execute 'btoa': 1 argument required, but only 0 present.");
    const s = String(data);
    const bytes = new Uint8Array(s.length);
    for (let i = 0; i < s.length; i++) {
      const c = StringPrototypeCharCodeAt(s, i);
      if (c > 255)
        throw new DOMException("Failed to execute 'btoa': The string to be encoded contains characters outside of the Latin1 range.", 'InvalidCharacterError');
      bytes[i] = c;
    }
    return apply(toBase64, bytes, []);
  }

  // ---------------- structuredClone ----------------
  // brand checks through the intrinsics, captured now so later changes to the prototypes cannot reach them
  const brand = (fn) => (v) => { try { apply(fn, v, []); return true; } catch { return false; } };
  const getter = (proto, k) => getOwnPropertyDescriptor(proto, k).get;
  const dateValue = Date.prototype.getTime;
  const isDate = brand(dateValue);
  const regexpSource = getter(RegExp.prototype, 'source');
  // a RegExp's flags from its internal slot, each flag's own getter (the flags getter would look each one up)
  const FLAG_GETTERS = [['d', 'hasIndices'], ['g', 'global'], ['i', 'ignoreCase'], ['m', 'multiline'], ['s', 'dotAll'],
    ['u', 'unicode'], ['v', 'unicodeSets'], ['y', 'sticky']];
  for (let i = 0; i < FLAG_GETTERS.length; i++) FLAG_GETTERS[i][1] = getter(RegExp.prototype, FLAG_GETTERS[i][1]);
  const regexpFlags = (re) => { let f = ''; for (let i = 0; i < FLAG_GETTERS.length; i++) if (apply(FLAG_GETTERS[i][1], re, [])) f += FLAG_GETTERS[i][0]; return f; };
  const isRegExp = (v) => { try { apply(regexpSource, v, []); return getPrototypeOf(v) !== null && v !== RegExp.prototype; } catch { return false; } };
  const boolValue = Boolean.prototype.valueOf, numValue = Number.prototype.valueOf, strValue = String.prototype.valueOf;
  const bigValue = BigInt.prototype.valueOf, symValue = Symbol.prototype.valueOf;
  const isBool = brand(boolValue), isNum = brand(numValue), isStr = brand(strValue), isBig = brand(bigValue);
  const isSym = brand(symValue);
  const abByteLength = getter(ArrayBuffer.prototype, 'byteLength');
  const abDetached = getter(ArrayBuffer.prototype, 'detached');
  const abResizable = getter(ArrayBuffer.prototype, 'resizable'), abMax = getter(ArrayBuffer.prototype, 'maxByteLength');
  const isAB = brand(abByteLength);
  const sabByteLength = getter(SharedArrayBuffer.prototype, 'byteLength');
  const isSAB = brand(sabByteLength);
  const TypedArrayProto = getPrototypeOf(Uint8Array.prototype);
  const taTag = getter(TypedArrayProto, Symbol.toStringTag);
  const taBuffer = getter(TypedArrayProto, 'buffer'), taOffset = getter(TypedArrayProto, 'byteOffset');
  const taLength = getter(TypedArrayProto, 'length');
  const dvBuffer = getter(DataView.prototype, 'buffer'), dvOffset = getter(DataView.prototype, 'byteOffset');
  const dvLength = getter(DataView.prototype, 'byteLength');
  const isDV = brand(dvBuffer);
  const mapForEach = Map.prototype.forEach, mapSet = Map.prototype.set, isMap = brand(getter(Map.prototype, 'size'));
  const setForEach = Set.prototype.forEach, setAdd = Set.prototype.add, isSet = brand(getter(Set.prototype, 'size'));
  const isError = Error.isError;
  const isArray = Array.isArray;
  const ERROR_TYPES = { Error, EvalError, RangeError, ReferenceError, SyntaxError, TypeError, URIError };
  const ctors = { Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array,
    Float32Array, Float64Array, BigInt64Array, BigUint64Array };

  function cloneError(what) {
    return new DOMException(`${what} could not be cloned.`, 'DataCloneError');
  }
  function describe(v) {
    if (typeof v === 'function') return v.name ? `function ${v.name}` : 'function';
    if (typeof v === 'symbol') return String(v);
    return 'value';
  }

  // the nesting a clone follows before it gives up, as a RangeError
  const MAX_DEPTH = 256;
  // the prototypes of the engine's and the host's classes (a prelude's class keeps its internal slots in private
  // fields, so an instance is an ordinary object): an object that inherits from one has no serialization; found once
  // every host prelude has run (host.js calls shared.collectCloneProtos), before any script can change the globals
  let intrinsicProtos = null;
  const fnToString = Function.prototype.toString, nativeSource = /\{\s*\[native code\]\s*\}\s*$/;
  function collectIntrinsicProtos() {
    const set = new Set();
    const add = (p) => { if (p !== null && typeof p === 'object' && p !== Object.prototype && p !== Array.prototype) set.add(p); };
    const scan = (ns) => {
      for (const k of ownKeys(ns)) {
        const d = getOwnPropertyDescriptor(ns, k);
        if (d === undefined || !('value' in d)) continue;
        const v = d.value;
        // a prelude's class: compiled JS whose source reads as native code (a native constructor's instances
        // carry their kind, which plainCloneable sees, and a script's class shows its source)
        if (typeof v === 'function' && n.isBytecode(v) && nativeSource.test(apply(fnToString, v, []))) {
          const pd = getOwnPropertyDescriptor(v, 'prototype');
          if (pd !== undefined && 'value' in pd) add(pd.value);
        }
      }
    };
    scan(G);
    // the global Blob is a Proxy over fetch.js's class (its [[Construct]] converts the arguments first)
    if (typeof shared.Blob === 'function') add(shared.Blob.prototype);
    for (const ns of ['Temporal', 'Intl']) if (G[ns] !== null && typeof G[ns] === 'object') scan(G[ns]);
    add(getPrototypeOf([][Symbol.iterator]()));
    add(getPrototypeOf(new Map().entries()));
    add(getPrototypeOf(new Set().values()));
    add(getPrototypeOf(''[Symbol.iterator]()));
    add(getPrototypeOf('a'.matchAll(/a/g)));
    add(getPrototypeOf([].values().map((x) => x)));
    add(getPrototypeOf(function* () {}.prototype));
    add(getPrototypeOf(async function* () {}.prototype));
    if (typeof Iterator === 'function') add(Iterator.prototype);
    return set;
  }
  function hasIntrinsicProto(v) {
    for (let p = getPrototypeOf(v); p !== null; p = getPrototypeOf(p)) if (SetPrototypeHas(intrinsicProtos, p)) return true;
    return false;
  }

  function structuredClone(value, options = undefined) {
    if (arguments.length < 1)
      throw new TypeError("Failed to execute 'structuredClone': 1 argument required, but only 0 present.");
    void options;
    const memory = new Map();
    if (intrinsicProtos === null) intrinsicProtos = collectIntrinsicProtos();   // a host that did not call it
    let depth = 0;
    const clone = (v) => {
      if (++depth > MAX_DEPTH) throw new RangeError('Maximum call stack size exceeded');
      try { return cloneAt(v); } finally { depth--; }
    };
    const cloneAt = (v) => {
      const t = typeof v;
      if (t === 'symbol') throw cloneError(describe(v));
      if (t === 'function') throw cloneError(describe(v));
      if (v === null || t !== 'object') return v;
      if (MapPrototypeHas(memory, v)) return MapPrototypeGet(memory, v);
      let out;
      if (isBool(v)) { out = ToObject(apply(boolValue, v, [])); MapPrototypeSet(memory, v, out); return out; }
      if (isNum(v)) { out = ToObject(apply(numValue, v, [])); MapPrototypeSet(memory, v, out); return out; }
      if (isBig(v)) { out = ToObject(apply(bigValue, v, [])); MapPrototypeSet(memory, v, out); return out; }
      if (isStr(v)) { out = ToObject(apply(strValue, v, [])); MapPrototypeSet(memory, v, out); return out; }
      if (isSym(v)) throw cloneError('Symbol object');
      if (isDate(v)) { out = new Date(apply(dateValue, v, [])); MapPrototypeSet(memory, v, out); return out; }
      if (isRegExp(v)) { out = new RegExp(apply(regexpSource, v, []), regexpFlags(v)); MapPrototypeSet(memory, v, out); return out; }
      if (isSAB(v)) {
        const len = apply(sabByteLength, v, []);
        out = new SharedArrayBuffer(len);
        TypedArrayPrototypeSet(new Uint8Array(out), new Uint8Array(v));
        MapPrototypeSet(memory, v, out);
        return out;
      }
      if (isAB(v)) {
        if (apply(abDetached, v, [])) throw new DOMException('An ArrayBuffer is detached and could not be cloned.', 'DataCloneError');
        if (apply(abResizable, v, [])) {
          out = new ArrayBuffer(apply(abByteLength, v, []), { maxByteLength: apply(abMax, v, []) });
          TypedArrayPrototypeSet(new Uint8Array(out), new Uint8Array(v));
        } else {
          // a copy (slice would look up the species constructor)
          out = new ArrayBuffer(apply(abByteLength, v, []));
          TypedArrayPrototypeSet(new Uint8Array(out), new Uint8Array(v));
        }
        MapPrototypeSet(memory, v, out);
        return out;
      }
      const tag = apply(taTag, v, []);
      if (tag !== undefined) {
        const info = n.viewInfo(v);   // [length-tracking, out of bounds]
        if (info[1]) throw new DOMException(`${tag} is out of bounds of its ArrayBuffer and could not be cloned.`, 'DataCloneError');
        const b = clone(apply(taBuffer, v, []));
        out = info[0] ? new ctors[tag](b, apply(taOffset, v, [])) : new ctors[tag](b, apply(taOffset, v, []), apply(taLength, v, []));
        MapPrototypeSet(memory, v, out);
        return out;
      }
      if (isDV(v)) {
        const dvInfo = n.viewInfo(v);
        if (dvInfo[1]) throw new DOMException('DataView is out of bounds of its ArrayBuffer and could not be cloned.', 'DataCloneError');
        const len = apply(dvLength, v, []);
        const tracking = dvInfo[0];
        const b = clone(apply(dvBuffer, v, []));
        out = tracking ? new DataView(b, apply(dvOffset, v, [])) : new DataView(b, apply(dvOffset, v, []), len);
        MapPrototypeSet(memory, v, out);
        return out;
      }
      if (isDOMException(v)) {
        out = new DOMException(v.message, v.name);
        defineProperty(out, 'stack', { value: v.stack, writable: true, enumerable: false, configurable: true });
        MapPrototypeSet(memory, v, out);
        return out;
      }
      if (isMap(v)) {
        out = new Map();
        MapPrototypeSet(memory, v, out);
        const entries = [];
        apply(mapForEach, v, [(val, key) => append(entries, [key, val])]);
        for (let i = 0; i < entries.length; i++) apply(mapSet, out, [clone(entries[i][0]), clone(entries[i][1])]);
        return out;
      }
      if (isSet(v)) {
        out = new SetCtor();
        MapPrototypeSet(memory, v, out);
        const items = [];
        apply(setForEach, v, [(val) => append(items, val)]);
        for (let i = 0; i < items.length; i++) apply(setAdd, out, [clone(items[i])]);
        return out;
      }
      if (isError(v)) {
        let name = v.name;
        name = name === undefined ? 'Error' : String(name);
        const Ctor = ObjectPrototypeHasOwnProperty(ERROR_TYPES, name) ? ERROR_TYPES[name] : Error;
        const hasMessage = getOwnPropertyDescriptor(v, 'message') !== undefined;
        const message = hasMessage ? String(v.message) : undefined;
        out = hasMessage ? new Ctor(message) : new Ctor();
        const stack = v.stack;
        if (typeof stack === 'string') defineProperty(out, 'stack', { value: stack, writable: true, enumerable: false, configurable: true });
        MapPrototypeSet(memory, v, out);
        return out;
      }
      if (!n.plainCloneable(v) || hasIntrinsicProto(v)) throw cloneError(StringPrototypeSlice(ObjectPrototypeToString(v), 8, -1) + ' object');
      if (isArray(v)) {
        out = new ArrayCtor(v.length);
        MapPrototypeSet(memory, v, out);
      } else {
        out = {};
        MapPrototypeSet(memory, v, out);
      }
      const keys = ownKeys(v);
      for (let i = 0; i < keys.length; i++) {
        const key = keys[i];
        if (typeof key !== 'string') continue;
        const d = getOwnPropertyDescriptor(v, key);
        if (d === undefined || !d.enumerable) continue;
        if (isArray(v) && key === 'length') continue;
        out[key] = clone(v[key]);
      }
      return out;
    };
    return clone(value);
  }

  // ---------------- TextEncoder / TextDecoder (WHATWG Encoding, UTF-8) ----------------
  const sabGrowable = getter(SharedArrayBuffer.prototype, 'growable');
  // Web IDL without [AllowResizable]: a resizable ArrayBuffer or a growable SharedArrayBuffer is refused, whatever
  // has happened to it since
  function resizableBuffer(b) {
    if (isAB(b)) return apply(abResizable, b, []);
    if (isSAB(b)) return apply(sabGrowable, b, []);
    return false;
  }
  const UTF8_LABELS = ['unicode-1-1-utf-8', 'unicode11utf8', 'unicode20utf8', 'utf-8', 'utf8', 'x-unicode20utf8'];
  class TextEncoder {
    #brand = true;
    constructor() {}
    static #check(e, what) {
      if (e === null || typeof e !== 'object' || !(#brand in e)) throw new TypeError(`TextEncoder.prototype.${what}: Illegal invocation`);
    }
    get encoding() { TextEncoder.#check(this, 'encoding'); return 'utf-8'; }
    // the host encodes (a lone surrogate becomes U+FFFD): the bytes are counted, reserved at once, then written
    encode(input = '') {
      TextEncoder.#check(this, 'encode');
      const s = String(input);
      const out = new Uint8Array(n.utf8Length(s));
      n.utf8Write(s, out, 0);
      return out;
    }
    encodeInto(source, destination) {
      TextEncoder.#check(this, 'encodeInto');
      if (!(destination instanceof Uint8Array)) throw new TypeError("Failed to execute 'encodeInto' on 'TextEncoder': parameter 2 is not of type 'Uint8Array'.");
      const s = String(source);   // converted first, as Web IDL converts the arguments in order
      if (resizableBuffer(taBufferOf(destination))) throw new TypeError("Failed to execute 'encodeInto' on 'TextEncoder': The provided Uint8Array value must not be resizable.");
      const r = n.utf8Write(s, destination, 0);
      return { read: r[0], written: r[1] };
    }
  }
  defineProperty(TextEncoder.prototype, Symbol.toStringTag, { value: 'TextEncoder', configurable: true });

  class TextDecoder {
    #fatal;
    #ignoreBOM;
    #pending = [];        // the bytes of an incomplete sequence a streaming decode kept
    #bomSeen = false;
    constructor(label = 'utf-8', options = undefined) {
      const l = StringPrototypeToLowerCase(strip(String(label), false));
      if (!ArrayPrototypeIncludes(UTF8_LABELS, l)) throw new RangeError(`The encoding label provided ('${String(label)}') is invalid.`);
      if (options !== undefined && options !== null && typeof options !== 'object' && typeof options !== 'function')
        throw new TypeError("Failed to construct 'TextDecoder': The provided value is not of type 'TextDecoderOptions'.");
      const o = options === undefined || options === null ? {} : options;
      this.#fatal = !!o.fatal;
      this.#ignoreBOM = !!o.ignoreBOM;
    }
    static #check(d, what) {
      if (d === null || typeof d !== 'object' || !(#fatal in d)) throw new TypeError(`TextDecoder.prototype.${what}: Illegal invocation`);
    }
    get encoding() { TextDecoder.#check(this, 'encoding'); return 'utf-8'; }
    get fatal() { TextDecoder.#check(this, 'fatal'); return this.#fatal; }
    get ignoreBOM() { TextDecoder.#check(this, 'ignoreBOM'); return this.#ignoreBOM; }
    decode(input = undefined, options = undefined) {
      TextDecoder.#check(this, 'decode');
      let bytes;
      const buffer = input === undefined ? undefined : isAB(input) || isSAB(input) ? input : isView(input) ? viewBuffer(input) : undefined;
      if (buffer !== undefined && resizableBuffer(buffer))
        throw new TypeError("Failed to execute 'decode' on 'TextDecoder': The provided ArrayBufferView value must not be resizable.");
      if (input === undefined) bytes = new Uint8Array(0);
      // a detached buffer, like a view of one, decodes as nothing
      else if (isAB(input) && apply(abDetached, input, [])) bytes = new Uint8Array(0);
      else if (isAB(input) || isSAB(input)) bytes = new Uint8Array(input);
      else if (isView(input)) {
        // a view of a detached buffer decodes as nothing
        bytes = isAB(buffer) && apply(abDetached, buffer, []) ? new Uint8Array(0)
          : new Uint8Array(buffer, viewOffset(input), viewLength(input));
      }
      else throw new TypeError("Failed to execute 'decode' on 'TextDecoder': The provided value is not of type '(ArrayBuffer or ArrayBufferView)'.");
      const stream = options !== undefined && options !== null && !!options.stream;
      let all = bytes, n = taLengthOf(bytes);
      if (this.#pending.length > 0) {
        all = [];
        for (let k = 0; k < this.#pending.length; k++) append(all, this.#pending[k]);
        for (let k = 0; k < n; k++) append(all, bytes[k]);
        n = all.length;
      }
      this.#pending = [];
      let out = '', i = 0;
      const bad = () => {
        if (this.#fatal) { this.#pending = []; this.#bomSeen = false; throw new TypeError("Failed to execute 'decode' on 'TextDecoder': The encoded data was not valid."); }
        out += '\uFFFD';
      };
      while (i < n) {
        const b = all[i];
        if (b < 0x80) { out += fromCharCode(b); i++; continue; }
        let need, cp, lower = 0x80, upper = 0xBF;
        if (b >= 0xC2 && b <= 0xDF) { need = 1; cp = b & 0x1F; }
        else if (b >= 0xE0 && b <= 0xEF) { need = 2; cp = b & 0xF; if (b === 0xE0) lower = 0xA0; if (b === 0xED) upper = 0x9F; }
        else if (b >= 0xF0 && b <= 0xF4) { need = 3; cp = b & 7; if (b === 0xF0) lower = 0x90; if (b === 0xF4) upper = 0x8F; }
        else { bad(); i++; continue; }
        let j = 1;
        for (; j <= need; j++) {
          if (i + j >= n) break;
          const c = all[i + j];
          if (c < lower || c > upper) break;
          lower = 0x80; upper = 0xBF;
          cp = (cp << 6) | (c & 0x3F);
        }
        if (j > need) {
          out += cp >= 0x10000 ? fromCharCode(0xD800 + ((cp - 0x10000) >> 10), 0xDC00 + ((cp - 0x10000) & 0x3FF)) : fromCharCode(cp);
          i += need + 1;
        } else if (i + j >= n && stream) {
          for (let k = i; k < n; k++) append(this.#pending, all[k]);
          i = n;
        } else { bad(); i += j; }
      }
      if (!this.#ignoreBOM && !this.#bomSeen && out.length > 0) {
        if (StringPrototypeCharCodeAt(out, 0) === 0xFEFF) out = StringPrototypeSlice(out, 1);
        this.#bomSeen = true;
      } else if (out.length > 0) this.#bomSeen = true;
      if (!stream) this.#bomSeen = false;
      return out;
    }
  }
  defineProperty(TextDecoder.prototype, Symbol.toStringTag, { value: 'TextDecoder', configurable: true });
  for (const [C, names] of [[TextEncoder, ['encoding']], [TextDecoder, ['encoding', 'fatal', 'ignoreBOM']]]) {
    for (const k of names) { const d = getOwnPropertyDescriptor(C.prototype, k); d.enumerable = true; defineProperty(C.prototype, k, d); }
  }

  hidden(G, 'TextEncoder', TextEncoder);
  hidden(G, 'TextDecoder', TextDecoder);
  hidden(G, 'DOMException', DOMException);
  hidden(G, 'atob', atob);
  shared.atob = atob;
  hidden(G, 'btoa', btoa);
  hidden(G, 'structuredClone', structuredClone);
  shared.collectCloneProtos = () => { intrinsicProtos = collectIntrinsicProtos(); };
  shared.DOMException = DOMException;
})
