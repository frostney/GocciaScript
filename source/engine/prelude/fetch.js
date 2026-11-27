// Blob (File API), Headers and Response (Fetch Standard) for the CLI host. The engine has no ReadableStream,
// so a body is bytes held whole. Called once per realm with the global object, the host's natives (n) and the
// preludes' shared internals; installs the globals and sets shared.Blob, shared.Headers and shared.Response for
// fetch.
(function (G, n, shared) {
  'use strict';
  const defineProperty = Object.defineProperty;
  const getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  const apply = Reflect.apply;
  const hidden = (o, k, v) => defineProperty(o, k, { __proto__: null, value: v, writable: true, enumerable: false, configurable: true });
  const isObject = (v) => v !== null && (typeof v === 'object' || typeof v === 'function');
  // Web IDL: an interface's operations and attributes are enumerable
  function webIDL(C, names) {
    for (const k of names) {
      const d = getOwnPropertyDescriptor(C.prototype, k);
      d.enumerable = true;
      defineProperty(C.prototype, k, d);
    }
  }
  const getter = (proto, k) => getOwnPropertyDescriptor(proto, k).get;
  const abByteLength = getter(ArrayBuffer.prototype, 'byteLength');
  const abResizable = getter(ArrayBuffer.prototype, 'resizable');
  const abDetached = getter(ArrayBuffer.prototype, 'detached');
  const sabByteLength = getter(SharedArrayBuffer.prototype, 'byteLength');
  const brand = (fn) => (v) => { try { apply(fn, v, []); return true; } catch { return false; } };
  // what the functions below call, captured before any script runs: a script that replaces a prototype method or a
  // global does not reach into these interfaces
  const { TypeError, RangeError, String, Number, Promise, Uint8Array } = G;
  const NumberIsNaN = Number.isNaN, NumberIsFinite = Number.isFinite, NumberIsInteger = Number.isInteger;
  const MathFloor = Math.floor, MathMax = Math.max, MathMin = Math.min, ownKeys = Reflect.ownKeys;
  const JSONParse = JSON.parse, JSONStringify = JSON.stringify, isView = ArrayBuffer.isView;
  const SymIterator = Symbol.iterator, SymToStringTag = Symbol.toStringTag;
  const uncurryThis = Function.prototype.bind.bind(Function.prototype.call);
  const StringPrototypeToLowerCase = uncurryThis(String.prototype.toLowerCase);
  const StringPrototypeToUpperCase = uncurryThis(String.prototype.toUpperCase);
  const ArrayPrototypeSort = uncurryThis(Array.prototype.sort);
  const ObjectPrototypeHasOwnProperty = uncurryThis(Object.prototype.hasOwnProperty);
  const RegExpPrototypeExec = uncurryThis(RegExp.prototype.exec);
  const MapPrototypeForEach = uncurryThis(Map.prototype.forEach);
  const promiseResolve = Promise.resolve, promiseReject = Promise.reject;
  const PromiseResolve = (v) => apply(promiseResolve, Promise, [v]), PromiseReject = (e) => apply(promiseReject, Promise, [e]);
  const TAP = Object.getPrototypeOf(Uint8Array.prototype), DVP = DataView.prototype;
  const TypedArrayPrototypeSet = uncurryThis(TAP.set);
  const taTag = uncurryThis(getter(TAP, SymToStringTag));
  const taBuffer = uncurryThis(getter(TAP, 'buffer')), taByteOffset = uncurryThis(getter(TAP, 'byteOffset'));
  const taByteLength = uncurryThis(getter(TAP, 'byteLength')), taLength = uncurryThis(getter(TAP, 'length'));
  const dvBuffer = uncurryThis(getter(DVP, 'buffer')), dvByteOffset = uncurryThis(getter(DVP, 'byteOffset'));
  const dvByteLength = uncurryThis(getter(DVP, 'byteLength'));
  // a typed array's or DataView's buffer, offset and length, read from its internal slots
  const viewBuffer = (v) => (taTag(v) !== undefined ? taBuffer(v) : dvBuffer(v));
  const viewOffset = (v) => (taTag(v) !== undefined ? taByteOffset(v) : dvByteOffset(v));
  const viewLength = (v) => (taTag(v) !== undefined ? taByteLength(v) : dvByteLength(v));
  // a new Uint8Array holding a copy of length bytes of buffer from offset (slice would look up the species)
  const copyBytes = (buffer, offset, length) => new Uint8Array(new Uint8Array(buffer, offset, length));
  const sliceBytes = (bytes, from, to) => copyBytes(taBuffer(bytes), taByteOffset(bytes) + from, to - from);
  // a list append that defines the index (a setter on Array.prototype must not see it)
  const append = (list, v) => { defineProperty(list, list.length, { __proto__: null, value: v, writable: true, enumerable: true, configurable: true }); };
  // whether a type string has a character outside U+0020 to U+007E
  const nonPrintable = (s) => { for (let i = 0; i < s.length; i++) if (s[i] < ' ' || s[i] > '~') return true; return false; };
  const isAB = brand(abByteLength), isSAB = brand(sabByteLength);
  const encoder = new G.TextEncoder();
  const decoder = new G.TextDecoder();
  const encode = uncurryThis(G.TextEncoder.prototype.encode), decodeWith = uncurryThis(G.TextDecoder.prototype.decode);
  const utf8 = (s) => encode(encoder, s);
  const decode = (bytes) => decodeWith(decoder, bytes);
  const URLCtor = G.URL, urlHref = uncurryThis(getter(G.URL.prototype, 'href'));
  const URLSearchParamsCtor = G.URLSearchParams, searchParamsToString = uncurryThis(G.URLSearchParams.prototype.toString);

  // ---------------- Blob ----------------
  let blobBytes, blobType;   // a Blob's bytes and type, for the other interfaces
  // Web IDL: the arguments of new Blob(blobParts, options) convert, the parts first, before the new object is made
  // (NewTarget's prototype read) and before the constructor steps copy any buffer's bytes
  const BLOB_INIT = {};
  function convertBlobArgs(blobParts, options) {
    // the parts convert first, then the options (Web IDL argument order); buffer bytes are copied by the
    // constructor, after every part has converted
    const pieces = [];
    if (blobParts !== undefined) {
      if (!isObject(blobParts) || typeof blobParts[SymIterator] !== 'function')
        throw new TypeError("Failed to construct 'Blob': The provided value cannot be converted to a sequence.");
      // Web IDL sequence<BlobPart>: the argument's own iterator
      for (const part of blobParts) {
        const partBytes = blobBytes(part);
        if (partBytes !== undefined) append(pieces, { __proto__: null, blob: partBytes });
        else if (isObject(part) && isAB(part)) {
          if (apply(abResizable, part, [])) throw new TypeError("Failed to construct 'Blob': The provided ArrayBuffer value must not be resizable.");
          append(pieces, { __proto__: null, buffer: part });
        } else if (isObject(part) && isView(part)) {
          const buffer = viewBuffer(part);
          if (isSAB(buffer)) throw new TypeError("Failed to construct 'Blob': The provided ArrayBufferView value must not be shared.");
          if (isAB(buffer) && apply(abResizable, buffer, [])) throw new TypeError("Failed to construct 'Blob': The provided ArrayBufferView value must not be resizable.");
          append(pieces, { __proto__: null, view: part });
        } else {
          if (typeof part === 'symbol') throw new TypeError('Cannot convert a Symbol value to a string');
          // USVString: the encoding turns a lone surrogate into U+FFFD, as converting first would
          append(pieces, { __proto__: null, string: String(part) });
        }
      }
    }
    let type = '', native = false;
    if (options !== undefined && options !== null) {
      if (!isObject(options)) throw new TypeError("Failed to construct 'Blob': The provided value is not of type 'BlobPropertyBag'.");
      const endings = options.endings;
      if (endings !== undefined) {
        const e = String(endings);
        if (e !== 'transparent' && e !== 'native')
          throw new TypeError(`Failed to construct 'Blob': The provided value '${e}' is not a valid enum value of type EndingType.`);
        native = e === 'native';
      }
      const t = options.type;
      if (t !== undefined) type = String(t);
    }
    if (nonPrintable(type)) type = '';
    else type = StringPrototypeToLowerCase(type);
    return { pieces, type, native };
  }
  // line endings as LF (the File API's convert line endings to native, as GocciaScript has it)
  function nativeEndings(s) {
    let r = '';
    for (let i = 0; i < s.length; i++) {
      if (s[i] === '\r') { r += '\n'; if (s[i + 1] === '\n') i++; } else r += s[i];
    }
    return r;
  }
  class Blob {
    #bytes;
    #type;
    constructor(token = undefined, pieces = undefined, type = undefined, native = undefined) {
      // public construction comes through BlobCtor with the arguments converted; the prelude's own calls convert
      // here
      if (token !== BLOB_INIT) ({ pieces, type, native } = convertBlobArgs(token, pieces));
      // each part's bytes counted, the Blob's reserved at once (over the memory limit: the RangeError, before any
      // part is copied), then each part written into them
      const sizes = [];
      let size = 0;
      for (let i = 0; i < pieces.length; i++) {
        const p = pieces[i];
        let len;
        if (p.blob !== undefined) len = taLength(p.blob);
        else if (p.buffer !== undefined) len = apply(abDetached, p.buffer, []) ? 0 : apply(abByteLength, p.buffer, []);
        else if (p.view !== undefined) {
          const v = p.view, buffer = viewBuffer(v);
          len = isAB(buffer) && apply(abDetached, buffer, []) ? 0 : viewLength(v);
        } else {
          if (native) p.string = nativeEndings(p.string);
          len = n.utf8Length(p.string);
        }
        append(sizes, len);
        size += len;
      }
      const bytes = new Uint8Array(size);
      let at = 0;
      for (let i = 0; i < pieces.length; i++) {
        const p = pieces[i];
        if (sizes[i] > 0) {
          if (p.blob !== undefined) TypedArrayPrototypeSet(bytes, p.blob, at);
          else if (p.buffer !== undefined) TypedArrayPrototypeSet(bytes, new Uint8Array(p.buffer), at);
          else if (p.view !== undefined) TypedArrayPrototypeSet(bytes, new Uint8Array(viewBuffer(p.view), viewOffset(p.view), viewLength(p.view)), at);
          else n.utf8Write(p.string, bytes, at);
        }
        at += sizes[i];
      }
      this.#bytes = bytes;
      this.#type = type;
    }
    static #check(b, what) {
      if (!isObject(b) || !(#bytes in b)) throw new TypeError(`Failed to execute '${what}' on 'Blob': Illegal invocation`);
    }
    get size() { Blob.#check(this, 'size'); return taLength(this.#bytes); }
    get type() { Blob.#check(this, 'type'); return this.#type; }
    slice(start = undefined, end = undefined, contentType = undefined) {
      Blob.#check(this, 'slice');
      const len = taLength(this.#bytes);
      const rel = (v, d) => {
        if (v === undefined) return d;
        // Web IDL [Clamp] long long: clamped, then rounded to the nearest integer, ties to even
        let x = Number(v);
        if (NumberIsNaN(x)) x = 0;
        else if (NumberIsFinite(x)) {
          const f = MathFloor(x), diff = x - f;
          x = diff > 0.5 ? f + 1 : diff < 0.5 ? f : (f % 2 === 0 ? f : f + 1);
        }
        return x < 0 ? MathMax(len + x, 0) : MathMin(x, len);
      };
      const from = rel(start, 0), to = rel(end, len);
      let type = contentType === undefined ? '' : String(contentType);
      type = nonPrintable(type) ? '' : StringPrototypeToLowerCase(type);
      const b = new Blob();
      b.#bytes = sliceBytes(this.#bytes, from, MathMax(from, to));
      b.#type = type;
      return b;
    }
    text() {
      try { Blob.#check(this, 'text'); } catch (e) { return PromiseReject(e); }
      return PromiseResolve(decode(this.#bytes));
    }
    arrayBuffer() {
      try { Blob.#check(this, 'arrayBuffer'); } catch (e) { return PromiseReject(e); }
      return PromiseResolve(taBuffer(sliceBytes(this.#bytes, 0, taLength(this.#bytes))));
    }
    bytes() {
      try { Blob.#check(this, 'bytes'); } catch (e) { return PromiseReject(e); }
      return PromiseResolve(sliceBytes(this.#bytes, 0, taLength(this.#bytes)));
    }
    static {
      blobBytes = (b) => (isObject(b) && #bytes in b ? b.#bytes : undefined);
      blobType = (b) => b.#type;
    }
  }
  defineProperty(Blob.prototype, SymToStringTag, { __proto__: null, value: 'Blob', configurable: true });
  webIDL(Blob, ['size', 'type', 'slice', 'text', 'arrayBuffer', 'bytes']);

  // ---------------- Headers ----------------
  const TOKEN = /^[!#$%&'*+\-.^_`|~0-9A-Za-z]+$/;
  // a header value without its leading and trailing HTTP whitespace
  const isHTTPSpace = (c) => c === '\t' || c === '\n' || c === '\r' || c === ' ';
  const normalizeValue = (value) => {
    const v = String(value);
    let a = 0, b = v.length;
    while (a < b && isHTTPSpace(v[a])) a++;
    while (b > a && isHTTPSpace(v[b - 1])) b--;
    let r = '';
    for (let i = a; i < b; i++) r += v[i];
    return r;
  };
  // a header list's index of name, or -1
  const findHeader = (list, k) => { for (let i = 0; i < list.length; i++) if (list[i][0] === k) return i; return -1; };
  // the list without the entries of name, after index keep
  const withoutHeader = (list, k, keep) => {
    const r = [];
    for (let i = 0; i < list.length; i++) if (i <= keep || list[i][0] !== k) append(r, list[i]);
    return r;
  };
  let headersList, headersAppend, headersSet, headersGet, headersHas;
  class Headers {
    #list = [];   // [lower-cased name, value], in insertion order
    constructor(init = undefined) {
      if (init === undefined || init === null) return;
      if (!isObject(init)) throw new TypeError("Failed to construct 'Headers': The provided value is not of type '(record<ByteString, ByteString> or sequence<sequence<ByteString>>)'.");
      if (#list in init) { const l = init.#list; for (let i = 0; i < l.length; i++) append(this.#list, [l[i][0], l[i][1]]); return; }
      if (typeof init[SymIterator] === 'function') {
        // Web IDL sequence<sequence<ByteString>>: the argument's and each pair's own iterator
        for (const pair of init) {
          const items = [...pair];
          if (items.length !== 2) throw new TypeError("Failed to construct 'Headers': Invalid value");
          this.#append(items[0], items[1], 'append');
        }
      } else {
        const keys = ownKeys(init);
        for (let i = 0; i < keys.length; i++) {
          const key = keys[i];
          if (typeof key !== 'string') continue;
          const d = getOwnPropertyDescriptor(init, key);
          if (d === undefined || !d.enumerable) continue;
          this.#append(key, init[key], 'append');
        }
      }
    }
    static #check(h, what) {
      if (!isObject(h) || !(#list in h)) throw new TypeError(`Failed to execute '${what}' on 'Headers': Illegal invocation`);
    }
    static #name(name, what) {
      const k = String(name);
      if (RegExpPrototypeExec(TOKEN, k) === null) throw new TypeError(`Failed to execute '${what}' on 'Headers': Invalid name`);
      return StringPrototypeToLowerCase(k);
    }
    #append(name, value, what) {
      const k = Headers.#name(name, what), v = normalizeValue(value);
      const at = findHeader(this.#list, k);
      if (at >= 0) this.#list[at][1] += ', ' + v; else append(this.#list, [k, v]);
    }
    #set(name, value) {
      const k = Headers.#name(name, 'set'), v = normalizeValue(value);
      const at = findHeader(this.#list, k);
      if (at >= 0) { this.#list[at][1] = v; this.#list = withoutHeader(this.#list, k, at); }
      else append(this.#list, [k, v]);
    }
    #get(name) {
      const at = findHeader(this.#list, Headers.#name(name, 'get'));
      return at < 0 ? null : this.#list[at][1];
    }
    append(name, value) {
      Headers.#check(this, 'append');
      this.#append(name, value, 'append');
    }
    delete(name) {
      Headers.#check(this, 'delete');
      const k = Headers.#name(name, 'delete');
      this.#list = withoutHeader(this.#list, k, -1);
    }
    get(name) {
      Headers.#check(this, 'get');
      return this.#get(name);
    }
    getSetCookie() {
      Headers.#check(this, 'getSetCookie');
      const r = [], list = this.#list;
      for (let i = 0; i < list.length; i++) if (list[i][0] === 'set-cookie') append(r, list[i][1]);
      return r;
    }
    has(name) {
      Headers.#check(this, 'has');
      return findHeader(this.#list, Headers.#name(name, 'has')) >= 0;
    }
    set(name, value) {
      Headers.#check(this, 'set');
      this.#set(name, value);
    }
    #sorted() {
      const r = [], list = this.#list;
      for (let i = 0; i < list.length; i++) append(r, list[i]);
      return ArrayPrototypeSort(r, (a, b) => (a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0));
    }
    forEach(callback, thisArg = undefined) {
      Headers.#check(this, 'forEach');
      if (typeof callback !== 'function') throw new TypeError("Failed to execute 'forEach' on 'Headers': The callback provided as parameter 1 is not a function.");
      const list = this.#sorted();
      for (let i = 0; i < list.length; i++) apply(callback, thisArg, [list[i][1], list[i][0], this]);
    }
    *entries() { Headers.#check(this, 'entries'); const l = this.#sorted(); for (let i = 0; i < l.length; i++) yield [l[i][0], l[i][1]]; }
    *keys() { Headers.#check(this, 'keys'); const l = this.#sorted(); for (let i = 0; i < l.length; i++) yield l[i][0]; }
    *values() { Headers.#check(this, 'values'); const l = this.#sorted(); for (let i = 0; i < l.length; i++) yield l[i][1]; }
    static {
      headersList = (h) => h.#list;
      headersAppend = (h, name, value) => h.#append(name, value, 'append');
      headersSet = (h, name, value) => h.#set(name, value);
      headersGet = (h, name) => h.#get(name);
      headersHas = (h, name) => findHeader(h.#list, Headers.#name(name, 'has')) >= 0;
    }
  }
  hidden(Headers.prototype, SymIterator, Headers.prototype.entries);
  defineProperty(Headers.prototype, SymToStringTag, { __proto__: null, value: 'Headers', configurable: true });
  webIDL(Headers, ['append', 'delete', 'get', 'getSetCookie', 'has', 'set', 'forEach', 'entries', 'keys', 'values']);

  // ---------------- Response ----------------
  // a body: its bytes and the content type it implies
  function extractBody(body) {
    if (body === null || body === undefined) return { bytes: null, type: null };
    const bb = blobBytes(body);
    if (bb !== undefined) return { bytes: sliceBytes(bb, 0, taLength(bb)), type: blobType(body) === '' ? null : blobType(body) };
    if (isAB(body)) return { bytes: copyBytes(body, 0, apply(abByteLength, body, [])), type: null };
    if (isView(body)) return { bytes: copyBytes(viewBuffer(body), viewOffset(body), viewLength(body)), type: null };
    if (shared.isURLSearchParams(body)) return { bytes: utf8(searchParamsToString(body)), type: 'application/x-www-form-urlencoded;charset=UTF-8' };
    return { bytes: utf8(String(body)), type: 'text/plain;charset=UTF-8' };
  }
  let makeResponse;
  class Response {
    #status = 200;
    #statusText = '';
    #headers;
    #body = null;    // Uint8Array or null
    #used = false;
    #type = 'default';
    #url = '';
    #redirected = false;
    constructor(body = null, init = undefined) {
      if (init !== undefined && init !== null && !isObject(init))
        throw new TypeError("Failed to construct 'Response': The provided value is not of type 'ResponseInit'.");
      const o = init === undefined || init === null ? { __proto__: null } : init;
      const status = o.status === undefined ? 200 : Number(o.status);
      if (!(status >= 200 && status <= 599) || !NumberIsInteger(status))
        throw new RangeError(`Failed to construct 'Response': The status provided (${o.status}) is outside the range [200, 599].`);
      const statusText = o.statusText === undefined ? '' : String(o.statusText);
      this.#status = status;
      this.#statusText = statusText;
      this.#headers = new Headers(o.headers);
      if (body !== null && body !== undefined) {
        if (status === 101 || status === 204 || status === 205 || status === 304)
          throw new TypeError("Failed to construct 'Response': Response with null body status cannot have body");
        const b = extractBody(body);
        this.#body = b.bytes;
        if (b.type !== null && !headersHas(this.#headers, 'content-type')) headersSet(this.#headers, 'content-type', b.type);
      }
    }
    static #check(r, what) {
      if (!isObject(r) || !(#status in r)) throw new TypeError(`Failed to execute '${what}' on 'Response': Illegal invocation`);
    }
    get status() { Response.#check(this, 'status'); return this.#status; }
    get statusText() { Response.#check(this, 'statusText'); return this.#statusText; }
    get ok() { Response.#check(this, 'ok'); return this.#status >= 200 && this.#status <= 299; }
    get headers() { Response.#check(this, 'headers'); return this.#headers; }
    get type() { Response.#check(this, 'type'); return this.#type === 'default' ? 'basic' : this.#type; }
    get url() { Response.#check(this, 'url'); return this.#url; }
    get redirected() { Response.#check(this, 'redirected'); return this.#redirected; }
    get bodyUsed() { Response.#check(this, 'bodyUsed'); return this.#used; }
    get body() { Response.#check(this, 'body'); return null; }
    #consume(what) {
      Response.#check(this, what);
      if (this.#used) throw new TypeError(`Failed to execute '${what}' on 'Response': body stream already read`);
      this.#used = true;
      return this.#body === null ? new Uint8Array(0) : this.#body;
    }
    text() { try { return PromiseResolve(decode(this.#consume('text'))); } catch (e) { return PromiseReject(e); } }
    json() {
      try { const t = decode(this.#consume('json')); return PromiseResolve(JSONParse(t)); }
      catch (e) { return PromiseReject(e); }
    }
    arrayBuffer() { try { const b = this.#consume('arrayBuffer'); return PromiseResolve(taBuffer(sliceBytes(b, 0, taLength(b)))); } catch (e) { return PromiseReject(e); } }
    bytes() { try { const b = this.#consume('bytes'); return PromiseResolve(sliceBytes(b, 0, taLength(b))); } catch (e) { return PromiseReject(e); } }
    blob() {
      try {
        const bytes = this.#consume('blob');
        const type = headersGet(this.#headers, 'content-type');
        const t = type === null || nonPrintable(type) ? '' : StringPrototypeToLowerCase(type);
        return PromiseResolve(new Blob(BLOB_INIT, [{ __proto__: null, blob: bytes }], t, false));
      } catch (e) { return PromiseReject(e); }
    }
    clone() {
      Response.#check(this, 'clone');
      if (this.#used) throw new TypeError("Failed to execute 'clone' on 'Response': Response body is already used");
      const r = new Response(null, { status: this.#status, statusText: this.#statusText, headers: this.#headers });
      r.#body = this.#body === null ? null : sliceBytes(this.#body, 0, taLength(this.#body));
      r.#type = this.#type; r.#url = this.#url; r.#redirected = this.#redirected;
      return r;
    }
    static error() {
      const r = new Response(null, { status: 200 });
      r.#status = 0; r.#type = 'error';
      return r;
    }
    static json(data, init = undefined) {
      const text = JSONStringify(data);
      if (text === undefined) throw new TypeError("Failed to execute 'json' on 'Response': The data is not JSON serializable");
      const r = new Response(text, init);
      if (!(init !== undefined && init !== null && init.headers !== undefined && headersHas(r.#headers, 'content-type')))
        headersSet(r.#headers, 'content-type', 'application/json');
      return r;
    }
    static redirect(url, status = 302) {
      const u = urlHref(new URLCtor(String(url)));
      const s = Number(status);
      if (s !== 301 && s !== 302 && s !== 303 && s !== 307 && s !== 308) throw new RangeError("Failed to execute 'redirect' on 'Response': Invalid status code");
      const r = new Response(null, { status: s });
      headersSet(r.#headers, 'location', u);
      return r;
    }
    static {
      // a network response for fetch: status, status text, headers (pairs), body bytes, URL, redirected
      makeResponse = (status, statusText, headers, bytes, url, redirected) => {
        const r = new Response(null, { status: 200 });
        r.#status = status; r.#statusText = statusText;
        for (let i = 0; i < headers.length; i++) headersAppend(r.#headers, headers[i][0], headers[i][1]);
        r.#body = bytes; r.#url = url; r.#redirected = redirected; r.#type = 'basic';
        return r;
      };
    }
  }
  defineProperty(Response.prototype, SymToStringTag, { __proto__: null, value: 'Response', configurable: true });
  webIDL(Response, ['status', 'statusText', 'ok', 'headers', 'type', 'url', 'redirected', 'bodyUsed', 'body', 'text',
    'json', 'arrayBuffer', 'bytes', 'blob', 'clone']);

  // the global Blob: its [[Construct]] converts the arguments, then Blob's own makes the object and copies the bytes
  const reflectConstruct = Reflect.construct;
  const BlobCtor = new Proxy(Blob, {
    construct(target, args, newTarget) {
      const c = convertBlobArgs(args.length > 0 ? args[0] : undefined, args.length > 1 ? args[1] : undefined);
      return reflectConstruct(target, [BLOB_INIT, c.pieces, c.type, c.native], newTarget);
    },
  });
  hidden(Blob.prototype, 'constructor', BlobCtor);
  hidden(G, 'Blob', BlobCtor);
  hidden(G, 'Headers', Headers);
  hidden(G, 'Response', Response);
  shared.Blob = Blob;
  shared.Headers = Headers;
  shared.Response = Response;
  shared.makeResponse = makeResponse;

  // ---------------- fetch ----------------
  // docs/built-ins-fetch.md: GET and HEAD, the net capability judging every destination (Goccia.Fetch), a signal
  // rejecting with its reason inside abort(), before the abort event. The requests run on host workers; the event
  // loop settles them through settle below.
  const pendingFetches = new Map();   // id -> { resolve, reject, signal }
  const getPending = Map.prototype.get, savePending = Map.prototype.set, forgetPending = Map.prototype.delete;
  const onFetchAbort = (id) => () => {
    const pending = apply(getPending, pendingFetches, [id]);
    if (pending === undefined) return;
    apply(forgetPending, pendingFetches, [id]);
    n.fetchCancel(id);
    pending.reject(shared.abortReason(pending.signal));
  };
  const REDIRECT_MODES = { follow: 0, error: 1, manual: 2 };
  function fetch(input, init = undefined) {
    if (arguments.length === 0) throw new TypeError('fetch requires a URL argument');
    const url = String(input);
    n.fetchCheck(url);
    let method = 'GET';
    let redirect = 0;
    let signal = null;
    const headers = [];
    if (isObject(init)) {
      const m = init.method;
      if (m !== undefined) method = StringPrototypeToUpperCase(String(m));
      const h = init.headers;
      if (h !== undefined && h !== null) {
        if (isObject(h) && brandHeaders(h)) { const l = headersList(h); for (let i = 0; i < l.length; i++) append(headers, [l[i][0], l[i][1]]); }
        else if (isObject(h)) {
          const keys = ownKeys(h);
          for (let i = 0; i < keys.length; i++) {
            const k = keys[i];
            if (typeof k !== 'string') continue;
            append(headers, [StringPrototypeToLowerCase(k), String(h[k])]);
          }
        }
      }
      const r = init.redirect;
      if (r !== undefined) {
        const t = String(r);
        if (!ObjectPrototypeHasOwnProperty(REDIRECT_MODES, t))
          throw new TypeError(`fetch redirect option '${t}' is not a valid redirect mode`);
        redirect = REDIRECT_MODES[t];
      }
      const s = init.signal;
      if (s !== undefined && s !== null) {
        if (!shared.isAbortSignal(s)) throw new TypeError('fetch signal must be an AbortSignal');
        signal = s;
      }
    }
    if (method !== 'GET' && method !== 'HEAD')
      throw new TypeError(`fetch does not support method '${method}'; only GET and HEAD are allowed`);
    return new Promise((resolve, reject) => {
      if (signal !== null && shared.isAborted(signal)) { reject(shared.abortReason(signal)); return; }
      const remaining = signal !== null ? shared.signalRemaining(signal) : -1;
      const id = n.fetchStart(url, method, headers, redirect, remaining);
      if (id < 0) { reject(new TypeError('fetch worker limit exceeded')); return; }
      apply(savePending, pendingFetches, [id, { resolve, reject, signal }]);
      // an abort algorithm: the rejection comes inside abort(), before the abort event
      if (signal !== null) shared.onAbort(signal, onFetchAbort(id));
    });
  }
  function brandHeaders(h) { try { headersList(h); return true; } catch { return false; } }
  // kind 0: a response; 1: a rejection; 2: a signal deadline passed (its signal is observed, which aborts it);
  // 3: nothing (the run's interrupt ends the run at this call)
  n.fetchRegister((id, kind, a, b, c, d, e, f) => {
    if (kind === 3) return;
    if (kind === 2) {
      const waiting = [];
      MapPrototypeForEach(pendingFetches, (p) => { append(waiting, p); });
      for (let i = 0; i < waiting.length; i++) if (waiting[i].signal !== null) shared.isAborted(waiting[i].signal);
      return;
    }
    const p = apply(getPending, pendingFetches, [id]);
    if (p === undefined) return;
    apply(forgetPending, pendingFetches, [id]);
    if (kind === 1) { p.reject(a); return; }
    p.resolve(makeResponse(a, b, c, d === null ? null : new Uint8Array(d), e, f));
  }, pendingFetches);
  hidden(G, 'fetch', fetch);
  shared.headersList = headersList;
  shared.runtimeGlobals.unshift('Blob');
})
