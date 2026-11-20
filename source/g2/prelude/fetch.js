// Blob (File API), Headers and Response (Fetch Standard) for the CLI host on G2. The engine has no ReadableStream,
// so a body is bytes held whole. Called once per realm with the global object, the host's natives (n) and the
// preludes' shared internals; installs the globals and sets shared.Blob, shared.Headers and shared.Response for
// fetch.
(function (G, n, shared) {
  'use strict';
  const defineProperty = Object.defineProperty;
  const getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  const apply = Reflect.apply;
  const hidden = (o, k, v) => defineProperty(o, k, { value: v, writable: true, enumerable: false, configurable: true });
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
  const isAB = brand(abByteLength), isSAB = brand(sabByteLength);
  const encoder = new G.TextEncoder();
  const decoder = new G.TextDecoder();
  const utf8 = (s) => encoder.encode(s);

  // ---------------- Blob ----------------
  let blobBytes;   // a Blob's bytes, for the other interfaces
  // Web IDL: the arguments of new Blob(blobParts, options) convert, the parts first, before the new object is made
  // (NewTarget's prototype read) and before the constructor steps copy any buffer's bytes
  const BLOB_INIT = {};
  function convertBlobArgs(blobParts, options) {
    // the parts convert first, then the options (Web IDL argument order); buffer bytes are copied by the
    // constructor, after every part has converted
    const pieces = [];
    if (blobParts !== undefined) {
      if (!isObject(blobParts) || typeof blobParts[Symbol.iterator] !== 'function')
        throw new TypeError("Failed to construct 'Blob': The provided value cannot be converted to a sequence.");
      for (const part of blobParts) {
        const partBytes = blobBytes(part);
        if (partBytes !== undefined) pieces.push({ blob: partBytes });
        else if (isObject(part) && isAB(part)) {
          if (apply(abResizable, part, [])) throw new TypeError("Failed to construct 'Blob': The provided ArrayBuffer value must not be resizable.");
          pieces.push({ buffer: part });
        } else if (isObject(part) && ArrayBuffer.isView(part)) {
          if (isSAB(part.buffer)) throw new TypeError("Failed to construct 'Blob': The provided ArrayBufferView value must not be shared.");
          if (isAB(part.buffer) && apply(abResizable, part.buffer, [])) throw new TypeError("Failed to construct 'Blob': The provided ArrayBufferView value must not be resizable.");
          pieces.push({ view: part });
        } else {
          if (typeof part === 'symbol') throw new TypeError('Cannot convert a Symbol value to a string');
          // USVString: the encoding turns a lone surrogate into U+FFFD, as converting first would
          pieces.push({ string: String(part) });
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
    if (/[^ -~]/.test(type)) type = '';
    else type = type.toLowerCase();
    return { pieces, type, native };
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
      for (const p of pieces) {
        let len;
        if (p.blob !== undefined) len = p.blob.length;
        else if (p.buffer !== undefined) len = apply(abDetached, p.buffer, []) ? 0 : apply(abByteLength, p.buffer, []);
        else if (p.view !== undefined) {
          const v = p.view;
          len = isAB(v.buffer) && apply(abDetached, v.buffer, []) ? 0 : v.byteLength;
        } else {
          if (native) p.string = p.string.replace(/\r\n|\r|\n/g, '\n');
          len = n.utf8Length(p.string);
        }
        sizes.push(len);
        size += len;
      }
      const bytes = new Uint8Array(size);
      let at = 0;
      for (let i = 0; i < pieces.length; i++) {
        const p = pieces[i];
        if (sizes[i] > 0) {
          if (p.blob !== undefined) bytes.set(p.blob, at);
          else if (p.buffer !== undefined) bytes.set(new Uint8Array(p.buffer), at);
          else if (p.view !== undefined) bytes.set(new Uint8Array(p.view.buffer, p.view.byteOffset, p.view.byteLength), at);
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
    get size() { Blob.#check(this, 'size'); return this.#bytes.length; }
    get type() { Blob.#check(this, 'type'); return this.#type; }
    slice(start = undefined, end = undefined, contentType = undefined) {
      Blob.#check(this, 'slice');
      const len = this.#bytes.length;
      const rel = (v, d) => {
        if (v === undefined) return d;
        // Web IDL [Clamp] long long: clamped, then rounded to the nearest integer, ties to even
        let x = Number(v);
        if (Number.isNaN(x)) x = 0;
        else if (Number.isFinite(x)) {
          const f = Math.floor(x), diff = x - f;
          x = diff > 0.5 ? f + 1 : diff < 0.5 ? f : (f % 2 === 0 ? f : f + 1);
        }
        return x < 0 ? Math.max(len + x, 0) : Math.min(x, len);
      };
      const from = rel(start, 0), to = rel(end, len);
      let type = contentType === undefined ? '' : String(contentType);
      type = /[^ -~]/.test(type) ? '' : type.toLowerCase();
      const b = new Blob();
      b.#bytes = this.#bytes.slice(from, Math.max(from, to));
      b.#type = type;
      return b;
    }
    text() {
      try { Blob.#check(this, 'text'); } catch (e) { return Promise.reject(e); }
      return Promise.resolve(decoder.decode(this.#bytes));
    }
    arrayBuffer() {
      try { Blob.#check(this, 'arrayBuffer'); } catch (e) { return Promise.reject(e); }
      return Promise.resolve(this.#bytes.slice().buffer);
    }
    bytes() {
      try { Blob.#check(this, 'bytes'); } catch (e) { return Promise.reject(e); }
      return Promise.resolve(this.#bytes.slice());
    }
    static { blobBytes = (b) => (isObject(b) && #bytes in b ? b.#bytes : undefined); }
  }
  defineProperty(Blob.prototype, Symbol.toStringTag, { value: 'Blob', configurable: true });
  webIDL(Blob, ['size', 'type', 'slice', 'text', 'arrayBuffer', 'bytes']);

  // ---------------- Headers ----------------
  const TOKEN = /^[!#$%&'*+\-.^_`|~0-9A-Za-z]+$/;
  const normalizeValue = (v) => String(v).replace(/^[\t\n\r ]+|[\t\n\r ]+$/g, '');
  let headersList;
  class Headers {
    #list = [];   // [lower-cased name, value], in insertion order
    constructor(init = undefined) {
      if (init === undefined || init === null) return;
      if (!isObject(init)) throw new TypeError("Failed to construct 'Headers': The provided value is not of type '(record<ByteString, ByteString> or sequence<sequence<ByteString>>)'.");
      if (#list in init) { for (const [k, v] of init.#list) this.#list.push([k, v]); return; }
      if (typeof init[Symbol.iterator] === 'function') {
        for (const pair of init) {
          const items = [...pair];
          if (items.length !== 2) throw new TypeError("Failed to construct 'Headers': Invalid value");
          this.append(items[0], items[1]);
        }
      } else {
        for (const key of Reflect.ownKeys(init)) {
          if (typeof key !== 'string') continue;
          const d = getOwnPropertyDescriptor(init, key);
          if (d === undefined || !d.enumerable) continue;
          this.append(key, init[key]);
        }
      }
    }
    static #check(h, what) {
      if (!isObject(h) || !(#list in h)) throw new TypeError(`Failed to execute '${what}' on 'Headers': Illegal invocation`);
    }
    static #name(name, what) {
      const k = String(name);
      if (!TOKEN.test(k)) throw new TypeError(`Failed to execute '${what}' on 'Headers': Invalid name`);
      return k.toLowerCase();
    }
    append(name, value) {
      Headers.#check(this, 'append');
      const k = Headers.#name(name, 'append'), v = normalizeValue(value);
      const at = this.#list.findIndex((p) => p[0] === k);
      if (at >= 0) this.#list[at][1] += ', ' + v; else this.#list.push([k, v]);
    }
    delete(name) {
      Headers.#check(this, 'delete');
      const k = Headers.#name(name, 'delete');
      this.#list = this.#list.filter((p) => p[0] !== k);
    }
    get(name) {
      Headers.#check(this, 'get');
      const k = Headers.#name(name, 'get');
      const p = this.#list.find((q) => q[0] === k);
      return p === undefined ? null : p[1];
    }
    getSetCookie() {
      Headers.#check(this, 'getSetCookie');
      return this.#list.filter((p) => p[0] === 'set-cookie').map((p) => p[1]);
    }
    has(name) {
      Headers.#check(this, 'has');
      const k = Headers.#name(name, 'has');
      return this.#list.some((p) => p[0] === k);
    }
    set(name, value) {
      Headers.#check(this, 'set');
      const k = Headers.#name(name, 'set'), v = normalizeValue(value);
      const at = this.#list.findIndex((p) => p[0] === k);
      if (at >= 0) { this.#list[at][1] = v; this.#list = this.#list.filter((p, i) => i <= at || p[0] !== k); }
      else this.#list.push([k, v]);
    }
    #sorted() { return this.#list.slice().sort((a, b) => (a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0)); }
    forEach(callback, thisArg = undefined) {
      Headers.#check(this, 'forEach');
      if (typeof callback !== 'function') throw new TypeError("Failed to execute 'forEach' on 'Headers': The callback provided as parameter 1 is not a function.");
      for (const [k, v] of this.#sorted()) apply(callback, thisArg, [v, k, this]);
    }
    *entries() { Headers.#check(this, 'entries'); for (const [k, v] of this.#sorted()) yield [k, v]; }
    *keys() { Headers.#check(this, 'keys'); for (const [k] of this.#sorted()) yield k; }
    *values() { Headers.#check(this, 'values'); for (const [, v] of this.#sorted()) yield v; }
    static { headersList = (h) => h.#list; }
  }
  hidden(Headers.prototype, Symbol.iterator, Headers.prototype.entries);
  defineProperty(Headers.prototype, Symbol.toStringTag, { value: 'Headers', configurable: true });
  webIDL(Headers, ['append', 'delete', 'get', 'getSetCookie', 'has', 'set', 'forEach', 'entries', 'keys', 'values']);

  // ---------------- Response ----------------
  // a body: its bytes and the content type it implies
  function extractBody(body) {
    if (body === null || body === undefined) return { bytes: null, type: null };
    const bb = blobBytes(body);
    if (bb !== undefined) return { bytes: bb.slice(), type: body.type === '' ? null : body.type };
    if (isAB(body)) return { bytes: new Uint8Array(body.slice(0)), type: null };
    if (ArrayBuffer.isView(body)) return { bytes: new Uint8Array(body.buffer.slice(body.byteOffset, body.byteOffset + body.byteLength)), type: null };
    if (body instanceof G.URLSearchParams) return { bytes: utf8(String(body)), type: 'application/x-www-form-urlencoded;charset=UTF-8' };
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
      const o = init === undefined || init === null ? {} : init;
      const status = o.status === undefined ? 200 : Number(o.status);
      if (!(status >= 200 && status <= 599) || !Number.isInteger(status))
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
        if (b.type !== null && !this.#headers.has('content-type')) this.#headers.set('content-type', b.type);
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
    text() { try { return Promise.resolve(decoder.decode(this.#consume('text'))); } catch (e) { return Promise.reject(e); } }
    json() {
      try { const t = decoder.decode(this.#consume('json')); return Promise.resolve(JSON.parse(t)); }
      catch (e) { return Promise.reject(e); }
    }
    arrayBuffer() { try { return Promise.resolve(this.#consume('arrayBuffer').slice().buffer); } catch (e) { return Promise.reject(e); } }
    bytes() { try { return Promise.resolve(this.#consume('bytes').slice()); } catch (e) { return Promise.reject(e); } }
    blob() {
      try {
        const bytes = this.#consume('blob');
        const type = this.#headers.get('content-type');
        return Promise.resolve(new Blob([bytes], { type: type === null ? '' : type }));
      } catch (e) { return Promise.reject(e); }
    }
    clone() {
      Response.#check(this, 'clone');
      if (this.#used) throw new TypeError("Failed to execute 'clone' on 'Response': Response body is already used");
      const r = new Response(null, { status: this.#status, statusText: this.#statusText, headers: this.#headers });
      r.#body = this.#body === null ? null : this.#body.slice();
      r.#type = this.#type; r.#url = this.#url; r.#redirected = this.#redirected;
      return r;
    }
    static error() {
      const r = new Response(null, { status: 200 });
      r.#status = 0; r.#type = 'error';
      return r;
    }
    static json(data, init = undefined) {
      const text = JSON.stringify(data);
      if (text === undefined) throw new TypeError("Failed to execute 'json' on 'Response': The data is not JSON serializable");
      const r = new Response(text, init);
      if (!(init !== undefined && init !== null && init.headers !== undefined && r.#headers.has('content-type')))
        r.#headers.set('content-type', 'application/json');
      return r;
    }
    static redirect(url, status = 302) {
      const u = new G.URL(String(url)).href;
      const s = Number(status);
      if (![301, 302, 303, 307, 308].includes(s)) throw new RangeError("Failed to execute 'redirect' on 'Response': Invalid status code");
      const r = new Response(null, { status: s });
      r.#headers.set('location', u);
      return r;
    }
    static {
      // a network response for fetch: status, status text, headers (pairs), body bytes, URL, redirected
      makeResponse = (status, statusText, headers, bytes, url, redirected) => {
        const r = new Response(null, { status: 200 });
        r.#status = status; r.#statusText = statusText;
        for (const [k, v] of headers) r.#headers.append(k, v);
        r.#body = bytes; r.#url = url; r.#redirected = redirected; r.#type = 'basic';
        return r;
      };
    }
  }
  defineProperty(Response.prototype, Symbol.toStringTag, { value: 'Response', configurable: true });
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
  // docs/built-ins-fetch.md: GET and HEAD, the net capability judging every destination (G2.Fetch), a signal
  // rejecting with its reason inside abort(), before the abort event. The requests run on host workers; the event
  // loop settles them through settle below.
  const pendingFetches = new Map();   // id -> { resolve, reject, signal }
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
      if (m !== undefined) method = String(m).toUpperCase();
      const h = init.headers;
      if (h !== undefined && h !== null) {
        if (isObject(h) && brandHeaders(h)) for (const [k, v] of headersList(h)) headers.push([k, v]);
        else if (isObject(h)) for (const k of Reflect.ownKeys(h)) {
          if (typeof k !== 'string') continue;
          headers.push([k.toLowerCase(), String(h[k])]);
        }
      }
      const r = init.redirect;
      if (r !== undefined) {
        const t = String(r);
        if (!Object.prototype.hasOwnProperty.call(REDIRECT_MODES, t))
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
      if (signal !== null && signal.aborted) { reject(signal.reason); return; }
      const remaining = signal !== null ? shared.signalRemaining(signal) : -1;
      const id = n.fetchStart(url, method, headers, redirect, remaining);
      if (id < 0) { reject(new TypeError('fetch worker limit exceeded')); return; }
      pendingFetches.set(id, { resolve, reject, signal });
      // an abort algorithm: the rejection comes inside abort(), before the abort event
      if (signal !== null) shared.onAbort(signal, () => {
        if (!pendingFetches.delete(id)) return;
        n.fetchCancel(id);
        reject(signal.reason);
      });
    });
  }
  function brandHeaders(h) { try { headersList(h); return true; } catch { return false; } }
  // kind 0: a response; 1: a rejection; 2: a signal deadline passed (its signal is observed, which aborts it);
  // 3: nothing (the run's interrupt ends the run at this call)
  n.fetchRegister((id, kind, a, b, c, d, e, f) => {
    if (kind === 3) return;
    if (kind === 2) {
      for (const p of [...pendingFetches.values()]) if (p.signal !== null) void p.signal.aborted;
      return;
    }
    const p = pendingFetches.get(id);
    if (p === undefined) return;
    pendingFetches.delete(id);
    if (kind === 1) { p.reject(a); return; }
    p.resolve(makeResponse(a, b, c, d === null ? null : new Uint8Array(d), e, f));
  });
  hidden(G, 'fetch', fetch);
  shared.headersList = headersList;
  shared.runtimeGlobals.unshift('Blob');
})
