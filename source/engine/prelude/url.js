// URL and URLSearchParams (WHATWG URL Standard) for the CLI host. The parser, serializers and
// percent-encoders are Goccia.URL.Parser's, reached through Goccia.HostAPI's natives (n): urlParse(input, base)
// returns the URL record as an array, null for a failure, or -1 for an invalid base; urlOrigin, urlEncode(kind,
// input, special) with kind 0 userinfo, 1 query, 2 fragment; formParse (names and values interleaved) and
// formSerialize. Called once per realm with the global object; installs URL and URLSearchParams.
(function (G, n, shared) {
  void shared;
  'use strict';
  const defineProperty = Object.defineProperty;
  const getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  // what the functions below call, captured before any script runs: a script that replaces a prototype method or a
  // global does not reach into URL or URLSearchParams
  const { TypeError, String, Number } = G;
  const ObjectCreate = Object.create, ownKeys = Reflect.ownKeys, ReflectApply = Reflect.apply;
  const SymIterator = Symbol.iterator;
  const uncurryThis = Function.prototype.bind.bind(Function.prototype.call);
  const toWellFormed = uncurryThis(String.prototype.toWellFormed);
  const StringPrototypeSlice = uncurryThis(String.prototype.slice), StringPrototypeIndexOf = uncurryThis(String.prototype.indexOf);
  const StringPrototypeToLowerCase = uncurryThis(String.prototype.toLowerCase), StringPrototypeTrim = uncurryThis(String.prototype.trim);
  const ArrayPrototypeJoin = uncurryThis(Array.prototype.join), RegExpPrototypeExec = uncurryThis(RegExp.prototype.exec);
  const WeakMapPrototypeGet = uncurryThis(WeakMap.prototype.get), WeakMapPrototypeSet = uncurryThis(WeakMap.prototype.set);
  const usv = v => toWellFormed(String(v));
  // a list append that defines the index (a setter on Array.prototype must not see it)
  const append = (list, v) => { defineProperty(list, list.length, { __proto__: null, value: v, writable: true, enumerable: true, configurable: true }); };
  // the pairs' index of name, or -1
  const findPair = (list, k) => { for (let i = 0; i < list.length; i++) if (list[i][0] === k) return i; return -1; };
  // the pairs that keep(pair, index) accepts
  const keepPairs = (list, keep) => { const r = []; for (let i = 0; i < list.length; i++) if (keep(list[i], i)) append(r, list[i]); return r; };

  function hidden(o, k, v) { defineProperty(o, k, { __proto__: null, value: v, writable: true, enumerable: false, configurable: true }); }
  function accessorsEnumerable(proto, names) {
    for (const k of names) {
      const d = getOwnPropertyDescriptor(proto, k);
      d.enumerable = true;
      defineProperty(proto, k, d);
    }
  }

  // ---------------- URLSearchParams ----------------
  let paramsList, listOf;   // set by URLSearchParams's static block

  class URLSearchParams {
    #list = [];   // [name, value] pairs
    #url = null;  // the URL whose query this is, if any

    constructor(init = undefined) {
      if (init === undefined) return;
      if (init !== null && typeof init === 'object' && #list in init) {
        const l = init.#list;
        for (let i = 0; i < l.length; i++) append(this.#list, [l[i][0], l[i][1]]);
      } else if (init !== null && (typeof init === 'object' || typeof init === 'function')) {
        const iter = init[SymIterator];
        if (iter !== undefined && iter !== null) {
          if (typeof iter !== 'function')
            throw new TypeError('Failed to construct URLSearchParams: The provided value cannot be converted to a sequence');
          // Web IDL sequence<sequence<USVString>>: the argument's and each pair's own iterator
          for (const pair of init) {
            if (pair === null || (typeof pair !== 'object' && typeof pair !== 'function') || typeof pair[SymIterator] !== 'function')
              throw new TypeError('Failed to construct URLSearchParams: The provided value cannot be converted to a sequence');
            const items = [...pair];
            if (items.length !== 2)
              throw new TypeError('Failed to construct URLSearchParams: Sequence initializer must contain pairs with exactly two items');
            append(this.#list, [usv(items[0]), usv(items[1])]);
          }
        } else {
          const keys = ownKeys(init);
          for (let i = 0; i < keys.length; i++) {
            const key = keys[i];
            if (typeof key !== 'string') continue;
            const d = getOwnPropertyDescriptor(init, key);
            if (d === undefined || !d.enumerable) continue;
            const name = usv(key), value = usv(init[key]);
            const at = findPair(this.#list, name);
            if (at >= 0) this.#list[at][1] = value; else append(this.#list, [name, value]);
          }
        }
      } else {
        this.#parse(usv(init));
      }
    }

    #parse(s) {
      this.#list = [];
      if (s.length > 0 && s[0] === '?') s = StringPrototypeSlice(s, 1);
      const flat = n.formParse(s);
      for (let i = 0; i < flat.length; i += 2) append(this.#list, [flat[i], flat[i + 1]]);
    }
    #serialize() {
      const parts = [], l = this.#list;
      for (let i = 0; i < l.length; i++) append(parts, n.formSerialize(l[i][0], l[i][1]));
      return ArrayPrototypeJoin(parts, '&');
    }
    #update() { if (this.#url !== null) urlSetQueryFromParams(this.#url, this.#serialize()); }
    static #check(o, what) {
      if (o === null || typeof o !== 'object' || !(#list in o))
        throw new TypeError(`URLSearchParams.prototype.${what}: not a URLSearchParams`);
    }

    get size() { URLSearchParams.#check(this, 'size'); return this.#list.length; }
    append(name, value) {
      URLSearchParams.#check(this, 'append');
      if (arguments.length < 2) throw new TypeError("Failed to execute 'append' on 'URLSearchParams': 2 arguments required");
      append(this.#list, [usv(name), usv(value)]);
      this.#update();
    }
    delete(name, value = undefined) {
      URLSearchParams.#check(this, 'delete');
      if (arguments.length < 1) throw new TypeError("Failed to execute 'delete' on 'URLSearchParams': 1 argument required");
      const k = usv(name);
      if (value === undefined) this.#list = keepPairs(this.#list, p => p[0] !== k);
      else { const v = usv(value); this.#list = keepPairs(this.#list, p => !(p[0] === k && p[1] === v)); }
      this.#update();
    }
    get(name) {
      URLSearchParams.#check(this, 'get');
      if (arguments.length < 1) throw new TypeError("Failed to execute 'get' on 'URLSearchParams': 1 argument required");
      const k = usv(name);
      const at = findPair(this.#list, k);
      return at < 0 ? null : this.#list[at][1];
    }
    getAll(name) {
      URLSearchParams.#check(this, 'getAll');
      if (arguments.length < 1) throw new TypeError("Failed to execute 'getAll' on 'URLSearchParams': 1 argument required");
      const k = usv(name);
      const r = [], l = this.#list;
      for (let i = 0; i < l.length; i++) if (l[i][0] === k) append(r, l[i][1]);
      return r;
    }
    has(name, value = undefined) {
      URLSearchParams.#check(this, 'has');
      if (arguments.length < 1) throw new TypeError("Failed to execute 'has' on 'URLSearchParams': 1 argument required");
      const k = usv(name);
      if (value === undefined) return findPair(this.#list, k) >= 0;
      const v = usv(value), l = this.#list;
      for (let i = 0; i < l.length; i++) if (l[i][0] === k && l[i][1] === v) return true;
      return false;
    }
    set(name, value) {
      URLSearchParams.#check(this, 'set');
      if (arguments.length < 2) throw new TypeError("Failed to execute 'set' on 'URLSearchParams': 2 arguments required");
      const k = usv(name), v = usv(value);
      const at = findPair(this.#list, k);
      if (at < 0) append(this.#list, [k, v]);
      else {
        this.#list[at][1] = v;
        this.#list = keepPairs(this.#list, (p, i) => i <= at || p[0] !== k);
      }
      this.#update();
    }
    sort() {
      URLSearchParams.#check(this, 'sort');
      // stable, by code units
      const l = this.#list;
      for (let i = 1; i < l.length; i++) {
        const t = l[i];
        let j = i - 1;
        while (j >= 0 && l[j][0] > t[0]) { l[j + 1] = l[j]; j--; }
        l[j + 1] = t;
      }
      this.#update();
    }
    toString() { URLSearchParams.#check(this, 'toString'); return this.#serialize(); }
    forEach(callback, thisArg = undefined) {
      URLSearchParams.#check(this, 'forEach');
      if (typeof callback !== 'function') throw new TypeError('URLSearchParams.prototype.forEach: callback is not a function');
      for (let i = 0; i < this.#list.length; i++) {
        const p = this.#list[i];
        ReflectApply(callback, thisArg, [p[1], p[0], this]);
      }
    }
    keys() { URLSearchParams.#check(this, 'keys'); return makeIterator(this, 0); }
    values() { URLSearchParams.#check(this, 'values'); return makeIterator(this, 1); }
    entries() { URLSearchParams.#check(this, 'entries'); return makeIterator(this, 2); }

    static {
      paramsList = (p, s, url) => { p.#parse(s); if (url !== undefined) p.#url = url; };
      listOf = p => p.#list;
      shared.isURLSearchParams = (v) => v !== null && typeof v === 'object' && #list in v;
    }
  }
  // the iterators see the list as it changes (an index into the owner's current pairs)
  const IteratorProto = Object.getPrototypeOf(Object.getPrototypeOf([][Symbol.iterator]()));
  const ParamsIteratorProto = Object.create(IteratorProto);
  const iterState = new WeakMap();
  hidden(ParamsIteratorProto, 'next', function next() {
    const st = WeakMapPrototypeGet(iterState, this);
    if (st === undefined) throw new TypeError('next method called on incompatible receiver');
    const list = listOf(st.owner);
    if (st.i >= list.length) return { value: undefined, done: true };
    const p = list[st.i++];
    return { value: st.kind === 0 ? p[0] : st.kind === 1 ? p[1] : [p[0], p[1]], done: false };
  });
  defineProperty(ParamsIteratorProto, Symbol.toStringTag, { __proto__: null, value: 'URLSearchParams Iterator', configurable: true });
  function makeIterator(owner, kind) {
    const it = ObjectCreate(ParamsIteratorProto);
    WeakMapPrototypeSet(iterState, it, { i: 0, kind, owner });
    return it;
  }
  hidden(URLSearchParams.prototype, Symbol.iterator, URLSearchParams.prototype.entries);
  defineProperty(URLSearchParams.prototype, Symbol.toStringTag, { __proto__: null, value: 'URLSearchParams', configurable: true });
  accessorsEnumerable(URLSearchParams.prototype, ['size']);

  // ---------------- URL ----------------
  let urlSetQueryFromParams;

  class URL {
    // the URL record: scheme, username, password, host, hasNullHost, port (-1 null), pathname, hasOpaquePath,
    // query, hasNullQuery, fragment, hasNullFragment
    #r;
    #params = null;

    constructor(url, base = undefined) {
      if (arguments.length === 0) throw new TypeError('URL constructor: 1 argument required');
      const s = usv(url);
      let r;
      if (arguments.length >= 2 && base !== undefined) {
        const b = usv(base);
        r = n.urlParse(s, b);
        if (r === -1) throw new TypeError(`Invalid base URL: ${b}`);
      } else r = n.urlParse(s);
      if (r === null || r === -1) throw new TypeError(`Invalid URL: ${s}`);
      this.#r = r;
    }

    static #check(o, what) {
      if (o === null || typeof o !== 'object' || !(#r in o)) throw new TypeError(`URL ${what}: not a URL`);
      return o.#r;
    }
    static #href(r) {
      let s = r[0] + ':';
      if (!r[4]) {
        s += '//';
        if (r[1] !== '' || r[2] !== '') s += r[1] + (r[2] !== '' ? ':' + r[2] : '') + '@';
        s += r[3];
        if (r[5] !== -1) s += ':' + r[5];
      } else if (r[0] === 'file') s += '//';
      s += r[6];
      if (!r[9]) s += '?' + r[8];
      if (!r[11]) s += '#' + r[10];
      return s;
    }
    #syncParams() { if (this.#params !== null) paramsList(this.#params, this.#r[9] ? '' : this.#r[8]); }

    static canParse(url, base = undefined) {
      if (arguments.length === 0) throw new TypeError('URL.canParse: 1 argument required');
      const r = arguments.length >= 2 && base !== undefined ? n.urlParse(usv(url), usv(base)) : n.urlParse(usv(url));
      return r !== null && r !== -1;
    }
    static parse(url, base = undefined) {
      if (arguments.length === 0) throw new TypeError('URL.parse: 1 argument required');
      try { return arguments.length >= 2 && base !== undefined ? new URL(url, base) : new URL(url); }
      catch { return null; }
    }

    get href() { return URL.#href(URL.#check(this, 'href getter')); }
    set href(v) {
      const r = URL.#check(this, 'href setter');
      void r;
      const s = usv(v);
      const p = n.urlParse(s);
      if (p === null || p === -1) throw new TypeError(`Invalid URL: ${s}`);
      this.#r = p;
      this.#syncParams();
    }
    get origin() { const r = URL.#check(this, 'origin getter'); return n.urlOrigin(r[0], r[3], r[4], r[5]); }
    get protocol() { return URL.#check(this, 'protocol getter')[0] + ':'; }
    set protocol(v) {
      const r = URL.#check(this, 'protocol setter');
      let s = usv(v);
      const colon = StringPrototypeIndexOf(s, ':');
      if (colon >= 0) s = StringPrototypeSlice(s, 0, colon);
      s = StringPrototypeToLowerCase(s);
      if (s === '' || RegExpPrototypeExec(/^[a-z][a-z0-9+\-.]*$/, s) === null) return;
      if (n.isSpecial(r[0]) !== n.isSpecial(s)) return;
      if ((r[0] === 'file') !== (s === 'file')) return;
      r[0] = s;
      if (r[5] === n.defaultPort(s)) r[5] = -1;
    }
    get username() { return URL.#check(this, 'username getter')[1]; }
    set username(v) {
      const r = URL.#check(this, 'username setter');
      if (r[4] || r[0] === 'file') return;
      r[1] = n.urlEncode(0, usv(v), false);
    }
    get password() { return URL.#check(this, 'password getter')[2]; }
    set password(v) {
      const r = URL.#check(this, 'password setter');
      if (r[4] || r[0] === 'file') return;
      r[2] = n.urlEncode(0, usv(v), false);
    }
    get host() { const r = URL.#check(this, 'host getter'); return r[5] === -1 ? r[3] : r[3] + ':' + r[5]; }
    set host(v) {
      const r = URL.#check(this, 'host setter');
      if (r[7]) return;
      const p = n.urlParse(r[0] + '://' + usv(v) + '/');
      if (p !== null && p !== -1 && !p[4]) { r[3] = p[3]; r[4] = false; r[5] = p[5]; }
    }
    get hostname() { return URL.#check(this, 'hostname getter')[3]; }
    set hostname(v) {
      const r = URL.#check(this, 'hostname setter');
      if (r[7]) return;
      const p = n.urlParse(r[0] + '://' + usv(v) + '/');
      if (p !== null && p !== -1 && !p[4]) { r[3] = p[3]; r[4] = false; }
    }
    get port() { const r = URL.#check(this, 'port getter'); return r[5] === -1 ? '' : String(r[5]); }
    set port(v) {
      const r = URL.#check(this, 'port setter');
      if (r[4] || r[0] === 'file') return;
      const s = StringPrototypeTrim(usv(v));
      if (s === '') { r[5] = -1; return; }
      if (RegExpPrototypeExec(/^[+-]?\d+$/, s) === null) return;
      const port = Number(s);
      if (port < 0 || port > 65535) return;
      r[5] = port === n.defaultPort(r[0]) ? -1 : port;
    }
    get pathname() { return URL.#check(this, 'pathname getter')[6]; }
    set pathname(v) {
      const r = URL.#check(this, 'pathname setter');
      if (r[7]) return;
      let s = usv(v);
      if (s.length > 0 && s[0] !== '/') s = '/' + s;
      const p = n.urlParse(r[4] ? r[0] + ':' + s : r[0] + '://' + r[3] + s);
      if (p !== null && p !== -1) r[6] = p[6];
    }
    get search() { const r = URL.#check(this, 'search getter'); return r[9] || r[8] === '' ? '' : '?' + r[8]; }
    set search(v) {
      const r = URL.#check(this, 'search setter');
      let s = usv(v);
      if (s === '') { r[9] = true; r[8] = ''; }
      else {
        if (s[0] === '?') s = StringPrototypeSlice(s, 1);
        r[9] = false;
        r[8] = n.urlEncode(1, s, n.isSpecial(r[0]));
      }
      this.#syncParams();
    }
    get searchParams() {
      const r = URL.#check(this, 'searchParams getter');
      if (this.#params === null) {
        this.#params = new URLSearchParams();
        paramsList(this.#params, r[9] ? '' : r[8], this);
      }
      return this.#params;
    }
    get hash() { const r = URL.#check(this, 'hash getter'); return r[11] || r[10] === '' ? '' : '#' + r[10]; }
    set hash(v) {
      const r = URL.#check(this, 'hash setter');
      let s = usv(v);
      if (s === '') { r[11] = true; r[10] = ''; }
      else {
        if (s[0] === '#') s = StringPrototypeSlice(s, 1);
        r[11] = false;
        r[10] = n.urlEncode(2, s, false);
      }
    }
    toString() { return URL.#href(URL.#check(this, 'prototype.toString')); }
    toJSON() { return URL.#href(URL.#check(this, 'prototype.toJSON')); }

    static {
      urlSetQueryFromParams = (u, s) => {
        if (s === '') { u.#r[9] = true; u.#r[8] = ''; } else { u.#r[9] = false; u.#r[8] = s; }
      };
    }
  }
  defineProperty(URL.prototype, Symbol.toStringTag, { __proto__: null, value: 'URL', configurable: true });
  accessorsEnumerable(URL.prototype, ['href', 'origin', 'protocol', 'username', 'password', 'host', 'hostname', 'port',
    'pathname', 'search', 'searchParams', 'hash']);

  hidden(G, 'URL', URL);
  hidden(G, 'URLSearchParams', URLSearchParams);
})
