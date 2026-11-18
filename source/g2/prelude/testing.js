// The test framework API (docs/testing-api.md) for G2: describe / test / expect, hooks, mocks and runTests, as the
// GocciaTestRunner injects them. Written in JavaScript so it runs on any engine; Vitest decides the semantics.
// G2.HostAPI calls this function with the global object and keeps the API it returns, whose report(done) runs the
// registered suites and passes done their result as JSON, or undefined and the error that stopped them.
(function (global, shared) {
  'use strict';
  const hasOwn = Object.prototype.hasOwnProperty;
  const objToString = Object.prototype.toString;
  const ASYM = Symbol('asymmetricMatcher');

  // ---------------- suites and tests ----------------
  function newSuite(name, parent, mode) {
    return { name, parent, mode, children: [], beforeAll: [], afterAll: [], beforeEach: [], afterEach: [] };
  }
  const root = newSuite('', null, 'run');
  let current = root;
  let hasOnly = false;

  function fullName(suite) {
    const parts = [];
    for (let s = suite; s && s !== root; s = s.parent) parts.unshift(s.name);
    return parts.join(' > ');
  }
  function addSuite(name, fn, mode) {
    const s = newSuite(String(name), current, mode);
    current.children.push({ kind: 'suite', suite: s });
    if (mode === 'only') hasOnly = true;
    const saved = current;
    current = s;
    try { if (typeof fn === 'function') fn(); } finally { current = saved; }
  }
  function addTest(name, fn, mode) {
    current.children.push({ kind: 'test', name: String(name), fn, mode, suite: current });
    if (mode === 'only') hasOnly = true;
  }

  // %s %d %i %f %j %o %O %% and $name / $a.b interpolation (Vitest's test.each titles)
  function formatTitle(title, args) {
    let i = 0;
    let out = String(title).replace(/%[sdifjoO%#]/g, (m) => {
      if (m === '%%') return '%';
      if (m === '%#') return String(formatTitle.index);
      if (i >= args.length) return m;
      const v = args[i++];
      switch (m) {
        case '%s': return typeof v === 'string' ? v : display(v);
        case '%d': case '%i': return String(m === '%i' ? Math.trunc(Number(v)) : Number(v));
        case '%f': return String(Number(v));
        case '%j': try { return JSON.stringify(v); } catch (e) { return '[Circular]'; }
        default: return display(v);
      }
    });
    if (args.length === 1 && args[0] !== null && typeof args[0] === 'object' && !Array.isArray(args[0]))
      out = out.replace(/\$([A-Za-z_$][\w$]*(?:\.[\w$]+)*)/g, (m, path) => {
        let v = args[0];
        for (const k of path.split('.')) { if (v == null) return m; v = v[k]; }
        return typeof v === 'string' ? v : display(v);
      });
    return out;
  }
  function makeEach(register) {
    return (table, ...interp) => {
      let rows = table;
      if (Array.isArray(table) && Array.isArray(table.raw)) {
        // tagged template: a header row of names, then the values
        const header = table.join('').split('|').map((s) => s.trim()).filter(Boolean);
        rows = [];
        for (let k = 0; k < interp.length; k += header.length) {
          const row = {};
          header.forEach((h, j) => { row[h] = interp[k + j]; });
          rows.push(row);
        }
      }
      return (title, fn) => {
        Array.from(rows).forEach((row, index) => {
          const args = Array.isArray(row) ? row : [row];
          formatTitle.index = index;
          register(formatTitle(title, args), typeof fn === 'function' ? () => fn(...args) : fn, args);
        });
      };
    };
  }

  function describe(name, fn) { addSuite(name, fn, 'run'); }
  describe.skip = (name, fn) => addSuite(name, fn, 'skip');
  describe.only = (name, fn) => addSuite(name, fn, 'only');
  describe.todo = (name) => addSuite(name, null, 'todo');
  describe.skipIf = (cond) => (cond ? describe.skip : describe);
  describe.runIf = (cond) => (cond ? describe : describe.skip);
  describe.each = makeEach((title, fn) => describe(title, fn));

  function test(name, fn) { addTest(name, fn, 'run'); }
  test.skip = (name, fn) => addTest(name, fn, 'skip');
  test.only = (name, fn) => addTest(name, fn, 'only');
  test.todo = (name) => addTest(name, null, 'todo');
  test.skipIf = (cond) => (cond ? test.skip : test);
  test.runIf = (cond) => (cond ? test : test.skip);
  test.each = makeEach((title, fn) => test(title, fn));
  test.skip.each = makeEach((title, fn) => test.skip(title, fn));
  test.only.each = makeEach((title, fn) => test.only(title, fn));

  function beforeAll(fn) { current.beforeAll.push(fn); }
  function afterAll(fn) { current.afterAll.push(fn); }
  function beforeEach(fn) { current.beforeEach.push(fn); }
  function afterEach(fn) { current.afterEach.push(fn); }
  let finishedHooks = null;
  function onTestFinished(fn) { if (finishedHooks) finishedHooks.push(fn); }

  // ---------------- display ----------------
  function display(v, depth) {
    depth = depth || 0;
    if (typeof v === 'string') return depth ? `'${v}'` : `'${v}'`;
    if (typeof v === 'bigint') return `${v}n`;
    if (typeof v === 'symbol') return v.toString();
    if (typeof v === 'function') return `[Function${v.name ? ': ' + v.name : ' (anonymous)'}]`;
    if (v === null || typeof v !== 'object') return Object.is(v, -0) ? '-0' : String(v);
    if (v[ASYM]) return v.toString();
    if (depth > 2) return Array.isArray(v) ? '[Array]' : '[Object]';
    try {
      if (Array.isArray(v)) return `[ ${v.map((x) => display(x, depth + 1)).join(', ')} ]`;
      if (v instanceof Error) return `${v.name}: ${v.message}`;
      if (v instanceof Map) return `Map(${v.size}) {}`;
      if (v instanceof Set) return `Set(${v.size}) {}`;
      const keys = Object.keys(v);
      return `{ ${keys.map((k) => `${k}: ${display(v[k], depth + 1)}`).join(', ')} }`;
    } catch (e) { return '[object]'; }
  }

  // ---------------- equality ----------------
  function isError(v) { return v instanceof Error || objToString.call(v) === '[object Error]'; }
  function isAsym(v) { return v !== null && typeof v === 'object' && v[ASYM] === true; }
  function typedKind(v) {
    const t = objToString.call(v);
    return t;
  }
  function equals(a, b, strict, seen) {
    if (isAsym(b)) return b.asymmetricMatch(a);
    if (isAsym(a)) return a.asymmetricMatch(b);
    if (Object.is(a, b)) return true;
    if (a === null || b === null || typeof a !== 'object' || typeof b !== 'object') {
      return false;
    }
    seen = seen || [];
    for (const [x, y] of seen) if (x === a && y === b) return true;
    seen.push([a, b]);
    try {
      const ta = typedKind(a), tb = typedKind(b);
      if (ta !== tb) return false;
      if (strict && a.constructor !== b.constructor) return false;
      if (isError(a) || isError(b)) {
        if (!isError(a) || !isError(b)) return false;
        if (a.name !== b.name || a.message !== b.message) return false;
        if (b.cause !== undefined && !equals(a.cause, b.cause, strict, seen)) return false;
        if (a instanceof AggregateError && b instanceof AggregateError && !equals(a.errors, b.errors, strict, seen)) return false;
      }
      if (a instanceof Date) return Object.is(a.getTime(), b.getTime());
      if (a instanceof RegExp) return a.source === b.source && a.flags === b.flags;
      if (a instanceof Number || a instanceof String || a instanceof Boolean) {
        if (!Object.is(a.valueOf(), b.valueOf())) return false;
      }
      if (a instanceof Map) {
        if (a.size !== b.size) return false;
        outer: for (const [k, v] of a) {
          if (b.has(k) && equals(v, b.get(k), strict, seen)) continue;
          for (const [k2, v2] of b) if (equals(k, k2, strict, seen) && equals(v, v2, strict, seen)) continue outer;
          return false;
        }
        return true;
      }
      if (a instanceof Set) {
        if (a.size !== b.size) return false;
        outer2: for (const v of a) {
          if (b.has(v)) continue;
          for (const v2 of b) if (equals(v, v2, strict, seen)) continue outer2;
          return false;
        }
        return true;
      }
      if (ArrayBuffer.isView(a) && !(a instanceof DataView)) {
        if (a.length !== b.length) return false;
        for (let i = 0; i < a.length; i++) if (!Object.is(a[i], b[i])) return false;
        return true;
      }
      if (a instanceof ArrayBuffer) {
        if (a.byteLength !== b.byteLength) return false;
        const x = new Uint8Array(a), y = new Uint8Array(b);
        for (let i = 0; i < x.length; i++) if (x[i] !== y[i]) return false;
        return true;
      }
      if (a instanceof DataView) {
        if (a.byteLength !== b.byteLength) return false;
        for (let i = 0; i < a.byteLength; i++) if (a.getUint8(i) !== b.getUint8(i)) return false;
        return true;
      }
      if (Array.isArray(a)) {
        if (a.length !== b.length) return false;
        for (let i = 0; i < a.length; i++) {
          if (strict && (i in a) !== (i in b)) return false;
          if (!equals(a[i], b[i], strict, seen)) return false;
        }
        // own non-index keys
        const ka = Object.keys(a).filter((k) => !/^\d+$/.test(k)), kb = Object.keys(b).filter((k) => !/^\d+$/.test(k));
        return keysEqual(a, b, ka, kb, strict, seen);
      }
      const ka = ownEnumKeys(a), kb = ownEnumKeys(b);
      return keysEqual(a, b, ka, kb, strict, seen);
    } finally {
      seen.pop();
    }
  }
  function ownEnumKeys(o) {
    const keys = Object.keys(o);
    for (const s of Object.getOwnPropertySymbols(o)) if (Object.prototype.propertyIsEnumerable.call(o, s)) keys.push(s);
    return keys;
  }
  function keysEqual(a, b, ka, kb, strict, seen) {
    if (!strict) {
      ka = ka.filter((k) => a[k] !== undefined);
      kb = kb.filter((k) => b[k] !== undefined);
    }
    if (ka.length !== kb.length) return false;
    for (const k of ka) {
      if (!hasOwn.call(b, k)) return false;
      if (!strict && b[k] === undefined) return false;
      if (!equals(a[k], b[k], strict, seen)) return false;
    }
    return true;
  }
  function matchObject(actual, expected, seen) {
    if (isAsym(expected)) return expected.asymmetricMatch(actual);
    if (expected === null || typeof expected !== 'object') return equals(actual, expected, false);
    if (actual === null || typeof actual !== 'object') return false;
    seen = seen || [];
    for (const [x, y] of seen) if (x === actual && y === expected) return true;
    seen.push([actual, expected]);
    try {
      if (Array.isArray(expected)) {
        if (!Array.isArray(actual) || actual.length !== expected.length) return false;
        for (let i = 0; i < expected.length; i++) if (!matchObject(actual[i], expected[i], seen)) return false;
        return true;
      }
      if (expected instanceof Date || expected instanceof RegExp || expected instanceof Map || expected instanceof Set ||
          isError(expected)) return equals(actual, expected, false);
      for (const k of ownEnumKeys(expected)) {
        if (!(k in actual)) return false;
        if (!matchObject(actual[k], expected[k], seen)) return false;
      }
      return true;
    } finally { seen.pop(); }
  }

  // ---------------- asymmetric matchers ----------------
  function asym(name, match, text) {
    return { [ASYM]: true, asymmetricMatch: match, toString: () => text || name, toJSON: () => text || name };
  }
  function anyMatcher(ctor) {
    return asym('Any', (v) => {
      if (ctor === Number) return typeof v === 'number' || v instanceof Number;
      if (ctor === String) return typeof v === 'string' || v instanceof String;
      if (ctor === Boolean) return typeof v === 'boolean' || v instanceof Boolean;
      if (ctor === BigInt) return typeof v === 'bigint';
      if (ctor === Symbol) return typeof v === 'symbol';
      if (ctor === Function) return typeof v === 'function';
      if (ctor === Object) return v !== null && typeof v === 'object';
      return v instanceof ctor;
    }, `Any<${ctor && ctor.name}>`);
  }
  function expectAsym(not) {
    const wrap = (m) => (not ? asym('Not' + m.toString(), (v) => !m.asymmetricMatch(v), 'Not' + m.toString()) : m);
    return {
      objectContaining: (o) => wrap(asym('ObjectContaining', (v) => v !== null && typeof v === 'object' &&
        ownEnumKeys(o).every((k) => k in Object(v) && equals(v[k], o[k], false)))),
      arrayContaining: (arr) => wrap(asym('ArrayContaining', (v) => Array.isArray(v) &&
        arr.every((x) => v.some((y) => equals(y, x, false))))),
      stringContaining: (s) => wrap(asym('StringContaining', (v) => typeof v === 'string' && v.includes(s))),
      stringMatching: (r) => wrap(asym('StringMatching', (v) => typeof v === 'string' &&
        (typeof r === 'string' ? v.includes(r) : new RegExp(r.source, r.flags.replace(/[gy]/g, '')).test(v)))),
    };
  }

  // ---------------- expect ----------------
  class AssertionError extends Error { constructor(m) { super(m); this.name = 'AssertionError'; } }
  let assertionCount = 0;
  let expectedAssertions = null, needAssertions = false;

  function thrownMessage(e) {
    if (typeof e === 'string') return e;
    if (e !== null && typeof e === 'object' && 'message' in e) return String(e.message);
    return String(e);
  }
  function errorMatches(thrown, expected) {
    if (expected === undefined) return true;
    if (isAsym(expected)) return expected.asymmetricMatch(thrown);
    if (typeof expected === 'string') {
      const m = thrownMessage(thrown);
      return expected === '' ? m === '' : m.includes(expected);
    }
    if (expected instanceof RegExp) return new RegExp(expected.source, expected.flags.replace(/[gy]/g, '')).test(thrownMessage(thrown));
    if (typeof expected === 'function') return thrown instanceof expected;
    if (isError(expected)) return isError(thrown) && equals(thrown, expected, false);
    if (expected !== null && typeof expected === 'object' && 'message' in expected) return thrownMessage(thrown) === String(expected.message);
    return false;
  }

  function pathSegments(path) {
    if (Array.isArray(path)) return path.map((p) => p);
    return String(path).replace(/\[(\w+)\]/g, '.$1').split('.').filter((s) => s !== '');
  }
  function getPath(obj, path) {
    if (obj == null) return { found: false };
    if (!Array.isArray(path) && Object(obj) && hasOwn.call(Object(obj), path)) return { found: true, value: obj[path] };
    const segs = pathSegments(path);
    if (segs.length === 0) return { found: false };
    let v = obj;
    for (const s of segs) {
      if (v == null || !(s in Object(v))) return { found: false };
      v = v[s];
    }
    return { found: true, value: v };
  }

  function makeMatchers(actual, not, promiseMode) {
    const m = {};
    const check = (pass, msg) => {
      assertionCount++;
      if (pass === not) throw new AssertionError(typeof msg === 'function' ? msg() : msg);
    };
    const neg = not ? ' not' : '';
    const defs = {
      toBe: (e) => check(Object.is(actual, e), () => `Expected ${display(actual)}${neg} to be ${display(e)}`),
      toEqual: (e) => check(equals(actual, e, false), () => `Expected ${display(actual)}${neg} to equal ${display(e)}`),
      toStrictEqual: (e) => check(equals(actual, e, true), () => `Expected ${display(actual)}${neg} to strictly equal ${display(e)}`),
      toBeNull: () => check(actual === null, () => `Expected ${display(actual)}${neg} to be null`),
      toBeUndefined: () => check(actual === undefined, () => `Expected ${display(actual)}${neg} to be undefined`),
      toBeDefined: () => check(actual !== undefined, () => `Expected ${display(actual)}${neg} to be defined`),
      toBeNaN: () => check(Number.isNaN(actual), () => `Expected ${display(actual)}${neg} to be NaN`),
      toBeTruthy: () => check(!!actual, () => `Expected ${display(actual)}${neg} to be truthy`),
      toBeFalsy: () => check(!actual, () => `Expected ${display(actual)}${neg} to be falsy`),
      toBeGreaterThan: (n) => check(actual > n, () => `Expected ${display(actual)}${neg} to be greater than ${display(n)}`),
      toBeGreaterThanOrEqual: (n) => check(actual >= n, () => `Expected ${display(actual)}${neg} to be greater than or equal to ${display(n)}`),
      toBeLessThan: (n) => check(actual < n, () => `Expected ${display(actual)}${neg} to be less than ${display(n)}`),
      toBeLessThanOrEqual: (n) => check(actual <= n, () => `Expected ${display(actual)}${neg} to be less than or equal to ${display(n)}`),
      toBeCloseTo: (n, digits = 2) => check(
        (actual === Infinity && n === Infinity) || (actual === -Infinity && n === -Infinity) ||
        Math.abs(n - actual) < Math.pow(10, -digits) / 2, () => `Expected ${display(actual)}${neg} to be close to ${display(n)}`),
      toBeInstanceOf: (C) => check(actual instanceof C, () => `Expected ${display(actual)}${neg} to be an instance of ${C && C.name}`),
      toBeOneOf: (arr) => check(Array.from(arr).some((x) => equals(actual, x, false)), () => `Expected ${display(actual)}${neg} to be one of ${display(arr)}`),
      toSatisfy: (fn) => check(!!fn(actual), () => `Expected ${display(actual)}${neg} to satisfy the predicate`),
      toContain: (item) => check(
        typeof actual === 'string' ? actual.includes(item)
          : actual instanceof Set ? actual.has(item)
          : actual != null && typeof actual[Symbol.iterator] === 'function' ? Array.from(actual).some((x) => Object.is(x, item) || x === item)
          : false, () => `Expected ${display(actual)}${neg} to contain ${display(item)}`),
      toContainEqual: (item) => check(actual != null && Array.from(actual).some((x) => equals(x, item, false)),
        () => `Expected ${display(actual)}${neg} to contain equal ${display(item)}`),
      toHaveLength: (n) => check(actual != null && actual.length === n, () => `Expected ${display(actual)}${neg} to have length ${n}`),
      toHaveProperty: (path, ...rest) => {
        const r = getPath(actual, path);
        const pass = r.found && (rest.length === 0 || equals(r.value, rest[0], false));
        check(pass, () => `Expected ${display(actual)}${neg} to have property ${display(path)}`);
      },
      toMatch: (p) => check(typeof actual === 'string' && (typeof p === 'string' ? actual.includes(p)
        : new RegExp(p.source, p.flags.replace(/[gy]/g, '')).test(actual)), () => `Expected ${display(actual)}${neg} to match ${display(p)}`),
      toMatchObject: (o) => check(matchObject(actual, o), () => `Expected ${display(actual)}${neg} to match object ${display(o)}`),
      toThrow: (e) => {
        let threw = false, thrown;
        if (promiseMode === 'rejects') { threw = true; thrown = actual; }
        else {
          if (typeof actual !== 'function') throw new AssertionError('Expected a function to check for a throw');
          try { actual(); } catch (x) { threw = true; thrown = x; }
        }
        check(threw && errorMatches(thrown, e), () => threw
          ? `Expected the thrown ${display(thrown)}${neg} to match ${display(e)}`
          : `Expected the function to throw${e !== undefined ? ' ' + (e && e.name || display(e)) : ''} but it did not throw`);
      },
      toHaveBeenCalled: () => check(mockOf(actual).calls.length > 0, () => `Expected the mock${neg} to have been called`),
      toHaveBeenCalledOnce: () => check(mockOf(actual).calls.length === 1, () => `Expected the mock${neg} to have been called once`),
      toHaveBeenCalledTimes: (n) => check(mockOf(actual).calls.length === n, () => `Expected the mock${neg} to have been called ${n} times`),
      toHaveBeenCalledWith: (...args) => check(mockOf(actual).calls.some((c) => equals(c, args, false)), () => `Expected the mock${neg} to have been called with ${display(args)}`),
      toHaveBeenLastCalledWith: (...args) => { const c = mockOf(actual).calls; check(c.length > 0 && equals(c[c.length - 1], args, false), () => `Expected the last call${neg} to be with ${display(args)}`); },
      toHaveBeenNthCalledWith: (n, ...args) => { const c = mockOf(actual).calls; check(c.length >= n && equals(c[n - 1], args, false), () => `Expected call ${n}${neg} to be with ${display(args)}`); },
      toHaveReturned: () => check(mockOf(actual).results.some((r) => r.type === 'return'), () => `Expected the mock${neg} to have returned`),
      toHaveReturnedTimes: (n) => check(mockOf(actual).results.filter((r) => r.type === 'return').length === n, () => `Expected the mock${neg} to have returned ${n} times`),
      toHaveReturnedWith: (v) => check(mockOf(actual).results.some((r) => r.type === 'return' && equals(r.value, v, false)), () => `Expected the mock${neg} to have returned ${display(v)}`),
      toHaveLastReturnedWith: (v) => { const r = mockOf(actual).results; check(r.length > 0 && r[r.length - 1].type === 'return' && equals(r[r.length - 1].value, v, false), () => `Expected the last return${neg} to be ${display(v)}`); },
      toHaveNthReturnedWith: (n, v) => { const r = mockOf(actual).results; check(r.length >= n && r[n - 1].type === 'return' && equals(r[n - 1].value, v, false), () => `Expected return ${n}${neg} to be ${display(v)}`); },
      toBeFasterThan: () => check(true, ''),
      toBeSlowerThan: () => check(true, ''),
    };
    defs.toBeCalled = defs.toHaveBeenCalled;
    defs.toBeCalledTimes = defs.toHaveBeenCalledTimes;
    defs.toBeCalledWith = defs.toHaveBeenCalledWith;
    defs.toThrowError = defs.toThrow;
    for (const k in defs) m[k] = defs[k];
    for (const [k, fn] of customMatchers) {
      m[k] = (...args) => {
        const r = fn.call({ isNot: not, equals: (a, b) => equals(a, b, false) }, actual, ...args);
        check(!!r.pass, () => (typeof r.message === 'function' ? r.message() : String(r.message || k)));
      };
    }
    return m;
  }
  const customMatchers = new Map();

  function promiseMatchers(actual, not, mode) {
    const out = {};
    const base = makeMatchers(undefined, not, mode);
    for (const k of Object.keys(base)) {
      out[k] = (...args) => Promise.resolve(actual).then(
        (v) => {
          if (mode === 'rejects') { assertionCount++; throw new AssertionError(`Expected the promise to reject but it resolved with ${display(v)}`); }
          return makeMatchers(v, not, null)[k](...args);
        },
        (e) => {
          if (mode === 'resolves') { assertionCount++; throw new AssertionError(`Expected the promise to resolve but it rejected with ${display(e)}`); }
          return makeMatchers(e, not, k === 'toThrow' || k === 'toThrowError' ? 'rejects' : null)[k](...args);
        });
    }
    return out;
  }

  function expect(actual) {
    const m = makeMatchers(actual, false, null);
    m.not = makeMatchers(actual, true, null);
    m.resolves = promiseMatchers(actual, false, 'resolves');
    m.rejects = promiseMatchers(actual, false, 'rejects');
    m.resolves.not = promiseMatchers(actual, true, 'resolves');
    m.rejects.not = promiseMatchers(actual, true, 'rejects');
    return m;
  }
  expect.any = anyMatcher;
  expect.anything = () => asym('Anything', (v) => v !== null && v !== undefined);
  Object.assign(expect, expectAsym(false));
  expect.not = expectAsym(true);
  expect.assertions = (n) => { expectedAssertions = n; };
  expect.hasAssertions = () => { needAssertions = true; };
  expect.extend = (obj) => { for (const k of Object.keys(obj)) customMatchers.set(k, obj[k]); };

  // ---------------- mocks ----------------
  const MOCK = Symbol('mock');
  function mockOf(f) {
    if (typeof f !== 'function' || !f[MOCK]) throw new AssertionError(`${display(f)} is not a mock function`);
    return f.mock;
  }
  function mock(impl) {
    const state = { calls: [], results: [], instances: [], contexts: [], lastCall: undefined };
    let implementation = impl;
    const once = [];
    let name = 'mock';
    const f = function (...args) {
      state.calls.push(args);
      state.lastCall = args;
      state.contexts.push(this);
      if (new.target) state.instances.push(this);
      const result = { type: 'incomplete', value: undefined };
      state.results.push(result);
      const fn = once.length ? once.shift() : implementation;
      try {
        const v = fn ? (new.target ? Reflect.construct(fn, args, new.target) : fn.apply(this, args)) : undefined;
        result.type = 'return'; result.value = v;
        return v;
      } catch (e) {
        result.type = 'throw'; result.value = e;
        throw e;
      }
    };
    Object.defineProperty(f, 'mock', { get: () => state });
    f[MOCK] = true;
    f.mockImplementation = (fn) => { implementation = fn; return f; };
    f.mockImplementationOnce = (fn) => { once.push(fn); return f; };
    f.mockReturnValue = (v) => f.mockImplementation(() => v);
    f.mockReturnValueOnce = (v) => f.mockImplementationOnce(() => v);
    f.mockResolvedValue = (v) => f.mockImplementation(() => Promise.resolve(v));
    f.mockResolvedValueOnce = (v) => f.mockImplementationOnce(() => Promise.resolve(v));
    f.mockRejectedValue = (v) => f.mockImplementation(() => Promise.reject(v));
    f.mockRejectedValueOnce = (v) => f.mockImplementationOnce(() => Promise.reject(v));
    f.mockReturnThis = () => f.mockImplementation(function () { return this; });
    f.mockClear = () => { state.calls.length = 0; state.results.length = 0; state.instances.length = 0; state.contexts.length = 0; state.lastCall = undefined; return f; };
    // mockReset reinstates the implementation the mock was made with (a spy's the method it replaced)
    f.mockReset = () => { f.mockClear(); implementation = impl; once.length = 0; return f; };
    f.mockRestore = () => { f.mockReset(); if (f.__restore) f.__restore(); return f; };
    f.getMockImplementation = () => implementation;
    f.mockName = (n) => { name = n; return f; };
    f.getMockName = () => name;
    return f;
  }
  function spyOn(obj, key, access) {
    const desc = Object.getOwnPropertyDescriptor(obj, key);
    if (access === 'get' || access === 'set') {
      const d = desc || Object.getOwnPropertyDescriptor(Object.getPrototypeOf(obj), key);
      const orig = d && d[access];
      const m = mock(orig);
      Object.defineProperty(obj, key, Object.assign({}, d, { [access]: m, configurable: true }));
      m.__restore = () => { if (desc) Object.defineProperty(obj, key, desc); else delete obj[key]; };
      return m;
    }
    const orig = obj[key];
    if (typeof orig !== 'function') throw new TypeError(`Cannot spy on ${String(key)}: not a function`);
    const m = mock(orig);
    obj[key] = m;
    m.__restore = () => { if (desc) Object.defineProperty(obj, key, desc); else delete obj[key]; };
    return m;
  }

  // ---------------- running ----------------
  function selected(suite) {
    // a suite or test is focused when it or an ancestor is .only
    for (let s = suite; s; s = s.parent) if (s.mode === 'only') return true;
    return false;
  }
  async function runHooks(list) { for (const fn of list) await fn(); }
  function chainHooks(suite, kind) {
    const out = [];
    for (let s = suite; s; s = s.parent) out.unshift(...s[kind]);
    return kind === 'afterEach' ? out.reverse() : out;
  }
  async function runTests() {
    const result = { totalTests: 0, passed: 0, failed: 0, skipped: 0, totalRunTests: 0, assertions: 0, failedTests: [] };
    async function runSuite(suite, skipped) {
      const skipAll = skipped || suite.mode === 'skip' || suite.mode === 'todo';
      let runnable = false;
      const items = suite.children;
      try { if (!skipAll) await runHooks(suite.beforeAll); }
      catch (e) {
        result.failed++;
        result.failedTests.push({ name: `beforeAll in ${fullName(suite) || 'root'}`, error: thrownMessage(e) });
        return;
      }
      for (const item of items) {
        if (item.kind === 'suite') { await runSuite(item.suite, skipAll); continue; }
        result.totalTests++;
        const focusedOut = hasOnly && item.mode !== 'only' && !selected(suite);
        if (skipAll || item.mode === 'skip' || item.mode === 'todo' || focusedOut || typeof item.fn !== 'function') {
          result.skipped++;
          continue;
        }
        runnable = true;
        result.totalRunTests++;
        assertionCount = 0; expectedAssertions = null; needAssertions = false;
        finishedHooks = [];
        let error = null;
        try {
          await runHooks(chainHooks(suite, 'beforeEach'));
          await item.fn();
          // the timers the test scheduled run before it ends; one a callback threw fails it
          const timerError = shared && shared.timers ? await shared.timers.drainTest() : null;
          if (timerError !== null) throw new Error('Uncaught exception in a timer callback: ' + thrownMessage(timerError.error));
          if (expectedAssertions !== null && assertionCount !== expectedAssertions)
            throw new AssertionError(`Expected ${expectedAssertions} assertions, got ${assertionCount}`);
          if (needAssertions && assertionCount === 0) throw new AssertionError('Expected at least one assertion');
        } catch (e) { error = e; }
        try { await runHooks(chainHooks(suite, 'afterEach')); } catch (e) { if (!error) error = e; }
        const fins = finishedHooks; finishedHooks = null;
        for (const fn of fins) { try { await fn(); } catch (e) { if (!error) error = e; } }
        result.assertions += assertionCount;
        if (error) {
          result.failed++;
          result.failedTests.push({ name: `Test "${item.name}" in suite "${fullName(suite)}"`, error: thrownMessage(error) });
        } else result.passed++;
      }
      try { if (!skipAll) await runHooks(suite.afterAll); }
      catch (e) { result.failed++; result.failedTests.push({ name: `afterAll in ${fullName(suite) || 'root'}`, error: thrownMessage(e) }); }
      return runnable;
    }
    await runSuite(root, false);
    return result;
  }

  const api = { describe, test, it: test, expect, beforeAll, afterAll, beforeEach, afterEach, onTestFinished, mock, spyOn, runTests };
  for (const k of Object.keys(api)) Object.defineProperty(global, k, { value: api[k], writable: true, configurable: true });
  if (shared && shared.hostModules) shared.hostModules['goccia:test'] = () => api;
  return {
    report(done) {
      return runTests().then(
        r => done(JSON.stringify({ totalTests: r.totalTests, passed: r.passed, failed: r.failed, skipped: r.skipped,
          assertions: r.assertions, failedTests: r.failedTests })),
        e => done(undefined, String(e)));
    }
  };
})
