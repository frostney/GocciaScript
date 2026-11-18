// The test framework API (docs/testing-api.md) for G2: describe / test / expect, hooks, mocks and runTests, as the
// GocciaTestRunner injects them. Written in JavaScript so it runs on any engine. The matchers, the asymmetric matchers
// and their equality follow GocciaScript's own testing library (Goccia.Builtins.TestingLibrary,
// Goccia.Builtins.Testing.DeepEquality, Goccia.Values.AsymmetricMatcher), messages included, so a suite reports the
// same results on both engines.
// G2.HostAPI calls this function with the global object and keeps the API it returns, whose report(done) runs the
// registered suites and passes done their result as JSON, or undefined and the error that stopped them.
(function (global, shared) {
  'use strict';
  // the intrinsics as the realm made them: a script's top-level declarations can replace these globals later
  // (function Object() {} in compat-function code) while the library keeps working
  const { Object, Array, Error, TypeError, RangeError, SyntaxError, String, Number, Boolean, Symbol, BigInt, RegExp,
    Promise, Math, Set, Map, JSON, Reflect, Function, Proxy, Uint8Array } = global;
  const hasOwn = Object.prototype.hasOwnProperty;
  const propertyIsEnumerable = Object.prototype.propertyIsEnumerable;
  const getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  const getOwnPropertySymbols = Object.getOwnPropertySymbols;
  const keysOf = Object.keys;
  const apply = Reflect.apply;
  const isArray = Array.isArray;
  const isObject = (v) => v !== null && (typeof v === 'object' || typeof v === 'function');
  const brand = (fn) => (v) => { try { apply(fn, v, []); return true; } catch { return false; } };
  const isMap = brand(getOwnPropertyDescriptor(Map.prototype, 'size').get);
  const isSet = brand(getOwnPropertyDescriptor(Set.prototype, 'size').get);
  const regExpSource = getOwnPropertyDescriptor(RegExp.prototype, 'source').get;
  const isRegExp = (v) => isObject(v) && v !== RegExp.prototype && brand(regExpSource)(v);
  // the typed array's kind ('Uint8Array', ...), or undefined for anything else
  const typedArrayTag = getOwnPropertyDescriptor(Object.getPrototypeOf(Int8Array.prototype), Symbol.toStringTag).get;
  const typedKind = (v) => apply(typedArrayTag, v, []);
  const isError = typeof Error.isError === 'function' ? Error.isError : (v) => v instanceof Error;
  const setForEach = Set.prototype.forEach, setHas = Set.prototype.has, mapForEach = Map.prototype.forEach;
  const promiseThen = Promise.prototype.then;
  const isPromise = (v) => v instanceof Promise;
  const StringCtor = String, NumberCtor = Number, BooleanCtor = Boolean, BigIntCtor = BigInt, SymbolCtor = Symbol;
  const FunctionCtor = Function, ObjectCtor = Object;
  // the primitive inside a string or number, boxed or not, or undefined
  const unbox = (proto, type) => (v) => {
    if (typeof v === type) return v;
    if (isObject(v)) { try { return apply(proto.valueOf, v, []); } catch {} }
    return undefined;
  };
  const stringOf = unbox(String.prototype, 'string');
  const numberOf = unbox(Number.prototype, 'number');
  // appends without [[Set]], so an index setter or read-only index a test installs on Array.prototype or
  // Object.prototype never sees the library's own bookkeeping
  const append = (list, v) => {
    Object.defineProperty(list, list.length, { value: v, writable: true, enumerable: true, configurable: true });
  };

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
  // the argument checks of Goccia.Arguments.Validator, whose failures are plain Errors
  function requireExactly(args, n, name) {
    if (args.length === n) return;
    throw new Error(n === 0 ? `${name} expects no arguments but got ${args.length}`
      : n === 1 ? `${name} expects 1 argument but got ${args.length}`
        : `${name} expects ${n} arguments but got ${args.length}`);
  }
  function requireRegistration(args, name) {
    requireExactly(args, 2, name);
    if (typeof args[0] !== 'string') throw new TypeError(`${name} expects first argument to be a string`);
    if (typeof args[1] !== 'function') throw new TypeError(`${name} expects second argument to be a function`);
  }
  function addSuite(args, name, mode) {
    requireRegistration(args, name);
    const s = newSuite(args[0], current, mode);
    current.children.push({ kind: 'suite', suite: s });
    if (mode === 'only') hasOnly = true;
    const saved = current;
    current = s;
    try { args[1](); } finally { current = saved; }
  }
  function addTest(args, name, mode) {
    requireRegistration(args, name);
    current.children.push({ kind: 'test', name: args[0], fn: args[1], mode, suite: current });
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
        case '%s': return typeof v === 'string' ? v : show(v);
        case '%d': case '%i': return String(m === '%i' ? Math.trunc(Number(v)) : Number(v));
        case '%f': return String(Number(v));
        case '%j': try { return JSON.stringify(v); } catch (e) { return '[Circular]'; }
        default: return show(v);
      }
    });
    if (args.length === 1 && args[0] !== null && typeof args[0] === 'object' && !isArray(args[0]))
      out = out.replace(/\$([A-Za-z_$][\w$]*(?:\.[\w$]+)*)/g, (m, path) => {
        let v = args[0];
        for (const k of path.split('.')) { if (v == null) return m; v = v[k]; }
        return typeof v === 'string' ? v : show(v);
      });
    return out;
  }
  function makeEach(name, register) {
    return (...eachArgs) => {
      const [table, ...interp] = eachArgs;
      const tagged = isArray(table) && isArray(table.raw);
      if (!tagged) requireExactly(eachArgs, 1, name);
      if (!isArray(table)) throw new TypeError(`${name} expects a table array`);
      let rows = table;
      if (tagged) {
        // tagged template: a header row of names, then the values
        const header = table.join('').split('|').map((s) => s.trim()).filter(Boolean);
        rows = [];
        for (let k = 0; k < interp.length; k += header.length) {
          const row = {};
          header.forEach((h, j) => { row[h] = interp[k + j]; });
          rows.push(row);
        }
      }
      return (...regArgs) => {
        requireRegistration(regArgs, name);
        const [title, fn] = regArgs;
        Array.from(rows).forEach((row, index) => {
          const args = isArray(row) ? row : [row];
          formatTitle.index = index;
          register(formatTitle(title, args), () => fn(...args));
        });
      };
    };
  }

  function describe(...args) { addSuite(args, 'describe', 'run'); }
  describe.skip = (...args) => addSuite(args, 'describe.skip', 'skip');
  describe.only = (...args) => addSuite(args, 'describe.only', 'only');
  describe.todo = (name) => {
    current.children.push({ kind: 'suite', suite: newSuite(String(name), current, 'todo') });
  };
  describe.skipIf = (...args) => {
    requireExactly(args, 1, 'describe.skipIf');
    return args[0] ? describe.skip : describe;
  };
  describe.runIf = (...args) => {
    requireExactly(args, 1, 'describe.runIf');
    return args[0] ? describe : describe.skip;
  };
  describe.each = makeEach('describe.each', (title, fn) => describe(title, fn));

  function test(...args) { addTest(args, 'test', 'run'); }
  test.skip = (...args) => addTest(args, 'test.skip', 'skip');
  test.only = (...args) => addTest(args, 'test.only', 'only');
  test.todo = (...args) => {
    requireExactly(args, 1, 'test.todo');
    if (typeof args[0] !== 'string') throw new TypeError('test.todo expects first argument to be a string');
    current.children.push({ kind: 'test', name: args[0], fn: null, mode: 'todo', suite: current });
  };
  test.skipIf = (...args) => { requireExactly(args, 1, 'test.skipIf'); return args[0] ? test.skip : test; };
  test.runIf = (...args) => { requireExactly(args, 1, 'test.runIf'); return args[0] ? test : test.skip; };
  test.each = makeEach('test.each', (title, fn) => test(title, fn));
  test.skip.each = makeEach('test.each', (title, fn) => test.skip(title, fn));
  test.only.each = makeEach('test.each', (title, fn) => test.only(title, fn));

  // a hook is any callable, a callable Proxy included
  function hook(kind) {
    return (...args) => {
      requireExactly(args, 1, kind);
      if (typeof args[0] !== 'function') throw new TypeError(`${kind} expects a function argument`);
      current[kind].push(args[0]);
    };
  }
  const beforeAll = hook('beforeAll'), afterAll = hook('afterAll');
  const beforeEach = hook('beforeEach'), afterEach = hook('afterEach');
  let finishedHooks = null;
  function onTestFinished(...args) {
    requireExactly(args, 1, 'onTestFinished');
    if (typeof args[0] !== 'function') throw new TypeError('onTestFinished expects a function argument');
    if (finishedHooks) append(finishedHooks, args[0]);
  }

  // ---------------- display ----------------
  // Goccia.Values.Formatting's display form: strings bare at the top level, quoted when nested
  function show(v) {
    if (AsymmetricMatcher.is(v)) return AsymmetricMatcher.spec(v).display();
    if (shared && shared.display) return shared.display(v);
    try { return String(v); } catch { return typeof v; }
  }
  // errors keep name and message off the enumerable keys, so they read as "Name: message"
  function describeThrown(v) {
    if (isObject(v)) {
      const name = v.name, message = v.message;
      if (typeof name === 'string' && typeof message === 'string') return name + ': ' + message;
    }
    return show(v);
  }
  function describeConstructor(c) {
    const name = isObject(c) && typeof c.name === 'string' ? c.name : '';
    return name !== '' ? name : show(c);
  }
  // an instance-of subject: an error as "Name: message", another object prefixed by its constructor's name
  function describeInstance(v) {
    const rendered = describeThrown(v);
    if (!isObject(v) || rendered !== show(v)) return rendered;
    const ctor = v.constructor;
    const ctorName = ctor === undefined ? '' : describeConstructor(ctor);
    return ctorName !== '' && ctorName !== 'Object' && ctorName !== rendered ? ctorName + rendered : rendered;
  }

  // ---------------- asymmetric matchers ----------------
  // One class holds every matcher; its spec carries the kind, the sample, the inverse flag and the behaviour.
  // match(other, eq) runs with the equality of the comparison that reached it (loose or strict).
  class AsymmetricMatcher {
    #spec;
    constructor(spec) { this.#spec = spec; }
    static is(v) { return v !== null && typeof v === 'object' && #spec in v; }
    static spec(v) { return v.#spec; }
    asymmetricMatch(other) { return this.#spec.match(other, looseEquals); }
    toString() { return this.#spec.display(); }
    toAsymmetricMatcher() { return this.#spec.display(); }
  }
  const isAsym = AsymmetricMatcher.is;
  function matcher(kind, names, sample, inverse, match, extra) {
    const spec = {
      kind, sample, inverse: !!inverse, hasSample: true, match,
      name: inverse ? names[1] : names[0],
      display() { return spec.name; },
      // matchers of the same kind and polarity whose samples are equal
      equivalent(other, eq) {
        if (other.kind !== spec.kind || other.inverse !== spec.inverse) return false;
        if (!spec.hasSample || !other.hasSample) return spec.hasSample === other.hasSample;
        return eq(spec.sample, other.sample);
      },
    };
    if (extra) Object.assign(spec, extra(spec));
    return new AsymmetricMatcher(spec);
  }
  const not = (spec, v) => v !== spec.inverse;

  function anything() {
    return matcher('anything', ['Anything'], undefined, false, (v) => v !== null && v !== undefined,
      () => ({ hasSample: false }));
  }
  function any(...args) {
    const ctor = args[0];
    if (ctor === undefined)
      throw new TypeError('any() expects to be passed a constructor function. ' +
        'Please pass one or use anything() to match any object.');
    const name = isObject(ctor) && typeof ctor.name === 'string' ? ctor.name : '<anonymous>';
    return matcher('any', ['Any'], ctor, false, (v) => {
      if (ctor === StringCtor) return stringOf(v) !== undefined;
      if (ctor === NumberCtor) return numberOf(v) !== undefined;
      if (ctor === FunctionCtor) return typeof v === 'function';
      if (ctor === BooleanCtor) return typeof v === 'boolean' || (isObject(v) && v instanceof BooleanCtor);
      if (ctor === BigIntCtor) return typeof v === 'bigint' || (isObject(v) && v instanceof BigIntCtor);
      if (ctor === SymbolCtor) return typeof v === 'symbol' || (isObject(v) && v instanceof SymbolCtor);
      // typeof, so expect.any(Object) also matches null
      if (ctor === ObjectCtor) return typeof v === 'object';
      return v instanceof ctor;
    }, () => ({
      display: () => `Any<${name}>`,
      equivalent: (other) => other.kind === 'any' && other.sample === ctor,
    }));
  }
  function arrayContaining(sample, inverse) {
    const names = ['ArrayContaining', 'ArrayNotContaining'];
    return matcher('arrayContaining', names, sample, inverse, function match(v, eq) {
      const spec = this;
      if (!isArray(sample)) throw new TypeError(`You must provide an array to ${spec.name}, not '${typeof sample}'.`);
      if (sample.length === 0) return not(spec, true);
      if (!isArray(v)) return not(spec, false);
      for (let i = 0; i < sample.length; i++) {
        if (!(i in sample)) continue;
        let found = false;
        for (let j = 0; j < v.length && !found; j++) found = eq(v[j], sample[i]);
        if (!found) return not(spec, false);
      }
      return not(spec, true);
    });
  }
  function enumerableSymbols(o) {
    return getOwnPropertySymbols(o).filter((s) => apply(propertyIsEnumerable, o, [s]));
  }
  function objectContaining(sample, inverse) {
    const names = ['ObjectContaining', 'ObjectNotContaining'];
    return matcher('objectContaining', names, sample, inverse, function match(v, eq) {
      const spec = this;
      if (sample === null || typeof sample !== 'object')
        throw new TypeError(`You must provide an object to ${spec.name}, not '${typeof sample}'.`);
      const keys = [...keysOf(sample), ...enumerableSymbols(sample)];
      if (keys.length === 0) return not(spec, true);
      if (!v) return not(spec, false);
      // the actual value is boxed, so a string matches { length: 3 }; inherited members count
      const o = ObjectCtor(v);
      for (const k of keys) if (!(k in o) || !eq(o[k], sample[k])) return not(spec, false);
      return not(spec, true);
    });
  }
  function stringContaining(sample, inverse) {
    const text = stringOf(sample);
    if (text === undefined) throw new Error('Expected is not a string');
    return matcher('stringContaining', ['StringContaining', 'StringNotContaining'], sample, inverse, function match(v) {
      const s = stringOf(v);
      return not(this, s !== undefined && s.includes(text));
    });
  }
  function stringMatching(sample, inverse) {
    const text = stringOf(sample);
    let re;
    if (text !== undefined) re = new RegExp(text, '');
    else if (isRegExp(sample)) re = new RegExp(sample.source, sample.flags);
    else throw new Error('Expected is not a String or a RegExp');
    return matcher('stringMatching', ['StringMatching', 'StringNotMatching'], re, inverse, function match(v) {
      const s = stringOf(v);
      if (s === undefined) return not(this, false);
      // one match from the start, whatever the global or sticky state
      re.lastIndex = 0;
      return not(this, re.test(s));
    }, (spec) => ({
      equivalent: (other) => other.kind === spec.kind && other.inverse === spec.inverse &&
        other.sample.source === re.source && other.sample.flags === re.flags,
    }));
  }
  function closeTo(sample, precisionArg, inverse) {
    const expected = numberOf(sample);
    if (expected === undefined) throw new Error('Expected is not a Number');
    let precision = 2;
    if (precisionArg !== undefined) {
      precision = numberOf(precisionArg);
      if (precision === undefined) throw new Error('Precision is not a Number');
    }
    return matcher('closeTo', ['NumberCloseTo', 'NumberNotCloseTo'], sample, inverse, function match(v) {
      const actual = numberOf(v);
      // a non-number never matches, not even the inverse
      if (actual === undefined) return false;
      if (Number.isNaN(actual) || Number.isNaN(expected) || Number.isNaN(precision)) return not(this, false);
      if (!Number.isFinite(actual) && !Number.isFinite(expected)) {
        if (Math.sign(actual) === Math.sign(expected)) return not(this, true);
      }
      let tolerance;
      if (!Number.isFinite(precision)) tolerance = precision > 0 ? 0 : Infinity;
      else if (Math.abs(precision) <= 308 && Number.isInteger(precision)) {
        // stepwise, as Goccia computes it, so the boundary cases round alike
        tolerance = 1;
        if (precision >= 0) for (let i = 1; i <= precision; i++) tolerance /= 10;
        else for (let i = -1; i >= precision; i--) tolerance *= 10;
        tolerance /= 2;
      } else tolerance = Math.pow(10, -precision) / 2;
      return not(this, tolerance === Infinity || Math.abs(expected - actual) < tolerance);
    }, (spec) => ({
      precision,
      display: () => `${spec.name} ${String(sample)} (${precision} ${precision === 1 ? 'digit' : 'digits'})`,
      equivalent(other, eq) {
        return other.kind === spec.kind && other.inverse === spec.inverse && eq(spec.sample, other.sample) &&
          Object.is(precision, other.precision);
      },
    }));
  }
  function schemaMatching(sample, inverse) {
    const invalid = () => new TypeError('SchemaMatching expected to receive a Standard Schema.');
    if (!isObject(sample)) throw invalid();
    const standard = sample['~standard'];
    if (!isObject(standard)) throw invalid();
    if (typeof standard.validate !== 'function') throw invalid();
    return matcher('schemaMatching', ['SchemaMatching', 'SchemaNotMatching'], sample, inverse, function match(v) {
      const result = apply(standard.validate, standard, [v]);
      if (isPromise(result)) throw new TypeError('Async schema validation is not supported in asymmetric matchers.');
      const issues = result.issues;
      return not(this, !issues || issues.length === 0);
    });
  }
  function toBeOneOf(sample, inverse) {
    return matcher('toBeOneOf', ['ToBeOneOf', 'NotToBeOneOf'], sample, inverse, function match(v, eq) {
      if (isArray(sample)) {
        if (sample.length === 0) return not(this, true);
        for (let i = 0; i < sample.length; i++) if (eq(sample[i], v)) return not(this, true);
        return not(this, false);
      }
      if (!isSet(sample)) throw new TypeError('You must provide an array or set to toBeOneOf');
      if (sample.size === 0 || apply(setHas, sample, [v])) return not(this, true);
      return not(this, setMembers(sample).some((x) => eq(x, v)));
    });
  }
  function toSatisfy(predicate, inverse) {
    return matcher('toSatisfy', ['ToSatisfy', 'NotToSatisfy'], predicate, inverse, function match(v) {
      if (typeof predicate !== 'function') throw new TypeError('toSatisfy expects a predicate function');
      return not(this, !!apply(predicate, undefined, [v]));
    });
  }
  // benchmark results compare on latency.mean, beyond an optional relative delta
  function benchmark(faster) {
    const names = faster ? ['ToBeFasterThan', 'NotToBeFasterThan'] : ['ToBeSlowerThan', 'NotToBeSlowerThan'];
    const meanOf = (v) => (isObject(v) && isObject(v.latency) ? numberOf(v.latency.mean) : undefined);
    return (sample, options, inverse) => {
      let delta = 0;
      if (isObject(options) && options.delta !== undefined) delta = Number(options.delta);
      return matcher(names[0], names, sample, inverse, function match(v) {
        const actual = meanOf(v), expected = meanOf(sample);
        const notResult = (side) => new TypeError(`${this.name} expects the ${side} value to be a benchmark result.`);
        if (actual === undefined) throw notResult('actual');
        if (expected === undefined) throw notResult('expected');
        return not(this, faster ? actual < expected * (1 - delta) : actual > expected * (1 + delta));
      });
    };
  }
  const toBeFasterThan = benchmark(true), toBeSlowerThan = benchmark(false);
  // spec.match is called as a method so the matchers above can read their own name and polarity
  function asymMatch(m, other, eq) {
    const spec = AsymmetricMatcher.spec(m);
    return apply(spec.match, spec, [other, eq]);
  }

  // ---------------- equality ----------------
  // pairs.head lists the (actual, expected) pairs under comparison, so a cycle reads as equal. A Set or Map member
  // is tried on a copy, so a rejected candidate leaves no pair behind to pass a later comparison.
  const newPairs = (from) => ({ head: from ? from.head : null });
  function seen(pairs, a, b) {
    for (let c = pairs.head; c !== null; c = c.next) if (c.a === a && c.b === b) return true;
    pairs.head = { a, b, next: pairs.head };
    return false;
  }
  function tryPairs(pairs, compare) {
    const trial = newPairs(pairs);
    if (!compare(trial)) return false;
    pairs.head = trial.head;
    return true;
  }
  const looseEquals = (a, b) => deepEqual(a, b, false, newPairs());
  const strictEquals = (a, b) => deepEqual(a, b, true, newPairs());
  const isUndefinedHole = (o, i) => !(i in o);
  function setMembers(s) { const out = []; apply(setForEach, s, [(x) => append(out, x)]); return out; }
  function mapEntries(m) { const out = []; apply(mapForEach, m, [(v, k) => append(out, [k, v])]); return out; }

  function deepEqual(a, b, strict, pairs) {
    const am = isAsym(a), bm = isAsym(b);
    const eq = strict ? strictEquals : looseEquals;
    if (am && bm) return AsymmetricMatcher.spec(a).equivalent(AsymmetricMatcher.spec(b), eq);
    if (am) return asymMatch(a, b, eq);
    if (bm) return asymMatch(b, a, eq);
    // Object.is for leaves: NaN equals NaN, +0 and -0 differ
    if (Object.is(a, b)) return true;
    // distinct functions are never equal, or every schema validator would match every other
    if (typeof a === 'function' || typeof b === 'function') return false;
    if (!isObject(a) || !isObject(b)) return false;
    // different kinds of container never compare equal, nor typed arrays of different kinds
    const aArray = isArray(a), aSet = isSet(a), aMap = isMap(a), aKind = typedKind(a);
    if (aArray !== isArray(b) || aSet !== isSet(b) || aMap !== isMap(b) || aKind !== typedKind(b)) return false;
    if (aArray) {
      // length first: [1] never equals [1, undefined]; loosely a hole reads as undefined, strictly only as a hole
      if (a.length !== b.length) return false;
      if (seen(pairs, a, b)) return true;
      for (let i = 0; i < a.length; i++) {
        const aHole = isUndefinedHole(a, i), bHole = isUndefinedHole(b, i);
        if (strict && (aHole || bHole)) {
          if (aHole !== bHole) return false;
          continue;
        }
        if (!deepEqual(aHole ? undefined : a[i], bHole ? undefined : b[i], strict, pairs)) return false;
      }
      return true;
    }
    // Sets and Maps ignore order; every actual member must equal some expected member, which may match several
    if (aSet) {
      if (a.size !== b.size) return false;
      if (seen(pairs, a, b)) return true;
      const xs = setMembers(a), ys = setMembers(b);
      if (xs.length !== ys.length) return false;
      for (const x of xs) if (!ys.some((y) => tryPairs(pairs, (p) => deepEqual(x, y, strict, p)))) return false;
      return true;
    }
    if (aMap) {
      if (a.size !== b.size) return false;
      if (seen(pairs, a, b)) return true;
      const xs = mapEntries(a), ys = mapEntries(b);
      if (xs.length !== ys.length) return false;
      const entryEqual = (x, y, p) => deepEqual(x[0], y[0], strict, p) && deepEqual(x[1], y[1], strict, p);
      for (const x of xs) if (!ys.some((y) => tryPairs(pairs, (p) => entryEqual(x, y, p)))) return false;
      return true;
    }
    // an error is never equal to a plain object, whose visible keys would otherwise match any error's
    const aError = isError(a), bError = isError(b);
    if (aError !== bError) return false;
    if (strict && a.constructor !== b.constructor) return false;
    const both = aError && bError;
    // an error's stack is incidental
    const comparable = (keys) => (both ? keys.filter((k) => k !== 'stack') : keys);
    const aKeys = comparable(keysOf(a)), bKeys = comparable(keysOf(b));
    if (strict && aKeys.length !== bKeys.length) return false;
    if (seen(pairs, a, b)) return true;
    if (both) {
      // name and message compare as values, never stringified; cause and errors only when the expected side has them
      if (!deepEqual(a.name, b.name, strict, pairs) || !deepEqual(a.message, b.message, strict, pairs)) return false;
      if (hasOwn.call(b, 'cause') && b.cause !== undefined &&
          (!hasOwn.call(a, 'cause') || !deepEqual(a.cause, b.cause, strict, pairs))) return false;
      if (hasOwn.call(b, 'errors') && (!hasOwn.call(a, 'errors') || !deepEqual(a.errors, b.errors, strict, pairs)))
        return false;
    }
    // loosely, a key holding undefined matches a missing one, in either direction
    for (const k of aKeys) {
      if (!hasOwn.call(b, k)) {
        if (strict || a[k] !== undefined) return false;
        continue;
      }
      if (!deepEqual(a[k], b[k], strict, pairs)) return false;
    }
    for (const k of bKeys) if (!hasOwn.call(a, k) && (strict || b[k] !== undefined)) return false;
    return true;
  }

  // toMatchObject: the expected shape describes a subset of the actual value's own keys
  function partialEqual(a, b, pairs) {
    // what a shape cannot describe a subset of compares in full: matchers, errors, Sets, Maps and typed arrays
    // against typed arrays
    if (isAsym(a) || isAsym(b) || isError(b) || isSet(b) || isMap(b) ||
        (typedKind(b) !== undefined && typedKind(a) !== undefined)) return deepEqual(a, b, false, newPairs(pairs));
    if (isArray(a) && isArray(b)) {
      // arrays stay whole: every element is described, each one partially
      if (a.length !== b.length) return false;
      if (seen(pairs, a, b)) return true;
      for (let i = 0; i < a.length; i++)
        if (!partialEqual(isUndefinedHole(a, i) ? undefined : a[i], isUndefinedHole(b, i) ? undefined : b[i], pairs))
          return false;
      return true;
    }
    // an object shape may describe an array, an array shape only an array
    if (isArray(b)) return false;
    if (isObject(a) && isObject(b)) {
      const keys = keysOf(b);
      if (seen(pairs, a, b)) return true;
      for (const k of keys) if (!hasOwn.call(a, k) || !partialEqual(a[k], b[k], pairs)) return false;
      return true;
    }
    return deepEqual(a, b, false, newPairs(pairs));
  }

  // ---------------- toThrow ----------------
  // the text the string and RegExp forms match: a thrown string itself, or an object's string message. A thrown
  // null or undefined matches any text; anything else offers none.
  const ANY_SUBJECT = {}, NO_SUBJECT = {};
  function thrownSubject(v) {
    if (v === null || v === undefined) return ANY_SUBJECT;
    if (isObject(v)) { const m = v.message; return typeof m === 'string' ? m : NO_SUBJECT; }
    return typeof v === 'string' ? v : NO_SUBJECT;
  }
  function thrownMatches(thrown, expected) {
    if (isAsym(expected)) return looseEquals(thrown, expected);
    const regExp = isRegExp(expected);
    if (typeof expected === 'string' || regExp) {
      const subject = thrownSubject(thrown);
      if (subject === ANY_SUBJECT) return true;
      if (subject === NO_SUBJECT) return false;
      if (regExp) return firstMatch(expected, subject);
      // an empty string asserts an empty message, as Vitest compiles it to /^$/
      return expected === '' ? subject === '' : subject.includes(expected);
    }
    // a class matches its subclasses, as instanceof does; an error instance compares as a value
    if (typeof expected === 'function') return thrown instanceof expected;
    return looseEquals(thrown, expected);
  }
  function throwExpectation(v) {
    if (isRegExp(v)) return String(v);
    if (typeof v === 'function') return describeConstructor(v);
    return describeThrown(v);
  }
  // one match from the start of the text, leaving the RegExp's lastIndex alone
  function firstMatch(re, text) {
    return new RegExp(re.source, re.flags.replace(/[gy]/g, '')).test(text);
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
  function passed() { assertionCount++; }
  function failed(message) { assertionCount++; throw new AssertionError(message); }

  // toHaveProperty's path: dotted and bracketed segments, empty ones dropped, or an array of segments
  function pathSegments(path) {
    if (isArray(path)) return path.map((p) => String(p));
    const out = [];
    let cur = '';
    const s = String(path);
    for (let i = 0; i < s.length;) {
      const c = s[i];
      if (c === '.') { if (cur !== '') append(out, cur); cur = ''; i++; } else if (c === '[') {
        if (cur !== '') append(out, cur);
        cur = '';
        let bracket = '';
        i++;
        while (i < s.length && s[i] !== ']') bracket += s[i++];
        if (i < s.length) i++;
        append(out, bracket);
        if (i < s.length && s[i] === '.') i++;
      } else { cur += c; i++; }
    }
    if (cur !== '') append(out, cur);
    return out;
  }
  function resolvePath(obj, segments) {
    let v = obj;
    for (const seg of segments) {
      if (v === null || v === undefined) return { found: false };
      // a primitive exposes its wrapper's members ("a.length" on a string)
      if (!(seg in ObjectCtor(v))) return { found: false };
      v = v[seg];
    }
    return { found: true, value: v };
  }

  const MOCK = Symbol('mock');
  const isMock = (f) => typeof f === 'function' && f[MOCK] === true;
  function positiveInt(n, min) {
    const v = Number(n);
    return Number.isFinite(v) && v >= min && v <= 2147483647 && Number.isInteger(v);
  }
  const argsEqual = (call, args) => call.length === args.length && args.every((x, i) => looseEquals(call[i], x));
  const returned = (r) => isObject(r) && r.type === 'return';

  class Expectation {
    #actual; #not; #rejection;
    constructor(actual, negated, rejection) { this.#actual = actual; this.#not = negated; this.#rejection = rejection; }
    get not() { return new Expectation(this.#actual, true, this.#rejection); }
    get resolves() { return new PromiseExpectation(this.#actual, this.#not, 'resolves'); }
    get rejects() { return new PromiseExpectation(this.#actual, this.#not, 'rejects'); }
    // a matcher's verdict: pass, or fail with the message for its polarity
    #check(ok, message) { if (ok !== this.#not) passed(); else failed(message(this.#not ? ' not' : '')); }
    #mock(name) {
      if (isMock(this.#actual)) return this.#actual.mock;
      failed('Value must be a mock or spy function');
    }

    toBe(...args) {
      requireExactly(args, 1, 'toBe');
      const a = this.#actual, e = args[0];
      this.#check(Object.is(a, e), (n) => `Expected ${show(a)}${n} to be ${show(e)}`);
    }
    toEqual(...args) {
      requireExactly(args, 1, 'toEqual');
      const a = this.#actual, e = args[0];
      this.#check(looseEquals(a, e), (n) => `Expected ${show(a)}${n} to equal ${show(e)}`);
    }
    toStrictEqual(...args) {
      requireExactly(args, 1, 'toStrictEqual');
      const a = this.#actual, e = args[0];
      this.#check(strictEquals(a, e), (n) => `Expected ${show(a)}${n} to strictly equal ${show(e)}`);
    }
    toContainEqual(...args) {
      requireExactly(args, 1, 'toContainEqual');
      const a = this.#actual, e = args[0];
      if (!isArray(a) && !isSet(a)) {
        if (this.#not) return passed();
        return failed('Expected an array or a Set but received ' + show(a));
      }
      const items = isSet(a) ? setMembers(a) : a;
      let found = false;
      for (let i = 0; i < items.length && !found; i++) found = looseEquals(items[i], e);
      this.#check(found, (n) => `Expected ${show(a)}${n} to contain equal ${show(e)}`);
    }
    toMatchObject(...args) {
      requireExactly(args, 1, 'toMatchObject');
      const a = this.#actual, e = args[0];
      if (!isObject(a)) return this.#not ? passed() : failed('Expected an object but received ' + show(a));
      if (!isObject(e)) return this.#not ? passed() : failed('Expected a match object but received ' + show(e));
      this.#check(partialEqual(a, e, newPairs()), (n) => `Expected ${show(a)}${n} to match object ${show(e)}`);
    }
    toMatch(...args) {
      requireExactly(args, 1, 'toMatch');
      const a = this.#actual, e = args[0];
      if (typeof a !== 'string') return this.#not ? passed() : failed('Expected a string but received ' + show(a));
      const regExp = isRegExp(e);
      if (typeof e !== 'string' && !regExp)
        return this.#not ? passed() : failed('Expected a string pattern or RegExp but received ' + show(e));
      const description = regExp ? String(e) : e;
      this.#check(regExp ? firstMatch(e, a) : a.includes(e), (n) => `Expected ${show(a)}${n} to match ${description}`);
    }
    toBeNull() { const a = this.#actual; this.#check(a === null, (n) => `Expected ${show(a)}${n} to be null`); }
    toBeUndefined() {
      const a = this.#actual;
      this.#check(a === undefined, (n) => `Expected ${show(a)}${n} to be undefined`);
    }
    toBeDefined() {
      const a = this.#actual;
      this.#check(a !== undefined, (n) => `Expected ${show(a)}${n} to be defined`);
    }
    toBeTruthy() { const a = this.#actual; this.#check(!!a, (n) => `Expected ${show(a)}${n} to be truthy`); }
    toBeFalsy() { const a = this.#actual; this.#check(!a, (n) => `Expected ${show(a)}${n} to be falsy`); }
    // NaN after ToNumber, so a non-numeric string is NaN too
    toBeNaN() {
      const a = this.#actual;
      let nan = false;
      try { nan = Number.isNaN(Number(a)); } catch {}
      this.#check(nan, (n) => `Expected ${show(a)}${n} to be NaN`);
    }
    toBeGreaterThan(...args) { this.#compare(args, 'toBeGreaterThan', 'greater than', (a, e) => a > e); }
    toBeGreaterThanOrEqual(...args) {
      this.#compare(args, 'toBeGreaterThanOrEqual', 'greater than or equal to', (a, e) => a >= e);
    }
    toBeLessThan(...args) { this.#compare(args, 'toBeLessThan', 'less than', (a, e) => a < e); }
    toBeLessThanOrEqual(...args) {
      this.#compare(args, 'toBeLessThanOrEqual', 'less than or equal to', (a, e) => a <= e);
    }
    #compare(args, name, words, op) {
      requireExactly(args, 1, name);
      const a = this.#actual, e = args[0];
      this.#check(op(a, e), (n) => `Expected ${show(a)}${n} to be ${words} ${show(e)}`);
    }
    toContain(...args) {
      requireExactly(args, 1, 'toContain');
      const a = this.#actual, e = args[0];
      let found = false;
      if (typeof a === 'string') found = a.includes(typeof e === 'string' ? e : show(e));
      else if (isArray(a)) found = Array.prototype.includes.call(a, e);
      else if (isSet(a)) found = apply(setHas, a, [e]);
      this.#check(found, (n) => `Expected ${show(a)}${n} to contain ${show(e)}`);
    }
    toBeInstanceOf(...args) {
      requireExactly(args, 1, 'toBeInstanceOf');
      const a = this.#actual, C = args[0];
      let ok = false;
      // the wrapper constructors match primitives, Object any object, Function anything callable
      if (C === FunctionCtor) ok = typeof a === 'function';
      else if (C === Array) ok = isArray(a);
      else if (C === ObjectCtor) ok = isObject(a);
      else if (C === StringCtor) ok = typeof a === 'string';
      else if (C === NumberCtor) ok = typeof a === 'number';
      else if (C === BooleanCtor) ok = typeof a === 'boolean';
      else if (typeof C === 'function' && isObject(a)) ok = a instanceof C;
      this.#check(ok, (n) => `Expected ${describeInstance(a)}${n} to be an instance of ${describeConstructor(C)}`);
    }
    toHaveLength(...args) {
      requireExactly(args, 1, 'toHaveLength');
      const a = this.#actual, e = args[0];
      const n = Number(e);
      let ok;
      // a Set and a Map report their entry count as size rather than length
      if (isArray(a)) ok = a.length === n;
      else if (isSet(a) || isMap(a)) ok = a.size === n;
      else if (isObject(a)) ok = Object.getOwnPropertyNames(a).length === n;
      else if (typeof a === 'string') ok = a.length === n;
      else ok = Number(a) === n;
      this.#check(ok, (m) => `Expected ${show(a)}${m} to have length ${show(e)}`);
    }
    toHaveProperty(...args) {
      if (args.length < 1 || args.length > 2)
        throw new Error(`toHaveProperty expects between 1 and 2 arguments but got ${args.length}`);
      const a = this.#actual, path = args[0];
      if (!isObject(a)) return this.#not ? passed() : failed('Expected an object but received ' + show(a));
      if (typeof path !== 'string' && typeof path !== 'number' && !isArray(path))
        throw new TypeError('toHaveProperty expects a string or number path, or an array of path segments');
      const segments = pathSegments(path);
      let description = segments.join('.');
      let r;
      // a string or number path is first tried as one own key, so { 'a.b': 5 } has property 'a.b'
      if (!isArray(path) && hasOwn.call(a, String(path))) {
        r = { found: true, value: a[String(path)] };
        description = String(path);
      } else r = segments.length === 0 ? { found: false } : resolvePath(a, segments);
      const expectsValue = args.length === 2, e = args[1];
      const ok = r.found && (!expectsValue || looseEquals(r.value, e));
      this.#check(ok, (n) => {
        if (!expectsValue) return `Expected ${show(a)}${n} to have property ${description}`;
        if (n) return `Expected ${show(a)} not to have property ${description} with value ${show(e)}`;
        return `Expected ${show(a)} to have property ${description} with value ${show(e)} but received ` +
          show(r.value);
      });
    }
    toThrow(...args) {
      // toThrow(undefined) is the no-argument form
      const hasExpectation = args.length > 0 && args[0] !== undefined, e = args[0];
      if (hasExpectation && typeof e !== 'string' && !isObject(e))
        throw new TypeError('toThrow expects a string, RegExp, error class, or Error instance');
      let threw = false, thrown;
      if (this.#rejection) { threw = true; thrown = this.#actual; } else {
        if (typeof this.#actual !== 'function') throw new TypeError('toThrow expects actual value to be a function');
        try { apply(this.#actual, undefined, []); } catch (x) { threw = true; thrown = x; }
      }
      const ok = threw && (!hasExpectation || thrownMatches(thrown, e));
      if (ok !== this.#not) return passed();
      const subject = this.#rejection ? 'the rejected promise' : 'the function';
      const what = hasExpectation ? ' to throw ' + throwExpectation(e) : ' to throw an exception';
      if (this.#not) failed(`Expected ${subject} not${what} but it threw: ${describeThrown(thrown)}`);
      if (threw) failed(`Expected ${subject}${what} but it threw: ${describeThrown(thrown)}`);
      failed(`Expected ${subject}${what} but it did not throw`);
    }
    toBeCloseTo(...args) {
      if (args.length < 1) throw new Error('toBeCloseTo expects at least 1 argument but got 0');
      const a = this.#actual, e = args[0];
      let precision = args.length >= 2 ? Number(args[1]) : 2;
      if (Number.isNaN(precision)) precision = 0;
      const x = Number(a), y = Number(e);
      let ok;
      if (Number.isNaN(x) || Number.isNaN(y)) ok = Number.isNaN(x) && Number.isNaN(y);
      else if (!Number.isFinite(x) || !Number.isFinite(y)) ok = x === y;
      else {
        let tolerance;
        if (!Number.isFinite(precision)) tolerance = precision > 0 ? 0 : Infinity;
        else if (precision > 308) tolerance = 0;
        else if (precision < -308) tolerance = Infinity;
        else tolerance = 0.5 * Math.pow(10, -precision);
        ok = tolerance === Infinity || Math.abs(x - y) < tolerance;
      }
      this.#check(ok, (n) => `Expected ${show(a)}${n} to be close to ${show(e)} (precision: ${precision})`);
    }

    // ---- mocks ----
    toHaveBeenCalled(...args) {
      requireExactly(args, 0, 'toHaveBeenCalled');
      const calls = this.#mock().calls.length;
      if ((calls > 0) !== this.#not) return passed();
      failed(this.#not ? `Expected mock not to have been called but it was called ${calls} time(s)`
        : 'Expected mock to have been called but it was not called');
    }
    toHaveBeenCalledOnce(...args) {
      requireExactly(args, 0, 'toHaveBeenCalledOnce');
      const calls = this.#mock().calls.length;
      if ((calls === 1) !== this.#not) return passed();
      failed(this.#not ? 'Expected mock not to have been called exactly once'
        : `Expected mock to have been called exactly once but was called ${calls} time(s)`);
    }
    toHaveBeenCalledTimes(...args) {
      const state = this.#mock();
      requireExactly(args, 1, 'toHaveBeenCalledTimes');
      if (!positiveInt(args[0], 0)) throw new TypeError('toHaveBeenCalledTimes expects a non-negative integer');
      const n = Number(args[0]), calls = state.calls.length;
      if ((calls === n) !== this.#not) return passed();
      failed(this.#not ? `Expected mock not to have been called ${n} time(s)`
        : `Expected mock to have been called ${n} time(s) but was called ${calls} time(s)`);
    }
    toHaveBeenCalledWith(...args) {
      const calls = this.#mock().calls;
      const found = calls.some((c) => isArray(c) && argsEqual(c, args));
      if (found !== this.#not) return passed();
      failed(this.#not ? 'Expected mock not to have been called with the specified arguments'
        : `Expected mock to have been called with the specified arguments (${calls.length} call(s) recorded)`);
    }
    toHaveBeenLastCalledWith(...args) {
      const calls = this.#mock().calls;
      if (calls.length === 0)
        return this.#not ? passed() : failed('Expected mock to have been called but it was not called');
      if (argsEqual(calls[calls.length - 1], args) !== this.#not) return passed();
      failed(this.#not ? 'Expected mock not to have been last called with the specified arguments'
        : 'Expected mock to have been last called with the specified arguments');
    }
    toHaveBeenNthCalledWith(...args) {
      const calls = this.#mock().calls;
      if (args.length < 1) throw new TypeError('toHaveBeenNthCalledWith requires at least 1 argument (call index)');
      if (!positiveInt(args[0], 1)) throw new TypeError('toHaveBeenNthCalledWith expects a positive integer index');
      const n = Number(args[0]);
      if (n > calls.length) {
        if (this.#not) return passed();
        failed(`Call index ${n} is out of range (mock was called ${calls.length} time(s))`);
      }
      if (argsEqual(calls[n - 1], args.slice(1)) !== this.#not) return passed();
      failed(this.#not ? `Expected call ${n} not to have the specified arguments`
        : `Expected call ${n} to have the specified arguments`);
    }
    toHaveReturned(...args) {
      requireExactly(args, 0, 'toHaveReturned');
      const ok = this.#mock().results.some(returned);
      if (ok !== this.#not) return passed();
      failed(this.#not ? 'Expected mock not to have returned' : 'Expected mock to have returned at least once');
    }
    toHaveReturnedTimes(...args) {
      const state = this.#mock();
      requireExactly(args, 1, 'toHaveReturnedTimes');
      if (!positiveInt(args[0], 0)) throw new TypeError('toHaveReturnedTimes expects a non-negative integer');
      const n = Number(args[0]), count = state.results.filter(returned).length;
      if ((count === n) !== this.#not) return passed();
      failed(this.#not ? `Expected mock not to have returned ${n} time(s)`
        : `Expected mock to have returned ${n} time(s) but returned ${count} time(s)`);
    }
    toHaveReturnedWith(...args) {
      const state = this.#mock();
      requireExactly(args, 1, 'toHaveReturnedWith');
      const e = args[0];
      const ok = state.results.some((r) => returned(r) && looseEquals(r.value, e));
      if (ok !== this.#not) return passed();
      failed(`Expected mock${this.#not ? ' not' : ''} to have returned with ${show(e)}`);
    }
    toHaveLastReturnedWith(...args) {
      const results = this.#mock().results;
      requireExactly(args, 1, 'toHaveLastReturnedWith');
      const e = args[0];
      if (results.length === 0)
        return this.#not ? passed() : failed('Expected mock to have returned but it was never called');
      const last = results[results.length - 1];
      if ((returned(last) && looseEquals(last.value, e)) !== this.#not) return passed();
      failed(`Expected mock${this.#not ? ' not' : ''} to have last returned with ${show(e)}`);
    }
    toHaveNthReturnedWith(...args) {
      const results = this.#mock().results;
      requireExactly(args, 2, 'toHaveNthReturnedWith');
      if (!positiveInt(args[0], 1)) throw new TypeError('toHaveNthReturnedWith expects a positive integer index');
      const n = Number(args[0]), e = args[1];
      if (n > results.length) {
        if (this.#not) return passed();
        failed(`Return index ${n} is out of range (mock returned ${results.length} time(s))`);
      }
      const r = results[n - 1];
      if ((returned(r) && looseEquals(r.value, e)) !== this.#not) return passed();
      failed(`Expected return ${n}${this.#not ? ' not' : ''} to be ${show(e)}`);
    }

    // Vitest's matchers beyond GocciaScript's own set
    toBeOneOf(...args) {
      requireExactly(args, 1, 'toBeOneOf');
      const a = this.#actual, list = args[0];
      this.#check(asymMatch(toBeOneOf(list, false), a, looseEquals),
        (n) => `Expected ${show(a)}${n} to be one of ${show(list)}`);
    }
    toSatisfy(...args) {
      requireExactly(args, 1, 'toSatisfy');
      const a = this.#actual;
      this.#check(asymMatch(toSatisfy(args[0], false), a, looseEquals),
        (n) => `Expected ${show(a)}${n} to satisfy the predicate`);
    }

    // custom matchers (expect.extend) run with the actual value and the polarity
    static custom(name, fn) {
      Expectation.prototype[name] = function (...args) {
        const r = apply(fn, { isNot: this.#not, equals: looseEquals }, [this.#actual, ...args]);
        this.#check(!!r.pass, () => (typeof r.message === 'function' ? r.message() : String(r.message || name)));
      };
      PromiseExpectation.forward(name);
    }
  }
  for (const [alias, name] of [['toBeCalled', 'toHaveBeenCalled'], ['toBeCalledTimes', 'toHaveBeenCalledTimes'],
    ['toBeCalledWith', 'toHaveBeenCalledWith'], ['toThrowError', 'toThrow']])
    Expectation.prototype[alias] = Expectation.prototype[name];

  // expect(promise).resolves / .rejects: each matcher waits for the outcome, then runs on the value or the reason
  class PromiseExpectation {
    #actual; #not; #mode;
    constructor(actual, negated, mode) { this.#actual = actual; this.#not = negated; this.#mode = mode; }
    get not() { return new PromiseExpectation(this.#actual, true, this.#mode); }
    static run(pe, name, args) {
      const actual = pe.#actual, negated = pe.#not, mode = pe.#mode;
      if (!isPromise(actual)) {
        assertionCount++;
        return Promise.reject(new AssertionError('Expected a Promise but received ' + show(actual)));
      }
      return apply(promiseThen, actual, [
        (v) => {
          if (mode === 'rejects') failed('Expected Promise to reject but it resolved with: ' + show(v));
          return new Expectation(v, negated, false)[name](...args);
        },
        (e) => {
          if (mode === 'resolves') failed('Expected Promise to resolve but it rejected with: ' + show(e));
          return new Expectation(e, negated, true)[name](...args);
        }]);
    }
    static forward(name) {
      PromiseExpectation.prototype[name] = function (...args) { return PromiseExpectation.run(this, name, args); };
    }
  }
  for (const name of Object.getOwnPropertyNames(Expectation.prototype))
    if (name !== 'constructor' && name !== 'not' && name !== 'resolves' && name !== 'rejects')
      PromiseExpectation.forward(name);

  function expect(...args) {
    requireExactly(args, 1, 'expect');
    return new Expectation(args[0], false, false);
  }
  expect.anything = () => anything();
  expect.any = any;
  expect.arrayContaining = (s) => arrayContaining(s, false);
  expect.objectContaining = (s) => objectContaining(s, false);
  expect.stringContaining = (s) => stringContaining(s, false);
  expect.stringMatching = (s) => stringMatching(s, false);
  expect.closeTo = (s, p) => closeTo(s, p, false);
  expect.schemaMatching = (s) => schemaMatching(s, false);
  expect.toBeOneOf = (s) => toBeOneOf(s, false);
  expect.toSatisfy = (s) => toSatisfy(s, false);
  expect.toBeFasterThan = (s, o) => toBeFasterThan(s, o, false);
  expect.toBeSlowerThan = (s, o) => toBeSlowerThan(s, o, false);
  // expect.not holds the inverse matchers; there is no inverse of any or anything
  Object.defineProperty(expect, 'not', {
    value: {
      arrayContaining: (s) => arrayContaining(s, true),
      objectContaining: (s) => objectContaining(s, true),
      stringContaining: (s) => stringContaining(s, true),
      stringMatching: (s) => stringMatching(s, true),
      closeTo: (s, p) => closeTo(s, p, true),
      schemaMatching: (s) => schemaMatching(s, true),
      toBeOneOf: (s) => toBeOneOf(s, true),
      toSatisfy: (s) => toSatisfy(s, true),
      toBeFasterThan: (s, o) => toBeFasterThan(s, o, true),
      toBeSlowerThan: (s, o) => toBeSlowerThan(s, o, true),
    },
    configurable: true,
  });
  expect.assertions = (n) => { expectedAssertions = n; };
  expect.hasAssertions = () => { needAssertions = true; };
  expect.extend = (obj) => { for (const k of keysOf(obj)) Expectation.custom(k, obj[k]); };

  // ---------------- mocks ----------------
  function mock(...args) {
    if (args.length > 0 && typeof args[0] !== 'function')
      throw new TypeError('mock() expects a function argument or no arguments');
    const impl = args[0];
    const state = { calls: [], results: [], instances: [], contexts: [], lastCall: undefined };
    let implementation = impl;
    const once = [];
    let name = 'mock';
    const f = function (...args) {
      append(state.calls, args);
      state.lastCall = args;
      append(state.contexts, this);
      if (new.target) append(state.instances, this);
      const result = { type: 'incomplete', value: undefined };
      append(state.results, result);
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
    f.mockClear = () => {
      state.calls.length = 0; state.results.length = 0; state.instances.length = 0; state.contexts.length = 0;
      state.lastCall = undefined;
      return f;
    };
    // mockReset reinstates the implementation the mock was made with (a spy's the method it replaced)
    f.mockReset = () => { f.mockClear(); implementation = impl; once.length = 0; return f; };
    f.mockRestore = () => { f.mockReset(); if (f.__restore) f.__restore(); return f; };
    f.getMockImplementation = () => implementation;
    f.mockName = (n) => { name = n; return f; };
    f.getMockName = () => name;
    return f;
  }
  function spyOn(...args) {
    if (args.length < 2) requireExactly(args, 2, 'spyOn');
    const [obj, key, access] = args;
    if (!isObject(obj)) throw new TypeError('spyOn expects first argument to be an object');
    if (typeof key !== 'string') throw new TypeError('spyOn expects second argument to be a string (method name)');
    if (!(key in obj)) throw new TypeError(`spyOn: cannot spy on non-existent property "${key}"`);
    const desc = getOwnPropertyDescriptor(obj, key);
    if (access === 'get' || access === 'set') {
      const d = desc || getOwnPropertyDescriptor(Object.getPrototypeOf(obj), key);
      const orig = d && d[access];
      const m = orig === undefined ? mock() : mock(orig);
      Object.defineProperty(obj, key, Object.assign({}, d, { [access]: m, configurable: true }));
      m.__restore = () => { if (desc) Object.defineProperty(obj, key, desc); else delete obj[key]; };
      return m;
    }
    const orig = obj[key];
    if (typeof orig !== 'function') throw new TypeError(`spyOn: property "${key}" is not a function`);
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
    const result = {
      totalTests: 0, passed: 0, failed: 0, skipped: 0, totalRunTests: 0, assertions: 0, failedTests: [],
    };
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
          if (timerError !== null)
            throw new Error('Uncaught exception in a timer callback: ' + thrownMessage(timerError.error));
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
          const name = `Test "${item.name}" in suite "${fullName(suite)}"`;
          result.failedTests.push({ name, error: thrownMessage(error) });
        } else result.passed++;
      }
      try { if (!skipAll) await runHooks(suite.afterAll); }
      catch (e) {
        result.failed++;
        result.failedTests.push({ name: `afterAll in ${fullName(suite) || 'root'}`, error: thrownMessage(e) });
      }
      return runnable;
    }
    await runSuite(root, false);
    return result;
  }

  const api = {
    describe, test, it: test, expect, beforeAll, afterAll, beforeEach, afterEach, onTestFinished, mock, spyOn, runTests,
  };
  for (const k of keysOf(api)) Object.defineProperty(global, k, { value: api[k], writable: true, configurable: true });
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
