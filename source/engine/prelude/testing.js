// The test framework API (docs/testing-api.md): describe / test / expect, hooks, mocks and runTests, as the
// GocciaTestRunner injects them. Written in JavaScript so it runs on any engine. The matchers, the asymmetric matchers
// and their equality follow GocciaScript's own testing library (Goccia.Builtins.TestingLibrary,
// Goccia.Builtins.Testing.DeepEquality, Goccia.Values.AsymmetricMatcher), messages included, so a suite reports the
// same results on both engines.
// Goccia.HostAPI calls this function with the global object and keeps the API it returns, whose report(done) runs the
// registered suites and passes done their result as JSON, or undefined and the error that stopped them.
(function (global, shared) {
  'use strict';
  // the intrinsics as the realm made them: a script's top-level declarations can replace these globals later
  // (function Object() {} in compat-function code) while the library keeps working
  const { Object, Array, Error, TypeError, RangeError, SyntaxError, String, Number, Boolean, Symbol, BigInt, RegExp,
    Promise, Set, Map, Function, Proxy, Uint8Array } = global;
  // the namespaces' members as the realm made them (a script may replace Math.trunc or JSON.stringify later), and
  // each prototype method the library calls as a function of its receiver
  const snapshotOf = (ns) => {
    const o = Object.create(null), keys = global.Reflect.ownKeys(ns);
    for (let i = 0; i < keys.length; i++) {
      const d = Object.getOwnPropertyDescriptor(ns, keys[i]);
      if ('value' in d) o[keys[i]] = d.value;
    }
    return Object.freeze(o);
  };
  const Math = snapshotOf(global.Math), JSON = snapshotOf(global.JSON), Reflect = snapshotOf(global.Reflect);
  const ObjectDefineProperty = Object.defineProperty, ObjectIs = Object.is, ObjectGetPrototypeOf = Object.getPrototypeOf;
  const ObjectGetOwnPropertyNames = Object.getOwnPropertyNames, ObjectAssign = Object.assign;
  const NumberIsNaN = Number.isNaN, NumberIsFinite = Number.isFinite, NumberIsInteger = Number.isInteger;
  const promiseResolve = Promise.resolve, promiseReject = Promise.reject;
  const PromiseResolve = (v) => Reflect.apply(promiseResolve, Promise, [v]);
  const PromiseReject = (e) => Reflect.apply(promiseReject, Promise, [e]);
  const uncurryThis = Function.prototype.bind.bind(Function.prototype.call);
  const StringPrototypeSplit = uncurryThis(String.prototype.split), StringPrototypeTrim = uncurryThis(String.prototype.trim);
  const StringPrototypeStartsWith = uncurryThis(String.prototype.startsWith);
  const StringPrototypeIncludes = uncurryThis(String.prototype.includes), StringPrototypeSlice = uncurryThis(String.prototype.slice);
  const ArrayPrototypeJoin = uncurryThis(Array.prototype.join), ArrayPrototypeIncludes = uncurryThis(Array.prototype.includes);
  const ArrayPrototypeSort = uncurryThis(Array.prototype.sort), ArrayPrototypeShift = uncurryThis(Array.prototype.shift);
  const ArrayPrototypeReverse = uncurryThis(Array.prototype.reverse);
  const RegExpPrototypeExec = uncurryThis(RegExp.prototype.exec);
  const FunctionPrototypeToString = uncurryThis(Function.prototype.toString);
  const ObjectPrototypeHasOwnProperty = uncurryThis(Object.prototype.hasOwnProperty);
  const mapSize = uncurryThis(Object.getOwnPropertyDescriptor(Map.prototype, 'size').get);
  const setSize = uncurryThis(Object.getOwnPropertyDescriptor(Set.prototype, 'size').get);
  // a Set's or Map's size through its internal slot
  const sizeOf = (v) => (isSet(v) ? setSize(v) : mapSize(v));
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
  const typedArrayTag = getOwnPropertyDescriptor(ObjectGetPrototypeOf(Int8Array.prototype), Symbol.toStringTag).get;
  const typedKind = (v) => apply(typedArrayTag, v, []);
  const isError = typeof Error.isError === 'function' ? Error.isError : (v) => v instanceof Error;
  const setForEach = Set.prototype.forEach, setHas = Set.prototype.has, mapForEach = Map.prototype.forEach;
  const promiseThen = Promise.prototype.then;
  // a promise: %Promise.prototype% on its chain (instanceof would call a Symbol.hasInstance a script defined)
  const isPromise = (v) => { if (!isObject(v)) return false; for (let p = ObjectGetPrototypeOf(v); p !== null; p = ObjectGetPrototypeOf(p)) if (p === Promise.prototype) return true; return false; };
  const StringCtor = String, NumberCtor = Number, BooleanCtor = Boolean, BigIntCtor = BigInt, SymbolCtor = Symbol;
  const FunctionCtor = Function, ObjectCtor = Object;
  // the primitive inside a string or number, boxed or not, or undefined
  const unbox = (proto, type) => {
    const valueOf = proto.valueOf;
    return (v) => {
    if (typeof v === type) return v;
    if (isObject(v)) { try { return apply(valueOf, v, []); } catch {} }
    return undefined;
    };
  };
  const stringOf = unbox(String.prototype, 'string');
  const numberOf = unbox(Number.prototype, 'number');
  // appends without [[Set]], so an index setter or read-only index a test installs on Array.prototype or
  // Object.prototype never sees the library's own bookkeeping
  const append = (list, v) => {
    ObjectDefineProperty(list, list.length, { __proto__: null, value: v, writable: true, enumerable: true, configurable: true });
  };
  // list operations by index: Array.prototype's map, filter and spreading would look up a constructor, Symbol.species
  // or the array iterator
  const copyList = (list) => { const r = []; for (let i = 0; i < list.length; i++) append(r, list[i]); return r; };
  const concatLists = (a, b) => { const r = copyList(a); for (let i = 0; i < b.length; i++) append(r, b[i]); return r; };
  const mapList = (list, f) => { const r = []; for (let i = 0; i < list.length; i++) append(r, f(list[i], i)); return r; };
  const filterList = (list, f) => { const r = []; for (let i = 0; i < list.length; i++) if (f(list[i], i)) append(r, list[i]); return r; };
  const someList = (list, f) => { for (let i = 0; i < list.length; i++) if (f(list[i], i)) return true; return false; };
  const everyList = (list, f) => { for (let i = 0; i < list.length; i++) if (!f(list[i], i)) return false; return true; };
  // s with each match of the global pattern re replaced by f(match), by RegExpBuiltinExec (replace would look up the
  // pattern's Symbol.replace, flags and exec)
  const replaceMatches = (s, re, f) => {
    let out = '', last = 0, m;
    re.lastIndex = 0;
    while ((m = RegExpPrototypeExec(re, s)) !== null) {
      out += StringPrototypeSlice(s, last, m.index) + f(m);
      last = m.index + m[0].length;
      if (m[0].length === 0) re.lastIndex++;
    }
    return out + StringPrototypeSlice(s, last);
  };
  // line breaks as LF (a CRLF or a lone CR)
  const toLF = (s) => {
    if (!StringPrototypeIncludes(s, '\r')) return s;
    let out = '';
    for (let i = 0; i < s.length; i++) { if (s[i] !== '\r') out += s[i]; else { out += '\n'; if (s[i + 1] === '\n') i++; } }
    return out;
  };

  // ---------------- suites and tests ----------------
  function newSuite(name, parent, mode) {
    return { name, parent, mode, children: [], beforeAll: [], afterAll: [], beforeEach: [], afterEach: [] };
  }
  const root = newSuite('', null, 'run');
  let collectionError = null;
  // the host work each unit leaves (Goccia.HostAPI's work natives): rejections with no handler, queued jobs that threw
  const work = shared && shared.work;
  const describeRejections = [];
  let current = root;
  let hasOnly = false;

  function fullName(suite) {
    const parts = [];
    for (let s = suite; s && s !== root; s = s.parent) append(parts, s.name);
    ArrayPrototypeReverse(parts);
    return ArrayPrototypeJoin(parts, ' > ');
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
    append(current.children, { kind: 'suite', suite: s });
    if (mode === 'only') hasOnly = true;
    const saved = current;
    current = s;
    // a describe callback that throws aborts collection: nothing of the file runs, and the file fails with one
    // suite error (Vitest discards it whole)
    const mark = work ? work.mark() : 0;
    const describeTimeoutMs = shared && shared.deadline ? shared.deadline.describeTimeoutMs : 0;
    try {
      // --describe-timeout: the callback (the suite's registration) under a deadline of its own
      if (describeTimeoutMs > 0) {
        const r = shared.deadline.callWithin(args[1], describeTimeoutMs);
        if (r[0] === 'timeout' && collectionError === null)
          collectionError = `Describe "${fullName(s)}": describe timed out after ${describeTimeoutMs}ms`;
        if (r[0] === 'threw') throw r[1];
      } else args[1]();
    } catch (e) {
      if (collectionError === null) collectionError = `Describe "${fullName(s)}": ${describeThrown(e)}`;
    } finally {
      current = saved;
      // what a describe body rejected belongs to no test: checked when the run starts
      if (work) {
        const taken = work.take(mark);
        for (let i = 0; i < taken.length; i++) append(describeRejections, taken[i]);
      }
    }
  }
  function addTest(args, name, mode) {
    requireRegistration(args, name);
    append(current.children, { kind: 'test', name: args[0], fn: args[1], mode, suite: current });
    if (mode === 'only') hasOnly = true;
  }

  // %s %d %i %f %j %o %O %% and $name / $a.b interpolation (Vitest's test.each titles)
  function formatTitle(title, args) {
    let i = 0;
    let out = replaceMatches(String(title), /%[sdifjoO%#]/g, (match) => {
      const m = match[0];
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
      out = replaceMatches(out, /\$([A-Za-z_$][\w$]*(?:\.[\w$]+)*)/g, (match) => {
        const m = match[0], path = StringPrototypeSplit(match[1], '.');
        let v = args[0];
        for (let i = 0; i < path.length; i++) { if (v == null) return m; v = v[path[i]]; }
        return typeof v === 'string' ? v : show(v);
      });
    return out;
  }
  function makeEach(name, register) {
    return (...eachArgs) => {
      const table = eachArgs[0], interp = [];
      for (let i = 1; i < eachArgs.length; i++) append(interp, eachArgs[i]);
      const tagged = isArray(table) && isArray(table.raw);
      if (!tagged) requireExactly(eachArgs, 1, name);
      if (!isArray(table)) throw new TypeError(`${name} expects a table array`);
      let rows = table;
      if (tagged) {
        // tagged template: a header row of names, then the values
        const header = filterList(mapList(StringPrototypeSplit(ArrayPrototypeJoin(table, ''), '|'), (s) => StringPrototypeTrim(s)), (s) => s !== '');
        rows = [];
        for (let k = 0; k < interp.length; k += header.length) {
          const row = {};
          for (let j = 0; j < header.length; j++) row[header[j]] = interp[k + j];
          append(rows, row);
        }
      }
      return (...regArgs) => {
        requireRegistration(regArgs, name);
        const title = regArgs[0], fn = regArgs[1];
        for (let index = 0; index < rows.length; index++) {
          const row = rows[index];
          const args = isArray(row) ? row : [row];
          formatTitle.index = index;
          register(formatTitle(title, args), () => apply(fn, undefined, args));
        }
      };
    };
  }

  function describe(...args) { addSuite(args, 'describe', 'run'); }
  describe.skip = (...args) => addSuite(args, 'describe.skip', 'skip');
  describe.only = (...args) => addSuite(args, 'describe.only', 'only');
  describe.todo = (name) => {
    append(current.children, { kind: 'suite', suite: newSuite(String(name), current, 'todo') });
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
    append(current.children, { kind: 'test', name: args[0], fn: null, mode: 'todo', suite: current });
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
      append(current[kind], args[0]);
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
    if (extra) ObjectAssign(spec, extra(spec));
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
    return filterList(getOwnPropertySymbols(o), (s) => apply(propertyIsEnumerable, o, [s]));
  }
  function objectContaining(sample, inverse) {
    const names = ['ObjectContaining', 'ObjectNotContaining'];
    return matcher('objectContaining', names, sample, inverse, function match(v, eq) {
      const spec = this;
      if (sample === null || typeof sample !== 'object')
        throw new TypeError(`You must provide an object to ${spec.name}, not '${typeof sample}'.`);
      const keys = concatLists(keysOf(sample), enumerableSymbols(sample));
      if (keys.length === 0) return not(spec, true);
      if (!v) return not(spec, false);
      // the actual value is boxed, so a string matches { length: 3 }; inherited members count
      const o = ObjectCtor(v);
      for (let i = 0; i < keys.length; i++) if (!(keys[i] in o) || !eq(o[keys[i]], sample[keys[i]])) return not(spec, false);
      return not(spec, true);
    });
  }
  function stringContaining(sample, inverse) {
    const text = stringOf(sample);
    if (text === undefined) throw new Error('Expected is not a string');
    return matcher('stringContaining', ['StringContaining', 'StringNotContaining'], sample, inverse, function match(v) {
      const s = stringOf(v);
      return not(this, s !== undefined && StringPrototypeIncludes(s, text));
    });
  }
  // each stringMatching pattern's source and flags, read when it was made (its own RegExp's getters live on
  // RegExp.prototype, which a script may replace)
  const patternSource = uncurryThis(regExpSource), patternOf = new global.WeakMap();
  const WeakMapPrototypeGet = uncurryThis(global.WeakMap.prototype.get), WeakMapPrototypeSet = uncurryThis(global.WeakMap.prototype.set);
  function stringMatching(sample, inverse) {
    const text = stringOf(sample);
    let re, flags;
    if (text !== undefined) { re = new RegExp(text, ''); flags = ''; }
    else if (isRegExp(sample)) { flags = sample.flags; re = new RegExp(sample.source, flags); }
    else throw new Error('Expected is not a String or a RegExp');
    WeakMapPrototypeSet(patternOf, re, [patternSource(re), flags]);
    return matcher('stringMatching', ['StringMatching', 'StringNotMatching'], re, inverse, function match(v) {
      const s = stringOf(v);
      if (s === undefined) return not(this, false);
      // one match from the start, whatever the global or sticky state
      re.lastIndex = 0;
      return not(this, RegExpPrototypeExec(re, s) !== null);
    }, (spec) => ({
      equivalent: (other) => other.kind === spec.kind && other.inverse === spec.inverse &&
        sameTextPattern(WeakMapPrototypeGet(patternOf, other.sample), WeakMapPrototypeGet(patternOf, re)),
    }));
  }
  const sameTextPattern = (a, b) => a !== undefined && b !== undefined && a[0] === b[0] && a[1] === b[1];
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
      if (NumberIsNaN(actual) || NumberIsNaN(expected) || NumberIsNaN(precision)) return not(this, false);
      if (!NumberIsFinite(actual) && !NumberIsFinite(expected)) {
        if (Math.sign(actual) === Math.sign(expected)) return not(this, true);
      }
      let tolerance;
      if (!NumberIsFinite(precision)) tolerance = precision > 0 ? 0 : Infinity;
      else if (Math.abs(precision) <= 308 && NumberIsInteger(precision)) {
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
          ObjectIs(precision, other.precision);
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
      if (setSize(sample) === 0 || apply(setHas, sample, [v])) return not(this, true);
      return not(this, someList(setMembers(sample), (x) => eq(x, v)));
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
    if (ObjectIs(a, b)) return true;
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
      if (setSize(a) !== setSize(b)) return false;
      if (seen(pairs, a, b)) return true;
      const xs = setMembers(a), ys = setMembers(b);
      if (xs.length !== ys.length) return false;
      for (let i = 0; i < xs.length; i++) { const x = xs[i]; if (!someList(ys, (y) => tryPairs(pairs, (p) => deepEqual(x, y, strict, p)))) return false; }
      return true;
    }
    if (aMap) {
      if (mapSize(a) !== mapSize(b)) return false;
      if (seen(pairs, a, b)) return true;
      const xs = mapEntries(a), ys = mapEntries(b);
      if (xs.length !== ys.length) return false;
      const entryEqual = (x, y, p) => deepEqual(x[0], y[0], strict, p) && deepEqual(x[1], y[1], strict, p);
      for (let i = 0; i < xs.length; i++) { const x = xs[i]; if (!someList(ys, (y) => tryPairs(pairs, (p) => entryEqual(x, y, p)))) return false; }
      return true;
    }
    // an error is never equal to a plain object, whose visible keys would otherwise match any error's
    const aError = isError(a), bError = isError(b);
    if (aError !== bError) return false;
    if (strict && a.constructor !== b.constructor) return false;
    const both = aError && bError;
    // an error's stack is incidental
    const comparable = (keys) => (both ? filterList(keys, (k) => k !== 'stack') : keys);
    const aKeys = comparable(keysOf(a)), bKeys = comparable(keysOf(b));
    if (strict && aKeys.length !== bKeys.length) return false;
    if (seen(pairs, a, b)) return true;
    if (both) {
      // name and message compare as values, never stringified; cause and errors only when the expected side has them
      if (!deepEqual(a.name, b.name, strict, pairs) || !deepEqual(a.message, b.message, strict, pairs)) return false;
      if (ObjectPrototypeHasOwnProperty(b, 'cause') && b.cause !== undefined &&
          (!ObjectPrototypeHasOwnProperty(a, 'cause') || !deepEqual(a.cause, b.cause, strict, pairs))) return false;
      if (ObjectPrototypeHasOwnProperty(b, 'errors') && (!ObjectPrototypeHasOwnProperty(a, 'errors') || !deepEqual(a.errors, b.errors, strict, pairs)))
        return false;
    }
    // loosely, a key holding undefined matches a missing one, in either direction
    for (let i = 0; i < aKeys.length; i++) {
      const k = aKeys[i];
      if (!ObjectPrototypeHasOwnProperty(b, k)) {
        if (strict || a[k] !== undefined) return false;
        continue;
      }
      if (!deepEqual(a[k], b[k], strict, pairs)) return false;
    }
    for (let i = 0; i < bKeys.length; i++) { const k = bKeys[i]; if (!ObjectPrototypeHasOwnProperty(a, k) && (strict || b[k] !== undefined)) return false; }
    return true;
  }

  // toMatchObject: the expected shape describes a subset of the actual value's own keys
  // a snapshot's property shape: inherited keys count too (Goccia's IsSnapshotPartialDeepEqual)
  const snapshotPartialEqual = (a, b) => partialEqual(a, b, newPairs(), true);
  function partialEqual(a, b, pairs, inherited) {
    // what a shape cannot describe a subset of compares in full: matchers, errors, Sets, Maps and typed arrays
    // against typed arrays
    if (isAsym(a) || isAsym(b) || isError(b) || isSet(b) || isMap(b) ||
        (typedKind(b) !== undefined && typedKind(a) !== undefined)) return deepEqual(a, b, false, newPairs(pairs));
    if (isArray(a) && isArray(b)) {
      // arrays stay whole: every element is described, each one partially
      if (a.length !== b.length) return false;
      if (seen(pairs, a, b)) return true;
      for (let i = 0; i < a.length; i++)
        if (!partialEqual(isUndefinedHole(a, i) ? undefined : a[i], isUndefinedHole(b, i) ? undefined : b[i], pairs,
          inherited))
          return false;
      return true;
    }
    // an object shape may describe an array, an array shape only an array
    if (isArray(b)) return false;
    if (isObject(a) && isObject(b)) {
      const keys = keysOf(b);
      if (seen(pairs, a, b)) return true;
      for (let i = 0; i < keys.length; i++) {
        const k = keys[i];
        if (!(inherited ? k in a : ObjectPrototypeHasOwnProperty(a, k)) || !partialEqual(a[k], b[k], pairs, inherited)) return false;
      }
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
      return expected === '' ? subject === '' : StringPrototypeIncludes(subject, expected);
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
    const flags = re.flags;
    let kept = '';
    for (let i = 0; i < flags.length; i++) if (flags[i] !== 'g' && flags[i] !== 'y') kept += flags[i];
    return RegExpPrototypeExec(new RegExp(re.source, kept), text) !== null;
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
    if (isArray(path)) return mapList(path, (p) => String(p));
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
    for (let i = 0; i < segments.length; i++) {
      const seg = segments[i];
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
    return NumberIsFinite(v) && v >= min && v <= 2147483647 && NumberIsInteger(v);
  }
  const argsEqual = (call, args) => call.length === args.length && everyList(args, (x, i) => looseEquals(call[i], x));
  const returned = (r) => isObject(r) && r.type === 'return';

  // ---------------- snapshots ----------------
  // Vitest's pretty-format as GocciaScript's snapshot formatter writes it,
  // over the snapshot state of the host (Goccia.HostAPI's snapshot natives over Goccia.Builtins.Testing.Snapshots)
  const snapshotNatives = shared && shared.snapshot;
  const testTimeoutMs = shared && shared.deadline ? shared.deadline.testTimeoutMs : 0;
  const dateToISOString = global.Date.prototype.toISOString;
  const dateGetTime = global.Date.prototype.getTime;
  const isDateValue = brand(dateGetTime);
  const objectToString = Object.prototype.toString;
  const weakMapHas = global.WeakMap.prototype.has, weakSetHas = global.WeakSet.prototype.has;
  const isWeakMap = (v) => { try { apply(weakMapHas, v, [{}]); return true; } catch { return false; } };
  const isWeakSet = (v) => { try { apply(weakSetHas, v, [{}]); return true; } catch { return false; } };
  const arrayBufferByteLength = getOwnPropertyDescriptor(global.ArrayBuffer.prototype, 'byteLength').get;
  const sharedBufferByteLength = global.SharedArrayBuffer ?
    getOwnPropertyDescriptor(global.SharedArrayBuffer.prototype, 'byteLength').get : undefined;
  const dataViewBuffer = getOwnPropertyDescriptor(global.DataView.prototype, 'buffer').get;
  const dataViewByteLength = getOwnPropertyDescriptor(global.DataView.prototype, 'byteLength').get;
  const dataViewByteOffset = getOwnPropertyDescriptor(global.DataView.prototype, 'byteOffset').get;
  const isArrayBuffer = brand(arrayBufferByteLength);
  const isSharedBuffer = (v) => sharedBufferByteLength !== undefined && brand(sharedBufferByteLength)(v);
  const isDataView = brand(dataViewBuffer);
  const setValues = Set.prototype.values;
  const regExpToString = RegExp.prototype.toString;
  const snapshotSerializers = [];
  const SNAPSHOT_SETTINGS = {
    callToJSON: true, escapeRegex: true, escapeString: false, indent: '  ', maxDepth: Infinity,
    maxWidth: Infinity, min: false, printBasicPrototype: false, printFunctionName: false, spacingInner: '\n',
    spacingOuter: '\n',
  };
  const quoteString = (s, escape) => '"' + (escape ? replaceMatches(s, /["\\]/g, (m) => '\\' + m[0]) : s) + '"';
  const escapeRegExpText = (s) => replaceMatches(s, /[$()*+.?[\\\]^{|}]/g, (m) => '\\' + m[0]);
  function stringTag(v) {
    try { const t = v[Symbol.toStringTag]; return typeof t === 'string' ? t : ''; } catch { return ''; }
  }
  // pretty-format prints a value as a Date when its string tag says so, else when it has a date value
  function isSnapshotDate(v) {
    const tag = stringTag(v);
    return tag !== '' ? tag === 'Date' : isDateValue(v);
  }
  const BOXES = [[brand(BooleanCtor.prototype.valueOf), 'Boolean'], [brand(NumberCtor.prototype.valueOf), 'Number'],
    [brand(StringCtor.prototype.valueOf), 'String'], [brand(BigIntCtor.prototype.valueOf), 'BigInt']];
  // the name a value prints with: a boxed primitive's type, an Intl object's, a class instance's class, its tag
  function objectDisplayName(v) {
    for (let i = 0; i < BOXES.length; i++) if (BOXES[i][0](v)) return BOXES[i][1];
    let tag = stringTag(v);
    if (StringPrototypeStartsWith(tag, 'Intl.')) return StringPrototypeSlice(tag, 5);
    if (tag === 'Generator' || tag === 'AsyncGenerator') return 'Object';
    if (StringPrototypeIncludes(tag, 'Iterator')) return 'Iterator';
    let proto;
    try { proto = ObjectGetPrototypeOf(v); } catch { proto = null; }
    if (proto !== null && proto !== Object.prototype && proto !== Array.prototype) {
      const d = getOwnPropertyDescriptor(proto, 'constructor');
      const ctor = d && d.value;
      if (typeof ctor === 'function' && typeof ctor.name === 'string' && ctor.name !== '' &&
          StringPrototypeStartsWith(FunctionPrototypeToString(ctor), 'class')) return ctor.name;
    }
    if (tag === '' && isArray(v)) return 'Array';
    return tag === '' ? 'Object' : tag;
  }
  function formatNumber(n) { return ObjectIs(n, -0) ? '-0' : String(n); }

  // the formatter's own serializers after the user's, as GocciaScript lists them (seven that never match, so a
  // serializer counting config.plugins sees the same list)
  const builtinSerializers = [];
  for (let i = 0; i < 7; i++) append(builtinSerializers, { test: () => false, serialize: () => '' });
  function formatSnapshot(value) {
    const settings = { ...SNAPSHOT_SETTINGS, plugins: concatLists(snapshotSerializers, builtinSerializers) };
    return toLF(format(value, settings, '', 0, [], false));
  }
  function readSettings(base, config) {
    const s = { ...base };
    if (!isObject(config)) return s;
    const flags = ['callToJSON', 'escapeRegex', 'escapeString', 'min', 'printBasicPrototype', 'printFunctionName'];
    for (let i = 0; i < flags.length; i++) if (config[flags[i]] !== undefined) s[flags[i]] = !!config[flags[i]];
    const texts = ['indent', 'spacingInner', 'spacingOuter'];
    for (let i = 0; i < texts.length; i++) if (config[texts[i]] !== undefined) s[texts[i]] = String(config[texts[i]]);
    if (typeof config.maxDepth === 'number') s.maxDepth = config.maxDepth;
    if (typeof config.maxWidth === 'number') s.maxWidth = config.maxWidth;
    if (isArray(config.plugins)) s.plugins = copyList(config.plugins);
    return s;
  }
  function configObject(s) {
    const colors = {};
    const names = ['comment', 'content', 'prop', 'tag', 'value'];
    for (let i = 0; i < names.length; i++) colors[names[i]] = { open: '', close: '' };
    return {
      callToJSON: s.callToJSON, colors, compareKeys: undefined, escapeRegex: s.escapeRegex,
      escapeString: s.escapeString, indent: s.indent, maxDepth: s.maxDepth, maxWidth: s.maxWidth, min: s.min,
      plugins: copyList(s.plugins), printBasicPrototype: s.printBasicPrototype, printFunctionName: s.printFunctionName,
      printShadowRoot: true, spacingInner: s.spacingInner, spacingOuter: s.spacingOuter, singleQuote: false,
      quoteKeys: true, maxOutputLength: 1 << 27, _outputLengthPerDepth: [],
    };
  }
  // a serializer whose test accepts the value prints it: serialize(value, config, indentation, depth, refs, printer)
  // or the older print(value, print, indent, options, colors)
  function formatWithSerializer(v, s, indentation, depth, refs) {
    for (let pi = 0; pi < s.plugins.length; pi++) {
      const plugin = s.plugins[pi];
      if (!isObject(plugin)) throw new Error('Snapshot serializer must be an object');
      if (typeof plugin.test !== 'function') throw new Error('Snapshot serializer test must be callable');
      if (!plugin.test(v)) continue;
      let printed;
      if (typeof plugin.serialize === 'function') {
        const printer = (val, config, ind, dep, rf, called) => format(val,
          config === undefined ? s : readSettings(s, config), ind === undefined ? indentation : String(ind),
          typeof dep === 'number' ? Math.trunc(dep) : depth, isArray(rf) ? copyList(rf) : refs, !!called);
        printed = apply(plugin.serialize, plugin, [v, configObject(s), indentation, depth, copyList(refs), printer]);
      } else {
        const print = (val) => format(val, s, indentation, depth, refs, false);
        const indent = (text) => {
          const ind = indentation + s.indent;
          return ind + ArrayPrototypeJoin(StringPrototypeSplit(String(text), '\n'), '\n' + ind);
        };
        const options = { edgeSpacing: s.spacingOuter, min: s.min, spacing: s.spacingInner };
        printed = apply(plugin.print, plugin, [v, print, indent, options, configObject(s).colors]);
      }
      if (typeof printed !== 'string') throw new TypeError('pretty-format: Plugin must return type "string"');
      return { printed };
    }
    return null;
  }
  function formatAsymmetric(m, s, indentation, depth, refs) {
    const spec = AsymmetricMatcher.spec(m);
    if (ArrayPrototypeIncludes(['arrayContaining', 'objectContaining', 'stringContaining', 'stringMatching'], spec.kind))
      return spec.name + ' ' + format(spec.sample, s, indentation, depth, refs, false);
    if (spec.kind === 'schemaMatching') {
      const r = spec.validationResult;
      if (isObject(r) && isArray(r.issues) && r.issues.length > 0)
        return spec.name + ' ' + format(r, s, indentation, depth, refs, false);
      return spec.name;
    }
    return spec.display();
  }
  // the items of a list as pretty-format writes them: one per line, cut at maxWidth
  function listBody(count, item, s, indentation, closeWithComma) {
    const next = indentation + s.indent;
    let out = s.spacingOuter;
    for (let i = 0; i < count; i++) {
      out += next;
      if (i === s.maxWidth) { out += `\u2026(${count - i})` + (closeWithComma ? '' : s.spacingOuter); break; }
      out += item(i, next);
      if (closeWithComma) out += i < count - 1 ? ',' + s.spacingInner : (s.min ? '' : ',');
      else out += ',' + s.spacingInner;
    }
    return closeWithComma ? out + s.spacingOuter + indentation : out + indentation;
  }
  const Int8ArrayCtor = global.Int8Array, TAP = ObjectGetPrototypeOf(global.Int8Array.prototype);
  const taBuffer = uncurryThis(getOwnPropertyDescriptor(TAP, 'buffer').get);
  const taByteOffset = uncurryThis(getOwnPropertyDescriptor(TAP, 'byteOffset').get);
  const taLength = uncurryThis(getOwnPropertyDescriptor(TAP, 'length').get);
  const symbolValueOf = Symbol.prototype.valueOf;
  function formatBytes(name, view, s, indentation) {
    const length = taLength(view);
    if (length === 0) return name + ' []';
    const signed = new Int8ArrayCtor(taBuffer(view), taByteOffset(view), length);
    return name + ' [' + listBody(length, (i) => String(signed[i]), s, indentation, false) + ']';
  }
  function format(v, s, indentation, depth, refs, calledToJSON) {
    const fromPlugin = formatWithSerializer(v, s, indentation, depth, refs);
    if (fromPlugin) return fromPlugin.printed;
    if (isAsym(v)) return formatAsymmetric(v, s, indentation, depth, refs);
    if (v === undefined) return 'undefined';
    if (v === null) return 'null';
    switch (typeof v) {
      case 'boolean': return String(v);
      case 'number': return formatNumber(v);
      case 'bigint': return String(v) + 'n';
      case 'string': return quoteString(v, s.escapeString);
      case 'symbol': return String(v);
    }
    if (isMock(v)) return formatMockValue(v, s, indentation, depth, refs);
    if (typeof v === 'function') return '[Function]';
    if (brand(symbolValueOf)(v)) return String(apply(symbolValueOf, v, []));
    if (isWeakMap(v)) return 'WeakMap {}';
    if (isWeakSet(v)) return 'WeakSet {}';
    if (isError(v)) {
      const name = v.name === undefined ? 'Error' : String(v.name);
      const message = v.message === undefined ? '' : String(v.message);
      if (name === '') return '[' + message + ']';
      if (message === '') return '[' + name + ']';
      return '[' + name + ': ' + message + ']';
    }
    if (isRegExp(v)) {
      const text = apply(regExpToString, v, []);
      return s.escapeRegex ? escapeRegExpText(text) : text;
    }
    if (isSnapshotDate(v)) {
      // a user valueOf takes part in the invalid-date check, as pretty-format's NumberIsNaN(+value)
      if (NumberIsNaN(+v)) return 'Date { NaN }';
      return String(apply(isDateValue(v) ? dateToISOString : v.toISOString, v, []));
    }
    if (ArrayPrototypeIncludes(refs, v)) return '[Circular]';
    const nextRefs = copyList(refs);
    append(nextRefs, v);
    if (depth + 1 > s.maxDepth) return '[' + objectDisplayName(v) + ']';
    if (s.callToJSON && !calledToJSON && typeof v.toJSON === 'function')
      return format(v.toJSON(), s, indentation, depth + 1, nextRefs, true);
    if (isArray(v)) {
      const name = objectDisplayName(v);
      const prefix = s.min || (!s.printBasicPrototype && name === 'Array') ? '' : name + ' ';
      if (v.length === 0) return prefix + '[]';
      return prefix + '[' + listBody(v.length, (i, next) => (i in v ? format(v[i], s, next, depth + 1, nextRefs,
        false) : ''), s, indentation, true) + ']';
    }
    if (apply(objectToString, v, []) === '[object Arguments]') {
      const len = Math.max(0, Math.trunc(Number(v.length)) || 0);
      if (len === 0) return 'Arguments []';
      return 'Arguments [' + listBody(len, (i, next) => format(v[i], s, next, depth + 1, nextRefs, false), s,
        indentation, false) + ']';
    }
    if (isMap(v)) {
      const entries = mapEntries(v);
      if (entries.length === 0) return 'Map {}';
      return 'Map {' + listBody(entries.length, (i, next) => format(entries[i][0], s, next, depth + 1, nextRefs,
        false) + ' => ' + format(entries[i][1], s, next, depth + 1, nextRefs, false), s, indentation, false) + '}';
    }
    if (isSet(v)) {
      const items = setMembers(v);
      if (items.length === 0) return 'Set {}';
      return 'Set {' + listBody(items.length, (i, next) => format(items[i], s, next, depth + 1, nextRefs, false), s,
        indentation, false) + '}';
    }
    const kind = typedKind(v);
    if (kind !== undefined) {
      const len = v.length;
      if (StringPrototypeStartsWith(kind, 'Big') || kind === 'Float16Array') {
        if (len === 0) return kind + ' {}';
        return kind + ' {' + listBody(len, (i, next) => quoteString(String(i), s.escapeString) + ': ' +
          format(v[i], s, next, depth + 1, nextRefs, false), s, indentation, false) + '}';
      }
      if (len === 0) return kind + ' []';
      return kind + ' [' + listBody(len, (i, next) => format(v[i], s, next, depth + 1, nextRefs, false), s,
        indentation, false) + ']';
    }
    if (isArrayBuffer(v)) return formatBytes('ArrayBuffer', new Uint8Array(v), s, indentation);
    if (isDataView(v)) {
      let len;
      try { len = apply(dataViewByteLength, v, []); } catch { return 'DataView []'; }
      return formatBytes('DataView', new Uint8Array(apply(dataViewBuffer, v, []), apply(dataViewByteOffset, v, []),
        len), s, indentation);
    }
    return formatObjectValue(v, s, indentation, depth, nextRefs);
  }
  function formatObjectValue(o, s, indentation, depth, refs) {
    const keys = ArrayPrototypeSort(keysOf(o), (a, b) => (a < b ? -1 : a > b ? 1 : 0));
    const symbols = filterList(getOwnPropertySymbols(o), (k) => apply(propertyIsEnumerable, o, [k]));
    const name = objectDisplayName(o);
    const prefix = s.min || (!s.printBasicPrototype && name === 'Object') ? '' : name + ' ';
    if (keys.length === 0 && symbols.length === 0) return prefix + '{}';
    const all = concatLists(keys, symbols);
    return prefix + '{' + listBody(all.length, (i, next) => {
      const k = all[i];
      const label = typeof k === 'symbol' ? String(k) : quoteString(k, s.escapeString);
      return label + ': ' + format(o[k], s, next, depth + 1, refs, false);
    }, s, indentation, false) + '}';
  }
  function formatMockValue(fn, s, indentation, depth, refs) {
    const name = String(fn.getMockName());
    const suffix = name === 'mock' ? '' : ' ' + name;
    const m = fn.mock;
    if (m.calls.length === 0) return '[MockFunction' + suffix + ']';
    const next = indentation + s.indent;
    return '[MockFunction' + suffix + '] {' + s.spacingOuter + next + '"calls": ' +
      format(m.calls, s, next, depth, refs, false) + ',' + s.spacingOuter + next + '"results": ' +
      format(m.results, s, next, depth, refs, false) + (s.min ? '' : ',') + s.spacingOuter + indentation + '}';
  }

  // a snapshot's text: one line break before and after a multi-line value (AddSnapshotLineBreaks)
  const addSnapshotLineBreaks = (t) => (StringPrototypeIncludes(t, '\n') ? '\n' + t + '\n' : t);
  const isPropertyShape = (v) => v !== null && typeof v === 'object';
  // the received value with the matcher's property shape put over it, for printing
  function mergePropertyShape(actual, shape) {
    if (isAsym(shape)) return shape;
    if (isArray(actual) && isArray(shape)) {
      const merged = copyList(actual);
      for (let i = 0; i < shape.length; i++) if (i in shape) merged[i] = mergePropertyShape(actual[i], shape[i]);
      return merged;
    }
    if (isPropertyShape(actual) && isPropertyShape(shape)) {
      const merged = {};
      const ak = keysOf(actual), as = getOwnPropertySymbols(actual), sk = keysOf(shape);
      for (let i = 0; i < ak.length; i++) merged[ak[i]] = actual[ak[i]];
      for (let i = 0; i < as.length; i++) if (apply(propertyIsEnumerable, actual, [as[i]])) merged[as[i]] = actual[as[i]];
      for (let i = 0; i < sk.length; i++) merged[sk[i]] = mergePropertyShape(actual[sk[i]], shape[sk[i]]);
      return merged;
    }
    return shape;
  }
  // an inline snapshot's common indentation removed, as Vitest's dedent does
  function stripInlineIndentation(value) {
    const text = toLF(value);
    const lines = StringPrototypeSplit(text, '\n');
    if (lines.length <= 2 || StringPrototypeTrim(lines[0]) !== '' || StringPrototypeTrim(lines[lines.length - 1]) !== '') return text;
    let indentation = '';
    for (let i = 1; i < lines.length - 1; i++)
      if (StringPrototypeTrim(lines[i]) !== '') { indentation = RegExpPrototypeExec(/^[ \t]*/, lines[i])[0]; break; }
    if (indentation === '') return text;
    for (let i = 1; i < lines.length - 1; i++)
      if (lines[i] !== '') {
        if (!StringPrototypeStartsWith(lines[i], indentation)) return text;
        lines[i] = StringPrototypeSlice(lines[i], indentation.length);
      }
    lines[lines.length - 1] = '';
    return ArrayPrototypeJoin(lines, '\n');
  }
  let snapshotTestName = '';
  function requireSnapshots(name) {
    if (!snapshotNatives || !snapshotNatives.available())
      throw new Error(`${name} is only available when a snapshot host is installed`);
  }

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
      this.#check(ObjectIs(a, e), (n) => `Expected ${show(a)}${n} to be ${show(e)}`);
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
    toMatchSnapshot(...args) {
      if (this.#not) throw new Error('toMatchSnapshot cannot be used with "not"');
      requireSnapshots('toMatchSnapshot');
      let properties, hasProperties = false, hint = '';
      if (args.length > 0) {
        if (isPropertyShape(args[0]) || args[0] === null) { properties = args[0]; hasProperties = true; }
        else if (typeof args[0] === 'string') hint = args[0];
      }
      if (args.length > 1 && typeof args[0] !== 'string' && args[1]) hint = String(args[1]);
      const actual = this.#actual;
      if (hasProperties && !isPropertyShape(actual))
        throw new Error('Received value must be an object when the matcher has properties');
      const matched = !hasProperties || snapshotPartialEqual(actual, properties);
      const received = addSnapshotLineBreaks(formatSnapshot(matched && hasProperties ?
        mergePropertyShape(actual, properties) : actual));
      const outcome = snapshotNatives.matchExternal(snapshotTestName, hint, received, matched), ok = outcome[0], message = outcome[1];
      if (ok) passed(); else failed(message);
    }
    toMatchInlineSnapshot(...args) {
      if (this.#not) throw new Error('toMatchInlineSnapshot cannot be used with "not"');
      requireSnapshots('toMatchInlineSnapshot');
      let properties, hasProperties = false, inline = '', hasInline = false, hint = '', argumentIndex = 0;
      if (args.length > 0) {
        if (isPropertyShape(args[0]) || args[0] === null) {
          properties = args[0]; hasProperties = true; argumentIndex = 1;
        } else if (typeof args[0] === 'string') { inline = stripInlineIndentation(args[0]); hasInline = true; }
      }
      if (args.length > 1) {
        const a = args[1];
        if (typeof args[0] === 'string' && a) hint = String(a);
        else if (typeof a === 'string' && a !== '') {
          inline = stripInlineIndentation(a); hasInline = true; argumentIndex = 1;
        } else if (a) throw new Error('toMatchInlineSnapshot inline snapshot must be a string');
      }
      if (args.length > 2 && args[2]) hint = String(args[2]);
      const actual = this.#actual;
      if (hasProperties && !isPropertyShape(actual))
        throw new Error('Received value must be an object when the matcher has properties');
      const matched = !hasProperties || snapshotPartialEqual(actual, properties);
      const received = addSnapshotLineBreaks(formatSnapshot(matched && hasProperties ?
        mergePropertyShape(actual, properties) : actual));
      const outcome = snapshotNatives.matchInline(snapshotTestName, hint, received, inline, hasInline, matched,
        argumentIndex);
      const ok = outcome[0], message = outcome[1];
      if (ok) passed(); else failed(message);
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
      this.#check(regExp ? firstMatch(e, a) : StringPrototypeIncludes(a, e), (n) => `Expected ${show(a)}${n} to match ${description}`);
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
      try { nan = NumberIsNaN(Number(a)); } catch {}
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
      if (typeof a === 'string') found = StringPrototypeIncludes(a, typeof e === 'string' ? e : show(e));
      else if (isArray(a)) found = ArrayPrototypeIncludes(a, e);
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
      else if (isSet(a) || isMap(a)) ok = sizeOf(a) === n;
      else if (isObject(a)) ok = ObjectGetOwnPropertyNames(a).length === n;
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
      let description = ArrayPrototypeJoin(segments, '.');
      let r;
      // a string or number path is first tried as one own key, so { 'a.b': 5 } has property 'a.b'
      if (!isArray(path) && ObjectPrototypeHasOwnProperty(a, String(path))) {
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
      if (NumberIsNaN(precision)) precision = 0;
      const x = Number(a), y = Number(e);
      let ok;
      if (NumberIsNaN(x) || NumberIsNaN(y)) ok = NumberIsNaN(x) && NumberIsNaN(y);
      else if (!NumberIsFinite(x) || !NumberIsFinite(y)) ok = x === y;
      else {
        let tolerance;
        if (!NumberIsFinite(precision)) tolerance = precision > 0 ? 0 : Infinity;
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
      const found = someList(calls, (c) => isArray(c) && argsEqual(c, args));
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
      if (argsEqual(calls[n - 1], filterList(args, (x, i) => i > 0)) !== this.#not) return passed();
      failed(this.#not ? `Expected call ${n} not to have the specified arguments`
        : `Expected call ${n} to have the specified arguments`);
    }
    toHaveReturned(...args) {
      requireExactly(args, 0, 'toHaveReturned');
      const ok = someList(this.#mock().results, returned);
      if (ok !== this.#not) return passed();
      failed(this.#not ? 'Expected mock not to have returned' : 'Expected mock to have returned at least once');
    }
    toHaveReturnedTimes(...args) {
      const state = this.#mock();
      requireExactly(args, 1, 'toHaveReturnedTimes');
      if (!positiveInt(args[0], 0)) throw new TypeError('toHaveReturnedTimes expects a non-negative integer');
      const n = Number(args[0]), count = filterList(state.results, returned).length;
      if ((count === n) !== this.#not) return passed();
      failed(this.#not ? `Expected mock not to have returned ${n} time(s)`
        : `Expected mock to have returned ${n} time(s) but returned ${count} time(s)`);
    }
    toHaveReturnedWith(...args) {
      const state = this.#mock();
      requireExactly(args, 1, 'toHaveReturnedWith');
      const e = args[0];
      const ok = someList(state.results, (r) => returned(r) && looseEquals(r.value, e));
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
        const callArgs = [this.#actual];
        for (let i = 0; i < args.length; i++) append(callArgs, args[i]);
        const r = apply(fn, { isNot: this.#not, equals: looseEquals }, callArgs);
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
        return PromiseReject(new AssertionError('Expected a Promise but received ' + show(actual)));
      }
      return apply(promiseThen, actual, [
        (v) => {
          if (mode === 'rejects') failed('Expected Promise to reject but it resolved with: ' + show(v));
          const ex = new Expectation(v, negated, false);
          return apply(ex[name], ex, args);
        },
        (e) => {
          if (mode === 'resolves') failed('Expected Promise to resolve but it rejected with: ' + show(e));
          const ex = new Expectation(e, negated, true);
          return apply(ex[name], ex, args);
        }]);
    }
    static forward(name) {
      PromiseExpectation.prototype[name] = function (...args) { return PromiseExpectation.run(this, name, args); };
    }
  }
  for (const name of ObjectGetOwnPropertyNames(Expectation.prototype))
    if (name !== 'constructor' && name !== 'not' && name !== 'resolves' && name !== 'rejects')
      PromiseExpectation.forward(name);

  function expect(...args) {
    requireExactly(args, 1, 'expect');
    if (snapshotNatives && snapshotNatives.observe) snapshotNatives.observe(args[0]);
    return new Expectation(args[0], false, false);
  }
  expect.anything = () => anything();
  expect.addSnapshotSerializer = (...args) => {
    requireExactly(args, 1, 'expect.addSnapshotSerializer');
    const before = copyList(snapshotSerializers);
    snapshotSerializers.length = 0;
    append(snapshotSerializers, args[0]);
    for (let i = 0; i < before.length; i++) append(snapshotSerializers, before[i]);
  };
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
  ObjectDefineProperty(expect, 'not', {
    __proto__: null,
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
  expect.extend = (obj) => { const keys = keysOf(obj); for (let i = 0; i < keys.length; i++) Expectation.custom(keys[i], obj[keys[i]]); };

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
      const fn = once.length ? ArrayPrototypeShift(once) : implementation;
      try {
        const v = fn ? (new.target ? Reflect.construct(fn, args, new.target) : apply(fn, this, args)) : undefined;
        result.type = 'return'; result.value = v;
        return v;
      } catch (e) {
        result.type = 'throw'; result.value = e;
        throw e;
      }
    };
    ObjectDefineProperty(f, 'mock', { __proto__: null, get: () => state });
    f[MOCK] = true;
    f.mockImplementation = (fn) => { implementation = fn; return f; };
    f.mockImplementationOnce = (fn) => { append(once, fn); return f; };
    f.mockReturnValue = (v) => f.mockImplementation(() => v);
    f.mockReturnValueOnce = (v) => f.mockImplementationOnce(() => v);
    f.mockResolvedValue = (v) => f.mockImplementation(() => PromiseResolve(v));
    f.mockResolvedValueOnce = (v) => f.mockImplementationOnce(() => PromiseResolve(v));
    f.mockRejectedValue = (v) => f.mockImplementation(() => PromiseReject(v));
    f.mockRejectedValueOnce = (v) => f.mockImplementationOnce(() => PromiseReject(v));
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
    const obj = args[0], key = args[1], access = args[2];
    if (!isObject(obj)) throw new TypeError('spyOn expects first argument to be an object');
    if (typeof key !== 'string') throw new TypeError('spyOn expects second argument to be a string (method name)');
    if (!(key in obj)) throw new TypeError(`spyOn: cannot spy on non-existent property "${key}"`);
    const desc = getOwnPropertyDescriptor(obj, key);
    if (access === 'get' || access === 'set') {
      const d = desc || getOwnPropertyDescriptor(ObjectGetPrototypeOf(obj), key);
      const orig = d && d[access];
      const m = orig === undefined ? mock() : mock(orig);
      ObjectDefineProperty(obj, key, ObjectAssign({ __proto__: null }, d, { [access]: m, configurable: true }));
      m.__restore = () => { if (desc) ObjectDefineProperty(obj, key, ObjectAssign({ __proto__: null }, desc)); else delete obj[key]; };
      return m;
    }
    const orig = obj[key];
    if (typeof orig !== 'function') throw new TypeError(`spyOn: property "${key}" is not a function`);
    const m = mock(orig);
    obj[key] = m;
    m.__restore = () => { if (desc) ObjectDefineProperty(obj, key, ObjectAssign({ __proto__: null }, desc)); else delete obj[key]; };
    return m;
  }

  // ---------------- running ----------------
  function selected(suite) {
    // a suite or test is focused when it or an ancestor is .only
    for (let s = suite; s; s = s.parent) if (s.mode === 'only') return true;
    return false;
  }
  async function runHooks(list) {
    for (let i = 0; i < list.length; i++) {
      const fn = list[i];
      let r;
      // a hook that throws: what it queued goes with it, before any of it runs
      try { r = fn(); } catch (e) { discardPendingWork(); throw e; }
      await r;
    }
  }
  const NONE = {};
  // the reporter's console lines (GocciaTestRunner's markers), unless the run reports JSON
  function reportLine(text) {
    if (shared && shared.deadline && shared.deadline.reporter) shared.deadline.print(text);
  }
  // a thrown value as a failure shows it (Goccia's FormatThrowValueDetail): its stack, else its message
  function throwValueDetail(v) {
    if (isObject(v)) {
      let stack, message;
      try { stack = v.stack; message = v.message; } catch { return show(v); }
      if (typeof stack === 'string' && stack !== '') return stack;
      if (typeof message === 'string') return message;
    }
    return show(v);
  }
  // a rejection's reason (Goccia's DescribeRejectionReason): a subclass of Error that inherits Error's name is
  // named by its class
  function describeRejection(v) {
    const text = describeThrown(v);
    if (!isObject(v)) return text;
    let name, message, ctor;
    try { name = v.name; message = v.message; ctor = v.constructor; } catch { return text; }
    if (typeof name !== 'string' || typeof message !== 'string' || name !== 'Error') return text;
    let owner = v;
    while (owner !== null && !ObjectPrototypeHasOwnProperty(owner, 'name')) owner = ObjectGetPrototypeOf(owner);
    if (owner !== Error.prototype) return text;
    if (typeof ctor === 'function' && typeof ctor.name === 'string' && ctor.name !== '' &&
        StringPrototypeStartsWith(FunctionPrototypeToString(ctor), 'class')) return ctor.name + ': ' + message;
    return text;
  }
  // a test's beforeEach or afterEach hooks: every one runs, a failing one gives up what was pending (its timers and
  // jobs), and the first failure is the result (NONE when all passed)
  async function runEachHooks(list) {
    let first = NONE;
    for (let i = 0; i < list.length; i++) {
      const fn = list[i];
      try { await fn(); } catch (e) {
        discardPendingWork();
        if (first === NONE) first = e;
      }
    }
    return first;
  }
  // what a failed unit left (its queued jobs and real timers) dropped, so none of it runs in the next one
  function discardPendingWork() {
    if (work) work.discard();
    if (shared && shared.timers && shared.timers.discard) shared.timers.discard();
  }
  // a unit (a hook list, a test): its error, else a rejection it left unhandled, else a queued job of it that threw
  async function runUnit(run) {
    const mark = work ? work.mark() : 0;
    let error = NONE;
    try { await run(); } catch (e) { error = e; }
    if (!work) return { error, rejection: NONE, jobError: NONE };
    const left = work.unhandled(work.take(mark));
    const jobErrors = work.jobErrors();
    return { error, rejection: left[0] ? left[1] : NONE, jobError: jobErrors.length > 0 ? jobErrors[0] : NONE };
  }
  function chainHooks(suite, kind) {
    const out = [];
    // the outermost suite's hooks first
    const chain = [];
    for (let s = suite; s; s = s.parent) append(chain, s);
    for (let c = chain.length - 1; c >= 0; c--) { const l = chain[c][kind]; for (let i = 0; i < l.length; i++) append(out, l[i]); }
    return kind === 'afterEach' ? ArrayPrototypeReverse(out) : out;
  }
  async function runTests() {
    const result = {
      totalTests: 0, passed: 0, failed: 0, skipped: 0, totalRunTests: 0, assertions: 0, suiteErrors: 0,
      failedTests: [],
    };
    if (collectionError !== null) {
      result.suiteErrors = 1;
      append(result.failedTests, { name: collectionError, error: '' });
      return result;
    }
    // a failed beforeAll or afterAll is a suite error, not a failed test: it fails the file
    const hookFailure = (kind, suite, u) => {
      let message;
      if (u.error !== NONE) message = 'Callback threw an exception: ' + describeThrown(u.error);
      else if (u.rejection !== NONE) message = 'Unhandled promise rejection: ' + describeThrown(u.rejection);
      else message = 'Callback threw an exception: ' + describeThrown(u.jobError);
      result.suiteErrors++;
      append(result.failedTests, { name: `Hook "${kind}" in suite "${fullName(suite)}" failed: ${message}`, error: '' });
      discardPendingWork();
    };
    const unitFailed = (u) => u.error !== NONE || u.rejection !== NONE || u.jobError !== NONE;
    // what the describe bodies and the file's top level left: suite errors, and the tests still run
    if (work) {
      work.collected();
      const outside = work.jobErrors();
      for (let i = 0; i < outside.length; i++) {
        result.suiteErrors++;
        append(result.failedTests, { name: 'Uncaught exception outside a test: ' + describeThrown(outside[i]), error: '' });
      }
      const left = work.unhandled(describeRejections);
      if (left[0]) {
        result.suiteErrors++;
        append(result.failedTests, { name: 'Unhandled promise rejection outside a test: ' + describeThrown(left[1]),
          error: '' });
      }
    }
    async function runSuite(suite, skipped) {
      let skipAll = skipped || suite.mode === 'skip' || suite.mode === 'todo';
      let runnable = false;
      const items = suite.children;
      // a failed beforeAll skips the suite's tests and its descendants' tests
      if (!skipAll) {
        const u = await runUnit(() => runHooks(suite.beforeAll));
        if (unitFailed(u)) { hookFailure('beforeAll', suite, u); skipAll = true; }
      }
      for (let ii = 0; ii < items.length; ii++) {
        const item = items[ii];
        if (item.kind === 'suite') { await runSuite(item.suite, skipAll); continue; }
        result.totalTests++;
        result.totalRunTests++;
        const suiteName = fullName(suite);
        snapshotTestName = suiteName !== '' ? suiteName + ' > ' + item.name : String(item.name);
        const focusedOut = hasOnly && item.mode !== 'only' && !selected(suite);
        if (skipAll || item.mode === 'skip' || item.mode === 'todo' || focusedOut || typeof item.fn !== 'function') {
          result.skipped++;
          // a test that does not run keeps its snapshots
          if (snapshotNatives) snapshotNatives.preserve(snapshotTestName);
          const where = suiteName !== '' ? ` in ${suiteName}` : '';
          if (item.mode === 'todo') reportLine(`    \u{1F4DD} ${item.name}${where}: TODO`);
          else reportLine(`    \u23F8\uFE0F ${item.name}${where}: SKIPPED`);
          continue;
        }
        runnable = true;
        assertionCount = 0; expectedAssertions = null; needAssertions = false;
        finishedHooks = [];
        const testMark = work ? work.mark() : 0;
        const label = suiteName !== '' ? `Test "${item.name}" in suite "${suiteName}"` : `Test "${item.name}"`;
        // the test's first failure (its failedTests detail) and a console line for each, as the old reporter
        // prints them
        let detail = null;
        // every failure counts as an assertion, as the old library's AssertionFailed counts it (a failed expect has
        // counted itself)
        const fail = (consoleText, detailText, counted) => {
          if (!counted) assertionCount++;
          reportLine(`    \u274C ${item.name}${suiteName !== '' ? ' in ' + suiteName : ''}: ${consoleText}`);
          if (detail === null) detail = detailText === undefined ? `${label}: ${consoleText}` : `${label}: ${detailText}`;
        };
        // what a test or hook threw: an assertion keeps its message, anything else its stack
        const thrownFailure = (e) => {
          if (e instanceof AssertionError) fail(e.message, undefined, true);
          else fail(throwValueDetail(e));
        };
        const body = async () => {
          // a failed beforeEach skips the body; the timers still pending run all the same
          const hookError = await runEachHooks(chainHooks(suite, 'beforeEach'));
          if (hookError !== NONE) fail('Callback threw an exception: ' + describeThrown(hookError));
          else {
            let ret;
            let threw = false;
            // --max-stack counts from the test function's frame, not the runner's
            const callTest = shared && shared.deadline && shared.deadline.callTest;
            try { ret = callTest ? callTest(item.fn) : item.fn(); } catch (e) { threw = true; thrownFailure(e); }
            if (!threw && isPromise(ret)) {
              try { await ret; } catch (e) {
                if (e instanceof AssertionError) fail(e.message, undefined, true);
                else {
                  const reason = describeRejection(e);
                  fail('Returned Promise rejected: ' + reason, 'Promise rejected: ' + reason);
                }
              }
            }
          }
          // the timers the test scheduled run before it ends; one a callback threw fails it
          const timerError = shared && shared.timers ? await shared.timers.drainTest() : null;
          if (timerError !== null) {
            const reason = describeRejection(timerError.error);
            fail('Uncaught exception in a timer callback: ' + reason, 'uncaught exception in a timer callback: ' + reason);
          }
          if (detail === null && expectedAssertions !== null && assertionCount !== expectedAssertions)
            fail(`Expected ${expectedAssertions} assertions, got ${assertionCount}`);
          if (detail === null && needAssertions && assertionCount === 0) fail('Expected at least one assertion');
        };
        let timedOut = false;
        try {
          // --test-timeout: the body and the work it waits for run under the test's own deadline
          if (testTimeoutMs > 0) {
            const r = shared.deadline.callWithin(body, testTimeoutMs);
            if (r[0] === 'timeout') timedOut = true;
            else if (r[0] === 'threw') thrownFailure(r[1]);
            else await r[1];
          } else await body();
        } catch (e) { thrownFailure(e); }
        if (timedOut) fail(`Test exceeded per-test timeout of ${testTimeoutMs}ms`, `TIMEOUT after ${testTimeoutMs}ms`);
        const afterError = await runEachHooks(chainHooks(suite, 'afterEach'));
        if (afterError !== NONE) fail('Callback threw an exception: ' + describeThrown(afterError));
        const fins = finishedHooks; finishedHooks = null;
        for (let fi = 0; fi < fins.length; fi++) {
          const fn = fins[fi];
          try { await fn(); } catch (e) { fail('Callback threw an exception: ' + describeThrown(e)); }
        }
        // what the test left: a rejection with no handler, or a queued job that threw, fails it
        if (work) {
          const left = work.unhandled(work.take(testMark));
          const jobErrors = work.jobErrors();
          const hadFailure = detail !== null;
          // a test that timed out already failed: its body's promise, rejected by the expired deadline, is not
          // a second failure
          if (left[0] && !timedOut) {
            const reason = describeRejection(left[1]);
            fail('Unhandled promise rejection: ' + reason, 'unhandled promise rejection: ' + reason);
          } else if (jobErrors.length > 0)
            fail('Uncaught exception in a queued job: ' + describeThrown(jobErrors[0]));
          if (detail !== null && !hadFailure && !timedOut) discardPendingWork();
        }
        result.assertions += assertionCount;
        if (detail !== null) {
          result.failed++;
          append(result.failedTests, { name: detail, error: '' });
        } else result.passed++;
      }
      if (!skipAll) {
        const u = await runUnit(() => runHooks(suite.afterAll));
        if (unitFailed(u)) hookFailure('afterAll', suite, u);
      }
      return runnable;
    }
    await runSuite(root, false);
    // the file's snapshots written, the obsolete ones pruned or reported: each problem fails the file
    if (snapshotNatives && snapshotNatives.available()) {
      const errors = snapshotNatives.finish(true);
      if (errors.length > 0) {
        result.totalTests++;
        result.failed++;
        for (let ei = 0; ei < errors.length; ei++) append(result.failedTests, { name: errors[ei], error: '' });
      }
    }
    return result;
  }

  const api = {
    describe, test, it: test, expect, beforeAll, afterAll, beforeEach, afterEach, onTestFinished, mock, spyOn, runTests,
  };
  // goccia:test outside the test runner (GocciaRunner imports it): no globals, and nothing runs what describe and
  // test register but runTests, which runs it all before it returns (docs/testing-api.md, "Running an imported
  // suite outside the runner")
  const moduleOnly = !!(shared && shared.moduleOnly);
  if (!moduleOnly) {
    const names = keysOf(api);
    for (let i = 0; i < names.length; i++) ObjectDefineProperty(global, names[i], { __proto__: null, value: api[names[i]], writable: true, configurable: true });
    // the generated test262 wrappers' private aliases: a conformance test may declare its own describe or test
    for (const [alias, fn] of [['__gocciaTest262Describe', describe], ['__gocciaTest262Test', test]])
      ObjectDefineProperty(global, alias, { __proto__: null, value: fn, writable: false, configurable: false });
  }
  // runTests({ exitOnFirstFailure, showTestResults }) of the module-only API: the whole registry run again (the
  // counts start over), its result returned, and the results block printed unless showTestResults is false or the
  // host mutes the reporter (--silent, JSON output). The result reports; the script decides what a failure means.
  function runTestsNow(options) {
    const show = !(options && options.showTestResults === false);
    const started = shared.deadline.clock();
    const r = shared.deadline.settle(runTests());
    const failedTests = mapList(r.failedTests, f => (f.error ? f.name + ': ' + f.error : f.name));
    const suites = mapList(filterList(root.children, c => c.kind === 'suite'), c => c.suite.name);
    let summary = `Tests: ${r.totalRunTests} total, ${r.passed} passed, ${r.failed} failed, ${r.skipped} skipped`;
    if (suites.length > 0) summary += ' (Suites: ' + ArrayPrototypeJoin(suites, ', ') + ')';
    const result = {
      passed: r.passed, failed: r.failed, skipped: r.skipped, totalTests: r.totalTests, totalRunTests: r.totalRunTests,
      assertions: r.assertions, duration: shared.deadline.clock() - started, suiteErrors: r.suiteErrors, failedTests,
      summary,
    };
    if (show && shared.deadline.reporter) {
      const out = ['', '=== Test Results ===', summary, 'Total Assertions: ' + r.assertions];
      if (failedTests.length > 0) {
        append(out, '');
        append(out, 'Failed Tests:');
        for (let i = 0; i < failedTests.length; i++) append(out, '  \u2022 ' + failedTests[i]);
      }
      if (r.failed === 0 && r.suiteErrors === 0)
        append(out, r.skipped > 0 ? `\u2705 All tests passed! (${r.skipped} skipped)` : '\u2705 All tests passed!');
      else append(out, '\u274c Some tests failed!');
      append(out, '==================');
      for (let i = 0; i < out.length; i++) shared.deadline.print(out[i]);
    }
    return result;
  }
  const exported = moduleOnly ? { ...api, runTests: runTestsNow } : api;
  if (shared && shared.hostModules) shared.hostModules['goccia:test'] = () => exported;
  return {
    api: exported,
    report(done) {
      const running = runTests();
      return (async () => {
        let r;
        try { r = await running; } catch (e) { return done(undefined, String(e)); }
        return done(JSON.stringify({ totalTests: r.totalTests, passed: r.passed, failed: r.failed, skipped: r.skipped,
          totalRunTests: r.totalRunTests, assertions: r.assertions, suiteErrors: r.suiteErrors,
          failedTests: r.failedTests }));
      })();
    }
  };
})
