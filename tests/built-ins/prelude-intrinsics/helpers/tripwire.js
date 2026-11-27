// A tripwire over every property a script can replace on the realm's intrinsics, for the tests that check that a
// built-in written in a prelude (source/engine/prelude) calls only what it captured when the realm started.
//
// installTripwire() turns each configurable property of the global object, of every constructor, prototype and
// namespace reachable from it, and of the hidden intrinsics (the iterator, generator and %TypedArray% prototypes)
// into an accessor that records its own lookup while the tripwire is armed. A data property keeps its value: its
// getter returns it and its setter stores a new one on the intrinsic, or an own property on any other receiver, as
// an ordinary assignment would. A getter keeps working and records each call. The values themselves are not
// replaced, so identity checks and the engine's fast paths see what they saw before (a fast path that checks for an
// unmodified prototype falls back to the prelude, which is the path under test).
//
// invoke(path, thisArg, ...args) calls the method or getter at path as the realm had it, armed, and returns what it
// returned or threw and every recorded lookup in order, named as a script would write it ("String.prototype.slice",
// "globalThis.Math", "%ArrayIteratorPrototype%[Symbol.iterator]"); a test builds its receiver and arguments first, so
// the only lookups recorded are the built-in's own. uncovered(prefix) lists the methods and getters of one intrinsic
// that no test has invoked, so a test can require a case for each.

const apply = Reflect.apply;
const construct = Reflect.construct;
const ownKeys = Reflect.ownKeys;
const getOwnDesc = Reflect.getOwnPropertyDescriptor;
const defineProperty = Reflect.defineProperty;
const getProto = Reflect.getPrototypeOf;
const MapCtor = Map;
const mapGet = Map.prototype.get;
const mapSet = Map.prototype.set;
const mapHas = Map.prototype.has;
const SetCtor = Set;
const StringCtor = String;
const PromiseCtor = Promise;
const promiseThen = Promise.prototype.then;
const promiseResolve = Promise.resolve;
const setHas = Set.prototype.has;
const setAdd = Set.prototype.add;
const symDescription = getOwnDesc(Symbol.prototype, "description").get;

// the realm's globals that are not intrinsics: the test framework's
const FRAMEWORK = [
  "describe", "test", "it", "expect", "beforeEach", "afterEach", "beforeAll", "afterAll", "vi", "jest", "mock",
  "fn", "spyOn", "onTestFinished", "bench", "suite",
];

const keyName = (k) => (typeof k === "symbol" ? `[${apply(symDescription, k, [])}]` : k);

const isObjectLike = (v) => v !== null && (typeof v === "object" || typeof v === "function");

// the hidden intrinsics a script reaches only through a value
const hiddenIntrinsics = () => {
  const list = [];
  const add = (name, get) => {
    try {
      const v = get();
      if (isObjectLike(v)) list[list.length] = [name, v];
    } catch (e) {
      // the realm does not have it
    }
  };
  const iterProto = (v) => getProto(v[Symbol.iterator]());
  add("%ArrayIteratorPrototype%", () => iterProto([]));
  add("%StringIteratorPrototype%", () => iterProto(""));
  add("%MapIteratorPrototype%", () => iterProto(new Map()));
  add("%SetIteratorPrototype%", () => iterProto(new Set()));
  add("%RegExpStringIteratorPrototype%", () => getProto(/a/g[Symbol.matchAll]("a")));
  add("%IteratorPrototype%", () => getProto(iterProto([])));
  add("%GeneratorFunction.prototype%", () => getProto({ *g() {} }.g));
  add("%GeneratorPrototype%", () => getProto({ *g() {} }.g).prototype);
  add("%AsyncGeneratorFunction.prototype%", () => getProto({ async *g() {} }.g));
  add("%AsyncGeneratorPrototype%", () => getProto({ async *g() {} }.g).prototype);
  add("%AsyncIteratorPrototype%", () => getProto(getProto({ async *g() {} }.g).prototype));
  add("%AsyncFunction.prototype%", () => getProto(async () => {}));
  add("%TypedArray%", () => getProto(Int8Array));
  add("%TypedArray%.prototype", () => getProto(Int8Array.prototype));
  add("%IteratorHelperPrototype%", () => getProto(Iterator.prototype.map.call([][Symbol.iterator](), (x) => x)));
  add("%WrapForValidIteratorPrototype%", () => getProto(Iterator.from({ next() { return { done: true }; } })));
  add("%SegmentsPrototype%", () => getProto(new Intl.Segmenter().segment("")));
  add("%SegmentIteratorPrototype%", () => getProto(new Intl.Segmenter().segment("")[Symbol.iterator]()));
  return list;
};

export const installTripwire = () => {
  let armed = false;
  let hits = [];
  const record = (name) => {
    if (!armed) return;
    armed = false;
    hits[hits.length] = name;
    armed = true;
  };
  const restore = [];
  const originals = new MapCtor();
  const seen = new SetCtor();

  const wireProperty = (owner, ownerName, key) => {
    const d = getOwnDesc(owner, key);
    if (d === undefined || !d.configurable) return;
    const name = `${ownerName}${typeof key === "symbol" ? "" : "."}${keyName(key)}`;
    restore[restore.length] = [owner, key, d, ownerName, name];
    if ("value" in d) {
      apply(mapSet, originals, [name, d.value]);
      let value = d.value;
      // a configurable property a script can redefine is watched whether or not it is writable; a read-only one
      // keeps refusing assignments (no setter)
      defineProperty(owner, key, {
        __proto__: null,
        get() { record(name); return value; },
        set: d.writable ? {
          set(v) {
            if (this === owner) value = v;
            else defineProperty(this, key, { __proto__: null, value: v, writable: true, enumerable: true, configurable: true });
          },
        }.set : undefined,
        enumerable: d.enumerable,
        configurable: true,
      });
    } else if (d.get !== undefined) {
      apply(mapSet, originals, [name, d.get]);
      const get = d.get;
      defineProperty(owner, key, {
        __proto__: null,
        get() { record(`${name} getter`); return apply(get, this, []); },
        set: d.set,
        enumerable: d.enumerable,
        configurable: true,
      });
    }
  };

  const objects = [];
  const visit = (name, v) => {
    if (!isObjectLike(v) || apply(setHas, seen, [v])) return;
    apply(setAdd, seen, [v]);
    objects[objects.length] = [name, v];
    // a namespace's or constructor's own constructors and namespaces (Intl.Locale, Temporal.PlainDate)
    for (const k of ownKeys(v)) {
      if (typeof k !== "string" || k === "prototype" || k === "constructor") continue;
      const d = getOwnDesc(v, k);
      if (!("value" in d) || !isObjectLike(d.value)) continue;
      const inner = d.value;
      if (k[0] >= "A" && k[0] <= "Z") visit(`${name}.${k}`, inner);
    }
    if (typeof v === "function") {
      const p = getOwnDesc(v, "prototype");
      if (p !== undefined && "value" in p && isObjectLike(p.value)) visit(`${name}.prototype`, p.value);
    }
  };
  for (const k of ownKeys(globalThis)) {
    if (typeof k !== "string" || FRAMEWORK.includes(k) || k === "globalThis") continue;
    const d = getOwnDesc(globalThis, k);
    if ("value" in d) visit(k, d.value);
  }
  for (const [name, v] of hiddenIntrinsics()) visit(name, v);

  // the global object's own bindings first, then each intrinsic's properties
  for (const k of ownKeys(globalThis)) {
    if (typeof k !== "string" || FRAMEWORK.includes(k)) continue;
    wireProperty(globalThis, "globalThis", k);
  }
  for (const [name, o] of objects) for (const k of ownKeys(o)) wireProperty(o, name, k);

  const covered = new SetCtor();
  const lookup = (path) => {
    if (!apply(mapHas, originals, [path])) throw new TypeError(`tripwire: no intrinsic property ${path}`);
    apply(setAdd, covered, [path]);
    return apply(mapGet, originals, [path]);
  };
  const run = (fn) => {
    hits = [];
    armed = true;
    let value;
    let error;
    let threw = false;
    try {
      value = fn();
    } catch (e) {
      threw = true;
      error = e;
    } finally {
      armed = false;
    }
    return { value, error, threw, hits };
  };
  // awaits fn's result while armed. It yields once first, so the caller's own await on what it returns (and the test
  // runner's on the test's promise, handed over at the test's first await) look up the promise's constructor before
  // it arms; and its own wait on fn's result is a thenable whose then subscribes disarmed
  const runAsync = async (fn) => {
    await undefined;
    hits = [];
    armed = true;
    let value;
    let error;
    let threw = false;
    try {
      const result = fn();
      value = await {
        then(resolve, reject) {
          armed = false;
          try {
            apply(promiseThen, apply(promiseResolve, PromiseCtor, [result]), [resolve, reject]);
          } finally {
            armed = true;
          }
        },
      };
    } catch (e) {
      threw = true;
      error = e;
    } finally {
      armed = false;
    }
    return { value, error, threw, hits };
  };

  return {
    // runs fn armed: { value, error, threw, hits }
    watch: run,
    // runs fn armed and awaits its result, armed throughout; await what it returns
    watchAsync: runAsync,
    // the intrinsic property at path (a value, or a getter's function) as the realm had it, marked covered
    original: lookup,
    // calls the method or getter at path on thisArg, armed
    invoke(path, thisArg, ...args) {
      const f = lookup(path);
      return run(() => apply(f, thisArg, args));
    },
    // calls it and awaits its result, armed throughout
    invokeAsync(path, thisArg, ...args) {
      const f = lookup(path);
      return runAsync(() => apply(f, thisArg, args));
    },
    // constructs the constructor at path, armed
    construct(path, ...args) {
      const C = lookup(path);
      return run(() => construct(C, args));
    },
    // invokes each case [name, args, expected, allowed] on receiver: the method or getter proto.name (or
    // proto[Symbol.x] for a name "[Symbol.x]") must look up nothing a script can replace beyond allowed, and give
    // expected when that is not undefined, or throw when it is THROWS
    covers(proto, receiver, cases) {
      for (const [name, args, expected, allowed = []] of cases) {
        const r = this.invoke(`${proto}${name[0] === "[" ? "" : "."}${name}`, receiver, ...args);
        if (expected === THROWS) {
          expect(r.threw).toBe(true);
          expect(r.hits.filter((h) => !allowed.includes(h))).toEqual([]);
        } else {
          const value = expectClean(r, allowed);
          if (expected !== undefined) expect(value).toEqual(expected);
        }
      }
    },
    call(fn, thisArg, ...args) {
      return apply(fn, thisArg, args);
    },
    // the function-valued properties and getters under prefix (an owner's name, such as "Intl.Locale.prototype")
    // that no test has looked up through original, invoke or construct
    uncovered(prefix) {
      const list = [];
      for (const [, , d, ownerName, path] of restore) {
        if (ownerName !== prefix) continue;
        if (path.endsWith(".constructor") || (typeof d.value !== "function" && d.get === undefined)) continue;
        if (!apply(setHas, covered, [path])) list[list.length] = path;
      }
      return list;
    },
    uninstall() {
      for (const [owner, key, d] of restore) defineProperty(owner, key, d);
    },
  };
};

// a case's expected value when the call must throw (the lookups up to the throw are still checked)
export const THROWS = Symbol("throws");

// asserts that a watched call finished without throwing and looked up nothing a script can replace beyond allowed,
// the lookups the specification makes observable for its input
export const expectClean = (r, allowed = []) => {
  if (r.threw) throw r.error;
  expect(r.hits.filter((h) => !allowed.includes(h))).toEqual([]);
  return r.value;
};

export const ARRAY_ITERATION = ["Array.prototype[Symbol.iterator]", "%ArrayIteratorPrototype%.next"];

// Runs fn with accessors on Object.prototype and Array.prototype for the names a script could add there: the
// descriptor fields, the record fields the preludes write (Temporal's), and the first array indices. Each records
// its read or write; a get gives undefined, as an absent property does. Returns { value, error, threw, hits }.
// A built-in that builds its descriptors, records and lists without those prototypes records nothing.
const DESCRIPTOR_FIELDS = ["value", "writable", "get", "set", "enumerable", "configurable"];
const RECORD_FIELDS = ["years", "months", "weeks", "days", "hours", "minutes", "seconds", "milliseconds", "microseconds",
  "nanoseconds", "year", "month", "monthCode", "day", "hour", "minute", "second", "millisecond", "microsecond",
  "nanosecond", "era", "eraYear", "offset", "timeZone", "calendar", "f", "type", "kind", "alphabet", "lastChunkHandling",
  "omitPadding", "read", "written", "a", "b", "x"];
const INDICES = Array.from({ length: 64 }, (_, i) => StringCtor(i));
export const withPrototypeAdditions = (fn) => {
  const hits = [];
  const traps = [];
  // the traps' own appends define the index (a store would reach the index traps)
  const note = (h) => { defineProperty(hits, hits.length, { __proto__: null, value: h, writable: true, enumerable: true, configurable: true }); };
  const trap = (owner, ownerName, key) => {
    if (getOwnDesc(owner, key) !== undefined) return;
    const name = `${ownerName}.${key}`;
    defineProperty(owner, key, {
      __proto__: null,
      get() { note(`get ${name}`); return undefined; },
      set(v) { note(`set ${name}`); },
      enumerable: false,
      configurable: true,
    });
    defineProperty(traps, traps.length, { __proto__: null, value: [owner, key], writable: true, enumerable: true, configurable: true });
  };
  for (const k of DESCRIPTOR_FIELDS) trap(Object.prototype, "Object.prototype", k);
  for (const k of RECORD_FIELDS) trap(Object.prototype, "Object.prototype", k);
  for (const i of INDICES) {
    trap(Array.prototype, "Array.prototype", i);
    trap(Object.prototype, "Object.prototype", i);
  }
  let value;
  let error;
  let threw = false;
  try {
    value = fn();
  } catch (e) {
    threw = true;
    error = e;
  } finally {
    for (const [owner, key] of traps) delete owner[key];
  }
  return { value, error, threw, hits };
};
