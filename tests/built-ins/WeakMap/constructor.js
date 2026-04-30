/*---
description: WeakMap constructor creates weak maps from iterable entries
features: [WeakMap, Symbol]
---*/

test("empty WeakMap constructor", () => {
  const map = new WeakMap();
  expect(map instanceof WeakMap).toBe(true);
});

test("WeakMap constructor accepts null and undefined", () => {
  expect(new WeakMap(null) instanceof WeakMap).toBe(true);
  expect(new WeakMap(undefined) instanceof WeakMap).toBe(true);
});

test("WeakMap constructor rejects non-iterables", () => {
  expect(() => new WeakMap(1)).toThrow(TypeError);
  expect(() => new WeakMap({})).toThrow(TypeError);
});

test("WeakMap constructor treats null @@iterator as absent", () => {
  try {
    new WeakMap({ [Symbol.iterator]: null });
    throw new Error("expected constructor to throw");
  } catch (error) {
    expect(error instanceof TypeError).toBe(true);
    expect(error.message).toBe("WeakMap constructor requires an iterable");
  }
});

test("WeakMap constructor rejects invalid iterator protocols", () => {
  expect(() => new WeakMap({ [Symbol.iterator]: 1 })).toThrow(TypeError);
  expect(() => new WeakMap({
    [Symbol.iterator]() {
      return 1;
    },
  })).toThrow(TypeError);
  expect(() => new WeakMap({
    [Symbol.iterator]() {
      return {};
    },
  })).toThrow(TypeError);
  expect(() => new WeakMap({
    [Symbol.iterator]() {
      return { next: 1 };
    },
  })).toThrow(TypeError);
});

test("WeakMap constructor with object-key entries", () => {
  const a = {};
  const b = {};
  const map = new WeakMap([[a, 1], [b, 2]]);
  expect(map.get(a)).toBe(1);
  expect(map.get(b)).toBe(2);
});

test("WeakMap constructor keeps last value for duplicate key", () => {
  const key = {};
  const map = new WeakMap([[key, "first"], [key, "second"]]);
  expect(map.get(key)).toBe("second");
});

test("WeakMap constructor accepts non-registered symbols", () => {
  const key = Symbol("local");
  const map = new WeakMap([[key, "value"]]);
  expect(map.get(key)).toBe("value");
});

test("WeakMap constructor rejects primitive keys", () => {
  expect(() => new WeakMap([[1, "bad"]])).toThrow(TypeError);
  expect(() => new WeakMap([[null, "bad"]])).toThrow(TypeError);
  expect(() => new WeakMap([[undefined, "bad"]])).toThrow(TypeError);
});

test("WeakMap constructor rejects registered symbol keys", () => {
  expect(() => new WeakMap([[Symbol.for("registered"), "bad"]])).toThrow(TypeError);
});

test("WeakMap constructor accepts arbitrary iterables", () => {
  const key = {};
  const iterable = {
    [Symbol.iterator]() {
      let done = false;
      return {
        next() {
          if (done) return { done: true };
          done = true;
          return { value: { 0: key, 1: "from-iterator" }, done: false };
        },
      };
    },
  };
  const map = new WeakMap(iterable);
  expect(map.get(key)).toBe("from-iterator");
});

test("WeakMap constructor uses the current set method", () => {
  const key = {};
  let calls = 0;
  let receiver;
  let receivedKey;
  let receivedValue;
  const original = WeakMap.prototype.set;
  WeakMap.prototype.set = {
    set(k, v) {
      calls = calls + 1;
      receiver = this;
      receivedKey = k;
      receivedValue = v;
      return this;
    },
  }.set;
  try {
    const map = new WeakMap([[key, "value"]]);
    expect(calls).toBe(1);
    expect(receiver).toBe(map);
    expect(receivedKey).toBe(key);
    expect(receivedValue).toBe("value");
  } finally {
    WeakMap.prototype.set = original;
  }
});

test("WeakMap constructor closes iterator on abrupt completion", () => {
  let closed = false;
  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: { 0: 1, 1: "bad" }, done: false };
        },
        return() {
          closed = true;
          return {};
        },
      };
    },
  };
  expect(() => new WeakMap(iterable)).toThrow(TypeError);
  expect(closed).toBe(true);
});

test("WeakMap constructor closes iterator on malformed entries", () => {
  let closed = false;
  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        return() {
          closed = true;
          return {};
        },
      };
    },
  };
  expect(() => new WeakMap(iterable)).toThrow(TypeError);
  expect(closed).toBe(true);
});

test("WeakMap.prototype.constructor is WeakMap", () => {
  expect(WeakMap.prototype.constructor).toBe(WeakMap);
  expect(new WeakMap().constructor).toBe(WeakMap);
});

test("WeakMap has no collection enumeration APIs", () => {
  expect(WeakMap.prototype.size).toBe(undefined);
  expect(WeakMap.prototype.clear).toBe(undefined);
  expect(WeakMap.prototype.forEach).toBe(undefined);
  expect(WeakMap.prototype.keys).toBe(undefined);
  expect(WeakMap.prototype.values).toBe(undefined);
  expect(WeakMap.prototype.entries).toBe(undefined);
  expect(WeakMap.prototype[Symbol.iterator]).toBe(undefined);
});

test("WeakMap.length is 0 with spec descriptor", () => {
  expect(WeakMap.length).toBe(0);
  const descriptor = Object.getOwnPropertyDescriptor(WeakMap, "length");
  expect(descriptor.value).toBe(0);
  expect(descriptor.writable).toBe(false);
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
});

test("WeakMap.prototype methods are not constructors", () => {
  const isConstructor = (f) => {
    try {
      Reflect.construct(class {}, [], f);
      return true;
    } catch {
      return false;
    }
  };
  expect(isConstructor(WeakMap.prototype.delete)).toBe(false);
  expect(isConstructor(WeakMap.prototype.get)).toBe(false);
  expect(isConstructor(WeakMap.prototype.has)).toBe(false);
  expect(isConstructor(WeakMap.prototype.set)).toBe(false);
  expect(isConstructor(WeakMap.prototype.getOrInsert)).toBe(false);
  expect(isConstructor(WeakMap.prototype.getOrInsertComputed)).toBe(false);
});

test("new on WeakMap.prototype methods throws TypeError", () => {
  // Reserved-word property names (delete) must parse after `new` per spec
  const wm = new WeakMap();
  expect(() => new wm.delete()).toThrow(TypeError);
  expect(() => new wm.has()).toThrow(TypeError);
  expect(() => new wm.get()).toThrow(TypeError);
  expect(() => new wm.set()).toThrow(TypeError);
});
