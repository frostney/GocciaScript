/*---
description: Map constructor creates Map objects correctly
features: [Map]
---*/

test("empty Map constructor", () => {
  const map = new Map();
  expect(map.size).toBe(0);
});

test("Map requires new", () => {
  // biome-ignore lint/correctness/noInvalidBuiltinInstantiation: intentional constructor contract
  expect(() => Map()).toThrow(TypeError);
});

test("Map constructor with entries array", () => {
  const map = new Map([
    ["a", 1],
    ["b", 2],
    ["c", 3],
  ]);
  expect(map.size).toBe(3);
  expect(map.get("a")).toBe(1);
  expect(map.get("b")).toBe(2);
  expect(map.get("c")).toBe(3);
});

test("Map constructor with duplicate keys keeps last value", () => {
  const map = new Map([
    ["a", 1],
    ["b", 2],
    ["a", 3],
  ]);
  expect(map.size).toBe(2);
  expect(map.get("a")).toBe(3);
  expect(map.get("b")).toBe(2);
});

test("Map constructor with non-string keys", () => {
  const objKey = { id: 1 };
  const map = new Map([
    [1, "number"],
    [true, "boolean"],
    [null, "null"],
    [objKey, "object"],
  ]);
  expect(map.size).toBe(4);
  expect(map.get(1)).toBe("number");
  expect(map.get(true)).toBe("boolean");
  expect(map.get(null)).toBe("null");
  expect(map.get(objKey)).toBe("object");
});

test("Map constructor with NaN key deduplication", () => {
  const map = new Map([
    [NaN, "first"],
    [NaN, "second"],
  ]);
  expect(map.size).toBe(1);
  expect(map.get(NaN)).toBe("second");
});

test("Map treats -0 and +0 as same key", () => {
  const map = new Map();
  map.set(-0, "neg");
  map.set(0, "pos");
  expect(map.size).toBe(1);
  expect(map.get(0)).toBe("pos");
  expect(map.get(-0)).toBe("pos");
});

test("Map.prototype is an object", () => {
  expect(typeof Map.prototype).toBe("object");
});

test("new Map instanceof Map", () => {
  const m = new Map();
  expect(m instanceof Map).toBe(true);
});

test("Map constructor rejects primitive iterator objects", () => {
  const iterable = {
    [Symbol.iterator]() {
      return 1;
    },
  };

  expect(() => new Map(iterable)).toThrow(TypeError);
});

test("Map constructor closes iterator when an entry is not an object", () => {
  let closed = false;
  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: "not an object", done: false };
        },
        return() {
          closed = true;
          return {};
        },
      };
    },
  };

  expect(() => new Map(iterable)).toThrow(TypeError);
  expect(closed).toBe(true);
});

test("Map constructor closes iterator when entry key getter throws", () => {
  const sentinel = new Error("key getter");
  let closed = false;
  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          return {
            value: {
              get 0() {
                throw sentinel;
              },
            },
            done: false,
          };
        },
        return() {
          closed = true;
          return {};
        },
      };
    },
  };

  try {
    new Map(iterable);
    throw new Error("expected Map constructor to throw");
  } catch (error) {
    expect(error).toBe(sentinel);
  }
  expect(closed).toBe(true);
});

test("Map constructor closes iterator when entry value getter throws", () => {
  const sentinel = new Error("value getter");
  let closed = false;
  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          return {
            value: {
              0: "key",
              get 1() {
                throw sentinel;
              },
            },
            done: false,
          };
        },
        return() {
          closed = true;
          return {};
        },
      };
    },
  };

  try {
    new Map(iterable);
    throw new Error("expected Map constructor to throw");
  } catch (error) {
    expect(error).toBe(sentinel);
  }
  expect(closed).toBe(true);
});

test("Map constructor closes iterator when subclass set throws and preserves original error", () => {
  const sentinel = new Error("set failed");
  let closed = false;
  class ThrowingMap extends Map {
    set() {
      throw sentinel;
    }
  }

  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: ["key", "value"], done: false };
        },
        return() {
          closed = true;
          throw new Error("return failed");
        },
      };
    },
  };

  try {
    new ThrowingMap(iterable);
    throw new Error("expected Map constructor to throw");
  } catch (error) {
    expect(error).toBe(sentinel);
  }
  expect(closed).toBe(true);
});

test("Map constructor calls iterator next without arguments", () => {
  let argumentCount = -1;
  const iterable = {
    [Symbol.iterator]() {
      return {
        next(...args) {
          argumentCount = args.length;
          return { done: true };
        },
      };
    },
  };

  new Map(iterable);
  expect(argumentCount).toBe(0);
});

test("Map constructor does not close iterator when next throws", () => {
  const sentinel = new Error("next failed");
  let closed = false;
  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          throw sentinel;
        },
        return() {
          closed = true;
          return {};
        },
      };
    },
  };

  try {
    new Map(iterable);
    throw new Error("expected Map constructor to throw");
  } catch (error) {
    expect(error).toBe(sentinel);
  }
  expect(closed).toBe(false);
});

test("Map constructor preserves primitive receiver for iterable lookup", () => {
  let receiverType = "";
  Object.defineProperty(Number.prototype, Symbol.iterator, {
    configurable: true,
    value() {
      "use strict";
      receiverType = typeof this;
      return {
        next() {
          return { done: true };
        },
      };
    },
  });

  try {
    new Map(0);
  } finally {
    delete Number.prototype[Symbol.iterator];
  }

  expect(receiverType).toBe("number");
});

test("Map constructor observes native iterator @@iterator overrides", () => {
  const iterator = [][Symbol.iterator]();
  Object.defineProperty(iterator, Symbol.iterator, {
    configurable: true,
    value() {
      return 1;
    },
  });

  expect(() => new Map(iterator)).toThrow(TypeError);
});
