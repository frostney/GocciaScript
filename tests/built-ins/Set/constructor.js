/*---
description: Set constructor creates Set objects correctly
features: [Set]
---*/

test("empty Set constructor", () => {
  const set = new Set();
  expect(set.size).toBe(0);
});

test("Set requires new", () => {
  // biome-ignore lint/correctness/noInvalidBuiltinInstantiation: intentional constructor contract
  expect(() => Set()).toThrow(TypeError);
});

test("Set constructor with array", () => {
  const set = new Set([1, 2, 3]);
  expect(set.size).toBe(3);
  expect(set.has(1)).toBe(true);
  expect(set.has(2)).toBe(true);
  expect(set.has(3)).toBe(true);
});

test("Set constructor removes duplicates", () => {
  const set = new Set([1, 2, 2, 3, 3, 3]);
  expect(set.size).toBe(3);
  expect(set.has(1)).toBe(true);
  expect(set.has(2)).toBe(true);
  expect(set.has(3)).toBe(true);
});

test("Set constructor with string values", () => {
  const set = new Set(["a", "b", "c", "a"]);
  expect(set.size).toBe(3);
  expect(set.has("a")).toBe(true);
  expect(set.has("b")).toBe(true);
  expect(set.has("c")).toBe(true);
});

test("Set constructor with mixed types", () => {
  const set = new Set([1, "1", true, null, undefined]);
  expect(set.size).toBe(5);
  expect(set.has(1)).toBe(true);
  expect(set.has("1")).toBe(true);
  expect(set.has(true)).toBe(true);
  expect(set.has(null)).toBe(true);
  expect(set.has(undefined)).toBe(true);
});

test("Set constructor with NaN deduplication", () => {
  const set = new Set([NaN, NaN, NaN]);
  expect(set.size).toBe(1);
  expect(set.has(NaN)).toBe(true);
});

test("Set constructor with another Set", () => {
  const original = new Set([1, 2, 3]);
  const copy = new Set(original);
  expect(copy.size).toBe(3);
  expect(copy.has(1)).toBe(true);
  expect(copy.has(2)).toBe(true);
  expect(copy.has(3)).toBe(true);
  expect(copy).not.toBe(original);
});

test("Set treats -0 and +0 as same value", () => {
  const set = new Set([-0, 0]);
  expect(set.size).toBe(1);
  expect(set.has(0)).toBe(true);
  expect(set.has(-0)).toBe(true);
});

test("Set.prototype is an object", () => {
  expect(typeof Set.prototype).toBe("object");
});

test("new Set instanceof Set", () => {
  const s = new Set();
  expect(s instanceof Set).toBe(true);
});

test("Set constructor closes iterator when subclass add throws and preserves original error", () => {
  const sentinel = new Error("add failed");
  let closed = false;
  class ThrowingSet extends Set {
    add() {
      throw sentinel;
    }
  }

  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: "value", done: false };
        },
        return() {
          closed = true;
          throw new Error("return failed");
        },
      };
    },
  };

  try {
    new ThrowingSet(iterable);
    throw new Error("expected Set constructor to throw");
  } catch (error) {
    expect(error).toBe(sentinel);
  }
  expect(closed).toBe(true);
});

test("Set constructor does not close iterator when next throws", () => {
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
    new Set(iterable);
    throw new Error("expected Set constructor to throw");
  } catch (error) {
    expect(error).toBe(sentinel);
  }
  expect(closed).toBe(false);
});

test("Set constructor observes native iterator @@iterator overrides", () => {
  const iterator = [][Symbol.iterator]();
  Object.defineProperty(iterator, Symbol.iterator, {
    configurable: true,
    value() {
      return 1;
    },
  });

  expect(() => new Set(iterator)).toThrow(TypeError);
});
