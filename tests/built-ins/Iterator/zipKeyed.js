/*---
description: Iterator.zipKeyed() combines keyed iterables into an iterator of objects
features: [Iterator, Iterator.zipKeyed]
---*/

describe("Iterator.zipKeyed()", () => {
  test("zips keyed object with array values", () => {
    const result = Iterator.zipKeyed({
      name: ["Alice", "Bob", "Carol"],
      age: [30, 25, 35],
    }).toArray();
    expect(result).toEqual([
      { name: "Alice", age: 30 },
      { name: "Bob", age: 25 },
      { name: "Carol", age: 35 },
    ]);
  });

  test("zips a single key", () => {
    const result = Iterator.zipKeyed({ x: [1, 2, 3] }).toArray();
    expect(result).toEqual([{ x: 1 }, { x: 2 }, { x: 3 }]);
  });

  test("zips empty object yields nothing", () => {
    const result = Iterator.zipKeyed({}).toArray();
    expect(result).toEqual([]);
  });

  test("default mode is shortest — stops at shortest iterable", () => {
    const result = Iterator.zipKeyed({
      x: [1, 2, 3],
      y: ["a", "b"],
    }).toArray();
    expect(result).toEqual([
      { x: 1, y: "a" },
      { x: 2, y: "b" },
    ]);
  });

  test("explicit shortest mode", () => {
    const result = Iterator.zipKeyed(
      { a: [1, 2, 3], b: ["x"] },
      { mode: "shortest" }
    ).toArray();
    expect(result).toEqual([{ a: 1, b: "x" }]);
  });

  test("zips with Set values", () => {
    const result = Iterator.zipKeyed({
      nums: new Set([1, 2]),
      letters: new Set(["a", "b"]),
    }).toArray();
    expect(result).toEqual([
      { nums: 1, letters: "a" },
      { nums: 2, letters: "b" },
    ]);
  });

  test("result is an iterator (has next and Symbol.iterator)", () => {
    const iter = Iterator.zipKeyed({ x: [1] });
    expect(typeof iter.next).toBe("function");
    expect(iter[Symbol.iterator]()).toBe(iter);
  });

  test("returns done after exhaustion", () => {
    const iter = Iterator.zipKeyed({ x: [1], y: [2] });
    expect(iter.next()).toEqual({ value: { x: 1, y: 2 }, done: false });
    expect(iter.next()).toEqual({ value: undefined, done: true });
    expect(iter.next()).toEqual({ value: undefined, done: true });
  });

  test("works with for-of", () => {
    const result = [];
    for (const obj of Iterator.zipKeyed({ a: [1, 2], b: [3, 4] })) {
      result.push(obj);
    }
    expect(result).toEqual([{ a: 1, b: 3 }, { a: 2, b: 4 }]);
  });

  test("works with destructuring in for-of", () => {
    const names = [];
    const cities = [];
    for (const { name, city } of Iterator.zipKeyed({
      name: ["Alice", "Bob"],
      city: ["NYC", "London"],
    })) {
      names.push(name);
      cities.push(city);
    }
    expect(names).toEqual(["Alice", "Bob"]);
    expect(cities).toEqual(["NYC", "London"]);
  });

  test("result works with iterator helper methods", () => {
    const result = Iterator.zipKeyed({
      x: [1, 2, 3, 4],
      y: [10, 20, 30, 40],
    })
      .filter(({ x }) => x > 2)
      .map(({ x, y }) => x + y)
      .toArray();
    expect(result).toEqual([33, 44]);
  });

  test("zips with string values (character iteration)", () => {
    const result = Iterator.zipKeyed({
      first: "ab",
      second: "cd",
    }).toArray();
    expect(result).toEqual([
      { first: "a", second: "c" },
      { first: "b", second: "d" },
    ]);
  });

  test("throws TypeError for non-object first argument", () => {
    expect(() => Iterator.zipKeyed(42)).toThrow(TypeError);
    expect(() => Iterator.zipKeyed(true)).toThrow(TypeError);
    expect(() => Iterator.zipKeyed(null)).toThrow(TypeError);
    expect(() => Iterator.zipKeyed("hello")).toThrow(TypeError);
  });

  test("throws TypeError for non-iterable property value", () => {
    expect(() => Iterator.zipKeyed({ x: 42 }).toArray()).toThrow(TypeError);
    expect(() => Iterator.zipKeyed({ x: [1], y: null }).toArray()).toThrow(TypeError);
  });

  test("throws TypeError with no arguments", () => {
    expect(() => Iterator.zipKeyed()).toThrow(TypeError);
  });

  test("throws RangeError for invalid mode", () => {
    expect(() => Iterator.zipKeyed({ x: [1] }, { mode: "bad" })).toThrow(RangeError);
  });

  test("ignores options if undefined", () => {
    const result = Iterator.zipKeyed({ x: [1], y: [2] }, undefined).toArray();
    expect(result).toEqual([{ x: 1, y: 2 }]);
  });
});

describe("Iterator.zipKeyed() — longest mode", () => {
  test("continues until all iterables are exhausted", () => {
    const result = Iterator.zipKeyed(
      { x: [1, 2], y: ["a", "b", "c"] },
      { mode: "longest" }
    ).toArray();
    expect(result).toEqual([
      { x: 1, y: "a" },
      { x: 2, y: "b" },
      { x: undefined, y: "c" },
    ]);
  });

  test("fills with undefined when no padding provided", () => {
    const result = Iterator.zipKeyed(
      { x: [1], y: ["a", "b", "c"] },
      { mode: "longest" }
    ).toArray();
    expect(result).toEqual([
      { x: 1, y: "a" },
      { x: undefined, y: "b" },
      { x: undefined, y: "c" },
    ]);
  });

  test("fills with padding values from object", () => {
    const result = Iterator.zipKeyed(
      { x: [1, 2], y: ["a", "b", "c", "d"] },
      { mode: "longest", padding: { x: 0, y: "?" } }
    ).toArray();
    expect(result).toEqual([
      { x: 1, y: "a" },
      { x: 2, y: "b" },
      { x: 0, y: "c" },
      { x: 0, y: "d" },
    ]);
  });

  test("padding missing keys use undefined", () => {
    const result = Iterator.zipKeyed(
      { x: [1], y: [2], z: [3, 4] },
      { mode: "longest", padding: { x: 0 } }
    ).toArray();
    expect(result).toEqual([
      { x: 1, y: 2, z: 3 },
      { x: 0, y: undefined, z: 4 },
    ]);
  });

  test("all same length behaves like shortest mode", () => {
    const result = Iterator.zipKeyed(
      { a: [1, 2], b: [3, 4] },
      { mode: "longest" }
    ).toArray();
    expect(result).toEqual([{ a: 1, b: 3 }, { a: 2, b: 4 }]);
  });

  test("empty iterables with longest mode yield nothing", () => {
    const result = Iterator.zipKeyed(
      { x: [], y: [] },
      { mode: "longest" }
    ).toArray();
    expect(result).toEqual([]);
  });
});

describe("Iterator.zipKeyed() — strict mode", () => {
  test("works when all iterables have same length", () => {
    const result = Iterator.zipKeyed(
      { name: ["Alice", "Bob"], age: [30, 25] },
      { mode: "strict" }
    ).toArray();
    expect(result).toEqual([
      { name: "Alice", age: 30 },
      { name: "Bob", age: 25 },
    ]);
  });

  test("throws TypeError when iterables have different lengths", () => {
    expect(() => {
      Iterator.zipKeyed(
        { x: [1, 2], y: [3, 4, 5] },
        { mode: "strict" }
      ).toArray();
    }).toThrow(TypeError);
  });

  test("throws TypeError when first is shorter", () => {
    expect(() => {
      Iterator.zipKeyed(
        { a: [1], b: [2, 3] },
        { mode: "strict" }
      ).toArray();
    }).toThrow(TypeError);
  });

  test("empty iterables in strict mode are fine", () => {
    const result = Iterator.zipKeyed(
      { x: [], y: [] },
      { mode: "strict" }
    ).toArray();
    expect(result).toEqual([]);
  });
});

describe("Iterator.zipKeyed() — table data example", () => {
  test("processes tabular data", () => {
    const table = {
      name: ["Caroline", "Danielle", "Evelyn"],
      age: [30, 25, 35],
      city: ["New York", "London", "Hong Kong"],
    };

    const result = [];
    for (const { name, age, city } of Iterator.zipKeyed(table)) {
      result.push(name + " (" + age + ", " + city + ")");
    }
    expect(result).toEqual([
      "Caroline (30, New York)",
      "Danielle (25, London)",
      "Evelyn (35, Hong Kong)",
    ]);
  });
});
