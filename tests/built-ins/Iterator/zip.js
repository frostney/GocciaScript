/*---
description: Iterator.zip() combines multiple iterables into an iterator of arrays
features: [Iterator, Iterator.zip]
---*/

describe("Iterator.zip()", () => {
  test("zips two arrays of equal length", () => {
    const result = Iterator.zip([[1, 2, 3], ["a", "b", "c"]]).toArray();
    expect(result).toEqual([[1, "a"], [2, "b"], [3, "c"]]);
  });

  test("zips three arrays of equal length", () => {
    const result = Iterator.zip([[1, 2], ["a", "b"], [true, false]]).toArray();
    expect(result).toEqual([[1, "a", true], [2, "b", false]]);
  });

  test("zips a single array", () => {
    const result = Iterator.zip([[1, 2, 3]]).toArray();
    expect(result).toEqual([[1], [2], [3]]);
  });

  test("zips empty iterables yields nothing", () => {
    const result = Iterator.zip([]).toArray();
    expect(result).toEqual([]);
  });

  test("zips empty arrays", () => {
    const result = Iterator.zip([[], []]).toArray();
    expect(result).toEqual([]);
  });

  test("default mode is shortest — stops at shortest iterable", () => {
    const result = Iterator.zip([[1, 2, 3], ["a", "b"]]).toArray();
    expect(result).toEqual([[1, "a"], [2, "b"]]);
  });

  test("explicit shortest mode", () => {
    const result = Iterator.zip([[1, 2, 3], ["x"]], { mode: "shortest" }).toArray();
    expect(result).toEqual([[1, "x"]]);
  });

  test("zips Sets", () => {
    const a = new Set([1, 2]);
    const b = new Set([3, 4]);
    const result = Iterator.zip([a, b]).toArray();
    expect(result).toEqual([[1, 3], [2, 4]]);
  });

  test("zips Maps (yields entries)", () => {
    const a = new Map([["x", 1]]);
    const b = new Map([["y", 2]]);
    const result = Iterator.zip([a, b]).toArray();
    expect(result[0][0][0]).toBe("x");
    expect(result[0][0][1]).toBe(1);
    expect(result[0][1][0]).toBe("y");
    expect(result[0][1][1]).toBe(2);
  });

  test("zips mixed iterable types", () => {
    const arr = [1, 2];
    const set = new Set(["a", "b"]);
    const result = Iterator.zip([arr, set]).toArray();
    expect(result).toEqual([[1, "a"], [2, "b"]]);
  });

  test("zips strings (character iteration)", () => {
    const result = Iterator.zip(["ab", "cd"]).toArray();
    expect(result).toEqual([["a", "c"], ["b", "d"]]);
  });

  test("result is an iterator (has next and Symbol.iterator)", () => {
    const iter = Iterator.zip([[1], [2]]);
    expect(typeof iter.next).toBe("function");
    expect(iter[Symbol.iterator]()).toBe(iter);
  });

  test("returns done after exhaustion", () => {
    const iter = Iterator.zip([[1], [2]]);
    expect(iter.next()).toEqual({ value: [1, 2], done: false });
    expect(iter.next()).toEqual({ value: undefined, done: true });
    expect(iter.next()).toEqual({ value: undefined, done: true });
  });

  test("works with for-of", () => {
    const result = [];
    for (const pair of Iterator.zip([[1, 2], ["a", "b"]])) {
      result.push(pair);
    }
    expect(result).toEqual([[1, "a"], [2, "b"]]);
  });

  test("works with destructuring in for-of", () => {
    const names = [];
    const ages = [];
    for (const [name, age] of Iterator.zip([["Alice", "Bob"], [30, 25]])) {
      names.push(name);
      ages.push(age);
    }
    expect(names).toEqual(["Alice", "Bob"]);
    expect(ages).toEqual([30, 25]);
  });

  test("result works with iterator helper methods", () => {
    const result = Iterator.zip([[1, 2, 3], [10, 20, 30]])
      .map(([a, b]) => a + b)
      .toArray();
    expect(result).toEqual([11, 22, 33]);
  });

  test("zips user-defined iterables", () => {
    const range = (start, end) => ({
      [Symbol.iterator]() {
        let i = start;
        return {
          next() {
            if (i <= end) {
              const v = i;
              i = i + 1;
              return { value: v, done: false };
            }
            return { value: undefined, done: true };
          },
        };
      },
    });

    const result = Iterator.zip([range(1, 3), range(10, 12)]).toArray();
    expect(result).toEqual([[1, 10], [2, 11], [3, 12]]);
  });

  test("throws TypeError for non-iterable first argument", () => {
    expect(() => Iterator.zip(42)).toThrow(TypeError);
    expect(() => Iterator.zip(true)).toThrow(TypeError);
    expect(() => Iterator.zip(null)).toThrow(TypeError);
  });

  test("throws TypeError for non-iterable item in iterables", () => {
    expect(() => Iterator.zip([[1, 2], 42]).toArray()).toThrow(TypeError);
    expect(() => Iterator.zip([[1], null]).toArray()).toThrow(TypeError);
  });

  test("throws TypeError with no arguments", () => {
    expect(() => Iterator.zip()).toThrow(TypeError);
  });

  test("throws RangeError for invalid mode", () => {
    expect(() => Iterator.zip([[1]], { mode: "invalid" })).toThrow(RangeError);
  });

  test("ignores options if undefined", () => {
    const result = Iterator.zip([[1, 2], [3, 4]], undefined).toArray();
    expect(result).toEqual([[1, 3], [2, 4]]);
  });
});

describe("Iterator.zip() — longest mode", () => {
  test("continues until all iterables are exhausted", () => {
    const result = Iterator.zip([[1, 2], ["a", "b", "c"]], { mode: "longest" }).toArray();
    expect(result).toEqual([[1, "a"], [2, "b"], [undefined, "c"]]);
  });

  test("fills with undefined when no padding provided", () => {
    const result = Iterator.zip([[1], ["a", "b", "c"]], { mode: "longest" }).toArray();
    expect(result).toEqual([[1, "a"], [undefined, "b"], [undefined, "c"]]);
  });

  test("fills with padding values", () => {
    const result = Iterator.zip(
      [[1, 2], ["a", "b", "c", "d"]],
      { mode: "longest", padding: [0, "?"] }
    ).toArray();
    expect(result).toEqual([[1, "a"], [2, "b"], [0, "c"], [0, "d"]]);
  });

  test("padding shorter than iterables count uses undefined for extra positions", () => {
    const result = Iterator.zip(
      [[1], [2], [3, 4]],
      { mode: "longest", padding: [0] }
    ).toArray();
    expect(result).toEqual([[1, 2, 3], [0, undefined, 4]]);
  });

  test("all same length behaves like shortest mode", () => {
    const result = Iterator.zip(
      [[1, 2], [3, 4]],
      { mode: "longest" }
    ).toArray();
    expect(result).toEqual([[1, 3], [2, 4]]);
  });

  test("empty iterables with longest mode yield nothing", () => {
    const result = Iterator.zip([[], []], { mode: "longest" }).toArray();
    expect(result).toEqual([]);
  });

  test("one empty and one non-empty with padding", () => {
    const result = Iterator.zip(
      [[], [1, 2]],
      { mode: "longest", padding: [99, 0] }
    ).toArray();
    expect(result).toEqual([[99, 1], [99, 2]]);
  });
});

describe("Iterator.zip() — strict mode", () => {
  test("works when all iterables have same length", () => {
    const result = Iterator.zip(
      [[1, 2, 3], ["a", "b", "c"]],
      { mode: "strict" }
    ).toArray();
    expect(result).toEqual([[1, "a"], [2, "b"], [3, "c"]]);
  });

  test("throws TypeError when iterables have different lengths", () => {
    expect(() => {
      Iterator.zip([[1, 2], ["a", "b", "c"]], { mode: "strict" }).toArray();
    }).toThrow(TypeError);
  });

  test("throws TypeError when first is shorter", () => {
    expect(() => {
      Iterator.zip([[1], [2, 3]], { mode: "strict" }).toArray();
    }).toThrow(TypeError);
  });

  test("throws TypeError when second is shorter", () => {
    expect(() => {
      Iterator.zip([[1, 2], [3]], { mode: "strict" }).toArray();
    }).toThrow(TypeError);
  });

  test("empty iterables in strict mode are fine", () => {
    const result = Iterator.zip([[], []], { mode: "strict" }).toArray();
    expect(result).toEqual([]);
  });

  test("single element strict mode", () => {
    const result = Iterator.zip([[42], [99]], { mode: "strict" }).toArray();
    expect(result).toEqual([[42, 99]]);
  });
});

describe("Iterator.zip() — creating Map from two arrays", () => {
  test("creates a Map from keys and values arrays", () => {
    const keys = ["Mon", "Tue", "Wed"];
    const vals = [22, 21, 23];
    const entries = Iterator.zip([keys, vals]).toArray();
    const map = new Map(entries);
    expect(map.get("Mon")).toBe(22);
    expect(map.get("Tue")).toBe(21);
    expect(map.get("Wed")).toBe(23);
    expect(map.size).toBe(3);
  });
});
