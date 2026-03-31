/*---
description: Basic for...of loop with arrays
features: [for-of]
---*/

describe("basic for...of", () => {
  test("iterates over array elements", () => {
    const result = [];
    for (const item of [1, 2, 3]) {
      result.push(item);
    }
    expect(result).toEqual([1, 2, 3]);
  });

  test("iterates over string characters", () => {
    const result = [];
    for (const ch of "abc") {
      result.push(ch);
    }
    expect(result).toEqual(["a", "b", "c"]);
  });

  test("const binding per iteration", () => {
    const fns = [];
    for (const val of [10, 20, 30]) {
      fns.push(() => val);
    }
    expect(fns[0]()).toBe(10);
    expect(fns[1]()).toBe(20);
    expect(fns[2]()).toBe(30);
  });

  test("let binding allows mutation", () => {
    const result = [];
    for (let val of [1, 2, 3]) {
      val = val * 10;
      result.push(val);
    }
    expect(result).toEqual([10, 20, 30]);
  });

  test("empty array produces no iterations", () => {
    const result = [];
    for (const item of []) {
      result.push(item);
    }
    expect(result).toEqual([]);
  });

  test("iterates over string characters including whitespace", () => {
    const result = [];
    for (const ch of "a b") {
      result.push(ch);
    }
    expect(result).toEqual(["a", " ", "b"]);
  });

  test("iterates arrays containing special values", () => {
    const result = [];
    for (const item of [undefined, null, NaN, "value"]) {
      result.push(item);
    }

    expect(result).toHaveLength(4);
    expect(result[0]).toBeUndefined();
    expect(result[1]).toBeNull();
    expect(result[2]).toBeNaN();
    expect(result[3]).toBe("value");
  });

  test("let binding creates a fresh binding per iteration", () => {
    const fns = [];
    for (let val of [10, 20, 30]) {
      fns.push(() => val);
    }

    expect(fns[0]()).toBe(10);
    expect(fns[1]()).toBe(20);
    expect(fns[2]()).toBe(30);
  });
});
