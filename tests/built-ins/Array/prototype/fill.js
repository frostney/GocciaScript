/*---
description: Array.prototype.fill
features: [Array.prototype.fill]
---*/

describe("Array.prototype.fill", () => {
  test("fill entire array", () => {
    const arr = [1, 2, 3, 4];
    arr.fill(0);
    expect(arr).toEqual([0, 0, 0, 0]);
  });

  test("fill with start index", () => {
    const arr = [1, 2, 3, 4];
    arr.fill(0, 2);
    expect(arr).toEqual([1, 2, 0, 0]);
  });

  test("fill with start and end index", () => {
    const arr = [1, 2, 3, 4];
    arr.fill(0, 1, 3);
    expect(arr).toEqual([1, 0, 0, 4]);
  });

  test("fill with negative indices", () => {
    const arr = [1, 2, 3, 4];
    arr.fill(0, -2);
    expect(arr).toEqual([1, 2, 0, 0]);
  });

  test("fill returns the modified array", () => {
    const arr = [1, 2, 3];
    const result = arr.fill(0);
    expect(result).toBe(arr);
  });

  test("fill with start >= end does nothing", () => {
    const arr = [1, 2, 3];
    arr.fill(0, 2, 1);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("fill with start beyond length does nothing", () => {
    const arr = [1, 2, 3];
    arr.fill(0, 100);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("fill with negative start and end", () => {
    const arr = [1, 2, 3, 4, 5];
    arr.fill(0, -3, -1);
    expect(arr).toEqual([1, 2, 0, 0, 5]);
  });

  test("fill on empty array", () => {
    const arr = [];
    arr.fill(0);
    expect(arr).toEqual([]);
  });

  test("fill with undefined value", () => {
    const arr = [1, 2, 3];
    arr.fill(undefined);
    expect(arr).toEqual([undefined, undefined, undefined]);
  });

  test("generic receiver fills array-like in place", () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
    Array.prototype.fill.call(obj, 'x', 1, 3);
    expect(obj[0]).toBe('a');
    expect(obj[1]).toBe('x');
    expect(obj[2]).toBe('x');
  });

  test("fill over a hole calls an inherited index setter", () => {
    const calls = [];
    Object.defineProperty(Array.prototype, "1", {
      set(value) { calls.push(value); },
      configurable: true,
    });
    try {
      const arr = [0, , 2];
      arr.fill(7);
      expect(calls).toEqual([7]);
      expect(Object.hasOwn(arr, 1)).toBe(false);
      expect(arr[0]).toBe(7);
      expect(arr[2]).toBe(7);
    } finally {
      delete Array.prototype[1];
    }
  });

  test("fill over a hole throws for an inherited read-only index", () => {
    Object.defineProperty(Object.prototype, "1", {
      value: 9,
      writable: false,
      configurable: true,
    });
    try {
      const arr = [0, , 2];
      expect(() => arr.fill(5)).toThrow(TypeError);
      expect(Object.hasOwn(arr, 1)).toBe(false);
    } finally {
      delete Object.prototype[1];
    }
  });

  test("fill writes own elements without consulting the prototype chain", () => {
    Object.defineProperty(Object.prototype, "0", {
      value: 9,
      writable: false,
      configurable: true,
    });
    try {
      const arr = [1, 2, 3];
      arr.fill(5);
      expect(arr).toEqual([5, 5, 5]);
    } finally {
      delete Object.prototype[0];
    }
  });

  test("fill reaches a Proxy prototype only for a hole", () => {
    const keys = [];
    const arr = [0, , 2];
    Object.setPrototypeOf(arr, new Proxy(Array.prototype, {
      set(target, key) {
        keys.push(key);
        return true;
      },
    }));
    arr.fill(6);
    expect(keys).toEqual(["1"]);
    expect(arr[0]).toBe(6);
    expect(Object.hasOwn(arr, 1)).toBe(false);
    expect(arr[2]).toBe(6);
  });

  test("fill writes own elements of an array with a named property without calling an inherited setter", () => {
    let calls = "";
    Object.defineProperty(Array.prototype, "0", {
      set(value) { calls += value; },
      configurable: true,
    });
    try {
      const arr = [1, 2, 3];
      arr.named = true;
      arr.fill(9);
      expect(calls).toBe("");
      expect(arr).toEqual([9, 9, 9]);
    } finally {
      delete Array.prototype[0];
    }
  });

  test("fill writes own elements of an array with a named property past an inherited read-only index", () => {
    Object.defineProperty(Array.prototype, "1", {
      value: "ro",
      writable: false,
      configurable: true,
    });
    try {
      const arr = [1, 2, 3];
      arr.named = true;
      arr.fill(7);
      expect(arr).toEqual([7, 7, 7]);
    } finally {
      delete Array.prototype[1];
    }
  });

  test("fill over a hole of an array with a named property still calls an inherited setter", () => {
    let calls = "";
    Object.defineProperty(Array.prototype, "1", {
      set(value) { calls += value; },
      configurable: true,
    });
    try {
      const arr = [0, , 2];
      arr.named = true;
      arr.fill(4);
      expect(calls).toBe("4");
      expect(Object.hasOwn(arr, 1)).toBe(false);
      expect(arr[0]).toBe(4);
      expect(arr[2]).toBe(4);
    } finally {
      delete Array.prototype[1];
    }
  });

  test("fill on a frozen array with a modified prototype throws", () => {
    Array.prototype[0] = "p";
    try {
      const arr = Object.freeze([1, 2]);
      expect(() => arr.fill(0)).toThrow(TypeError);
      expect(arr[0]).toBe(1);
    } finally {
      delete Array.prototype[0];
    }
  });
});
