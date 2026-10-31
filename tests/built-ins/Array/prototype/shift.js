/*---
description: Array.prototype.shift
features: [Array.prototype.shift]
---*/

describe("Array.prototype.shift", () => {
  test("removes and returns the first element", () => {
    const arr = [1, 2, 3];
    const first = arr.shift();
    expect(first).toBe(1);
    expect(arr).toEqual([2, 3]);
  });

  test("returns undefined for empty arrays", () => {
    const arr = [];
    expect(arr.shift()).toBe(undefined);
  });

  test("works with single-element arrays", () => {
    const arr = ["only"];
    expect(arr.shift()).toBe("only");
    expect(arr.length).toBe(0);
  });

  test("generic receiver removes first from array-like", () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
    const removed = Array.prototype.shift.call(obj);
    expect(removed).toBe('a');
    expect(obj[0]).toBe('b');
    expect(obj[1]).toBe('c');
    expect(obj.length).toBe(2);
    expect(obj.hasOwnProperty('2')).toBe(false);
  });

  test("shift on an array with a named property writes own elements without calling an inherited setter", () => {
    let calls = "";
    Object.defineProperty(Array.prototype, "0", {
      set(value) { calls += value; },
      configurable: true,
    });
    try {
      const arr = [1, 2, 3];
      arr.named = true;
      arr.shift();
      expect(calls).toBe("");
      expect(Array.from(arr)).toEqual([2, 3]);
    } finally {
      delete Array.prototype[0];
    }
  });

  test("shift on an array with a named property writes own elements past an inherited read-only index", () => {
    Object.defineProperty(Array.prototype, "0", {
      value: "ro",
      writable: false,
      configurable: true,
    });
    try {
      const arr = [1, 2, 3];
      arr.named = true;
      arr.shift();
      expect(Array.from(arr)).toEqual([2, 3]);
    } finally {
      delete Array.prototype[0];
    }
  });
});
