/*---
description: Dense integer appends preserve ordinary array assignment semantics
features: [compat-non-strict-mode]
---*/

describe("non-strict dense array append", () => {
  test("appends consecutive integer indices", () => {
    const values = [];
    let length = 0;

    for (const value of [2, 3, 4, 5, 6]) {
      values[length++] = value;
    }

    expect(values.length).toBe(5);
    expect(values[0]).toBe(2);
    expect(values[4]).toBe(6);
  });

  test("calls an inherited indexed setter", () => {
    let observed = 0;
    Object.defineProperty(Array.prototype, "0", {
      configurable: true,
      set(value) {
        observed = value;
      },
    });

    try {
      const values = [];
      values[0] = 42;

      expect(observed).toBe(42);
      expect(values.length).toBe(0);
      expect(Object.hasOwn(values, "0")).toBe(false);
    } finally {
      delete Array.prototype[0];
    }
  });

  test("respects a custom prototype chain", () => {
    let observed = 0;
    const prototype = {};
    Object.defineProperty(prototype, "0", {
      configurable: true,
      set(value) {
        observed = value;
      },
    });

    const values = [];
    Object.setPrototypeOf(values, prototype);
    values[0] = 23;

    expect(observed).toBe(23);
    expect(values.length).toBe(0);
    expect(Object.hasOwn(values, "0")).toBe(false);
  });

  test("does not append to a non-extensible array", () => {
    const values = [];
    Object.preventExtensions(values);
    values[0] = 42;

    expect(values.length).toBe(0);
    expect(Object.hasOwn(values, "0")).toBe(false);
  });

  test("respects a non-writable length", () => {
    const values = [];
    Object.defineProperty(values, "length", { writable: false });
    values[0] = 42;

    expect(values.length).toBe(0);
    expect(Object.hasOwn(values, "0")).toBe(false);
  });
});
