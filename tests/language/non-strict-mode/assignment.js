/*---
description: Non-strict compatibility assignment semantics
features: [compat-non-strict-mode]
---*/

describe("non-strict assignment", () => {
  test("read-only property writes silently fail and return the assigned value", () => {
    const obj = {};
    Object.defineProperty(obj, "fixed", {
      value: 1,
      writable: false,
      configurable: true,
    });

    const result = (obj.fixed = 2);

    expect(result).toBe(2);
    expect(obj.fixed).toBe(1);
  });

  test("computed read-only property writes silently fail", () => {
    const obj = {};
    const key = "fixed";
    Object.defineProperty(obj, key, {
      value: 1,
      writable: false,
      configurable: true,
    });

    const result = (obj[key] = 2);

    expect(result).toBe(2);
    expect(obj.fixed).toBe(1);
  });

  test("compound read-only property writes return the computed value", () => {
    const obj = {};
    Object.defineProperty(obj, "fixed", {
      value: 1,
      writable: false,
      configurable: true,
    });

    const result = (obj.fixed += 3);

    expect(result).toBe(4);
    expect(obj.fixed).toBe(1);
  });

  test("update read-only property writes return update expression values", () => {
    const obj = {};
    Object.defineProperty(obj, "fixed", {
      value: 1,
      writable: false,
      configurable: true,
    });

    const postfix = obj.fixed++;
    const prefix = ++obj.fixed;

    expect(postfix).toBe(1);
    expect(prefix).toBe(2);
    expect(obj.fixed).toBe(1);
  });

  test("with object writes use non-strict object-environment assignment", () => {
    const obj = {};
    let result = 0;
    Object.defineProperty(obj, "value", {
      value: 1,
      writable: false,
      configurable: true,
    });

    with (obj) {
      result = (value = 2);
    }

    expect(result).toBe(2);
    expect(obj.value).toBe(1);
  });

  test("global object identifier writes silently fail for non-writable properties", () => {
    Object.defineProperty(globalThis, "__gocciaNonStrictFixedAssignment", {
      value: 1,
      writable: false,
      configurable: true,
    });

    const result = (__gocciaNonStrictFixedAssignment = 2);

    expect(result).toBe(2);
    expect(globalThis.__gocciaNonStrictFixedAssignment).toBe(1);

    delete globalThis.__gocciaNonStrictFixedAssignment;
  });

  test("non-extensible object additions silently fail", () => {
    const obj = {};
    Object.preventExtensions(obj);

    const result = (obj.added = 1);

    expect(result).toBe(1);
    expect(Object.hasOwn(obj, "added")).toBe(false);
  });
});
