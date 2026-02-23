/*---
description: structuredClone throws on non-cloneable values
features: [structuredClone]
---*/

test("throws DOMException with DataCloneError on function", () => {
  try {
    structuredClone(() => {});
  } catch (e) {
    expect(e instanceof DOMException).toBe(true);
    expect(e.name).toBe("DataCloneError");
    expect(e.code).toBe(25);
    expect(e.message.includes("could not be cloned")).toBe(true);
    return;
  }
  expect(true).toBe(false);
});

test("throws DOMException with DataCloneError on symbol", () => {
  try {
    structuredClone(Symbol("test"));
  } catch (e) {
    expect(e instanceof DOMException).toBe(true);
    expect(e.name).toBe("DataCloneError");
    expect(e.code).toBe(25);
    return;
  }
  expect(true).toBe(false);
});

test("throws when no arguments provided", () => {
  expect(() => structuredClone()).toThrow(TypeError);
});

test("throws DOMException with DataCloneError on object containing a function", () => {
  try {
    structuredClone({ fn: () => {} });
  } catch (e) {
    expect(e instanceof DOMException).toBe(true);
    expect(e.name).toBe("DataCloneError");
    expect(e.code).toBe(25);
    return;
  }
  expect(true).toBe(false);
});

test("clones object with accessor property by reading getter value", () => {
  const obj = {};
  Object.defineProperty(obj, "value", {
    get() {
      return 42;
    },
    enumerable: true,
    configurable: true,
  });
  const clone = structuredClone(obj);
  expect(clone.value).toBe(42);
  expect(clone !== obj).toBe(true);
});

test("second argument (options) is accepted and ignored", () => {
  const original = { a: 1 };
  const clone = structuredClone(original, { transfer: [] });
  expect(clone.a).toBe(1);
  expect(clone !== original).toBe(true);
});

test("second argument with arbitrary value does not throw", () => {
  expect(structuredClone(42, undefined)).toBe(42);
  expect(structuredClone("hello", null)).toBe("hello");
  expect(structuredClone(true, {})).toBe(true);
});
