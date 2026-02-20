/*---
description: structuredClone throws on non-cloneable values
features: [structuredClone]
---*/

test("throws on function", () => {
  let threw = false;
  try {
    structuredClone(() => {});
  } catch (e) {
    threw = true;
    expect(e.message.includes("could not be cloned")).toBe(true);
  }
  expect(threw).toBe(true);
});

test("throws on symbol", () => {
  let threw = false;
  try {
    structuredClone(Symbol("test"));
  } catch (e) {
    threw = true;
    expect(e.message.includes("could not be cloned")).toBe(true);
  }
  expect(threw).toBe(true);
});

test("throws when no arguments provided", () => {
  let threw = false;
  try {
    structuredClone();
  } catch (e) {
    threw = true;
    expect(e.message.includes("1 argument required")).toBe(true);
  }
  expect(threw).toBe(true);
});

test("throws on object containing a function", () => {
  let threw = false;
  try {
    structuredClone({ fn: () => {} });
  } catch (e) {
    threw = true;
    expect(e.message.includes("could not be cloned")).toBe(true);
  }
  expect(threw).toBe(true);
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
