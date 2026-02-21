/*---
description: structuredClone handles primitive values correctly
features: [structuredClone]
---*/

test("clones numbers", () => {
  expect(structuredClone(42)).toBe(42);
  expect(structuredClone(3.14)).toBe(3.14);
  expect(structuredClone(0)).toBe(0);
  expect(structuredClone(-0)).toBe(-0);
  expect(Number.isNaN(structuredClone(NaN))).toBe(true);
  expect(structuredClone(Infinity)).toBe(Infinity);
  expect(structuredClone(-Infinity)).toBe(-Infinity);
});

test("clones strings", () => {
  expect(structuredClone("hello")).toBe("hello");
  expect(structuredClone("")).toBe("");
});

test("clones booleans", () => {
  expect(structuredClone(true)).toBe(true);
  expect(structuredClone(false)).toBe(false);
});

test("clones null and undefined", () => {
  expect(structuredClone(null)).toBe(null);
  expect(structuredClone(undefined)).toBe(undefined);
});
