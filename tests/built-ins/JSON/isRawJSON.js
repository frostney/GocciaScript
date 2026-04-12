/*---
description: JSON.isRawJSON checks whether a value was created by JSON.rawJSON
features: [JSON.isRawJSON, JSON.rawJSON]
---*/

test("JSON.isRawJSON returns true for raw JSON values", () => {
  expect(JSON.isRawJSON(JSON.rawJSON("123"))).toBe(true);
  expect(JSON.isRawJSON(JSON.rawJSON('"hello"'))).toBe(true);
  expect(JSON.isRawJSON(JSON.rawJSON("true"))).toBe(true);
  expect(JSON.isRawJSON(JSON.rawJSON("false"))).toBe(true);
  expect(JSON.isRawJSON(JSON.rawJSON("null"))).toBe(true);
});

test("JSON.isRawJSON returns false for primitives", () => {
  expect(JSON.isRawJSON(123)).toBe(false);
  expect(JSON.isRawJSON("hello")).toBe(false);
  expect(JSON.isRawJSON(true)).toBe(false);
  expect(JSON.isRawJSON(false)).toBe(false);
  expect(JSON.isRawJSON(null)).toBe(false);
  expect(JSON.isRawJSON(undefined)).toBe(false);
});

test("JSON.isRawJSON returns false for regular objects", () => {
  expect(JSON.isRawJSON({})).toBe(false);
  expect(JSON.isRawJSON([])).toBe(false);
  expect(JSON.isRawJSON({ rawJSON: "123" })).toBe(false);
});

test("JSON.isRawJSON returns false for objects mimicking raw JSON structure", () => {
  const fake = Object.create(null);
  fake.rawJSON = "123";
  Object.freeze(fake);
  expect(JSON.isRawJSON(fake)).toBe(false);
});

test("JSON.isRawJSON returns false for symbols and functions", () => {
  expect(JSON.isRawJSON(Symbol("test"))).toBe(false);
  expect(JSON.isRawJSON(() => {})).toBe(false);
});

test("JSON.isRawJSON returns false when called with no arguments", () => {
  expect(JSON.isRawJSON()).toBe(false);
});
