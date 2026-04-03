/*---
description: RegExp.prototype.test
features: [RegExp.prototype.test]
---*/

test("test returns true when a match exists", () => {
  expect(/abc/.test("zabcz")).toBe(true);
});

test("test returns false when a match does not exist", () => {
  expect(/abc/.test("zzz")).toBe(false);
});

test("test respects global lastIndex", () => {
  const regex = /a/g;

  expect(regex.test("aba")).toBe(true);
  expect(regex.lastIndex).toBe(1);
  expect(regex.test("aba")).toBe(true);
  expect(regex.lastIndex).toBe(3);
  expect(regex.test("aba")).toBe(false);
  expect(regex.lastIndex).toBe(0);
});
