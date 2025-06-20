/*---
description: Number.parseInt functions work correctly
features: [Number.parseInt]
---*/

test("Number.parseInt", () => {
  expect(Number.parseInt("123")).toBe(123);
  expect(Number.parseInt("123.45")).toBe(123);
  expect(Number.parseInt("123abc")).toBe(123);

  expect(Number.parseInt("0x1A")).toBe(26);
  expect(Number.parseInt("0b1010")).toBe(10);
  expect(Number.parseInt("0o12")).toBe(10);
  expect(Number.parseInt("123", 10)).toBe(123);
  expect(Number.parseInt("123", 8)).toBe(83);
  expect(Number.parseInt("123", 16)).toBe(291);
  expect(Number.parseInt("123", 2)).toBe(5);
});
