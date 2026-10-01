/*---
description: Spread syntax for function calls
features: [function-spread]
---*/

test("spread syntax for function calls", () => {
  const numbers = [42, 17, 89, 3, 56];

  const max = Math.max(...numbers);
  const min = Math.min(...numbers);

  expect(max).toBe(89);
  expect(min).toBe(3);
});

test("a wide spread call leaves no arguments behind for later calls", () => {
  const many = Array.from({ length: 100 }, (_, index) => index);

  expect(Math.max(...many)).toBe(99);
  expect(Math.max(1, 2)).toBe(2);
  expect(Math.max()).toBe(-Infinity);
  expect(Math.min(...many, -5)).toBe(-5);
  expect(Math.min(4)).toBe(4);
  expect(String.fromCharCode(...[72, 105])).toBe("Hi");
  expect(Math.max(...many.slice(0, 33))).toBe(32);
  expect(Math.max(7)).toBe(7);
});

test("a built-in called inside another built-in's callback keeps its own arguments", () => {
  const rows = [[3, 9, 1], [8, 2], [5]];
  const maxima = rows.map((row) => Math.max(...row, Math.min(...row)));

  expect(maxima).toEqual([9, 8, 5]);
  expect([1, 2, 3].map((value) => Math.pow(value, 2)).join(",")).toBe("1,4,9");
  expect(
    [4, 5].reduce((sum, value) => sum + Math.max(value, Math.abs(-10)), 0),
  ).toBe(20);
});
