/*---
description: String.prototype.charCodeAt
features: [String.prototype.charCodeAt]
---*/

test("charCodeAt", () => {
  expect("hello".charCodeAt(0)).toBe(104);
  expect("hello".charCodeAt(1)).toBe(101);
  expect("hello".charCodeAt(2)).toBe(108);
  expect("hello".charCodeAt(3)).toBe(108);
  expect("hello".charCodeAt(4)).toBe(111);
  expect("hello".charCodeAt(5)).toBeNaN(); // out-of-bounds returns NaN
});

test("charCodeAt with empty string", () => {
  expect("".charCodeAt(0)).toBeNaN(); // empty string out-of-bounds returns NaN
});

test("charCodeAt with edge cases", () => {
  expect("hello".charCodeAt(undefined)).toBe(104); // 'h'
  expect("hello".charCodeAt(null)).toBe(104); // 'h'
  expect("hello".charCodeAt(NaN)).toBe(104); // 'h'

  expect("hello".charCodeAt(Infinity)).toBeNaN();
  expect("hello".charCodeAt(-Infinity)).toBeNaN();
});

test("charCodeAt throws TypeError for Symbol argument", () => {
  expect(() => "hello".charCodeAt(Symbol())).toThrow(TypeError);
});
