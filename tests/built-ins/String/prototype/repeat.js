/*---
description: String.prototype.repeat works correctly
features: [String.prototype.repeat]
---*/

test("String.prototype.repeat repeats strings", () => {
  expect("abc".repeat(3)).toBe("abcabcabc");
  expect("x".repeat(0)).toBe("");
  expect("x".repeat(-0)).toBe("");
  expect("test".repeat(1)).toBe("test");
  expect("".repeat(1)).toBe("");
});

test("String.prototype.repeat throws RangeError for negative count", () => {
  expect(() => "abc".repeat(-1)).toThrow(RangeError);
  expect(() => "x".repeat(-5)).toThrow(RangeError);
});

test("String.prototype.repeat throws RangeError for Infinity", () => {
  expect(() => "abc".repeat(Infinity)).toThrow(RangeError);
  expect(() => "x".repeat(-Infinity)).toThrow(RangeError);
});

test("String.prototype.repeat treats NaN as 0", () => {
  expect("abc".repeat(NaN)).toBe("");
});
