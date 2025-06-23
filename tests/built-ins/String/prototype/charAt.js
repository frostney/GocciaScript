/*---
description: String.prototype.charAt
features: [String.prototype.charAt]
---*/

test("charAt", () => {
  expect("hello".charAt(0)).toBe("h");
  expect("hello".charAt(1)).toBe("e");
  expect("hello".charAt(2)).toBe("l");
  expect("hello".charAt(3)).toBe("l");
  expect("hello".charAt(4)).toBe("o");
  expect("hello".charAt(5)).toBe("");
});

test("charAt vs direct string access behavior", () => {
  const str = "hello";

  // charAt and string indexing match for valid indices
  expect(str.charAt(0)).toBe(str[0]);
  expect(str.charAt(1)).toBe(str[1]);
  expect(str.charAt(2)).toBe(str[2]);
  expect(str.charAt(3)).toBe(str[3]);
  expect(str.charAt(4)).toBe(str[4]);

  // charAt and string indexing differ for out-of-bounds indices
  expect(str.charAt(5)).toBe(""); // charAt returns empty string
  expect(str[5]).toBe(undefined); // indexing returns undefined
});

test("charAt with negative index", () => {
  expect("hello".charAt(-1)).toBe("");
});

test("charAt with empty string", () => {
  expect("".charAt(0)).toBe("");
});

test("charAt with edge cases", () => {
  // undefined, null, NaN convert to 0, so they return first character
  expect("hello".charAt(undefined)).toBe("h");
  expect("hello".charAt(null)).toBe("h");
  expect("hello".charAt(NaN)).toBe("h");

  expect("hello".charAt(Infinity)).toBe("");
  expect("hello".charAt(-Infinity)).toBe("");
});
