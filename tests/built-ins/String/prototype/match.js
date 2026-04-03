/*---
description: String.prototype.match with regex arguments
features: [String.prototype.match]
---*/

test("match returns the first match array for non-global regexes", () => {
  const match = "zabcq".match(/(ab)(c)/);

  expect(match[0]).toBe("abc");
  expect(match[1]).toBe("ab");
  expect(match[2]).toBe("c");
  expect(match.index).toBe(1);
});

test("match returns all matched substrings for global regexes", () => {
  expect("ababa".match(/a./g)).toEqual(["ab", "ab"]);
});

test("match returns null when nothing matches", () => {
  expect("hello".match(/z+/)).toBe(null);
});

test("match dispatches through Symbol.match", () => {
  const matcher = {
    [Symbol.match](input) {
      expect(input).toBe("abc");
      return ["custom match"];
    },
  };

  expect("abc".match(matcher)).toEqual(["custom match"]);
});

test("match advances by code point for zero-length global unicode matches", () => {
  expect("😀".match(/(?:)/gu)).toEqual(["", ""]);
});
