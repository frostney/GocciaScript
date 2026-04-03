/*---
description: String.prototype.matchAll with regex arguments
features: [String.prototype.matchAll]
---*/

test("matchAll returns an iterator of match arrays", () => {
  const matches = [];

  for (const match of "ababa".matchAll(/(a)(b)/g)) {
    matches.push([match[0], match[1], match[2], match.index]);
  }

  expect(matches).toEqual([
    ["ab", "a", "b", 0],
    ["ab", "a", "b", 2],
  ]);
});

test("matchAll requires a global regex", () => {
  expect(() => {
    "hello".matchAll(/l/);
  }).toThrow(TypeError);
});

test("matchAll dispatches through Symbol.matchAll", () => {
  const matcher = {
    [Symbol.matchAll](input) {
      expect(input).toBe("abc");
      return ["custom matchAll"];
    },
  };

  expect("abc".matchAll(matcher)).toEqual(["custom matchAll"]);
});
