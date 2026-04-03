/*---
description: RegExp.prototype[Symbol.matchAll]
features: [RegExp.prototype[Symbol.matchAll]]
---*/

test("Symbol.matchAll returns a single match for non-global regexes", () => {
  const regex = /a/;
  const matches = [];

  for (const match of regex[Symbol.matchAll]("ba")) {
    matches.push([match[0], match.index]);
  }

  expect(matches).toEqual([["a", 1]]);
});

test("Symbol.matchAll does not mutate the original regex lastIndex", () => {
  const regex = /a/g;

  regex.lastIndex = 1;
  for (const _match of regex[Symbol.matchAll]("aba")) {
  }

  expect(regex.lastIndex).toBe(1);
});
