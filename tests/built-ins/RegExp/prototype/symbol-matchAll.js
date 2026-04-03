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

test("Symbol.match resets lastIndex on global regexes", () => {
  const regex = /a/g;

  regex.lastIndex = 1;
  expect(regex[Symbol.match]("aba")).toEqual(["a", "a"]);
  expect(regex.lastIndex).toBe(0);
});

test("Symbol.replace resets lastIndex on global regexes", () => {
  const regex = /a/g;

  regex.lastIndex = 1;
  expect("aba".replace(regex, "x")).toBe("xbx");
  expect(regex.lastIndex).toBe(0);
});

test("Symbol.match respects sticky lastIndex and updates it", () => {
  const regex = /a/y;

  regex.lastIndex = 1;
  expect(regex[Symbol.match]("ba")[0]).toBe("a");
  expect(regex.lastIndex).toBe(2);
});

test("Symbol.replace respects sticky lastIndex and updates it", () => {
  const regex = /a/y;

  regex.lastIndex = 1;
  expect("ba".replace(regex, "x")).toBe("bx");
  expect(regex.lastIndex).toBe(2);
});
