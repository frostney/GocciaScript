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

test("Symbol.matchAll returns a lazy iterator", () => {
  const regex = /a/g;
  const iter = regex[Symbol.matchAll]("aaa");

  const first = iter.next();
  expect(first.done).toBe(false);
  expect(first.value[0]).toBe("a");
  expect(first.value.index).toBe(0);

  const second = iter.next();
  expect(second.done).toBe(false);
  expect(second.value.index).toBe(1);

  const third = iter.next();
  expect(third.done).toBe(false);
  expect(third.value.index).toBe(2);

  const fourth = iter.next();
  expect(fourth.done).toBe(true);
});

test("Symbol.matchAll with global and sticky flags iterates all matches", () => {
  const regex = /a/gy;
  const matches = [...regex[Symbol.matchAll]("aab")];

  expect(matches.length).toBe(2);
  expect(matches[0][0]).toBe("a");
  expect(matches[1][0]).toBe("a");
});

test("Symbol.matchAll with sticky-only yields a single match", () => {
  const regex = /a/y;
  const matches = [...regex[Symbol.matchAll]("aaa")];

  // Sticky without global: iterator yields one match then stops
  expect(matches.length).toBe(1);
  expect(matches[0][0]).toBe("a");
  expect(matches[0].index).toBe(0);
});

test("Symbol.matchAll preserves lastIndex from cloned regex", () => {
  const regex = /a/g;
  regex.lastIndex = 1;

  const matches = [...regex[Symbol.matchAll]("aaa")];

  // Iterator starts from cloned lastIndex (1), so first match is at index 1
  expect(matches.length).toBe(2);
  expect(matches[0].index).toBe(1);
  expect(matches[1].index).toBe(2);

  // Original regex lastIndex is not mutated
  expect(regex.lastIndex).toBe(1);
});
