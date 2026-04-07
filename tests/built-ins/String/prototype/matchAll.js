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

test("matchAll without an argument uses an empty global regex", () => {
  const matches = [];

  for (const match of "ab".matchAll()) {
    matches.push(match[0]);
  }

  expect(matches).toEqual(["", "", ""]);
});

test("matchAll coerces string arguments to a global regex", () => {
  const matches = [];

  for (const match of "hello".matchAll("l")) {
    matches.push([match[0], match.index]);
  }

  expect(matches).toEqual([
    ["l", 2],
    ["l", 3],
  ]);
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

test("matchAll is lazy - partial consumption does not compute all matches", () => {
  const iter = "aaa".matchAll(/a/g);
  const first = iter.next();
  expect(first.done).toBe(false);
  expect(first.value[0]).toBe("a");
  expect(first.value.index).toBe(0);

  const second = iter.next();
  expect(second.done).toBe(false);
  expect(second.value.index).toBe(1);
});

test("matchAll iterator returns done after all matches consumed", () => {
  const iter = "ab".matchAll(/a/g);
  const first = iter.next();
  expect(first.done).toBe(false);
  expect(first.value[0]).toBe("a");

  const second = iter.next();
  expect(second.done).toBe(true);
  expect(second.value).toBe(undefined);
});

test("matchAll iterator works with spread", () => {
  const matches = [...("hello".matchAll(/l/g))];
  expect(matches.length).toBe(2);
  expect(matches[0][0]).toBe("l");
  expect(matches[0].index).toBe(2);
  expect(matches[1].index).toBe(3);
});

test("matchAll iterator works with for-of", () => {
  const indices = [];
  for (const match of "abab".matchAll(/a/g)) {
    indices.push(match.index);
  }
  expect(indices).toEqual([0, 2]);
});

test("matchAll with capture groups in lazy iteration", () => {
  const iter = "a1b2".matchAll(/([a-z])(\d)/g);

  const first = iter.next();
  expect(first.value[0]).toBe("a1");
  expect(first.value[1]).toBe("a");
  expect(first.value[2]).toBe("1");

  const second = iter.next();
  expect(second.value[0]).toBe("b2");
  expect(second.value[1]).toBe("b");
  expect(second.value[2]).toBe("2");

  const third = iter.next();
  expect(third.done).toBe(true);
});

test("matchAll iterator does not mutate the original regex", () => {
  const regex = /a/g;
  regex.lastIndex = 2;

  const iter = "aaa".matchAll(regex);
  iter.next();
  iter.next();

  expect(regex.lastIndex).toBe(2);
});
