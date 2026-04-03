/*---
description: RegExp constructor
features: [RegExp]
---*/

test("RegExp constructor creates regex objects with flags and properties", () => {
  const regex = new RegExp("ab", "gi");

  expect(regex.source).toBe("ab");
  expect(regex.flags).toBe("gi");
  expect(regex.global).toBe(true);
  expect(regex.ignoreCase).toBe(true);
  expect(regex.multiline).toBe(false);
  expect(regex.dotAll).toBe(false);
  expect(regex.sticky).toBe(false);
  expect(regex.lastIndex).toBe(0);
});

test("RegExp canonicalizes flags and exposes unicode", () => {
  const regex = new RegExp("ab", "yugim");

  expect(regex.flags).toBe("gimuy");
  expect(regex.global).toBe(true);
  expect(regex.ignoreCase).toBe(true);
  expect(regex.multiline).toBe(true);
  expect(regex.unicode).toBe(true);
  expect(regex.sticky).toBe(true);
  expect(regex.toString()).toBe("/ab/gimuy");
});

test("RegExp can be called without new", () => {
  const regex = RegExp("ab", "g");

  expect(regex.test("zabz")).toBe(true);
  expect(regex.flags).toBe("g");
});

test("RegExp clones another regex when passed a regex argument", () => {
  const original = /test/gi;
  const cloned = new RegExp(original);

  expect(cloned.source).toBe("test");
  expect(cloned.flags).toBe("gi");
  expect(cloned.toString()).toBe("/test/gi");
});

test("RegExp called without new returns the same regex when flags are omitted", () => {
  const original = /test/gi;
  const result = RegExp(original);

  expect(result).toBe(original);
});

test("new RegExp clones regex arguments even when flags are omitted", () => {
  const original = /test/gi;
  const cloned = new RegExp(original);

  expect(cloned).not.toBe(original);
  expect(cloned.source).toBe("test");
  expect(cloned.flags).toBe("gi");
});

test("RegExp throws SyntaxError for invalid flags", () => {
  expect(() => {
    new RegExp("a", "gg");
  }).toThrow(SyntaxError);

  expect(() => {
    new RegExp("a", "z");
  }).toThrow(SyntaxError);
});

test("RegExp throws SyntaxError for invalid patterns at construction time", () => {
  expect(() => {
    new RegExp("[");
  }).toThrow(SyntaxError);
});
