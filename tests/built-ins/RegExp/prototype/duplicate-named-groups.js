/*---
description: ES2025 Duplicate named capture groups
features: [Duplicate Named Capture Groups]
---*/

test("duplicate named groups in different alternatives — first alternative matches", () => {
  const re = new RegExp("(?<year>[0-9][0-9][0-9][0-9])-[0-9][0-9]|[0-9][0-9]-(?<year>[0-9][0-9][0-9][0-9])");
  const result = re.exec("2026-04");
  expect(result.groups.year).toBe("2026");
});

test("duplicate named groups in different alternatives — second alternative matches", () => {
  const re = new RegExp("(?<year>[0-9][0-9][0-9][0-9])-[0-9][0-9]|[0-9][0-9]-(?<year>[0-9][0-9][0-9][0-9])");
  const result = re.exec("04-2026");
  expect(result.groups.year).toBe("2026");
});

test("non-participating duplicate group is undefined in groups object", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result = re.exec("b");
  expect(result.groups.x).toBe("b");
});

test("first alternative captures with duplicate names", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result = re.exec("a");
  expect(result.groups.x).toBe("a");
});

test("three alternatives with same named group", () => {
  const re = new RegExp("(?<val>\\d+)|(?<val>[a-z]+)|(?<val>[A-Z]+)");
  const result1 = re.exec("123");
  expect(result1.groups.val).toBe("123");
  const result2 = re.exec("abc");
  expect(result2.groups.val).toBe("abc");
  const result3 = re.exec("ABC");
  expect(result3.groups.val).toBe("ABC");
});

test("duplicate names inside non-capturing group", () => {
  const re = new RegExp("(?:(?<x>a)|(?<x>b))c");
  const result1 = re.exec("ac");
  expect(result1.groups.x).toBe("a");
  const result2 = re.exec("bc");
  expect(result2.groups.x).toBe("b");
});

test("multiple sets of duplicate names", () => {
  const re = new RegExp("(?<a>x)(?<b>1)|(?<a>y)(?<b>2)");
  const result1 = re.exec("x1");
  expect(result1.groups.a).toBe("x");
  expect(result1.groups.b).toBe("1");
  const result2 = re.exec("y2");
  expect(result2.groups.a).toBe("y");
  expect(result2.groups.b).toBe("2");
});

test("duplicate names with String.prototype.match", () => {
  const re = new RegExp("(?<n>\\d+)|(?<n>[a-z]+)");
  const result = "hello".match(re);
  expect(result.groups.n).toBe("hello");
});

test("duplicate names with String.prototype.replace and $<name>", () => {
  const re = new RegExp("(?<val>\\d+)|(?<val>[a-z]+)");
  const r1 = "42".replace(re, "[$<val>]");
  expect(r1).toBe("[42]");
  const r2 = "abc".replace(re, "[$<val>]");
  expect(r2).toBe("[abc]");
});

test("duplicate names with function replacer", () => {
  const re = new RegExp("(?<tag>\\w+)=(\\d+)|(?<tag>\\w+)");
  const result = "foo=42".replace(re, (match, g1, g2, g3, offset, str, groups) => {
    return groups.tag.toUpperCase();
  });
  expect(result).toBe("FOO");
});

test("duplicate names with matchAll", () => {
  const re = new RegExp("(?<v>\\d+)|(?<v>[a-z]+)", "g");
  const matches = [...("42 hello 7".matchAll(re))];
  expect(matches.length).toBe(3);
  expect(matches[0].groups.v).toBe("42");
  expect(matches[1].groups.v).toBe("hello");
  expect(matches[2].groups.v).toBe("7");
});

test("duplicate names in same alternative throws SyntaxError", () => {
  expect(() => {
    new RegExp("(?<x>a)(?<x>b)");
  }).toThrow();
});

test("duplicate names in nested same alternative throws SyntaxError", () => {
  expect(() => {
    new RegExp("(?<x>a(?<x>b))");
  }).toThrow();
});

test("backreference with \\k<name> in same alternative as duplicate group", () => {
  const re = new RegExp("(?<x>a)\\k<x>|(?<x>b)\\k<x>");
  const result1 = re.exec("aa");
  expect(result1.groups.x).toBe("a");
  const result2 = re.exec("bb");
  expect(result2.groups.x).toBe("b");
});

test("groups object has null prototype", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result = re.exec("a");
  expect(result.groups.toString).toBe(undefined);
});

test("non-matching input returns null", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result = re.exec("c");
  expect(result).toBe(null);
});

test("duplicate names with global flag collects all matches", () => {
  const re = new RegExp("(?<d>\\d+)|(?<d>[a-z]+)", "g");
  const matches = "123 abc 456".match(re);
  expect(matches.length).toBe(3);
  expect(matches[0]).toBe("123");
  expect(matches[1]).toBe("abc");
  expect(matches[2]).toBe("456");
});

test("duplicate names do not interfere with non-duplicate groups", () => {
  const re = new RegExp("(?<x>a)(?<y>1)|(?<x>b)(?<y>2)");
  const result = re.exec("b2");
  expect(result.groups.x).toBe("b");
  expect(result.groups.y).toBe("2");
});

test("duplicate names via constructor from regex literal source", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result = re.exec("b");
  expect(result.groups.x).toBe("b");
  expect(result[0]).toBe("b");
});
