/*---
description: RegExp.prototype.exec
features: [RegExp.prototype.exec]
---*/

test("exec returns a match array with captures and metadata", () => {
  const match = /(ab)(c)/.exec("zabcq");

  expect(match[0]).toBe("abc");
  expect(match[1]).toBe("ab");
  expect(match[2]).toBe("c");
  expect(match.index).toBe(1);
  expect(match.input).toBe("zabcq");
});

test("exec returns null when there is no match", () => {
  expect(/abc/.exec("zzz")).toBe(null);
});

test("exec updates lastIndex for global regexes", () => {
  const regex = /a/g;
  const first = regex.exec("aba");

  expect(first[0]).toBe("a");
  expect(first.index).toBe(0);
  expect(regex.lastIndex).toBe(1);

  const second = regex.exec("aba");
  expect(second[0]).toBe("a");
  expect(second.index).toBe(2);
  expect(regex.lastIndex).toBe(3);

  const third = regex.exec("aba");
  expect(third).toBe(null);
  expect(regex.lastIndex).toBe(0);
});

test("exec preserves anchor semantics when lastIndex is non-zero", () => {
  const regex = /^/g;

  regex.lastIndex = 1;
  expect(regex.exec("ab")).toBe(null);
  expect(regex.lastIndex).toBe(0);
});

test("exec returns groups property with named captures", () => {
  const re = new RegExp("(?<first>\\w+) (?<last>\\w+)");
  const result = re.exec("John Doe");
  expect(result.groups.first).toBe("John");
  expect(result.groups.last).toBe("Doe");
});

test("exec returns undefined groups when no named captures", () => {
  const result = /(\w+)/.exec("hello");
  expect(result.groups).toBe(undefined);
});

test("exec with duplicate named groups — first alternative matches", () => {
  const re = new RegExp("(?<year>[0-9][0-9][0-9][0-9])-[0-9][0-9]|[0-9][0-9]-(?<year>[0-9][0-9][0-9][0-9])");
  const result = re.exec("2026-04");
  expect(result.groups.year).toBe("2026");
});

test("exec with duplicate named groups — second alternative matches", () => {
  const re = new RegExp("(?<year>[0-9][0-9][0-9][0-9])-[0-9][0-9]|[0-9][0-9]-(?<year>[0-9][0-9][0-9][0-9])");
  const result = re.exec("04-2026");
  expect(result.groups.year).toBe("2026");
});

test("exec with duplicate named groups selects the participating alternative", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result1 = re.exec("b");
  expect(result1.groups.x).toBe("b");
  const result2 = re.exec("a");
  expect(result2.groups.x).toBe("a");
});

test("exec with three alternatives sharing the same named group", () => {
  const re = new RegExp("(?<val>\\d+)|(?<val>[a-z]+)|(?<val>[A-Z]+)");
  const result1 = re.exec("123");
  expect(result1.groups.val).toBe("123");
  const result2 = re.exec("abc");
  expect(result2.groups.val).toBe("abc");
  const result3 = re.exec("ABC");
  expect(result3.groups.val).toBe("ABC");
});

test("exec with duplicate names inside non-capturing group", () => {
  const re = new RegExp("(?:(?<x>a)|(?<x>b))c");
  const result1 = re.exec("ac");
  expect(result1.groups.x).toBe("a");
  const result2 = re.exec("bc");
  expect(result2.groups.x).toBe("b");
});

test("exec with multiple sets of duplicate names", () => {
  const re = new RegExp("(?<a>x)(?<b>1)|(?<a>y)(?<b>2)");
  const result1 = re.exec("x1");
  expect(result1.groups.a).toBe("x");
  expect(result1.groups.b).toBe("1");
  const result2 = re.exec("y2");
  expect(result2.groups.a).toBe("y");
  expect(result2.groups.b).toBe("2");
});

test("exec with duplicate names and String.prototype.match", () => {
  const re = new RegExp("(?<n>\\d+)|(?<n>[a-z]+)");
  const result = "hello".match(re);
  expect(result.groups.n).toBe("hello");
});

test("exec with duplicate names and String.prototype.replace using $<name>", () => {
  const re = new RegExp("(?<val>\\d+)|(?<val>[a-z]+)");
  const r1 = "42".replace(re, "[$<val>]");
  expect(r1).toBe("[42]");
  const r2 = "abc".replace(re, "[$<val>]");
  expect(r2).toBe("[abc]");
});

test("exec with duplicate names and function replacer", () => {
  const re = new RegExp("(?<tag>\\w+)=(\\d+)|(?<tag>\\w+)");
  const result = "foo=42".replace(re, (match, g1, g2, g3, offset, str, groups) => {
    return groups.tag.toUpperCase();
  });
  expect(result).toBe("FOO");
});

test("exec with duplicate names and matchAll", () => {
  const re = new RegExp("(?<v>\\d+)|(?<v>[a-z]+)", "g");
  const matches = [...("42 hello 7".matchAll(re))];
  expect(matches.length).toBe(3);
  expect(matches[0].groups.v).toBe("42");
  expect(matches[1].groups.v).toBe("hello");
  expect(matches[2].groups.v).toBe("7");
});

test("exec with duplicate names in same alternative throws SyntaxError", () => {
  expect(() => {
    new RegExp("(?<x>a)(?<x>b)");
  }).toThrow();
});

test("exec with duplicate names in nested same alternative throws SyntaxError", () => {
  expect(() => {
    new RegExp("(?<x>a(?<x>b))");
  }).toThrow();
});

test("exec with duplicate named backreference in same alternative", () => {
  const re = new RegExp("(?<x>a)\\k<x>|(?<x>b)\\k<x>");
  const result1 = re.exec("aa");
  expect(result1.groups.x).toBe("a");
  const result2 = re.exec("bb");
  expect(result2.groups.x).toBe("b");
});

test("exec with duplicate named groups has null prototype on groups", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result = re.exec("a");
  expect(result.groups.toString).toBe(undefined);
});

test("exec with duplicate named groups returns null on non-match", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result = re.exec("c");
  expect(result).toBe(null);
});

test("exec with duplicate named groups and global flag collects all matches", () => {
  const re = new RegExp("(?<d>\\d+)|(?<d>[a-z]+)", "g");
  const matches = "123 abc 456".match(re);
  expect(matches.length).toBe(3);
  expect(matches[0]).toBe("123");
  expect(matches[1]).toBe("abc");
  expect(matches[2]).toBe("456");
});

test("exec with duplicate names does not interfere with non-duplicate groups", () => {
  const re = new RegExp("(?<x>a)(?<y>1)|(?<x>b)(?<y>2)");
  const result = re.exec("b2");
  expect(result.groups.x).toBe("b");
  expect(result.groups.y).toBe("2");
});
