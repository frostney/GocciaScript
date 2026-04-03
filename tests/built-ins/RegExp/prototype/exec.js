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
  const second = regex.exec("aba");
  const third = regex.exec("aba");

  expect(first[0]).toBe("a");
  expect(first.index).toBe(0);
  expect(regex.lastIndex).toBe(0);
  expect(second[0]).toBe("a");
  expect(second.index).toBe(2);
  expect(third).toBe(null);
});
