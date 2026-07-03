/*---
description: RegExp named capture groups
features: [RegExp named capture groups]
---*/

test("named groups are captured and exposed in the groups property", () => {
  const re = new RegExp("(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})");
  const result = re.exec("2026-04-07");
  expect(result[0]).toBe("2026-04-07");
  expect(result[1]).toBe("2026");
  expect(result[2]).toBe("04");
  expect(result[3]).toBe("07");
  expect(result.groups.year).toBe("2026");
  expect(result.groups.month).toBe("04");
  expect(result.groups.day).toBe("07");
});

test("groups is undefined when regex has no named groups", () => {
  const result = /(\d+)/.exec("123");
  expect(result.groups).toBe(undefined);
});

test("non-participating named groups are undefined", () => {
  const re = new RegExp("(?<a>a)|(?<b>b)");
  const result = re.exec("b");
  expect(result.groups.a).toBe(undefined);
  expect(result.groups.b).toBe("b");
});

test("named groups work with String.prototype.match", () => {
  const re = new RegExp("(?<year>\\d{4})-(?<month>\\d{2})");
  const result = "2026-04-07".match(re);
  expect(result.groups.year).toBe("2026");
  expect(result.groups.month).toBe("04");
});

test("named groups work with matchAll", () => {
  const re = new RegExp("(?<letter>[a-z])(?<digit>\\d)", "g");
  const matches = [...("a1 b2".matchAll(re))];
  expect(matches.length).toBe(2);
  expect(matches[0].groups.letter).toBe("a");
  expect(matches[0].groups.digit).toBe("1");
  expect(matches[1].groups.letter).toBe("b");
  expect(matches[1].groups.digit).toBe("2");
});

test("named group backreference with k<name>", () => {
  const re = new RegExp("(?<word>[a-z]+) \\k<word>");
  const result = re.exec("hello hello");
  expect(result[0]).toBe("hello hello");
  expect(result.groups.word).toBe("hello");
});

test("named group names accept IdentifierName code points", () => {
  const ascii = new RegExp("(?<$>a)(?<_>b)");
  const asciiResult = ascii.exec("ab");
  expect(asciiResult.groups.$).toBe("a");
  expect(asciiResult.groups._).toBe("b");

  const unicode = new RegExp("(?<π>a)(?<the𝟚>b)");
  const unicodeResult = unicode.exec("ab");
  expect(unicodeResult.groups.π).toBe("a");
  expect(unicodeResult.groups.the𝟚).toBe("b");

  const escaped = new RegExp("(?<\\u03C0>a)\\k<\\u03C0>");
  const escapedResult = escaped.exec("aa");
  expect(escapedResult.groups.π).toBe("a");
});

test("invalid named group names throw SyntaxError", () => {
  expect(() => new RegExp("(?<>a)")).toThrow(SyntaxError);
  expect(() => new RegExp("(?<1>a)")).toThrow(SyntaxError);
  expect(() => new RegExp("(?<\\u0031>a)")).toThrow(SyntaxError);
  expect(() => new RegExp("(?<a-b>a)")).toThrow(SyntaxError);
  expect(() => new RegExp("(?<a\\u002d>a)")).toThrow(SyntaxError);
  expect(() => new RegExp("(?<🦊>a)")).toThrow(SyntaxError);
  expect(() => new RegExp("\\k<a-b>")).toThrow(SyntaxError);
});

test("named groups in replace with $<name>", () => {
  const re = new RegExp("(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})");
  const result = "2026-04-07".replace(re, "$<day>/$<month>/$<year>");
  expect(result).toBe("07/04/2026");
});

test("named groups in replace with function replacer", () => {
  const re = new RegExp("(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})");
  const result = "2026-04-07".replace(
    re,
    (match, y, m, d, offset, str, groups) => {
      return `${groups.day}/${groups.month}/${groups.year}`;
    }
  );
  expect(result).toBe("07/04/2026");
});

test("$<name> with no matching group name produces empty string", () => {
  const re = new RegExp("(?<x>a)");
  const result = "abc".replace(re, "$<y>");
  expect(result).toBe("bc");
});

test("$< without closing > is literal", () => {
  const result = "abc".replace(/a/, "$<");
  expect(result).toBe("$<bc");
});

test("named groups have null prototype", () => {
  const re = new RegExp("(?<x>a)");
  const result = re.exec("a");
  expect(result.groups.x).toBe("a");
  // groups object should not have inherited properties
  expect(result.groups.toString).toBe(undefined);
});

test("forward backreference resolves correctly", () => {
  // \k<word> appears before (?<word>...) — must still resolve
  const re = new RegExp("\\k<word> (?<word>[a-z]+)");
  // Forward backreference matches empty string before group is captured,
  // so it matches " hello" (empty backreference + space + "hello")
  const result = re.exec(" hello");
  expect(result[0]).toBe(" hello");
  expect(result.groups.word).toBe("hello");
});

test("unknown named backreference is legacy literal when no named groups exist", () => {
  const re = new RegExp("\\k<nosuchgroup>");
  expect(re.test("k<nosuchgroup>")).toBe(true);
});

test("unknown unicode named backreference is legacy literal when no named groups exist", () => {
  const re = new RegExp("\\k<π>");
  expect(re.test("k<π>")).toBe(true);
});

test("invalid named backreference throws when named groups exist", () => {
  expect(() => {
    new RegExp("(?<x>a)\\k<nosuchgroup>");
  }).toThrow(SyntaxError);
});
