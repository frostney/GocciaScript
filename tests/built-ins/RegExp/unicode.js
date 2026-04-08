/*---
description: RegExp Unicode mode semantics
features: [RegExp, Unicode]
---*/

test("u flag is accepted and exposed", () => {
  const regex = /abc/u;
  expect(regex.unicode).toBe(true);
  expect(regex.flags).toBe("u");
});

test("u flag with basic matching", () => {
  expect(/hello/u.test("hello")).toBe(true);
  expect(/hello/u.test("HELLO")).toBe(false);
});

test("u flag with case-insensitive matching", () => {
  expect(/hello/iu.test("HELLO")).toBe(true);
});

test("u flag with dot matches single character", () => {
  expect(/^.$/u.test("a")).toBe(true);
});

test("u flag with global matching", () => {
  const matches = "abc".match(/./gu);
  expect(matches).toEqual(["a", "b", "c"]);
});

test("unicode escape \\u{} syntax in pattern", () => {
  expect(new RegExp("\\u{41}", "u").test("A")).toBe(true);
  expect(new RegExp("\\u{61}", "u").test("a")).toBe(true);
});

test("unicode escape \\u{} does not match wrong character", () => {
  expect(new RegExp("\\u{41}", "u").test("B")).toBe(false);
});

test("unicode four-digit escape \\uHHHH syntax", () => {
  expect(new RegExp("\\u0041", "u").test("A")).toBe(true);
  expect(new RegExp("\\u0061", "u").test("a")).toBe(true);
});

test("unicode property escape \\p{ASCII}", () => {
  expect(new RegExp("^\\p{ASCII}+$", "u").test("hello123")).toBe(true);
});

test("unicode property escape \\p{ASCII_Hex_Digit}", () => {
  expect(new RegExp("^\\p{ASCII_Hex_Digit}+$", "u").test("0123456789abcdefABCDEF")).toBe(true);
  expect(new RegExp("\\p{ASCII_Hex_Digit}", "u").test("g")).toBe(false);
});

test("negated unicode property escape \\P{ASCII}", () => {
  expect(new RegExp("\\P{ASCII}", "u").test("hello")).toBe(false);
});

test("unicode property \\p{Letter}", () => {
  expect(new RegExp("^\\p{Letter}+$", "u").test("hello")).toBe(true);
  expect(new RegExp("\\p{Letter}", "u").test("123")).toBe(false);
});

test("unicode property \\p{Number}", () => {
  expect(new RegExp("^\\p{Number}+$", "u").test("123")).toBe(true);
  expect(new RegExp("\\p{Number}", "u").test("abc")).toBe(false);
});

test("unicode property \\p{Uppercase_Letter}", () => {
  expect(new RegExp("^\\p{Uppercase_Letter}+$", "u").test("ABC")).toBe(true);
  expect(new RegExp("\\p{Uppercase_Letter}", "u").test("abc")).toBe(false);
});

test("unicode property \\p{Lowercase_Letter}", () => {
  expect(new RegExp("^\\p{Lowercase_Letter}+$", "u").test("abc")).toBe(true);
  expect(new RegExp("\\p{Lowercase_Letter}", "u").test("ABC")).toBe(false);
});

test("\\p{Lu} shorthand for Uppercase_Letter", () => {
  expect(new RegExp("^\\p{Lu}+$", "u").test("ABC")).toBe(true);
  expect(new RegExp("\\p{Lu}", "u").test("abc")).toBe(false);
});

test("\\p{Ll} shorthand for Lowercase_Letter", () => {
  expect(new RegExp("^\\p{Ll}+$", "u").test("abc")).toBe(true);
  expect(new RegExp("\\p{Ll}", "u").test("ABC")).toBe(false);
});

test("unicode property \\p{White_Space}", () => {
  expect(new RegExp("\\p{White_Space}", "u").test(" ")).toBe(true);
  expect(new RegExp("\\p{White_Space}", "u").test("\t")).toBe(true);
  expect(new RegExp("\\p{White_Space}", "u").test("a")).toBe(false);
});

test("u flag zero-width match advances correctly", () => {
  const matches = "abc".match(/(?:)/gu);
  expect(matches.length).toBe(4);
});

test("u flag with exec advances correctly on empty matches", () => {
  const regex = /(?:)/gu;
  const result1 = regex.exec("ab");
  expect(result1.index).toBe(0);
  expect(regex.lastIndex).toBe(1);

  const result2 = regex.exec("ab");
  expect(result2.index).toBe(1);
  expect(regex.lastIndex).toBe(2);
});

test("\\p{L} shorthand for Letter", () => {
  expect(new RegExp("^\\p{L}+$", "u").test("hello")).toBe(true);
  expect(new RegExp("\\p{L}", "u").test("123")).toBe(false);
});

test("\\p{N} shorthand for Number", () => {
  expect(new RegExp("^\\p{N}+$", "u").test("42")).toBe(true);
});

test("\\p{Nd} for Decimal_Number", () => {
  expect(new RegExp("^\\p{Nd}+$", "u").test("42")).toBe(true);
});

test("\\p{Punctuation}", () => {
  expect(new RegExp("\\p{Punctuation}", "u").test("!")).toBe(true);
  expect(new RegExp("\\p{Punctuation}", "u").test(".")).toBe(true);
  expect(new RegExp("\\p{Punctuation}", "u").test("a")).toBe(false);
});

test("\\p{P} shorthand for Punctuation", () => {
  expect(new RegExp("\\p{P}", "u").test("!")).toBe(true);
});

test("\\p{Control}", () => {
  expect(new RegExp("\\p{Control}", "u").test("\x00")).toBe(true);
  expect(new RegExp("\\p{Control}", "u").test("a")).toBe(false);
});

test("\\p{Cc} shorthand for Control", () => {
  expect(new RegExp("\\p{Cc}", "u").test("\x00")).toBe(true);
});

test("invalid unicode property throws SyntaxError", () => {
  expect(() => {
    new RegExp("\\p{Invalid_Property_Name}", "u");
  }).toThrow(SyntaxError);
});

test("invalid hex digits in \\u{} escape throw SyntaxError", () => {
  expect(() => {
    new RegExp("\\u{ZZZZ}", "u");
  }).toThrow(SyntaxError);
});

test("u flag combined with other flags", () => {
  const regex = new RegExp("hello", "giu");
  expect(regex.global).toBe(true);
  expect(regex.ignoreCase).toBe(true);
  expect(regex.unicode).toBe(true);
  expect(regex.test("HELLO")).toBe(true);
});

test("u flag does not affect non-unicode patterns", () => {
  expect(/^[a-z]+$/u.test("hello")).toBe(true);
  expect(/^[a-z]+$/u.test("HELLO")).toBe(false);
  expect(/^\d+$/u.test("123")).toBe(true);
  expect(/^\w+$/u.test("hello_123")).toBe(true);
});

test("u flag with multiline mode", () => {
  const regex = new RegExp("^hello$", "mu");
  expect(regex.test("hello\nworld")).toBe(true);
});

test("u flag with dotAll mode", () => {
  const regex = new RegExp("hello.world", "su");
  expect(regex.test("hello\nworld")).toBe(true);
});

test("u flag with sticky mode", () => {
  const regex = new RegExp("hello", "yu");
  expect(regex.test("hello world")).toBe(true);
  expect(regex.test("hello world")).toBe(false);
});

test("\\p{Symbol}", () => {
  expect(new RegExp("\\p{Symbol}", "u").test("+")).toBe(true);
  expect(new RegExp("\\p{Symbol}", "u").test("=")).toBe(true);
  expect(new RegExp("\\p{Symbol}", "u").test("a")).toBe(false);
});

test("\\p{S} shorthand for Symbol", () => {
  expect(new RegExp("\\p{S}", "u").test("+")).toBe(true);
});

test("\\p{Separator}", () => {
  expect(new RegExp("\\p{Separator}", "u").test(" ")).toBe(true);
  expect(new RegExp("\\p{Separator}", "u").test("a")).toBe(false);
});

test("\\p{Z} shorthand for Separator", () => {
  expect(new RegExp("\\p{Z}", "u").test(" ")).toBe(true);
});
