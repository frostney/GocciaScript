/*---
description: RegExp v flag character classes follow the ClassSetExpression grammar
features: [RegExp, UnicodeSets]
---*/

// ES2026 §22.2.1 Patterns: under the v flag a class is a ClassUnion, a
// ClassIntersection or a ClassSubtraction of ClassSetOperands, and a
// ClassSetCharacter, in the class or in \q{...}, excludes the
// ClassSetSyntaxCharacters ( ) [ ] { } / - \ | and the
// ClassSetReservedDoublePunctuators unless escaped. Any other pattern does
// not parse, which is a SyntaxError.

const rejects = (pattern) => {
  expect(() => new RegExp(pattern, "v")).toThrow(SyntaxError);
};

test("a ClassSetSyntaxCharacter inside \\q{} is a SyntaxError", () => {
  rejects("[[a]\\q{(}]");
  rejects("[\\q{(}]");
  rejects("[\\q{)}]");
  rejects("[\\q{[}]");
  rejects("[\\q{]}]");
  rejects("[\\q{{}]");
  rejects("[\\q{/}]");
  rejects("[\\q{-}]");
  rejects("[\\q{a(b}]");
  rejects("[\\q{ab|c)}]");
});

test("a ClassSetReservedDoublePunctuator inside \\q{} is a SyntaxError", () => {
  rejects("[\\q{&&}]");
  rejects("[\\q{!!}]");
  rejects("[\\q{##}]");
  rejects("[\\q{$$}]");
  rejects("[\\q{**}]");
  rejects("[\\q{..}]");
  rejects("[\\q{~~}]");
  rejects("[\\q{a&&b}]");
});

test("a class escape or unknown escape inside \\q{} is a SyntaxError", () => {
  rejects("[\\q{\\d}]");
  rejects("[\\q{\\p{L}}]");
  rejects("[\\q{\\q{a}}]");
  rejects("[\\q{\\a}]");
  rejects("[\\q{\\01}]");
});

test("escapes inside \\q{} keep their meaning", () => {
  expect(/^[\q{\x41}]$/v.test("A")).toBe(true);
  expect(/^[\q{\x41}]$/v.test("x41")).toBe(false);
  expect(/^[\q{\b}]$/v.test("\b")).toBe(true);
  expect(/^[\q{\f}]$/v.test("\f")).toBe(true);
  expect(/^[\q{\v}]$/v.test("\v")).toBe(true);
  expect(/^[\q{\cA}]$/v.test("\x01")).toBe(true);
  expect(/^[\q{\0}]$/v.test("\0")).toBe(true);
  expect(/^[\q{\u{1F600}x}]$/v.test("\u{1F600}x")).toBe(true);
  expect(/^[\q{a\(b|\-\&}]$/v.test("a(b")).toBe(true);
  expect(/^[\q{a\(b|\-\&}]$/v.test("-&")).toBe(true);
  expect(/^[\q{a\\}]$/v.test("a\\")).toBe(true);
  expect(/^[\q{a&b|!}]$/v.test("a&b")).toBe(true);
});

test("an unescaped ClassSetSyntaxCharacter in the class is a SyntaxError", () => {
  rejects("[{]");
  rejects("[}]");
  rejects("[[]");
  rejects("[-]");
  rejects("[a-]");
  rejects("[-a]");
  rejects("[\\d-a]");
  rejects("[[a]-b]");
});

test("a ClassSetReservedDoublePunctuator in the class is a SyntaxError", () => {
  for (const punctuator of "!#$%*+,.:;<=>?@`~") {
    rejects("[" + punctuator + punctuator + "]");
    rejects("[a" + punctuator + punctuator + "b]");
  }
  rejects("[&&]");
  rejects("[^^^]");
  rejects("[_^^]");
  rejects("[a&&&b]");
});

test("a single reserved punctuator in the class is a character", () => {
  expect(/^[&]$/v.test("&")).toBe(true);
  expect(/^[a&b]$/v.test("&")).toBe(true);
  expect(/^[a!]$/v.test("!")).toBe(true);
  expect(/^[a^]$/v.test("^")).toBe(true);
  expect(/^[a\-b]$/v.test("-")).toBe(true);
  expect(/^[a--&]$/v.test("a")).toBe(true);
  expect(/^[a--&]$/v.test("&")).toBe(false);
});

test("&& and -- join single operands, never a range or a union", () => {
  rejects("[ab&&c]");
  rejects("[a&&bc]");
  rejects("[a-z&&b]");
  rejects("[a&&b-z]");
  rejects("[a-z--b]");
  rejects("[ab--c]");
  rejects("[a&&]");
  rejects("[&&a]");
  rejects("[a--]");
  rejects("[--a]");
  rejects("[a&&b--c]");
  rejects("[a--b&&c]");
});

test("a range bound must be a ClassSetCharacter", () => {
  rejects("[a-\\q{b}]");
  rejects("[a-\\d]");
  rejects("[a-[b]]");
  rejects("[z-a]");
  expect(/^[a-\x7a]$/v.test("m")).toBe(true);
  expect(/^[\x61-z]$/v.test("m")).toBe(true);
});

test("operands chain under one operator", () => {
  expect(/^[a--b--c]$/v.test("a")).toBe(true);
  expect(/^[a--b--c]$/v.test("b")).toBe(false);
  expect(/^[a&&a&&a]$/v.test("a")).toBe(true);
  expect(/^[[ab]&&[bc]]$/v.test("b")).toBe(true);
  expect(/^[[ab]&&[bc]]$/v.test("a")).toBe(false);
  expect(/^[\q{ab}&&\q{ab|c}]$/v.test("ab")).toBe(true);
  expect(/^[\d&&[0-4]]$/v.test("3")).toBe(true);
  expect(/^[\d&&[0-4]]$/v.test("7")).toBe(false);
});

// ES2026 §22.2.1.1: a negated class whose contents MayContainStrings
// (§22.2.1.8) is a SyntaxError, even when no string survives the operation.
test("a negated class whose contents may contain strings is a SyntaxError", () => {
  rejects("[^\\q{}]");
  rejects("[^\\q{a|}]");
  rejects("[^[\\q{ab}]--\\q{ab}]");
  rejects("[^[\\p{RGI_Emoji}]--\\p{RGI_Emoji}]");
  rejects("[[^\\q{ab}]]");
  expect(/^[^\q{a}]$/v.test("b")).toBe(true);
  expect(/^[^[\q{ab}]&&a]$/v.test("b")).toBe(true);
  expect(/^[^a&&[\q{ab}]]$/v.test("a")).toBe(true);
});
