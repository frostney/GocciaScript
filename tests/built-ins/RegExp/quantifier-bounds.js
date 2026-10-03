/*---
description: Quantifier bounds above 1,000,000 keep their value and are compared exactly
features: [RegExp, Goccia]
---*/

// Bounds used to be clamped to 1,000,000, so a{1000001} matched 1,000,000
// characters and two bounds out of order after clamping compiled.

test("a bound above 1,000,000 keeps its value", () => {
  const million = "a".repeat(1000000);
  expect(/^a{1000001}$/.test(million)).toBe(false);
  expect(/^a{1000001}$/.test(million + "a")).toBe(true);
  expect(/^a{1000001,}$/.test(million)).toBe(false);
  expect(/^a{1000001,}$/.test(million + "aa")).toBe(true);
});

test("bounds out of order are a SyntaxError by their real values", () => {
  expect(() => new RegExp("a{2000000,1000001}")).toThrow(SyntaxError);
  expect(() => new RegExp("a{1000001,1000000}")).toThrow(SyntaxError);
  expect(new RegExp("a{1099511627776,1099511627777}").test("a")).toBe(false);
  expect(new RegExp("a{0099,100}").test("a".repeat(99))).toBe(true);
  expect(new RegExp("a{1000000,1000001}").source).toBe("a{1000000,1000001}");
});

// ECMA-262 §22.2.1.1 compares the bounds' mathematical values. Node.js
// accepts this pattern because V8 saturates both bounds to the same value.
test("bounds beyond 2^31 out of order are a SyntaxError", () => {
  expect(() => new RegExp("a{99999999999,99999999998}")).toThrow(SyntaxError);
  expect(() => new RegExp("a{100000000000,99999999999}")).toThrow(SyntaxError);
  expect(() => new RegExp("a{2000000000000,1999999999999}")).toThrow(SyntaxError);
});

// No string has 2^31 code units, so a minimum of 2^31 or more on a body that
// always consumes a character never matches, and a maximum of MaxInt or
// more on such a body is the same as no maximum. After its minimum a
// repetition stops at an empty iteration, so on any body a maximum of
// minimum + MaxInt or more is no maximum.
test("a bound beyond any string length compiles", () => {
  const max = Number.MAX_SAFE_INTEGER;
  expect(new RegExp("b{" + max + "}", "u").test("")).toBe(false);
  expect(new RegExp("b{" + max + ",}?").test("a")).toBe(false);
  expect(new RegExp("b{" + max + "," + max + "}").test("b")).toBe(false);
  expect(/x{2147483648}x|y/.exec("xy")[0]).toBe("y");
  expect([...new RegExp("^(a){0," + max + "}$").exec("aaa")]).toEqual(["aaa", "a"]);
  expect(new RegExp("^a{2," + max + "}?").exec("aaaa")[0]).toBe("aa");
  expect(/^.{0,2147483647}$/.test("ab12")).toBe(true);
  expect(/^\d{1,2147483647}$/.test("ab12")).toBe(false);
  expect(/^[a-z]{2,2147483648}$/.test("ab12")).toBe(false);
  expect(/^(?:a?){0,2147483647}$/.test("aaa")).toBe(true);
});

test("a huge minimum on any body that always consumes never matches", () => {
  const huge = "{2147483648}";
  expect(new RegExp("(?:a|b)" + huge).test("ab")).toBe(false);
  expect(new RegExp("[\\q{ab|cd}e]" + huge, "v").test("ab")).toBe(false);
  // RGI_Emoji has more than 128 strings, so it compiles to a string set.
  expect(new RegExp("\\p{RGI_Emoji}" + huge, "v").test("\u{1F600}")).toBe(false);
  expect(new RegExp("(?:(?=a)a)" + huge).test("aa")).toBe(false);
  expect(new RegExp("(?:(a){2})" + huge).test("aa")).toBe(false);
  expect([...new RegExp("(a)" + huge + "|b").exec("ab")]).toEqual(["b", undefined]);
});

test("an empty body takes any bound", () => {
  expect(/^(?:){99999999999}$/.test("")).toBe(true);
  expect(/^(?:){0,99999999999}x$/.test("x")).toBe(true);
});

// The compiler copies the body once per counted iteration and instructions
// address each other with 24-bit operands, so a minimum from 16,777,216 to
// 2^31 - 1, a maximum from 16,777,216 below the point where it equals no
// maximum, or a minimum of 2^31 or more on a body that may match empty, is
// an engine limit. Node.js accepts these patterns.
test("a count the compiler cannot copy is a SyntaxError", () => {
  expect(() => new RegExp("a{16777216}")).toThrow(SyntaxError);
  expect(() => new RegExp("a{2147483647}")).toThrow(SyntaxError);
  expect(() => new RegExp("a{1,16777216}")).toThrow(SyntaxError);
  expect(() => new RegExp("a{0,2147483646}")).toThrow(SyntaxError);
  expect(() => new RegExp("(?:a?){1,2147483647}")).toThrow(SyntaxError);
  expect(() => new RegExp("(?:a?){2147483648}")).toThrow(SyntaxError);
  expect(() => new RegExp("(?:(?=a)){2147483648}")).toThrow(SyntaxError);
  expect(() => new RegExp("(a)(?:\\1){2147483648}")).toThrow(SyntaxError);
});

test("an unterminated huge brace is literal text without the u flag", () => {
  expect(/a{99999999999/.test("a{99999999999")).toBe(true);
});

// Five million optional copies take about 100 MB to compile, more than a
// 32-bit process running parallel test workers can spare, so this runs on
// 64-bit targets only.
const is64Bit = typeof Goccia !== "undefined" &&
  ["x86_64", "aarch64", "powerpc64"].includes(Goccia.build.arch);

test.runIf(is64Bit)("a failed optional iteration does not walk the remaining copies", () => {
  expect(/^a{0,5000000}$/.test("aaa")).toBe(true);
  expect(/x{1,5000000}y/.test("xxz xy")).toBe(true);
});
