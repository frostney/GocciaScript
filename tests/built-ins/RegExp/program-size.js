/*---
description: A regular expression whose compiled program does not fit the VM's operands is a SyntaxError
features: [RegExp, Goccia]
---*/

// The VM encodes an instruction as an 8-bit opcode and a 24-bit operand, so a
// program can hold 16,777,215 instructions and a lookaround can end at most
// 8,388,607 instructions in. A larger pattern used to compile with truncated
// jump targets and return wrong results; it is now a SyntaxError. (ECMA-262
// sets no size limit, and Node.js accepts these patterns: the error is this
// engine's limit, reported instead of a wrong answer.)
//
// Each pattern compiles to millions of instructions (tens of MB), more than a
// 32-bit process running parallel test workers can spare, so the tests run
// on 64-bit targets only.
const is64Bit = typeof Goccia !== "undefined" &&
  ["x86_64", "aarch64", "powerpc64"].includes(Goccia.build.arch);

describe.runIf(is64Bit)("regular expression program size", () => {
  test("a program of 16 million instructions compiles and matches", () => {
    expect(new RegExp("(?:a{1000}){16000}|b").test("b")).toBe(true);
  });

  test("a program past 16,777,215 instructions is a SyntaxError", () => {
    expect(() => new RegExp("(?:a{1000}){17000}|b")).toThrow(SyntaxError);
    expect(() => new RegExp("b|(?:a{1000}){17000}")).toThrow(SyntaxError);
  });

  test("a lookaround ending past 8,388,607 instructions is a SyntaxError", () => {
    expect(() => new RegExp("(?=(?:a{1000}){8400})|b")).toThrow(SyntaxError);
    expect(new RegExp("(?<=(?:a{1000}){8300})|b").test("b")).toBe(true);
  });
});
