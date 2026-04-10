/*---
description: Numeric separators (underscores) in numeric literals
features: [numeric-separators]
---*/

describe("numeric separators", () => {
  test("decimal integer separators", () => {
    expect(1_000).toBe(1000);
    expect(1_000_000).toBe(1000000);
    expect(1_2_3).toBe(123);
    expect(10_00).toBe(1000);
  });

  test("decimal with fractional separators", () => {
    expect(1_000.5).toBe(1000.5);
    expect(1_000.00_1).toBe(1000.001);
    expect(3.141_592).toBe(3.141592);
    expect(1_0.1_0).toBe(10.10);
  });

  test("scientific notation separators", () => {
    expect(1_000e2).toBe(100000);
    expect(1e1_0).toBe(10000000000);
    expect(1_0e1_0).toBe(100000000000);
    expect(1.5e1_0).toBe(15000000000);
  });

  test("hexadecimal separators", () => {
    expect(0xFF_FF).toBe(65535);
    expect(0xA0_B0_C0).toBe(10531008);
    expect(0x00_FF).toBe(255);
    expect(0xDEAD_BEEF).toBe(3735928559);
  });

  test("binary separators", () => {
    expect(0b1010_0001).toBe(161);
    expect(0b1111_0000).toBe(240);
    expect(0b1_0).toBe(2);
    expect(0b1111_1111).toBe(255);
  });

  test("octal separators", () => {
    expect(0o77_77).toBe(4095);
    expect(0o1_0).toBe(8);
    expect(0o755_644).toBe(252836);
    expect(0o123_456).toBe(42798);
  });

  test("single separator between each digit group", () => {
    expect(1_2).toBe(12);
    expect(0xFF_FF_FF).toBe(16777215);
    expect(0b1_0_1_0).toBe(10);
    expect(0o7_7_7).toBe(511);
  });

  test("separators preserve numeric identity", () => {
    expect(1_000 === 1000).toBe(true);
    expect(0xFF_FF === 0xFFFF).toBe(true);
    expect(0b1010_0001 === 0b10100001).toBe(true);
    expect(0o77_77 === 0o7777).toBe(true);
  });

  test("separators in arithmetic expressions", () => {
    expect(1_000 + 2_000).toBe(3000);
    expect(1_000_000 / 1_000).toBe(1000);
    expect(1_0 * 1_0).toBe(100);
    expect(0xFF_00 - 0x00_FF).toBe(65025);
  });

  test("typeof returns number", () => {
    expect(typeof 1_000).toBe("number");
    expect(typeof 0xFF_FF).toBe("number");
    expect(typeof 0b1010_0001).toBe("number");
    expect(typeof 0o77_77).toBe("number");
  });

  test("separator with zero prefix decimal", () => {
    expect(0_1).toBe(1);
  });

  test("separator in negative expressions", () => {
    expect(-1_000).toBe(-1000);
    expect(-0xFF_FF).toBe(-65535);
  });

  test("separator with scientific notation and sign", () => {
    expect(1e+1_0).toBe(10000000000);
    expect(1e-1_0).toBe(1e-10);
  });

  // Invalid separator placement (trailing, leading, consecutive, adjacent to
  // decimal point or exponent) is rejected at lex time, before any JS code
  // executes. These cases cannot be tested from within JS because `eval` is
  // excluded in GocciaScript and any file containing the invalid literal would
  // fail to parse entirely. Lex-level rejection is verified via ScriptLoader:
  //   printf '1_000_' | ./build/ScriptLoader   → SyntaxError
  //   printf '0x_FF'  | ./build/ScriptLoader   → SyntaxError
  //   printf '1__000' | ./build/ScriptLoader   → SyntaxError
  //   printf '1_.5'   | ./build/ScriptLoader   → SyntaxError
  //   printf '1._5'   | ./build/ScriptLoader   → SyntaxError
  //   printf '1e_10'  | ./build/ScriptLoader   → SyntaxError
});
