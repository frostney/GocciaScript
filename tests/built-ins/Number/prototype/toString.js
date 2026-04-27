/*---
description: Number.prototype.toString correctly converts numbers to string representation
features: [Number.prototype.toString]
---*/

describe("Number.prototype.toString", () => {
  test("toString with no radix returns decimal string", () => {
    expect((42).toString()).toBe("42");
    expect((3.14).toString()).toBe("3.14");
    expect((0).toString()).toBe("0");
    expect((-7).toString()).toBe("-7");
  });

  test("toString with radix 16 (hex)", () => {
    expect((255).toString(16)).toBe("ff");
    expect((16).toString(16)).toBe("10");
    expect((0).toString(16)).toBe("0");
  });

  test("toString with radix 10", () => {
    expect((42).toString(10)).toBe("42");
    expect((0).toString(10)).toBe("0");
  });

  test("toString on NaN", () => {
    expect(NaN.toString()).toBe("NaN");
  });

  test("toString on Infinity", () => {
    expect(Infinity.toString()).toBe("Infinity");
    expect((-Infinity).toString()).toBe("-Infinity");
  });

  test("toString on negative zero", () => {
    expect((-0).toString()).toBe("0");
  });

  test("integers >= 1e15 and < 1e21 use fixed-point notation", () => {
    expect((1e15).toString()).toBe("1000000000000000");
    expect((1e16).toString()).toBe("10000000000000000");
    expect((1e17).toString()).toBe("100000000000000000");
    expect((1e18).toString()).toBe("1000000000000000000");
    expect((1e19).toString()).toBe("10000000000000000000");
    expect((1e20).toString()).toBe("100000000000000000000");
  });

  test("integers >= 1e21 use scientific notation with lowercase e+", () => {
    expect((1e21).toString()).toBe("1e+21");
    expect((1e25).toString()).toBe("1e+25");
    expect((1e100).toString()).toBe("1e+100");
  });

  test("Number.MAX_SAFE_INTEGER uses fixed-point notation", () => {
    expect((9007199254740991).toString()).toBe("9007199254740991");
  });

  test("large integers near 1e15 boundary", () => {
    expect((999999999999999).toString()).toBe("999999999999999");
    expect((1234567890123456).toString()).toBe("1234567890123456");
    expect((4294967295).toString()).toBe("4294967295");
    expect((4294967296).toString()).toBe("4294967296");
  });

  test("negative large integers use fixed-point notation", () => {
    expect((-1e15).toString()).toBe("-1000000000000000");
    expect((-1e20).toString()).toBe("-100000000000000000000");
    expect((-9007199254740991).toString()).toBe("-9007199254740991");
  });

  test("negative large integers >= 1e21 use scientific notation", () => {
    expect((-1e21).toString()).toBe("-1e+21");
  });

  test("very small numbers use scientific notation with e-", () => {
    expect((1e-7).toString()).toBe("1e-7");
    expect((1.5e-10).toString()).toBe("1.5e-10");
    expect((5e-324).toString()).toBe("5e-324");
  });

  test("small numbers above 1e-7 threshold use fixed-point", () => {
    expect((0.000001).toString()).toBe("0.000001");
    expect((0.0000001).toString()).toBe("1e-7");
  });

  test("String() coercion matches toString for large integers", () => {
    expect(String(1e15)).toBe("1000000000000000");
    expect(String(1e20)).toBe("100000000000000000000");
    expect(String(1e21)).toBe("1e+21");
    expect(String(9007199254740991)).toBe("9007199254740991");
  });

  test("template literal coercion matches toString for large integers", () => {
    expect(`${1e15}`).toBe("1000000000000000");
    expect(`${1e20}`).toBe("100000000000000000000");
    expect(`${1e21}`).toBe("1e+21");
    expect(`${9007199254740991}`).toBe("9007199254740991");
  });

  test("string concatenation coercion matches toString for large integers", () => {
    expect("" + 1e15).toBe("1000000000000000");
    expect("" + 1e20).toBe("100000000000000000000");
    expect("" + 1e21).toBe("1e+21");
  });
});
