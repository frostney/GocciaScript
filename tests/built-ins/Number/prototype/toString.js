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

  test("toString with undefined radix returns decimal string", () => {
    expect((42).toString(undefined)).toBe("42");
    expect((0).toString(undefined)).toBe("0");
  });

  test("toString throws RangeError for radix < 2 or > 36", () => {
    expect(() => (42).toString(0)).toThrow(RangeError);
    expect(() => (42).toString(1)).toThrow(RangeError);
    expect(() => (42).toString(37)).toThrow(RangeError);
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

  test("Number.prototype has Number wrapper internal data", () => {
    expect(Number.prototype.toString()).toBe("0");
    expect(Number.prototype.toString(2)).toBe("0");
    expect(Object.prototype.toString.call(Number.prototype)).toBe("[object Number]");
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

  test("toString returns the shortest round-tripping form for computed fractional values", () => {
    expect((0.1 + 0.2).toString()).toBe("0.30000000000000004");
    expect((1 / 3).toString()).toBe("0.3333333333333333");
    expect((2 / 3).toString()).toBe("0.6666666666666666");
    expect(Math.PI.toString()).toBe("3.141592653589793");
    expect(Math.sqrt(2).toString()).toBe("1.4142135623730951");
  });

  test("toString uses the fewest significant digits for scientific-notation values", () => {
    expect(Number.MAX_VALUE.toString()).toBe("1.7976931348623157e+308");
    expect((9.18742501042e222).toString()).toBe("9.18742501042e+222");
    expect((6.1103717251161e201).toString()).toBe("6.1103717251161e+201");
    expect((7.5183158306161e142).toString()).toBe("7.5183158306161e+142");
    expect((5.7016275775556e-8).toString()).toBe("5.7016275775556e-8");
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

  test("String, template, and concatenation coercion share the non-integer shortest round-trip", () => {
    expect(String(0.1 + 0.2)).toBe("0.30000000000000004");
    expect(`${0.1 + 0.2}`).toBe("0.30000000000000004");
    expect("" + (0.1 + 0.2)).toBe("0.30000000000000004");

    expect(String(Math.PI)).toBe("3.141592653589793");
    expect(`${Math.PI}`).toBe("3.141592653589793");
    expect("" + Math.PI).toBe("3.141592653589793");
  });
});

describe("Number.prototype.toString non-finite radix", () => {
  test("Infinity radix throws RangeError", () => {
    expect(() => (5).toString(Infinity)).toThrow(RangeError);
  });

  test("NaN radix throws RangeError", () => {
    expect(() => (5).toString(NaN)).toThrow(RangeError);
  });

  test("special receivers coerce and validate radix before stringifying", () => {
    expect(() => NaN.toString(1)).toThrow(RangeError);
    expect(() => Infinity.toString(37)).toThrow(RangeError);
  });

  test("special receivers propagate abrupt radix coercion", () => {
    const radix = {
      valueOf() {
        throw new Error("radix");
      },
    };
    expect(() => NaN.toString(radix)).toThrow(Error);
    expect(() => Infinity.toString(radix)).toThrow(Error);
    expect(() => (0).toString(radix)).toThrow(Error);
  });
});
