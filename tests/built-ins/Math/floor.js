/*---
description: Math.floor
features: [Math.floor]
---*/

describe("Math.floor", () => {
  test("returns the greatest integer less than or equal to its argument", () => {
    expect(Math.floor(4.3)).toBe(4);
    expect(Math.floor(4.7)).toBe(4);
    expect(Math.floor(-4.3)).toBe(-5);
    expect(Math.floor(-4.7)).toBe(-5);
  });

  test("preserves special values and signed zero", () => {
    expect(Math.floor(NaN)).toBeNaN();
    expect(Math.floor(Infinity)).toBe(Infinity);
    expect(Math.floor(-Infinity)).toBe(-Infinity);
    expect(Math.floor(0)).toBe(0);
    expect(Object.is(Math.floor(-0), -0)).toBe(true);
  });

  test("coerces only its first argument", () => {
    expect(Math.floor("1.9", { valueOf() { throw new Error("unused"); } })).toBe(1);
    expect(() => Math.floor(1n)).toThrow(TypeError);
  });
});

test("Math.floor retains Number range beyond 32-bit integers", () => {
  for (const base of [2 ** 31, 2 ** 32, 2 ** 40]) {
    expect(Math.floor(base + 0.75)).toBe(base);
    expect(Math.floor(-base - 0.75)).toBe(-base - 1);
  }
  for (const value of [2 ** 52 + 1, 2 ** 53 + 2, Number.MAX_VALUE]) {
    expect(Math.floor(value)).toBe(value);
    expect(Math.floor(-value)).toBe(-value);
  }
});
