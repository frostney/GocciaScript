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
