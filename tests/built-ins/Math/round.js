/*---
description: Math.round
features: [Math.round]
---*/

describe("Math.round", () => {
  test("rounds to the nearest integer toward positive infinity on ties", () => {
    expect(Math.round(4.3)).toBe(4);
    expect(Math.round(4.7)).toBe(5);
    expect(Math.round(-4.3)).toBe(-4);
    expect(Math.round(-4.7)).toBe(-5);
    expect(Math.round(0.5)).toBe(1);
    expect(Math.round(1.5)).toBe(2);
    expect(Object.is(Math.round(-0.5), -0)).toBe(true);
    expect(Object.is(Math.round(0.5 - Number.EPSILON / 4), 0)).toBe(true);
  });

  test("preserves special values and signed zero", () => {
    expect(Math.round(NaN)).toBeNaN();
    expect(Math.round(Infinity)).toBe(Infinity);
    expect(Math.round(-Infinity)).toBe(-Infinity);
    expect(Math.round(0)).toBe(0);
    expect(Object.is(Math.round(-0), -0)).toBe(true);
    expect(Object.is(Math.round(-0.25), -0)).toBe(true);
  });

  test("coerces only its first argument", () => {
    expect(Math.round("1.4", { valueOf() { throw new Error("unused"); } })).toBe(1);
    expect(() => Math.round(1n)).toThrow(TypeError);
  });
});
