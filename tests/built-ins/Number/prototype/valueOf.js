/*---
description: Number.prototype.valueOf returns the primitive number value
features: [Number.prototype.valueOf]
---*/

describe("Number.prototype.valueOf", () => {
  test("valueOf on positive numbers", () => {
    expect((42).valueOf()).toBe(42);
    expect((3.14).valueOf()).toBe(3.14);
  });

  test("valueOf on zero and negative zero", () => {
    expect((0).valueOf()).toBe(0);
    expect((-0).valueOf()).toBe(-0);
  });

  test("valueOf on negative numbers", () => {
    expect((-5).valueOf()).toBe(-5);
    expect((-3.14).valueOf()).toBe(-3.14);
  });

  test("valueOf on NaN", () => {
    expect(Number.isNaN(NaN.valueOf())).toBe(true);
  });

  test("valueOf on Infinity", () => {
    expect(Infinity.valueOf()).toBe(Infinity);
    expect((-Infinity).valueOf()).toBe(-Infinity);
  });
});
