/*---
description: Math.log2
features: [Math.log2]
---*/

describe("Math.log2", () => {
  test("returns exact integers for powers of two", () => {
    expect(Math.log2(1)).toBe(0);
    expect(Math.log2(2)).toBe(1);
    expect(Math.log2(8)).toBe(3);
    expect(Math.log2(1024)).toBe(10);
  });

  test("handles zero, negative values, infinities, and NaN", () => {
    expect(Math.log2(0)).toBe(-Infinity);
    expect(Math.log2(-0)).toBe(-Infinity);
    expect(Math.log2(-1)).toBeNaN();
    expect(Math.log2(Infinity)).toBe(Infinity);
    expect(Math.log2(-Infinity)).toBeNaN();
    expect(Math.log2(NaN)).toBeNaN();
  });
});
