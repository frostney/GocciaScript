/*---
description: Math.imul
features: [Math.imul]
---*/

describe("Math.imul", () => {
  test("returns the signed low 32 bits of the product", () => {
    expect(Math.imul(2, 3)).toBe(6);
    expect(Math.imul(-1, 5)).toBe(-5);
    expect(Math.imul(0xffffffff, 0xffffffff)).toBe(1);
    expect(Math.imul(0x40000000, 0x7fffffff)).toBe(-1073741824);
    expect(Math.imul(0x80000000, 4)).toBe(0);
  });

  test("converts both arguments to unsigned 32-bit integers", () => {
    expect(Math.imul(3.9, 2.1)).toBe(6);
    expect(Math.imul("3", true)).toBe(3);
    expect(Math.imul(NaN, Infinity)).toBe(0);
    expect(Math.imul(2)).toBe(0);
  });
});
