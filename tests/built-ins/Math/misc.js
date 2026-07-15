describe("Math miscellaneous methods", () => {
  test("Math.cbrt", () => {
    expect(Math.cbrt(27)).toBeCloseTo(3, 3);
    expect(Math.cbrt(0)).toBe(0);
    expect(Object.is(Math.cbrt(-0), -0)).toBe(true);
    expect(Math.cbrt(-8)).toBeCloseTo(-2, 3);
    expect(Number.isNaN(Math.cbrt(NaN))).toBe(true);
  });

  test("Math.expm1", () => {
    expect(Math.expm1(0)).toBe(0);
    expect(Object.is(Math.expm1(-0), -0)).toBe(true);
    expect(Number.isNaN(Math.expm1(NaN))).toBe(true);
  });

  test("Math.fround", () => {
    expect(Math.fround(0)).toBe(0);
    expect(Object.is(Math.fround(-0), -0)).toBe(true);
    expect(Number.isNaN(Math.fround())).toBe(true);
    expect(Number.isNaN(Math.fround(NaN))).toBe(true);
  });

  test("Math.hypot", () => {
    expect(Math.hypot(3, 4)).toBe(5);
    expect(Math.hypot(0, 0)).toBe(0);
    expect(Math.hypot()).toBe(0);
    expect(Math.hypot(2, 3, 6)).toBe(7);
    expect(Math.hypot(Infinity, NaN)).toBe(Infinity);
    expect(Number.isNaN(Math.hypot(NaN, 1))).toBe(true);
  });

  test("Math.imul", () => {
    expect(Math.imul(2, 3)).toBe(6);
    expect(Math.imul(0, 5)).toBe(0);
    expect(Math.imul(-1, 5)).toBe(-5);
    expect(Math.imul(0xffffffff, 5)).toBe(-5);
    expect(Math.imul(0xffffffff, 0xffffffff)).toBe(1);
    expect(Math.imul(0x40000000, 0x7fffffff)).toBe(-1073741824);
    expect(Math.imul(0x80000000, 4)).toBe(0);
  });

  test("Math.log1p", () => {
    expect(Math.log1p(0)).toBe(0);
    expect(Object.is(Math.log1p(-0), -0)).toBe(true);
    expect(Number.isNaN(Math.log1p(NaN))).toBe(true);
  });

  test("Math.log2", () => {
    expect(Math.log2(1)).toBe(0);
    expect(Math.log2(2)).toBe(1);
    expect(Math.log2(8)).toBeCloseTo(3, 3);
    expect(Number.isNaN(Math.log2(NaN))).toBe(true);
  });

  test("Math.clz32", () => {
    expect(Math.clz32(1)).toBe(31);
    expect(Math.clz32(0)).toBe(32);
    expect(Math.clz32()).toBe(32);
    expect(Math.clz32(4294967296)).toBe(32);
  });
});
