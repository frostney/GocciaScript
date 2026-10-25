/*---
description: NaN, -0 and values past the int32 range survive the typed Number operations the compiler infers
features: [Number, bitwise-operators, exponentiation]
---*/

describe("NaN, -0 and integer overflow", () => {
  test("NaN stays NaN through typed operations", () => {
    const n = (0 / 0) * 1;
    expect(n === n).toBe(false);
    expect(Number.isNaN(n + n)).toBe(true);
  });

  test("-0 is kept", () => {
    const z = -0 * 1;
    expect(Object.is(z, -0)).toBe(true);
    expect(Object.is(z + 0, 0)).toBe(true);
    expect(Object.is(z * 1, -0)).toBe(true);
  });

  test("int32 and uint32 results overflow into doubles", () => {
    const big = 2147483647 | 0;
    expect(big + 1).toBe(2147483648);
    const s = 1 << 31;
    expect(s - 1).toBe(-2147483649);
    const u = -1 >>> 0;
    expect(u + 1).toBe(4294967296);
    const m = 4294967295 | 0;
    expect(m).toBe(-1);
    const prod = 2147483647 * 2147483647;
    expect(prod + 1).toBe(4611686014132420610);
    const inf = 1e308 * 10;
    expect(inf - inf).toBeNaN();
    const big53 = 9007199254740991 * 1;
    expect(big53 + 2).toBe(9007199254740992);
  });

  test("operations on bitwise results leave the integers", () => {
    const f = (x, y) => {
      const a = x | 0;
      const b = y | 0;
      const sq = a * a;
      const quad = sq * sq;
      const octo = quad * quad;
      const sixteen = octo * octo;
      const thirtyTwo = sixteen * sixteen;
      return [a / b, a % b, a ** b, thirtyTwo * thirtyTwo];
    };
    const r = f(5, 2);
    expect(r[0]).toBe(2.5);
    expect(r[1]).toBe(1);
    expect(r[2]).toBe(25);
    expect(r[3] > 1e44).toBe(true);
    const g = f(2147483647, 0);
    expect(g[0]).toBe(Infinity);
    expect(g[1]).toBeNaN();
    expect(g[2]).toBe(1);
    expect(g[3]).toBe(Infinity);
    const h = f(2, -1);
    expect(h[0]).toBe(-2);
    expect(h[2]).toBe(0.5);
  });
});
