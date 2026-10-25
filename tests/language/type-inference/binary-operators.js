/*---
description: The type the compiler infers for a binary operator's result never changes what the operator computes
features: [binary-operators, ToPrimitive, ToNumeric, BigInt]
---*/

// The compiler types the result of `-`, `*`, `/`, `%` and `**` as a Number once
// one operand is a proven Number, because ES2026 §13.15.3
// ApplyStringOrNumericBinaryOperator runs ToNumeric on both operands and throws
// a TypeError for a Number mixed with a BigInt. `+` is typed only when one side
// is a String. A wrong type would show in a typed instruction that does not
// check its operands: `r === r` turns undefined or a string into NaN, and
// `r + r` adds where `+` concatenates. The probes are written inline, because in
// a helper they would read an untyped parameter.

describe("one Number operand", () => {
  test("a BigInt with a Number throws", () => {
    const big = 1n;
    const f = () => 2 * big;
    expect(() => f()).toThrow(TypeError);
    const g = () => big - 1;
    expect(() => g()).toThrow(TypeError);
    const h = () => big ** 2;
    expect(() => h()).toThrow(TypeError);
    const k = () => big | 0;
    expect(() => k()).toThrow(TypeError);
    const m = () => big >>> 0n;
    expect(() => m()).toThrow(TypeError);
    const p = () => +big;
    expect(() => p()).toThrow(TypeError);
  });

  test("bitwise operators on two BigInts yield a BigInt", () => {
    const a = 5n;
    const b = 3n;
    const or = a | b;
    expect(or + or).toBe(14n);
    const and = a & b;
    expect(and + and).toBe(2n);
    const shift = a << b;
    expect(shift + shift).toBe(80n);
    const mixed = a ^ 1n;
    expect(mixed + mixed).toBe(8n);
  });

  test("valueOf and toString run and the result is a Number", () => {
    const calls = [];
    const o = {
      valueOf() {
        calls.push("v");
        return 5;
      },
    };
    const r = o * 2;
    expect([r === r, r + r]).toEqual([true, 20]);
    const s = { valueOf: () => "7" };
    const t = s - 1;
    expect([t === t, t + t]).toEqual([true, 12]);
    expect(t + "x").toBe("6x");
    const ts = { toString: () => "3", valueOf: undefined };
    const u = ts % 2;
    expect([u === u, u + u]).toEqual([true, 2]);
    expect(calls).toEqual(["v"]);
  });

  test("a string operand of %, -, / is converted", () => {
    const r = "7" % 2;
    expect(r + "x").toBe("1x");
    expect([r === r, r + r]).toEqual([true, 2]);
    const q = "abc" - 1;
    expect(Number.isNaN(q)).toBe(true);
  });

  test("+ with one Number operand stays generic", () => {
    const x = "4";
    const s = x + 1;
    expect([s === s, s + s]).toEqual([true, "4141"]);
    const n = 1 + x;
    expect(n).toBe("14");
  });

  test("+ with a String operand is a String", () => {
    const o = { toString: () => "o" };
    const s = "a" + o;
    expect([s === s, s + s]).toEqual([true, "aoao"]);
    const t = `t${1}` + 2;
    expect(t + t).toBe("t12t12");
  });
});
