/*---
description: The type the compiler infers for a unary operator's result never changes what the operator computes
features: [unary-operators, ToNumeric, BigInt]
---*/

// Unary + is always a Number (ES2026 §13.5.4: ToNumber, which throws for a
// BigInt). Unary - and ~ keep a Number a Number (§13.5.5, §13.5.6) but yield a
// BigInt for a BigInt, so they are typed only when the operand is a proven
// Number. ! is a Boolean and typeof a String (§13.5.7, §13.5.3).

describe("unary operators", () => {
  test("+ is always a Number", () => {
    const v = +{ valueOf: () => "4" };
    expect([v === v, v + v]).toEqual([true, 8]);
    const w = +"x";
    expect(Number.isNaN(w)).toBe(true);
  });

  test("- of an unknown value can be a BigInt", () => {
    const b = 3n;
    const n = -b;
    expect(n + n).toBe(-6n);
    const z = -(0 * 1);
    expect(Object.is(z, -0)).toBe(true);
    expect(1 / z).toBe(-Infinity);
  });

  test("~ of a Number is an int32", () => {
    const x = 2147483647;
    const y = ~(x * 1);
    expect(y + 1).toBe(-2147483647);
    const b = 5n;
    const c = ~b;
    expect(c + c).toBe(-12n);
  });

  test("! and typeof", () => {
    const b = !0;
    expect(b + b).toBe(2);
    const t = typeof 1;
    expect(t + t).toBe("numbernumber");
  });
});
