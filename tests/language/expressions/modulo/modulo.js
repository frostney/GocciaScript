/*---
description: Modulo operator with floating-point arithmetic
features: [modulo]
---*/

describe("modulo operator", () => {
  test("basic integer modulo", () => {
    expect(10 % 3).toBe(1);
    expect(7 % 2).toBe(1);
    expect(6 % 3).toBe(0);
  });

  test("floating-point modulo", () => {
    expect(5.5 % 2).toBe(1.5);
    expect(10.5 % 3).toBe(1.5);
    expect(7.5 % 2.5).toBe(0);
  });

  test("negative operands", () => {
    expect(-10 % 3).toBe(-1);
    expect(10 % -3).toBe(1);
    expect(-10 % -3).toBe(-1);
  });

  test("modulo by zero returns NaN", () => {
    expect(Number.isNaN(5 % 0)).toBe(true);
    expect(Number.isNaN(0 % 0)).toBe(true);
  });

  test("compound modulo assignment", () => {
    let x = 10;
    x %= 3;
    expect(x).toBe(1);

    let y = 5.5;
    y %= 2;
    expect(y).toBe(1.5);
  });
});
