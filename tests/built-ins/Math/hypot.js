/*---
description: Math.hypot
features: [Math.hypot]
---*/

describe("Math.hypot", () => {
  test("returns the square root of the sum of squared arguments", () => {
    expect(Math.hypot(3, 4)).toBe(5);
    expect(Math.hypot(2, 3, 6)).toBe(7);
    expect(Math.hypot()).toBe(0);
    expect(Math.hypot(-3, -4)).toBe(5);
  });

  test("infinity takes precedence over NaN after all arguments are coerced", () => {
    let converted = false;
    const value = {
      valueOf() {
        converted = true;
        return Infinity;
      },
    };

    expect(Math.hypot(NaN, value)).toBe(Infinity);
    expect(converted).toBe(true);
    expect(Math.hypot(NaN, 1)).toBeNaN();
  });

  test("coerces arguments from left to right", () => {
    const calls = [];
    const first = { valueOf() { calls.push("first"); return 3; } };
    const second = { valueOf() { calls.push("second"); return 4; } };

    expect(Math.hypot(first, second)).toBe(5);
    expect(calls).toEqual(["first", "second"]);
  });
});
