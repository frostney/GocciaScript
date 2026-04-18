/*---
description: Trailing-dot numeric literals and method calls (e.g. 10..toString())
features: [numeric-literals]
---*/

describe("trailing-dot numeric literals", () => {
  test("10..toString() returns decimal string", () => {
    expect(10..toString()).toBe("10");
  });

  test("10..toString(16) returns hex string", () => {
    expect(10..toString(16)).toBe("a");
  });

  test("255..toString(16) returns hex string", () => {
    expect(255..toString(16)).toBe("ff");
  });

  test("0..toString() returns '0'", () => {
    expect(0..toString()).toBe("0");
  });

  test("trailing dot produces correct numeric value", () => {
    expect(10.).toBe(10);
    expect(0.).toBe(0);
    expect(123.).toBe(123);
  });

  test("trailing dot with exponent", () => {
    expect(10.e2).toBe(1000);
    expect(1.e3).toBe(1000);
  });

  test("numeric separator with trailing dot method call", () => {
    expect(1_000..toString()).toBe("1000");
  });

  test("spread after numeric literal still works", () => {
    const arr = [1, 2, 3];
    const result = [0, ...arr];
    expect(result.length).toBe(4);
    expect(result[0]).toBe(0);
    expect(result[1]).toBe(1);
  });
});
