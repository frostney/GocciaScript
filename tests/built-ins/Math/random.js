/*---
description: Math.random function works correctly
features: [Math.random]
---*/

describe("Math.random", () => {
  test("returns numbers in the half-open interval from zero to one", () => {
    Array.from({ length: 32 }).forEach(() => {
      const value = Math.random();
      expect(typeof value).toBe("number");
      expect(value).toBeGreaterThanOrEqual(0);
      expect(value).toBeLessThan(1);
    });
  });

  test("has the correct name and length", () => {
    expect(Math.random.name).toBe("random");
    expect(Math.random.length).toBe(0);
  });
});
