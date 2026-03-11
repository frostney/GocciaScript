/*---
description: performance.now
features: [performance]
---*/

describe("performance.now", () => {
  test("performance is exposed as an object", () => {
    expect(typeof performance).toBe("object");
  });

  test("returns a non-negative number", () => {
    const value = performance.now();

    expect(typeof value).toBe("number");
    expect(value >= 0).toBe(true);
  });

  test("is monotonic across successive calls", () => {
    const first = performance.now();
    const second = performance.now();
    const third = performance.now();

    expect(second >= first).toBe(true);
    expect(third >= second).toBe(true);
  });

  test("globalThis.performance is identical", () => {
    expect(globalThis.performance === performance).toBe(true);
  });
});
