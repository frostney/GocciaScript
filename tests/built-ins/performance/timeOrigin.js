/*---
description: performance.timeOrigin
features: [performance]
---*/

describe("performance.timeOrigin", () => {
  test("is a readonly accessor", () => {
    const desc = Object.getOwnPropertyDescriptor(performance, "timeOrigin");

    expect(desc).toBeDefined();
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
  });

  test("returns a positive number", () => {
    expect(typeof performance.timeOrigin).toBe("number");
    expect(performance.timeOrigin > 0).toBe(true);
  });

  test("is stable across reads while performance.now advances from it", () => {
    const firstOrigin = performance.timeOrigin;
    const secondOrigin = performance.timeOrigin;
    const firstNow = performance.now();
    const secondNow = performance.now();

    expect(firstOrigin).toBe(secondOrigin);
    expect(firstOrigin + secondNow >= firstOrigin + firstNow).toBe(true);
  });

});
