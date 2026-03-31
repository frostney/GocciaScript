/*---
description: Performance interface object
features: [performance]
---*/

describe("Performance", () => {
  test("is exposed as a global function object", () => {
    expect(typeof Performance).toBe("function");
    expect(globalThis.Performance).toBe(Performance);
  });

  test("shares its prototype with the global performance instance", () => {
    expect(Performance.prototype).toBe(Object.getPrototypeOf(performance));
    expect(performance.constructor).toBe(Performance);
    expect(performance instanceof Performance).toBe(true);
  });

  test("cannot be called without new", () => {
    expect(() => Performance()).toThrow(TypeError);
  });

  test("cannot be constructed with new", () => {
    expect(() => new Performance()).toThrow(TypeError);
  });
});
