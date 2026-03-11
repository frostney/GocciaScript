/*---
description: performance[Symbol.toStringTag]
features: [performance]
---*/

describe("performance[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag is 'Performance'", () => {
    expect(performance[Symbol.toStringTag]).toBe("Performance");
  });

  test("Object.prototype.toString returns [object Performance]", () => {
    expect(Object.prototype.toString.call(performance)).toBe("[object Performance]");
  });

  test("performance.toString() returns [object Performance]", () => {
    expect(performance.toString()).toBe("[object Performance]");
  });

  test("performance.hasOwnProperty works", () => {
    expect(performance.hasOwnProperty("now")).toBe(true);
    expect(performance.hasOwnProperty("timeOrigin")).toBe(true);
    expect(performance.hasOwnProperty("missing")).toBe(false);
  });
});
