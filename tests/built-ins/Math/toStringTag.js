/*---
description: Math[Symbol.toStringTag]
features: [math]
---*/

describe("Math[Symbol.toStringTag]", () => {
  test("Object.prototype.toString returns [object Math]", () => {
    expect(Object.prototype.toString.call(Math)).toBe("[object Math]");
  });

  test("Math.toString() returns [object Math]", () => {
    expect(Math.toString()).toBe("[object Math]");
  });

  test("Math.hasOwnProperty works", () => {
    expect(Math.hasOwnProperty("PI")).toBe(true);
    expect(Math.hasOwnProperty("abs")).toBe(true);
    expect(Math.hasOwnProperty("missing")).toBe(false);
  });
});
