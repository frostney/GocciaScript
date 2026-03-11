/*---
description: Temporal[Symbol.toStringTag]
features: [temporal]
---*/

describe("Temporal[Symbol.toStringTag]", () => {
  test("Object.prototype.toString returns [object Temporal]", () => {
    expect(Object.prototype.toString.call(Temporal)).toBe("[object Temporal]");
  });

  test("Temporal.toString() returns [object Temporal]", () => {
    expect(Temporal.toString()).toBe("[object Temporal]");
  });

  test("Temporal.hasOwnProperty works", () => {
    expect(Temporal.hasOwnProperty("Now")).toBe(true);
    expect(Temporal.hasOwnProperty("Duration")).toBe(true);
    expect(Temporal.hasOwnProperty("missing")).toBe(false);
  });
});
