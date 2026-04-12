/*---
description: Temporal.PlainDateTime.prototype[Symbol.toStringTag]
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag", () => {
    const dt = new Temporal.PlainDateTime(2024, 1, 1);
    expect(Object.prototype.toString.call(dt)).toBe("[object Temporal.PlainDateTime]");
  });
});
