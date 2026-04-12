/*---
description: Temporal.Duration.prototype[Symbol.toStringTag]
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag", () => {
    const d = new Temporal.Duration(1);
    expect(Object.prototype.toString.call(d)).toBe("[object Temporal.Duration]");
  });
});
