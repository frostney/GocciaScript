/*---
description: Temporal.PlainDate.prototype[Symbol.toStringTag]
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag", () => {
    const d = new Temporal.PlainDate(2024, 1, 1);
    expect(Object.prototype.toString.call(d)).toBe("[object Temporal.PlainDate]");
  });
});
