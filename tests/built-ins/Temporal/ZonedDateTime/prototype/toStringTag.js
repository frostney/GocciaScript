/*---
description: Temporal.ZonedDateTime.prototype[Symbol.toStringTag]
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(Object.prototype.toString.call(zdt)).toBe("[object Temporal.ZonedDateTime]");
  });
});
