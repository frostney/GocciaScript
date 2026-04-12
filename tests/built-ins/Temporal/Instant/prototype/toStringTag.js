/*---
description: Temporal.Instant.prototype[Symbol.toStringTag]
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(Object.prototype.toString.call(instant)).toBe("[object Temporal.Instant]");
  });
});
