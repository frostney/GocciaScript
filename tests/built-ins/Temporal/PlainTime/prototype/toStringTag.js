/*---
description: Temporal.PlainTime.prototype[Symbol.toStringTag]
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag", () => {
    const t = new Temporal.PlainTime(10, 30);
    expect(Object.prototype.toString.call(t)).toBe("[object Temporal.PlainTime]");
  });
});
