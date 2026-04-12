/*---
description: Temporal.PlainTime.prototype.valueOf
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.valueOf", () => {
  test("valueOf throws", () => {
    const t = new Temporal.PlainTime(10, 30);
    expect(() => t.valueOf()).toThrow(TypeError);
  });
});
