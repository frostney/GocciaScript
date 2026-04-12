/*---
description: Temporal.ZonedDateTime.prototype.valueOf
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.valueOf", () => {
  test("valueOf throws", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(() => zdt.valueOf()).toThrow(TypeError);
  });
});
