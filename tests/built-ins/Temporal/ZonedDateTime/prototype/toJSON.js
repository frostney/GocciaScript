/*---
description: Temporal.ZonedDateTime.prototype.toJSON
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.toJSON", () => {
  test("toJSON matches toString", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(zdt.toJSON()).toBe(zdt.toString());
  });
});
