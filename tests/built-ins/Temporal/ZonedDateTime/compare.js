/*---
description: Temporal.ZonedDateTime.compare
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.compare", () => {
  test("compare", () => {
    const a = new Temporal.ZonedDateTime(1000000000, "UTC");
    const b = new Temporal.ZonedDateTime(2000000000, "UTC");
    expect(Temporal.ZonedDateTime.compare(a, b)).toBe(-1);
    expect(Temporal.ZonedDateTime.compare(b, a)).toBe(1);
    expect(Temporal.ZonedDateTime.compare(a, a)).toBe(0);
  });
});
