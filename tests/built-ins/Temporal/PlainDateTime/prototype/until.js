/*---
description: Temporal.PlainDateTime.prototype.until
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.until", () => {
  test("until()", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 2, 12, 0);
    const dur = dt1.until(dt2);
    expect(dur.days).toBe(1);
    expect(dur.hours).toBe(12);
  });
});
