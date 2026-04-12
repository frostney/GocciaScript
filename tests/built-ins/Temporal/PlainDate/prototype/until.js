/*---
description: Temporal.PlainDate.prototype.until
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.until", () => {
  test("until()", () => {
    const d1 = new Temporal.PlainDate(2024, 1, 1);
    const d2 = new Temporal.PlainDate(2024, 1, 11);
    const dur = d1.until(d2);
    expect(dur.days).toBe(10);
  });
});
