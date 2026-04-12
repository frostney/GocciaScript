/*---
description: Temporal.Duration.prototype.abs
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.abs", () => {
  test("abs()", () => {
    const d = new Temporal.Duration(-1, -2, -3, -4);
    const abs = d.abs();
    expect(abs.years).toBe(1);
    expect(abs.months).toBe(2);
    expect(abs.weeks).toBe(3);
    expect(abs.days).toBe(4);
  });
});
