/*---
description: Temporal.Duration.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.with", () => {
  test("with()", () => {
    const d = new Temporal.Duration(1, 2, 3, 4);
    const updated = d.with({ days: 10 });
    expect(updated.years).toBe(1);
    expect(updated.months).toBe(2);
    expect(updated.weeks).toBe(3);
    expect(updated.days).toBe(10);
  });
});
