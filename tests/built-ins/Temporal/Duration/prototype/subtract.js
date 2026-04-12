/*---
description: Temporal.Duration.prototype.subtract
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.subtract", () => {
  test("subtract()", () => {
    const d1 = new Temporal.Duration(0, 0, 0, 10);
    const d2 = new Temporal.Duration(0, 0, 0, 3);
    const diff = d1.subtract(d2);
    expect(diff.days).toBe(7);
  });
});
