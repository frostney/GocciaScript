/*---
description: Temporal.Duration.prototype.add
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.add", () => {
  test("add()", () => {
    const d1 = new Temporal.Duration(1, 0, 0, 5);
    const d2 = new Temporal.Duration(0, 0, 0, 10);
    const sum = d1.add(d2);
    expect(sum.years).toBe(1);
    expect(sum.days).toBe(15);
  });
});
