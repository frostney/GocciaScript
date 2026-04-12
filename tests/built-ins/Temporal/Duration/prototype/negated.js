/*---
description: Temporal.Duration.prototype.negated
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.negated", () => {
  test("negated()", () => {
    const d = new Temporal.Duration(1, 2, 3, 4, 5, 6, 7);
    const neg = d.negated();
    expect(neg.years).toBe(-1);
    expect(neg.months).toBe(-2);
    expect(neg.weeks).toBe(-3);
    expect(neg.days).toBe(-4);
    expect(neg.hours).toBe(-5);
    expect(neg.minutes).toBe(-6);
    expect(neg.seconds).toBe(-7);
  });
});
