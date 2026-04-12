/*---
description: Temporal.Duration.compare
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.compare", () => {
  test("compare()", () => {
    const d1 = new Temporal.Duration(0, 0, 0, 0, 1);
    const d2 = new Temporal.Duration(0, 0, 0, 0, 2);
    expect(Temporal.Duration.compare(d1, d2)).toBe(-1);
    expect(Temporal.Duration.compare(d2, d1)).toBe(1);
    expect(Temporal.Duration.compare(d1, d1)).toBe(0);
  });
});
