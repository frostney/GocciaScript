/*---
description: Temporal.PlainDate.prototype.equals
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.equals", () => {
  test("equals()", () => {
    const d1 = new Temporal.PlainDate(2024, 3, 15);
    const d2 = new Temporal.PlainDate(2024, 3, 15);
    const d3 = new Temporal.PlainDate(2024, 3, 16);
    expect(d1.equals(d2)).toBe(true);
    expect(d1.equals(d3)).toBe(false);
  });
});
