/*---
description: Temporal.PlainDate.compare
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.compare", () => {
  test("compare()", () => {
    const d1 = new Temporal.PlainDate(2024, 1, 1);
    const d2 = new Temporal.PlainDate(2024, 6, 1);
    expect(Temporal.PlainDate.compare(d1, d2)).toBe(-1);
    expect(Temporal.PlainDate.compare(d2, d1)).toBe(1);
    expect(Temporal.PlainDate.compare(d1, d1)).toBe(0);
  });
});
