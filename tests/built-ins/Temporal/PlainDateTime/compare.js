/*---
description: Temporal.PlainDateTime.compare
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.compare", () => {
  test("compare()", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 6, 1, 0, 0);
    expect(Temporal.PlainDateTime.compare(dt1, dt2)).toBe(-1);
    expect(Temporal.PlainDateTime.compare(dt2, dt1)).toBe(1);
    expect(Temporal.PlainDateTime.compare(dt1, dt1)).toBe(0);
  });
});
