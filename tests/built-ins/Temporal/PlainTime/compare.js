/*---
description: Temporal.PlainTime.compare
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.compare", () => {
  test("compare()", () => {
    const t1 = new Temporal.PlainTime(10, 0);
    const t2 = new Temporal.PlainTime(12, 0);
    expect(Temporal.PlainTime.compare(t1, t2)).toBe(-1);
    expect(Temporal.PlainTime.compare(t2, t1)).toBe(1);
    expect(Temporal.PlainTime.compare(t1, t1)).toBe(0);
  });
});
