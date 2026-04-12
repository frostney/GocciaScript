/*---
description: Temporal.PlainTime.prototype.equals
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.equals", () => {
  test("equals()", () => {
    const t1 = new Temporal.PlainTime(10, 30, 0);
    const t2 = new Temporal.PlainTime(10, 30, 0);
    const t3 = new Temporal.PlainTime(10, 31, 0);
    expect(t1.equals(t2)).toBe(true);
    expect(t1.equals(t3)).toBe(false);
  });
});
