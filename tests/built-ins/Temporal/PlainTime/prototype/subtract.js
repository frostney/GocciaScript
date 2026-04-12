/*---
description: Temporal.PlainTime.prototype.subtract
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.subtract", () => {
  test("subtract()", () => {
    const t = new Temporal.PlainTime(10, 30, 0);
    const result = t.subtract(new Temporal.Duration(0, 0, 0, 0, 2, 15));
    expect(result.hour).toBe(8);
    expect(result.minute).toBe(15);
  });
});
