/*---
description: Temporal.PlainDateTime.prototype.subtract
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.subtract", () => {
  test("subtract()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 1, 0, 0);
    const result = dt.subtract(new Temporal.Duration(0, 0, 0, 1));
    expect(result.month).toBe(2);
    expect(result.day).toBe(29);
  });
});
