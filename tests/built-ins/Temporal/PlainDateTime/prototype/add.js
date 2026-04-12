/*---
description: Temporal.PlainDateTime.prototype.add
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.add", () => {
  test("add()", () => {
    const dt = new Temporal.PlainDateTime(2024, 1, 31, 23, 0);
    const result = dt.add(new Temporal.Duration(0, 1, 0, 0, 2));
    expect(result.month).toBe(3);
    expect(result.day).toBe(1);
    expect(result.hour).toBe(1);
  });
});
