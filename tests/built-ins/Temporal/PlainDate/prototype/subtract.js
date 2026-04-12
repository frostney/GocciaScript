/*---
description: Temporal.PlainDate.prototype.subtract
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.subtract", () => {
  test("subtract()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const result = d.subtract(new Temporal.Duration(0, 0, 0, 15));
    expect(result.toString()).toBe("2024-02-29");
  });
});
