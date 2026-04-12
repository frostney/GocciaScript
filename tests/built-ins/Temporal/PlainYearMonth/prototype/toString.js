/*---
description: Temporal.PlainYearMonth.prototype.toString
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.toString", () => {
  test("toString", () => {
    expect(new Temporal.PlainYearMonth(2024, 3).toString()).toBe("2024-03");
    expect(new Temporal.PlainYearMonth(2024, 12).toString()).toBe("2024-12");
  });
});
