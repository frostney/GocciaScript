/*---
description: Temporal.PlainYearMonth.prototype.toJSON
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.toJSON", () => {
  test("toJSON", () => {
    expect(new Temporal.PlainYearMonth(2024, 3).toJSON()).toBe("2024-03");
  });
});
