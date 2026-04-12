/*---
description: Temporal.PlainMonthDay.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay.prototype.with", () => {
  test("with accepts numeric month", () => {
    const md = new Temporal.PlainMonthDay(3, 15);
    const updated = md.with({ month: 8 });
    expect(updated.monthCode).toBe("M08");
  });
});
