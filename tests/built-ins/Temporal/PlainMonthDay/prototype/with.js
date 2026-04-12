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

  test("with throws on conflicting month and monthCode", () => {
    const md = new Temporal.PlainMonthDay(3, 15);
    expect(() => md.with({ monthCode: "M05", month: 6 })).toThrow();
  });

  test("with accepts consistent month and monthCode", () => {
    const md = new Temporal.PlainMonthDay(3, 15);
    const updated = md.with({ monthCode: "M05", month: 5 });
    expect(updated.monthCode).toBe("M05");
  });
});
