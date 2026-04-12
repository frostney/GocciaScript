/*---
description: Temporal.PlainMonthDay.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay.from", () => {
  test("from string", () => {
    const md = Temporal.PlainMonthDay.from("12-25");
    expect(md.monthCode).toBe("M12");
    expect(md.day).toBe(25);
  });

  test("from PlainMonthDay", () => {
    const original = new Temporal.PlainMonthDay(6, 15);
    const copy = Temporal.PlainMonthDay.from(original);
    expect(copy.monthCode).toBe("M06");
    expect(copy.day).toBe(15);
  });

  test("from object with monthCode", () => {
    const md = Temporal.PlainMonthDay.from({ monthCode: "M07", day: 4 });
    expect(md.monthCode).toBe("M07");
    expect(md.day).toBe(4);
  });

  test("from object with month", () => {
    const md = Temporal.PlainMonthDay.from({ month: 7, day: 4 });
    expect(md.monthCode).toBe("M07");
    expect(md.day).toBe(4);
  });
});
