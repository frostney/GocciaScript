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

  test("add walks Coptic month 13 into the next year", () => {
    const dt = Temporal.PlainDateTime.from({
      year: 1742,
      monthCode: "M13",
      day: 1,
      hour: 12,
      minute: 34,
      calendar: "coptic",
    });
    const result = dt.add({ months: 1 });
    expect(result.year).toBe(1743);
    expect(result.month).toBe(1);
    expect(result.monthCode).toBe("M01");
    expect(result.day).toBe(1);
    expect(result.hour).toBe(12);
    expect(result.minute).toBe(34);
  });

  test("rejects invalid duration strings", () => {
    expect(() => new Temporal.PlainDateTime(2024, 1, 1).add("not-a-duration")).toThrow(RangeError);
  });
});
