/*---
description: Temporal.Duration construction, properties, and methods
features: [Temporal]
---*/

describe("Temporal.Duration", () => {

  test("constructor with all zeros", () => {
    const d = new Temporal.Duration();
    expect(d.years).toBe(0);
    expect(d.months).toBe(0);
    expect(d.weeks).toBe(0);
    expect(d.days).toBe(0);
    expect(d.hours).toBe(0);
    expect(d.minutes).toBe(0);
    expect(d.seconds).toBe(0);
    expect(d.milliseconds).toBe(0);
    expect(d.microseconds).toBe(0);
    expect(d.nanoseconds).toBe(0);
  });

  test("constructor with components", () => {
    const d = new Temporal.Duration(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    expect(d.years).toBe(1);
    expect(d.months).toBe(2);
    expect(d.weeks).toBe(3);
    expect(d.days).toBe(4);
    expect(d.hours).toBe(5);
    expect(d.minutes).toBe(6);
    expect(d.seconds).toBe(7);
    expect(d.milliseconds).toBe(8);
    expect(d.microseconds).toBe(9);
    expect(d.nanoseconds).toBe(10);
  });

  test("sign property", () => {
    expect(new Temporal.Duration(1).sign).toBe(1);
    expect(new Temporal.Duration(-1).sign).toBe(-1);
    expect(new Temporal.Duration().sign).toBe(0);
    expect(new Temporal.Duration(0, 0, 0, 0, 0, 0, 0, 0, 0, 1).sign).toBe(1);
  });

  test("blank property", () => {
    expect(new Temporal.Duration().blank).toBe(true);
    expect(new Temporal.Duration(1).blank).toBe(false);
    expect(new Temporal.Duration(0, 0, 0, 0, 0, 0, 1).blank).toBe(false);
  });

  test("negated()", () => {
    const d = new Temporal.Duration(1, 2, 3, 4, 5, 6, 7);
    const neg = d.negated();
    expect(neg.years).toBe(-1);
    expect(neg.months).toBe(-2);
    expect(neg.weeks).toBe(-3);
    expect(neg.days).toBe(-4);
    expect(neg.hours).toBe(-5);
    expect(neg.minutes).toBe(-6);
    expect(neg.seconds).toBe(-7);
  });

  test("abs()", () => {
    const d = new Temporal.Duration(-1, -2, -3, -4);
    const abs = d.abs();
    expect(abs.years).toBe(1);
    expect(abs.months).toBe(2);
    expect(abs.weeks).toBe(3);
    expect(abs.days).toBe(4);
  });

  test("add()", () => {
    const d1 = new Temporal.Duration(1, 0, 0, 5);
    const d2 = new Temporal.Duration(0, 0, 0, 10);
    const sum = d1.add(d2);
    expect(sum.years).toBe(1);
    expect(sum.days).toBe(15);
  });

  test("subtract()", () => {
    const d1 = new Temporal.Duration(0, 0, 0, 10);
    const d2 = new Temporal.Duration(0, 0, 0, 3);
    const diff = d1.subtract(d2);
    expect(diff.days).toBe(7);
  });

  test("with()", () => {
    const d = new Temporal.Duration(1, 2, 3, 4);
    const updated = d.with({ days: 10 });
    expect(updated.years).toBe(1);
    expect(updated.months).toBe(2);
    expect(updated.weeks).toBe(3);
    expect(updated.days).toBe(10);
  });

  test("toString()", () => {
    expect(new Temporal.Duration(1, 2, 3, 4, 5, 6, 7).toString()).toBe("P1Y2M3W4DT5H6M7S");
    expect(new Temporal.Duration().toString()).toBe("PT0S");
    expect(new Temporal.Duration(0, 0, 0, 0, 0, 0, 0, 500).toString()).toBe("PT0.500S");
  });

  test("toJSON() returns same as toString()", () => {
    const d = new Temporal.Duration(1, 0, 0, 5);
    expect(d.toJSON()).toBe(d.toString());
  });

  test("total()", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 30);
    expect(d.total("minutes")).toBe(90);
    expect(d.total("hours")).toBe(1.5);
    expect(d.total("seconds")).toBe(5400);
  });

  test("total() throws RangeError for durations with years or months", () => {
    const withYears = new Temporal.Duration(1);
    expect(() => withYears.total("days")).toThrow("RangeError");

    const withMonths = new Temporal.Duration(0, 3);
    expect(() => withMonths.total("days")).toThrow("RangeError");

    const withBoth = new Temporal.Duration(1, 2, 0, 5);
    expect(() => withBoth.total("hours")).toThrow("RangeError");
  });

  test("from() with string", () => {
    const d = Temporal.Duration.from("P1Y2M3DT4H5M6S");
    expect(d.years).toBe(1);
    expect(d.months).toBe(2);
    expect(d.days).toBe(3);
    expect(d.hours).toBe(4);
    expect(d.minutes).toBe(5);
    expect(d.seconds).toBe(6);
  });

  test("from() with object", () => {
    const d = Temporal.Duration.from({ hours: 5, minutes: 30 });
    expect(d.hours).toBe(5);
    expect(d.minutes).toBe(30);
    expect(d.days).toBe(0);
  });

  test("compare()", () => {
    const d1 = new Temporal.Duration(0, 0, 0, 0, 1);
    const d2 = new Temporal.Duration(0, 0, 0, 0, 2);
    expect(Temporal.Duration.compare(d1, d2)).toBe(-1);
    expect(Temporal.Duration.compare(d2, d1)).toBe(1);
    expect(Temporal.Duration.compare(d1, d1)).toBe(0);
  });

});
