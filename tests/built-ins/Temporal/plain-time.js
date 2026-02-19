/*---
description: Temporal.PlainTime construction, properties, and methods
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime", () => {

  test("constructor with defaults", () => {
    const t = new Temporal.PlainTime();
    expect(t.hour).toBe(0);
    expect(t.minute).toBe(0);
    expect(t.second).toBe(0);
    expect(t.millisecond).toBe(0);
    expect(t.microsecond).toBe(0);
    expect(t.nanosecond).toBe(0);
  });

  test("constructor with values", () => {
    const t = new Temporal.PlainTime(13, 45, 30, 500, 200, 100);
    expect(t.hour).toBe(13);
    expect(t.minute).toBe(45);
    expect(t.second).toBe(30);
    expect(t.millisecond).toBe(500);
    expect(t.microsecond).toBe(200);
    expect(t.nanosecond).toBe(100);
  });

  test("toString()", () => {
    expect(new Temporal.PlainTime(13, 45, 30).toString()).toBe("13:45:30");
    expect(new Temporal.PlainTime(9, 5, 0).toString()).toBe("09:05:00");
    expect(new Temporal.PlainTime(0, 0, 0, 500).toString()).toBe("00:00:00.500");
  });

  test("toJSON() returns same as toString()", () => {
    const t = new Temporal.PlainTime(10, 30);
    expect(t.toJSON()).toBe(t.toString());
  });

  test("equals()", () => {
    const t1 = new Temporal.PlainTime(10, 30, 0);
    const t2 = new Temporal.PlainTime(10, 30, 0);
    const t3 = new Temporal.PlainTime(10, 31, 0);
    expect(t1.equals(t2)).toBe(true);
    expect(t1.equals(t3)).toBe(false);
  });

  test("with()", () => {
    const t = new Temporal.PlainTime(10, 30, 0);
    const updated = t.with({ hour: 14 });
    expect(updated.hour).toBe(14);
    expect(updated.minute).toBe(30);
  });

  test("add()", () => {
    const t = new Temporal.PlainTime(10, 30, 0);
    const result = t.add(new Temporal.Duration(0, 0, 0, 0, 2, 45));
    expect(result.hour).toBe(13);
    expect(result.minute).toBe(15);
  });

  test("add() wraps around midnight", () => {
    const t = new Temporal.PlainTime(23, 0, 0);
    const result = t.add(new Temporal.Duration(0, 0, 0, 0, 2));
    expect(result.hour).toBe(1);
  });

  test("subtract()", () => {
    const t = new Temporal.PlainTime(10, 30, 0);
    const result = t.subtract(new Temporal.Duration(0, 0, 0, 0, 2, 15));
    expect(result.hour).toBe(8);
    expect(result.minute).toBe(15);
  });

  test("until()", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(12, 30, 0);
    const dur = t1.until(t2);
    expect(dur.hours).toBe(2);
    expect(dur.minutes).toBe(30);
  });

  test("since()", () => {
    const t1 = new Temporal.PlainTime(12, 30, 0);
    const t2 = new Temporal.PlainTime(10, 0, 0);
    const dur = t1.since(t2);
    expect(dur.hours).toBe(2);
    expect(dur.minutes).toBe(30);
  });

  test("round()", () => {
    const t = new Temporal.PlainTime(13, 45, 30);
    const rounded = t.round("minute");
    expect(rounded.hour).toBe(13);
    expect(rounded.minute).toBe(46);
    expect(rounded.second).toBe(0);
  });

  test("from() with string", () => {
    const t = Temporal.PlainTime.from("13:45:30");
    expect(t.hour).toBe(13);
    expect(t.minute).toBe(45);
    expect(t.second).toBe(30);
  });

  test("from() with object", () => {
    const t = Temporal.PlainTime.from({ hour: 10, minute: 30 });
    expect(t.hour).toBe(10);
    expect(t.minute).toBe(30);
  });

  test("compare()", () => {
    const t1 = new Temporal.PlainTime(10, 0);
    const t2 = new Temporal.PlainTime(12, 0);
    expect(Temporal.PlainTime.compare(t1, t2)).toBe(-1);
    expect(Temporal.PlainTime.compare(t2, t1)).toBe(1);
    expect(Temporal.PlainTime.compare(t1, t1)).toBe(0);
  });

});
