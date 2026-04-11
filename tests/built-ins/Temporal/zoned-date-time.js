/*---
description: Temporal.ZonedDateTime construction, properties, and methods
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime", () => {

  test("constructor from epoch nanoseconds", () => {
    // 2024-03-15T13:45:30Z = epoch ms 1710510330000
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    expect(zdt.timeZoneId).toBe("UTC");
    expect(zdt.year).toBe(2024);
    expect(zdt.month).toBe(3);
    expect(zdt.day).toBe(15);
    expect(zdt.hour).toBe(13);
    expect(zdt.minute).toBe(45);
    expect(zdt.second).toBe(30);
  });

  test("calendarId is iso8601", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(zdt.calendarId).toBe("iso8601");
  });

  test("epochMilliseconds", () => {
    const zdt = new Temporal.ZonedDateTime(1710510330000000000, "UTC");
    expect(zdt.epochMilliseconds).toBe(1710510330000);
  });

  test("epochNanoseconds", () => {
    const zdt = new Temporal.ZonedDateTime(1710510330000000000, "UTC");
    expect(zdt.epochNanoseconds).toBe(1710510330000000000);
  });

  test("offset for UTC", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(zdt.offset).toBe("+00:00");
  });

  test("offsetNanoseconds for UTC", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(zdt.offsetNanoseconds).toBe(0);
  });

  test("calendar getters", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    expect(zdt.dayOfWeek).toBe(5); // Friday
    expect(zdt.dayOfYear).toBe(75);
    expect(zdt.daysInWeek).toBe(7);
    expect(zdt.daysInMonth).toBe(31);
    expect(zdt.daysInYear).toBe(366);
    expect(zdt.monthsInYear).toBe(12);
    expect(zdt.inLeapYear).toBe(true);
    expect(zdt.monthCode).toBe("M03");
  });

  test("withTimeZone", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    const rezoned = zdt.withTimeZone("UTC");
    expect(rezoned.timeZoneId).toBe("UTC");
    expect(rezoned.epochMilliseconds).toBe(0);
  });

  test("withPlainTime", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const updated = zdt.withPlainTime(new Temporal.PlainTime(0, 0, 0));
    expect(updated.hour).toBe(0);
    expect(updated.minute).toBe(0);
    expect(updated.second).toBe(0);
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(3);
    expect(updated.day).toBe(15);
  });

  test("add duration", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const added = zdt.add({ hours: 2, minutes: 30 });
    expect(added.hour).toBe(16);
    expect(added.minute).toBe(15);
  });

  test("subtract duration", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const sub = zdt.subtract({ hours: 1 });
    expect(sub.hour).toBe(12);
    expect(sub.minute).toBe(45);
  });

  test("equals", () => {
    const a = new Temporal.ZonedDateTime(1000000000, "UTC");
    const b = new Temporal.ZonedDateTime(1000000000, "UTC");
    const c = new Temporal.ZonedDateTime(2000000000, "UTC");
    expect(a.equals(b)).toBe(true);
    expect(a.equals(c)).toBe(false);
  });

  test("startOfDay", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const sod = zdt.startOfDay();
    expect(sod.hour).toBe(0);
    expect(sod.minute).toBe(0);
    expect(sod.second).toBe(0);
    expect(sod.year).toBe(2024);
    expect(sod.month).toBe(3);
    expect(sod.day).toBe(15);
  });

  test("toInstant", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const inst = zdt.toInstant();
    expect(inst.epochMilliseconds).toBe(1710510330000);
  });

  test("toPlainDate", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const d = zdt.toPlainDate();
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });

  test("toPlainTime", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const t = zdt.toPlainTime();
    expect(t.hour).toBe(13);
    expect(t.minute).toBe(45);
    expect(t.second).toBe(30);
  });

  test("toPlainDateTime", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const dt = zdt.toPlainDateTime();
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(13);
    expect(dt.minute).toBe(45);
    expect(dt.second).toBe(30);
  });

  test("toString includes timezone annotation", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const s = zdt.toString();
    expect(s).toContain("[UTC]");
    expect(s).toContain("2024-03-15");
    expect(s).toContain("13:45:30");
  });

  test("toJSON matches toString", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(zdt.toJSON()).toBe(zdt.toString());
  });

  test("valueOf throws", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(() => zdt.valueOf()).toThrow(TypeError);
  });

  test("compare", () => {
    const a = new Temporal.ZonedDateTime(1000000000, "UTC");
    const b = new Temporal.ZonedDateTime(2000000000, "UTC");
    expect(Temporal.ZonedDateTime.compare(a, b)).toBe(-1);
    expect(Temporal.ZonedDateTime.compare(b, a)).toBe(1);
    expect(Temporal.ZonedDateTime.compare(a, a)).toBe(0);
  });

  test("hoursInDay", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(zdt.hoursInDay).toBe(24);
  });

  test("Symbol.toStringTag", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(Object.prototype.toString.call(zdt)).toBe("[object Temporal.ZonedDateTime]");
  });
});
