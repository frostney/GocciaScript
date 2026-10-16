/*---
description: Date.parse
features: [Date]
---*/

test("parses an ISO date-time string", () => {
  expect(Date.parse("2024-06-15T11:30:45.123Z")).toBe(1718451045123);
});

test("rejects invalid ISO fields instead of normalizing", () => {
  for (const value of [
    "2024-13-01",
    "2024-00-01",
    "2024-02-30",
    "2024-+1-01",
    "2024-01-01T25:00:00Z",
    "2024-01-01T24:01:00Z",
    "2024-01-01T00:60:00Z",
    "2024-01-01T00:00:60Z",
    "2024-01-01T00:00:00Zgarbage",
    "2024-01-01T00:00:00Z+01:00",
    "2024-01-01T00:00:00+99:99",
  ]) {
    expect(Number.isNaN(Date.parse(value))).toBe(true);
  }
});

test("accepts 24:00 only at the end of a day", () => {
  expect(Date.parse("1995-02-04T24:00Z")).toBe(Date.parse("1995-02-05T00:00Z"));
  expect(Number.isNaN(Date.parse("1995-02-04T24:00:00.001Z"))).toBe(true);
});

test("handles supported legacy slash and month-name dates", () => {
  expect(new Date("5/1/49").getFullYear()).toBe(2049);
  expect(new Date("5/1/50").getFullYear()).toBe(1950);
  expect(new Date("49/5/1").getMonth()).toBe(4);
  expect(new Date("may 1 49").getTime()).toBe(new Date("5/1/49").getTime());
  expect(Number.isNaN(new Date("13/13/13").getTime())).toBe(true);
});

// The expectations below are written against the host zone (new Date(y, m, d)
// is local time), so they hold in any zone. In UTC local time and UTC agree;
// a zone east or west of UTC is what tells a local reading from a UTC one.
test("reads a legacy date without an offset as local midnight", () => {
  const localMay1 = new Date(2049, 4, 1).getTime();
  for (const value of ["5/1/49", "49/5/1", "may 1 49", "1 may 49", "49 may 1"])
    expect(Date.parse(value)).toBe(localMay1);
  expect(Date.parse("2024/06/15")).toBe(new Date(2024, 5, 15).getTime());
  expect(Date.parse("Jun 15 2024")).toBe(new Date(2024, 5, 15).getTime());

  const date = new Date("49/5/1");
  expect(date.getFullYear()).toBe(2049);
  expect(date.getMonth()).toBe(4);
  expect(date.getDate()).toBe(1);
  expect(date.getHours()).toBe(0);
});

test("reads a display date without an offset as local time", () => {
  const local = new Date(2024, 5, 15, 11, 30, 45).getTime();
  expect(Date.parse("Sat Jun 15 2024 11:30:45")).toBe(local);
  expect(Date.parse("Sat, 15 Jun 2024 11:30:45")).toBe(local);
});

test("applies the offset a display date names", () => {
  const utc = Date.UTC(2024, 5, 15, 11, 30, 45);
  const minutes = 60000;
  expect(Date.parse("Sat Jun 15 2024 11:30:45 GMT")).toBe(utc);
  expect(Date.parse("Sat Jun 15 2024 11:30:45 UTC")).toBe(utc);
  expect(Date.parse("Sat Jun 15 2024 11:30:45 GMT+0200")).toBe(utc - 120 * minutes);
  expect(Date.parse("Sat Jun 15 2024 11:30:45 GMT+02:00")).toBe(utc - 120 * minutes);
  expect(Date.parse("Sat Jun 15 2024 11:30:45 GMT+2")).toBe(utc - 120 * minutes);
  expect(Date.parse("Sat Jun 15 2024 11:30:45 GMT+130")).toBe(utc - 90 * minutes);
  expect(Date.parse("Sat Jun 15 2024 11:30:45 GMT-0730")).toBe(utc + 450 * minutes);
  expect(Date.parse("Sat Jun 15 2024 11:30:45 GMT+0200 (Central European Summer Time)"))
    .toBe(utc - 120 * minutes);
  expect(Date.parse("Sat, 15 Jun 2024 11:30:45 GMT")).toBe(utc);
  expect(Date.parse("Sat, 15 Jun 2024 11:30:45 GMT+0200")).toBe(utc - 120 * minutes);
  expect(Date.parse("Sat, 15 Jun 2024 11:30:45 +0000")).toBe(utc);
  expect(Date.parse("Sat, 15 Jun 2024 11:30:45 -0700")).toBe(utc + 420 * minutes);
  expect(Date.parse("Sat Jun 15 2024 11:30:45 UTC+0200")).toBe(utc - 120 * minutes);
  expect(Number.isNaN(Date.parse("Sat Jun 15 2024 11:30:45 GMT+02000"))).toBe(true);
  expect(Number.isNaN(Date.parse("Sat Jun 15 2024 11:30:45 GMT+"))).toBe(true);
  expect(Number.isNaN(Date.parse("Sat Jun 15 2024 11:30:45 GMT+0200x"))).toBe(true);
});

test("applies the RFC 2822 zone names a display date names", () => {
  const utc = Date.UTC(2024, 5, 15, 11, 30, 45);
  const hours = 3600000;
  const zones = [
    ["UT", 0], ["Z", 0], ["gmt", 0],
    ["EST", 5], ["EDT", 4], ["CST", 6], ["CDT", 5],
    ["MST", 7], ["MDT", 6], ["PST", 8], ["pdt", 7],
  ];
  for (const [zone, hoursBehind] of zones) {
    expect(Date.parse(`Sat Jun 15 2024 11:30:45 ${zone}`)).toBe(utc + hoursBehind * hours);
    expect(Date.parse(`Sat, 15 Jun 2024 11:30:45 ${zone}`)).toBe(utc + hoursBehind * hours);
  }
  expect(Number.isNaN(Date.parse("Sat, 15 Jun 2024 11:30:45 CET"))).toBe(true);
});

test("reads back what toString, toUTCString and toISOString write", () => {
  for (const date of [
    new Date(2024, 5, 15, 11, 30, 45),
    new Date(2024, 0, 1),
    new Date(1969, 11, 31, 23, 59, 59),
  ]) {
    expect(Date.parse(date.toString())).toBe(date.getTime());
    expect(Date.parse(date.toUTCString())).toBe(date.getTime());
    expect(Date.parse(date.toISOString())).toBe(date.getTime());
  }
});

test("keeps ISO date-only forms UTC and date-time forms local", () => {
  expect(Date.parse("2024-06-15")).toBe(Date.UTC(2024, 5, 15));
  expect(Date.parse("2024-06")).toBe(Date.UTC(2024, 5, 1));
  expect(Date.parse("2024-06-15T00:00")).toBe(new Date(2024, 5, 15).getTime());
  expect(Date.parse("2024-06-15T11:30:45")).toBe(new Date(2024, 5, 15, 11, 30, 45).getTime());
});

test("returns NaN rather than throwing for a local time past the time value range", () => {
  for (const value of ["9/13/275760", "+275760-09-13T00:00", "Sat Sep 13 275760 00:00:00"]) {
    const parsed = Date.parse(value);
    expect(Number.isNaN(parsed) || Math.abs(parsed) <= 8.64e15).toBe(true);
  }
  expect(Number.isNaN(Date.parse("9/14/275760"))).toBe(true);
});

test("applies an explicit offset before the time value range", () => {
  // The range applies to the UTC instant, not to the fields before the offset.
  expect(Date.parse("+275760-09-13T01:00:00+02:00")).toBe(8.64e15 - 3600000);
  expect(Date.parse("-271821-04-19T23:00:00-02:00")).toBe(-8.64e15 + 3600000);
  expect(Date.parse("Sat Sep 13 275760 01:00:00 GMT+0200")).toBe(8.64e15 - 3600000);
  expect(Number.isNaN(Date.parse("+275760-09-13T00:00:00-01:00"))).toBe(true);
  expect(Number.isNaN(Date.parse("-271821-04-20T00:00:00+01:00"))).toBe(true);
  expect(Number.isNaN(new Date("+275760-09-13T00:00:00-01:00").getTime())).toBe(true);
});

test("reads back what toString writes for the last valid instant", () => {
  const date = new Date(8.64e15);
  expect(Date.parse(date.toString())).toBe(8.64e15);
  expect(Date.parse(date.toUTCString())).toBe(8.64e15);
  expect(Date.parse(date.toISOString())).toBe(8.64e15);
});
