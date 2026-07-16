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
