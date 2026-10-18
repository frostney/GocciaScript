/*---
description: Date.prototype.toString
features: [Date]
---*/

test("returns a formatted local date-time string", () => {
  const value = new Date(1718451045123).toString();
  expect(value).toContain("2024");
  expect(value).toContain("GMT");
});

test("returns Invalid Date for an invalid time value", () => {
  expect(new Date(NaN).toString()).toBe("Invalid Date");
});

// ES2026 §21.4.4.41.3 TimeZoneString writes the offset as hours and minutes.
// Before standard time most zones used local mean time, whose offset has
// seconds (America/Los_Angeles -07:52:58); the string drops them.
const expectedOffsetSuffix = (date) => {
  const offset = Math.round(-date.getTimezoneOffset() * 60000);
  const absOffset = Math.abs(offset);
  const pad = (value) => String(value).padStart(2, "0");
  return "GMT" + (offset >= 0 ? "+" : "-") + pad(Math.floor(absOffset / 3600000)) +
    pad(Math.floor(absOffset / 60000) % 60);
};

const offsetSampleDates = () => [
  new Date(0),
  new Date(2024, 5, 15, 11, 30, 45),
  new Date(1800, 0, 1),
  new Date("0020-01-01T00:00:00Z"),
  new Date(-8.64e15),
  new Date(8.64e15),
];

test("writes the UTC offset as GMT with a sign and four digits", () => {
  const format = /^(Sun|Mon|Tue|Wed|Thu|Fri|Sat) (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]{2} -?[0-9]{4,6} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT[+-][0-9]{4}$/;
  for (const date of offsetSampleDates()) {
    const value = date.toString();
    expect(format.test(value)).toBe(true);
    expect(value.endsWith(expectedOffsetSuffix(date))).toBe(true);
  }
});
