/*---
description: Date.prototype.toTimeString
features: [Date]
---*/

// ES2026 §21.4.4.41.3 TimeZoneString writes the offset as hours and minutes.
// A local mean time offset such as -07:52:58 loses its seconds in the string.
const expectedOffsetSuffix = (date) => {
  const offset = Math.round(-date.getTimezoneOffset() * 60000);
  const absOffset = Math.abs(offset);
  const pad = (value) => String(value).padStart(2, "0");
  return "GMT" + (offset >= 0 ? "+" : "-") + pad(Math.floor(absOffset / 3600000)) +
    pad(Math.floor(absOffset / 60000) % 60);
};

test("writes the local time and the UTC offset as GMT with a sign and four digits", () => {
  for (const date of [new Date(0), new Date(2024, 5, 15, 11, 30, 45), new Date(1800, 0, 1), new Date("0020-01-01T00:00:00Z")]) {
    const value = date.toTimeString();
    expect(/^[0-9]{2}:[0-9]{2}:[0-9]{2} GMT[+-][0-9]{4}$/.test(value)).toBe(true);
    expect(value.startsWith(String(date.getHours()).padStart(2, "0") + ":")).toBe(true);
    expect(value.endsWith(expectedOffsetSuffix(date))).toBe(true);
  }
});

test("returns Invalid Date for an invalid time value", () => {
  expect(new Date(NaN).toTimeString()).toBe("Invalid Date");
});
