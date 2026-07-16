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
