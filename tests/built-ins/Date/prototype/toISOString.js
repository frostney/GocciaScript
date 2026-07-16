/*---
description: Date.prototype.toISOString
features: [Date]
---*/

test("returns an ISO 8601 string", () => {
  expect(new Date(1718451045123).toISOString()).toBe("2024-06-15T11:30:45.123Z");
});

test("throws for an invalid Date", () => {
  expect(() => new Date(NaN).toISOString()).toThrow(RangeError);
});
