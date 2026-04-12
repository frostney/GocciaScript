/*---
description: Temporal.ZonedDateTime.compare
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.compare", () => {
  test("compare", () => {
    const a = new Temporal.ZonedDateTime(1000000000, "UTC");
    const b = new Temporal.ZonedDateTime(2000000000, "UTC");
    expect(Temporal.ZonedDateTime.compare(a, b)).toBe(-1);
    expect(Temporal.ZonedDateTime.compare(b, a)).toBe(1);
    expect(Temporal.ZonedDateTime.compare(a, a)).toBe(0);
  });

  test("compare accepts ISO strings", () => {
    const result = Temporal.ZonedDateTime.compare(
      "2024-03-15T12:00:00+00:00[UTC]",
      "2024-03-15T13:00:00+00:00[UTC]"
    );
    expect(result).toBe(-1);
  });
});
