/*---
description: Temporal.Duration.prototype.total
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.total", () => {
  test("total()", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 30);
    expect(d.total("minutes")).toBe(90);
    expect(d.total("hours")).toBe(1.5);
    expect(d.total("seconds")).toBe(5400);
  });

  test("total() throws RangeError for durations with years or months", () => {
    const withYears = new Temporal.Duration(1);
    expect(() => withYears.total("days")).toThrow(RangeError);

    const withMonths = new Temporal.Duration(0, 3);
    expect(() => withMonths.total("days")).toThrow(RangeError);

    const withBoth = new Temporal.Duration(1, 2, 0, 5);
    expect(() => withBoth.total("hours")).toThrow(RangeError);
  });
});
