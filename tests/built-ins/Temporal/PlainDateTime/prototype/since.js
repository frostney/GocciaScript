/*---
description: Temporal.PlainDateTime.prototype.since
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.since", () => {
  test("since() default returns days and time", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 2, 12, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dur = dt1.since(dt2);
    expect(dur.days).toBe(1);
    expect(dur.hours).toBe(12);
  });

  test("since() with largestUnit months", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 4, 15, 14, 30);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 1, 10, 0);
    expect(dt1.since(dt2, { largestUnit: "months" }).toString()).toBe("P3M14DT4H30M");
  });

  test("since() with largestUnit hours", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 2, 12, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    expect(dt1.since(dt2, { largestUnit: "hours" }).toString()).toBe("PT36H");
  });
});
