/*---
description: Temporal.PlainDateTime.prototype.until
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.until", () => {
  test("until() default returns days and time", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 2, 12, 0);
    const dur = dt1.until(dt2);
    expect(dur.days).toBe(1);
    expect(dur.hours).toBe(12);
  });

  test("until() with largestUnit months", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 10, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 4, 15, 14, 30);
    expect(dt1.until(dt2, { largestUnit: "months" }).toString()).toBe("P3M14DT4H30M");
  });

  test("until() with largestUnit years", () => {
    const dt1 = new Temporal.PlainDateTime(2020, 6, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 9, 15, 12, 0);
    expect(dt1.until(dt2, { largestUnit: "years" }).toString()).toBe("P4Y3M14DT12H");
  });

  test("until() with largestUnit hours", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 10, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 4, 15, 14, 30);
    expect(dt1.until(dt2, { largestUnit: "hours" }).toString()).toBe("PT2524H30M");
  });

  test("until() with largestUnit minutes", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 1, 2, 30);
    expect(dt1.until(dt2, { largestUnit: "minutes" }).toString()).toBe("PT150M");
  });

  test("until() with largestUnit seconds", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 1, 0, 1, 30);
    expect(dt1.until(dt2, { largestUnit: "seconds" }).toString()).toBe("PT90S");
  });

  test("until() time borrowing across day with months", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 15, 20, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 3, 15, 8, 0);
    expect(dt1.until(dt2, { largestUnit: "months" }).toString()).toBe("P1M28DT12H");
  });

  test("until() with largestUnit weeks", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 22, 12, 0);
    expect(dt1.until(dt2, { largestUnit: "weeks" }).toString()).toBe("P3WT12H");
  });

  test("until() with smallestUnit hour truncates minutes", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 10, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 1, 12, 45);
    expect(dt1.until(dt2, { smallestUnit: "hour" }).toString()).toBe("PT2H");
  });

  test("until() with smallestUnit hour and roundingMode halfExpand", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 10, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 1, 12, 45);
    expect(dt1.until(dt2, { smallestUnit: "hour", roundingMode: "halfExpand" }).toString()).toBe("PT3H");
  });

  test("until() with smallestUnit minute truncates seconds", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 1, 0, 5, 45);
    expect(dt1.until(dt2, { smallestUnit: "minute" }).toString()).toBe("PT5M");
  });

  test("until() with smallestUnit day truncates time", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 3, 18, 0);
    expect(dt1.until(dt2, { smallestUnit: "day" }).toString()).toBe("P2D");
  });

  test("until() with smallestUnit day and roundingMode ceil rounds up", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 3, 1, 0);
    expect(dt1.until(dt2, { smallestUnit: "day", roundingMode: "ceil" }).toString()).toBe("P3D");
  });

  test("until() with smallestUnit month and largestUnit year", () => {
    const dt1 = new Temporal.PlainDateTime(2020, 1, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2020, 4, 20, 12, 0);
    expect(dt1.until(dt2, { largestUnit: "years", smallestUnit: "month", roundingMode: "halfExpand" }).toString()).toBe("P4M");
  });
});
