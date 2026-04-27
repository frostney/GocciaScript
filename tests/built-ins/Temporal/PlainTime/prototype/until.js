/*---
description: Temporal.PlainTime.prototype.until
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.until", () => {
  test("until()", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(12, 30, 0);
    const dur = t1.until(t2);
    expect(dur.hours).toBe(2);
    expect(dur.minutes).toBe(30);
  });

  test("until() with smallestUnit hour truncates minutes", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(12, 45, 0);
    expect(t1.until(t2, { smallestUnit: "hour" }).toString()).toBe("PT2H");
  });

  test("until() with smallestUnit hour and roundingMode halfExpand", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(12, 45, 0);
    expect(t1.until(t2, { smallestUnit: "hour", roundingMode: "halfExpand" }).toString()).toBe("PT3H");
  });

  test("until() with smallestUnit minute truncates seconds", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(10, 5, 45);
    expect(t1.until(t2, { smallestUnit: "minute" }).toString()).toBe("PT5M");
  });

  test("until() with roundingIncrement 15 and smallestUnit minutes", () => {
    const t1 = new Temporal.PlainTime(0, 0, 0);
    const t2 = new Temporal.PlainTime(0, 50, 0);
    expect(t1.until(t2, { smallestUnit: "minute", roundingIncrement: 15 }).toString()).toBe("PT45M");
  });
});
