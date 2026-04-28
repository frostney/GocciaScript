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

  test("until() with largestUnit minute collapses hours into minutes", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(12, 30, 0);
    expect(t1.until(t2, { largestUnit: "minute" }).toString()).toBe("PT150M");
  });

  test("until() with largestUnit second collapses all into seconds", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(10, 1, 30);
    expect(t1.until(t2, { largestUnit: "second" }).toString()).toBe("PT90S");
  });

  test("until() throws RangeError when smallestUnit is larger than largestUnit", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(12, 30, 0);
    expect(() => t1.until(t2, { largestUnit: "second", smallestUnit: "hour" })).toThrow(RangeError);
  });

  test("until() throws RangeError when roundingIncrement does not divide unit maximum", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(12, 30, 0);
    expect(() => t1.until(t2, { smallestUnit: "minute", roundingIncrement: 7 })).toThrow(RangeError);
    expect(() => t1.until(t2, { smallestUnit: "second", roundingIncrement: 7 })).toThrow(RangeError);
    expect(() => t1.until(t2, { smallestUnit: "millisecond", roundingIncrement: 3 })).toThrow(RangeError);
  });

  test("until() allows roundingIncrement that divides unit maximum", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(10, 50, 0);
    expect(t1.until(t2, { smallestUnit: "minute", roundingIncrement: 15 }).toString()).toBe("PT45M");
    expect(t1.until(t2, { smallestUnit: "minute", roundingIncrement: 10 }).toString()).toBe("PT50M");
  });

  test("until() allows any roundingIncrement when smallestUnit equals largestUnit", () => {
    const t1 = new Temporal.PlainTime(10, 0, 0);
    const t2 = new Temporal.PlainTime(10, 50, 0);
    const dur = t1.until(t2, { largestUnit: "minute", smallestUnit: "minute", roundingIncrement: 7 });
    expect(dur.minutes).toBe(49);
  });
});
