/*---
description: Temporal.PlainTime.prototype.round
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.round", () => {
  test("round()", () => {
    const t = new Temporal.PlainTime(13, 45, 30);
    const rounded = t.round("minute");
    expect(rounded.hour).toBe(13);
    expect(rounded.minute).toBe(46);
    expect(rounded.second).toBe(0);
  });

  test("roundingMode ceil", () => {
    const t = new Temporal.PlainTime(13, 45, 30, 500);
    const rounded = t.round({ smallestUnit: "second", roundingMode: "ceil" });
    expect(rounded.second).toBe(31);
  });

  test("roundingMode floor", () => {
    const t = new Temporal.PlainTime(13, 45, 30, 500);
    const rounded = t.round({ smallestUnit: "second", roundingMode: "floor" });
    expect(rounded.second).toBe(30);
  });

  test("roundingMode trunc", () => {
    const t = new Temporal.PlainTime(13, 45, 30, 999);
    const rounded = t.round({ smallestUnit: "second", roundingMode: "trunc" });
    expect(rounded.second).toBe(30);
  });

  test("roundingIncrement", () => {
    const t = new Temporal.PlainTime(13, 47, 0);
    const rounded = t.round({ smallestUnit: "minute", roundingIncrement: 15 });
    expect(rounded.minute).toBe(45);
  });
});
