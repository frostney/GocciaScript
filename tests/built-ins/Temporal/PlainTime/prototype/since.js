/*---
description: Temporal.PlainTime.prototype.since
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.since", () => {
  test("since()", () => {
    const t1 = new Temporal.PlainTime(12, 30, 0);
    const t2 = new Temporal.PlainTime(10, 0, 0);
    const dur = t1.since(t2);
    expect(dur.hours).toBe(2);
    expect(dur.minutes).toBe(30);
  });
});
