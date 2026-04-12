/*---
description: Temporal.PlainDate.prototype.since
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.since", () => {
  test("since()", () => {
    const d1 = new Temporal.PlainDate(2024, 1, 11);
    const d2 = new Temporal.PlainDate(2024, 1, 1);
    const dur = d1.since(d2);
    expect(dur.days).toBe(10);
  });
});
