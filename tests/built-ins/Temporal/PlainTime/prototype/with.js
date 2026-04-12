/*---
description: Temporal.PlainTime.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.with", () => {
  test("with()", () => {
    const t = new Temporal.PlainTime(10, 30, 0);
    const updated = t.with({ hour: 14 });
    expect(updated.hour).toBe(14);
    expect(updated.minute).toBe(30);
  });
});
