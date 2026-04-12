/*---
description: Temporal.PlainDateTime.prototype.toPlainTime
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.toPlainTime", () => {
  test("toPlainTime()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30, 45);
    const t = dt.toPlainTime();
    expect(t.hour).toBe(10);
    expect(t.minute).toBe(30);
    expect(t.second).toBe(45);
  });
});
