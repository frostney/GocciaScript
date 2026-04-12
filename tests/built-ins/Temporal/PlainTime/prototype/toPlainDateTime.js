/*---
description: Temporal.PlainTime.prototype.toPlainDateTime
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.toPlainDateTime", () => {
  test("toPlainDateTime with a PlainDate", () => {
    const t = new Temporal.PlainTime(13, 45, 30);
    const d = new Temporal.PlainDate(2024, 3, 15);
    const dt = d.toPlainDateTime(t);
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(13);
    expect(dt.minute).toBe(45);
    expect(dt.second).toBe(30);
  });
});
