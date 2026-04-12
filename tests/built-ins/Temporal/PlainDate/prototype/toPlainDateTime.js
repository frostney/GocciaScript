/*---
description: Temporal.PlainDate.prototype.toPlainDateTime
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.toPlainDateTime", () => {
  test("toPlainDateTime()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const dt = d.toPlainDateTime();
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(0);
    expect(dt.minute).toBe(0);
  });
});
