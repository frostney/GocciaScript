/*---
description: Temporal.PlainDateTime.prototype.toPlainDate
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.toPlainDate", () => {
  test("toPlainDate()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const d = dt.toPlainDate();
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });
});
