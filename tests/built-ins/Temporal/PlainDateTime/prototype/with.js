/*---
description: Temporal.PlainDateTime.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.with", () => {
  test("with()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const updated = dt.with({ hour: 14, month: 6 });
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(6);
    expect(updated.day).toBe(15);
    expect(updated.hour).toBe(14);
    expect(updated.minute).toBe(30);
  });
});
