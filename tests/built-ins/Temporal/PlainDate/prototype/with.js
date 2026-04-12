/*---
description: Temporal.PlainDate.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.with", () => {
  test("with()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const updated = d.with({ month: 6 });
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(6);
    expect(updated.day).toBe(15);
  });
});
