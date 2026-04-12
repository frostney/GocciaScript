/*---
description: Temporal.PlainDateTime.prototype.equals
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.equals", () => {
  test("equals()", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const dt2 = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const dt3 = new Temporal.PlainDateTime(2024, 3, 15, 10, 31);
    expect(dt1.equals(dt2)).toBe(true);
    expect(dt1.equals(dt3)).toBe(false);
  });
});
