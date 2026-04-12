/*---
description: Temporal.PlainDateTime.prototype.withPlainTime
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.withPlainTime", () => {
  test("withPlainTime()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const result = dt.withPlainTime();
    expect(result.hour).toBe(0);
    expect(result.minute).toBe(0);
    expect(result.day).toBe(15);
  });
});
