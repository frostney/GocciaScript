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

  test("withPlainTime ignores a calendar annotation", () => {
    const dateTime = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const result = dateTime.withPlainTime("12:34:56.987654321[UTC][u-ca=unknown]");
    expect(result.toString()).toBe("2024-03-15T12:34:56.987654321");
  });
});
