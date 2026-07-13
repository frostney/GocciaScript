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

  test("toPlainDateTime ignores a time string calendar annotation", () => {
    const date = new Temporal.PlainDate(2024, 3, 15);
    const result = date.toPlainDateTime("12:34:56.987654321[u-ca=unknown]");
    expect(result.toString()).toBe("2024-03-15T12:34:56.987654321");
  });
});
