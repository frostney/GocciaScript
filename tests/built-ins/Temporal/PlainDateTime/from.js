/*---
description: Temporal.PlainDateTime.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.from", () => {
  test("from() with string", () => {
    const dt = Temporal.PlainDateTime.from("2024-03-15T10:30:00");
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(10);
    expect(dt.minute).toBe(30);
  });

  test("from() with date-only string", () => {
    const dt = Temporal.PlainDateTime.from("2024-03-15");
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(0);
  });
});
