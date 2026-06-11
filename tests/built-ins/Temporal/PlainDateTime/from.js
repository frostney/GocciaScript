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

  test("from() accepts +000000 as year zero", () => {
    const dt = Temporal.PlainDateTime.from("+000000-03-31T00:45");
    expect(dt.year).toBe(0);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(31);
    expect(dt.hour).toBe(0);
    expect(dt.minute).toBe(45);
  });
});

describe("Temporal.PlainDateTime.from non-finite fields", () => {
  test("hour Infinity throws RangeError", () => {
    expect(() => Temporal.PlainDateTime.from({ year: 2026, month: 6, day: 11, hour: Infinity })).toThrow(RangeError);
  });
});
