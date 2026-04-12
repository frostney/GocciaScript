/*---
description: Temporal.PlainDate.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.from", () => {
  test("from() with string", () => {
    const d = Temporal.PlainDate.from("2024-03-15");
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });

  test("from() with object", () => {
    const d = Temporal.PlainDate.from({ year: 2024, month: 6, day: 15 });
    expect(d.year).toBe(2024);
    expect(d.month).toBe(6);
    expect(d.day).toBe(15);
  });

  test("constrain clamps day", () => {
    const d = Temporal.PlainDate.from({ year: 2024, month: 2, day: 30 }, { overflow: "constrain" });
    expect(d.day).toBe(29);
  });

  test("reject throws on invalid day", () => {
    expect(() => {
      Temporal.PlainDate.from({ year: 2024, month: 2, day: 30 }, { overflow: "reject" });
    }).toThrow(RangeError);
  });

  test("from with overflow constrain clamps month", () => {
    const d = Temporal.PlainDate.from({ year: 2024, month: 13, day: 15 }, { overflow: "constrain" });
    expect(d.month).toBe(12);
    expect(d.day).toBe(15);
  });

  test("from with overflow constrain clamps day to 1", () => {
    const d = Temporal.PlainDate.from({ year: 2024, month: 3, day: 0 }, { overflow: "constrain" });
    expect(d.day).toBe(1);
  });
});
