/*---
description: Temporal.PlainDate.prototype.add
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.add", () => {
  test("add() with duration", () => {
    const d = new Temporal.PlainDate(2024, 1, 31);
    const result = d.add(new Temporal.Duration(0, 1));
    expect(result.year).toBe(2024);
    expect(result.month).toBe(2);
    expect(result.day).toBe(29);
  });

  test("add() with days", () => {
    const d = new Temporal.PlainDate(2024, 1, 1);
    const result = d.add(new Temporal.Duration(0, 0, 0, 10));
    expect(result.toString()).toBe("2024-01-11");
  });
});
