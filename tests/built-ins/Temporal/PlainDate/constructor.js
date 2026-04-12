/*---
description: Temporal.PlainDate constructor
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate constructor", () => {
  test("constructor", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });
});
