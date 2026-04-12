/*---
description: Temporal.PlainTime constructor
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime constructor", () => {
  test("constructor with defaults", () => {
    const t = new Temporal.PlainTime();
    expect(t.hour).toBe(0);
    expect(t.minute).toBe(0);
    expect(t.second).toBe(0);
    expect(t.millisecond).toBe(0);
    expect(t.microsecond).toBe(0);
    expect(t.nanosecond).toBe(0);
  });

  test("constructor with values", () => {
    const t = new Temporal.PlainTime(13, 45, 30, 500, 200, 100);
    expect(t.hour).toBe(13);
    expect(t.minute).toBe(45);
    expect(t.second).toBe(30);
    expect(t.millisecond).toBe(500);
    expect(t.microsecond).toBe(200);
    expect(t.nanosecond).toBe(100);
  });
});
