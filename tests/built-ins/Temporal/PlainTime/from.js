/*---
description: Temporal.PlainTime.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.from", () => {
  test("from() with string", () => {
    const t = Temporal.PlainTime.from("13:45:30");
    expect(t.hour).toBe(13);
    expect(t.minute).toBe(45);
    expect(t.second).toBe(30);
  });

  test("from() with object", () => {
    const t = Temporal.PlainTime.from({ hour: 10, minute: 30 });
    expect(t.hour).toBe(10);
    expect(t.minute).toBe(30);
  });
});
