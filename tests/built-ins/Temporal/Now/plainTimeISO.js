/*---
description: Temporal.Now.plainTimeISO
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Now.plainTimeISO", () => {
  test("plainTimeISO() returns a PlainTime", () => {
    const time = Temporal.Now.plainTimeISO();
    expect(time.hour >= 0).toBe(true);
    expect(time.hour <= 23).toBe(true);
    expect(time.minute >= 0).toBe(true);
    expect(time.minute <= 59).toBe(true);
  });
});
