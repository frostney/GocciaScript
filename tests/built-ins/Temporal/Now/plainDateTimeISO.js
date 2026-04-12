/*---
description: Temporal.Now.plainDateTimeISO
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Now.plainDateTimeISO", () => {
  test("plainDateTimeISO() returns a PlainDateTime", () => {
    const dt = Temporal.Now.plainDateTimeISO();
    expect(dt.year > 2020).toBe(true);
    expect(dt.hour >= 0).toBe(true);
    expect(dt.hour <= 23).toBe(true);
    const str = dt.toString();
    expect(str.length > 0).toBe(true);
  });
});
