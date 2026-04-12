/*---
description: Temporal.Now.plainDateISO
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Now.plainDateISO", () => {
  test("plainDateISO() returns a PlainDate", () => {
    const date = Temporal.Now.plainDateISO();
    expect(date.year > 2020).toBe(true);
    expect(date.month >= 1).toBe(true);
    expect(date.month <= 12).toBe(true);
    expect(date.day >= 1).toBe(true);
    expect(date.day <= 31).toBe(true);
  });
});
