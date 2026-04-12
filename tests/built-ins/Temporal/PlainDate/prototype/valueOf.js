/*---
description: Temporal.PlainDate.prototype.valueOf
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.valueOf", () => {
  test("valueOf throws", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    expect(() => d.valueOf()).toThrow(TypeError);
  });
});
