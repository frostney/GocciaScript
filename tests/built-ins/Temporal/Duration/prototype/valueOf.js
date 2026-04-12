/*---
description: Temporal.Duration.prototype.valueOf
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.valueOf", () => {
  test("valueOf throws", () => {
    const d = new Temporal.Duration(1, 2, 3);
    expect(() => d.valueOf()).toThrow(TypeError);
  });
});
