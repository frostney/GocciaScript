/*---
description: Temporal.PlainDateTime.prototype.valueOf
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.valueOf", () => {
  test("valueOf throws", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    expect(() => dt.valueOf()).toThrow(TypeError);
  });
});
