/*---
description: Temporal.Duration.prototype.toJSON
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.toJSON", () => {
  test("toJSON() returns same as toString()", () => {
    const d = new Temporal.Duration(1, 0, 0, 5);
    expect(d.toJSON()).toBe(d.toString());
  });
});
