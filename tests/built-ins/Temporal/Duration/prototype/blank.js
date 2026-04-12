/*---
description: Temporal.Duration.prototype.blank
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.blank", () => {
  test("blank property", () => {
    expect(new Temporal.Duration().blank).toBe(true);
    expect(new Temporal.Duration(1).blank).toBe(false);
    expect(new Temporal.Duration(0, 0, 0, 0, 0, 0, 1).blank).toBe(false);
  });
});
