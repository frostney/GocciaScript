/*---
description: Temporal.Duration.prototype.sign
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.sign", () => {
  test("sign property", () => {
    expect(new Temporal.Duration(1).sign).toBe(1);
    expect(new Temporal.Duration(-1).sign).toBe(-1);
    expect(new Temporal.Duration().sign).toBe(0);
    expect(new Temporal.Duration(0, 0, 0, 0, 0, 0, 0, 0, 0, 1).sign).toBe(1);
  });
});
