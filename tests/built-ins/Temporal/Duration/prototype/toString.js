/*---
description: Temporal.Duration.prototype.toString
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.toString", () => {
  test("toString()", () => {
    expect(new Temporal.Duration(1, 2, 3, 4, 5, 6, 7).toString()).toBe("P1Y2M3W4DT5H6M7S");
    expect(new Temporal.Duration().toString()).toBe("PT0S");
    expect(new Temporal.Duration(0, 0, 0, 0, 0, 0, 0, 500).toString()).toBe("PT0.500S");
  });
});
