/*---
description: Temporal.PlainDate.prototype.toString
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.toString", () => {
  test("toString()", () => {
    expect(new Temporal.PlainDate(2024, 3, 15).toString()).toBe("2024-03-15");
    expect(new Temporal.PlainDate(2024, 1, 1).toString()).toBe("2024-01-01");
  });
});
