/*---
description: Temporal.PlainDateTime.prototype.toString
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.toString", () => {
  test("toString()", () => {
    expect(new Temporal.PlainDateTime(2024, 3, 15, 10, 30, 0).toString()).toBe("2024-03-15T10:30:00");
    expect(new Temporal.PlainDateTime(2024, 1, 1, 0, 0, 0).toString()).toBe("2024-01-01T00:00:00");
  });
});
