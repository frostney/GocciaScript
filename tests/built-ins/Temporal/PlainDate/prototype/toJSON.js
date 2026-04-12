/*---
description: Temporal.PlainDate.prototype.toJSON
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.toJSON", () => {
  test("toJSON() returns same as toString()", () => {
    const d = new Temporal.PlainDate(2024, 6, 15);
    expect(d.toJSON()).toBe(d.toString());
  });
});
