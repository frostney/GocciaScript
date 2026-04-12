/*---
description: Temporal.PlainDateTime.prototype.toJSON
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.toJSON", () => {
  test("toJSON()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    expect(dt.toJSON()).toBe(dt.toString());
  });
});
