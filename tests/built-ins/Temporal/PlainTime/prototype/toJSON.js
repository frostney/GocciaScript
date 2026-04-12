/*---
description: Temporal.PlainTime.prototype.toJSON
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.toJSON", () => {
  test("toJSON() returns same as toString()", () => {
    const t = new Temporal.PlainTime(10, 30);
    expect(t.toJSON()).toBe(t.toString());
  });
});
