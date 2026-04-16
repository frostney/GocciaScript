/*---
description: Temporal.PlainDateTime.from throws RangeError for invalid ISO strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.from invalid ISO", () => {
  test("throws RangeError for non-datetime string", () => {
    expect(() => {
      Temporal.PlainDateTime.from("not-a-datetime");
    }).toThrow(RangeError);
  });
});
