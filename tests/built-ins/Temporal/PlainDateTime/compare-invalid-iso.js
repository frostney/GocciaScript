/*---
description: Temporal.PlainDateTime.compare throws RangeError for invalid ISO strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.compare invalid ISO", () => {
  test("throws RangeError for non-datetime string", () => {
    expect(() => {
      Temporal.PlainDateTime.compare("not-a-datetime", new Temporal.PlainDateTime(2024, 1, 1));
    }).toThrow(RangeError);
  });
});
