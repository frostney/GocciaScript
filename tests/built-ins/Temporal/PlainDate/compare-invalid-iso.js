/*---
description: Temporal.PlainDate.compare throws RangeError for invalid ISO strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.compare invalid ISO", () => {
  test("throws RangeError for non-date string", () => {
    expect(() => {
      Temporal.PlainDate.compare("not-a-date", new Temporal.PlainDate(2024, 1, 1));
    }).toThrow(RangeError);
  });
});
