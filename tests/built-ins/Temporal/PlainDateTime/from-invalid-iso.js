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

  test("throws RangeError when an explicit offset has no time part", () => {
    expect(() => {
      Temporal.PlainDateTime.from("2020-01-01TZ");
    }).toThrow(RangeError);
    expect(() => {
      Temporal.PlainDateTime.from("2020-01-01T+01:00");
    }).toThrow(RangeError);
  });
});
