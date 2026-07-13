/*---
description: Temporal.PlainTime.from throws RangeError for invalid ISO strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.from invalid ISO", () => {
  test("throws RangeError for non-time string", () => {
    expect(() => {
      Temporal.PlainTime.from("not-a-time");
    }).toThrow(RangeError);
  });

  test("throws RangeError for an empty calendar annotation value", () => {
    expect(() => {
      Temporal.PlainTime.from("12:00[u-ca=]");
    }).toThrow(RangeError);
  });

  test("throws RangeError for malformed calendar annotation values", () => {
    const values = [
      "12:00[u-ca=$$$]",
      "12:00[u-ca=foo bar]",
      "12:00[u-ca=-unknown]",
      "12:00[u-ca=unknown-]",
      "12:00[u-ca=unknown--calendar]"
    ];
    for (const value of values) {
      expect(() => Temporal.PlainTime.from(value)).toThrow(RangeError);
    }
  });
});
