/*---
description: Temporal.PlainDate.from throws RangeError for invalid ISO strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.from invalid ISO", () => {
  test("throws RangeError for non-date string", () => {
    expect(() => {
      Temporal.PlainDate.from("not-a-date");
    }).toThrow(RangeError);
  });

  test("throws RangeError for empty string", () => {
    expect(() => {
      Temporal.PlainDate.from("");
    }).toThrow(RangeError);
  });
});
