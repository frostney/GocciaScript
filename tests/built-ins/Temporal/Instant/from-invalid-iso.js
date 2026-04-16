/*---
description: Temporal.Instant.from throws RangeError for invalid ISO strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.from invalid ISO", () => {
  test("throws RangeError for non-instant string", () => {
    expect(() => {
      Temporal.Instant.from("not-an-instant");
    }).toThrow(RangeError);
  });
});
