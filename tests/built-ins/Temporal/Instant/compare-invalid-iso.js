/*---
description: Temporal.Instant.compare throws RangeError for invalid ISO strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.compare invalid ISO", () => {
  test("throws RangeError for non-instant string", () => {
    expect(() => {
      Temporal.Instant.compare("not-an-instant", Temporal.Instant.fromEpochMilliseconds(0));
    }).toThrow(RangeError);
  });
});
