/*---
description: Temporal.Instant.prototype.since
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.since", () => {
  test("since()", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(3600000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(0);
    const dur = i1.since(i2);
    expect(dur.hours).toBe(1);
  });
});
