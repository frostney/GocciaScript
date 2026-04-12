/*---
description: Temporal.Instant.fromEpochMilliseconds
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.fromEpochMilliseconds", () => {
  test("fromEpochMilliseconds()", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(instant.epochMilliseconds).toBe(0);
  });

  test("fromEpochMilliseconds with positive value", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(1000);
    expect(instant.epochMilliseconds).toBe(1000);
  });

  test("fromEpochMilliseconds with negative value", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(-86400000);
    expect(instant.epochMilliseconds).toBe(-86400000);
  });
});
