/*---
description: Temporal.Instant.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.from", () => {
  test("from() with Instant", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(5000);
    const i2 = Temporal.Instant.from(i1);
    expect(i2.epochMilliseconds).toBe(5000);
  });

  test("from() with ISO string", () => {
    const instant = Temporal.Instant.from("2024-01-15T12:30:45Z");
    expect(instant.epochMilliseconds).toBe(1705321845000);
  });

  test("from() accepts +000000 as year zero", () => {
    const instant = Temporal.Instant.from("+000000-03-31T00:45Z");
    expect(instant.epochMilliseconds).toBe(-62159440500000);
  });
});
