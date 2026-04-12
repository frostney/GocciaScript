/*---
description: Temporal.Instant.prototype.add
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.add", () => {
  test("add() with time duration", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    const result = instant.add(new Temporal.Duration(0, 0, 0, 0, 1));
    expect(result.epochMilliseconds).toBe(3600000);
  });

  test("add() throws on years", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(() => {
      instant.add(new Temporal.Duration(1));
    }).toThrow(RangeError);
  });

  test("add() throws on months", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(() => {
      instant.add(new Temporal.Duration(0, 1));
    }).toThrow(RangeError);
  });
});
