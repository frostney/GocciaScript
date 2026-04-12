/*---
description: Temporal.Now.instant
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Now.instant", () => {
  test("instant() returns an Instant", () => {
    const instant = Temporal.Now.instant();
    expect(instant.epochMilliseconds > 0).toBe(true);
    const str = instant.toString();
    expect(str.length > 0).toBe(true);
  });
});
