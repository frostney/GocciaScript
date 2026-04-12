/*---
description: Temporal.Now.timeZoneId
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Now.timeZoneId", () => {
  test("timeZoneId() returns a non-empty string", () => {
    const tz = Temporal.Now.timeZoneId();
    expect(typeof tz).toBe("string");
    expect(tz.length > 0).toBe(true);
  });

  test("returns a string with length > 0", () => {
    const tz = Temporal.Now.timeZoneId();
    expect(typeof tz).toBe("string");
    expect(tz.length).toBeGreaterThan(0);
  });
});
