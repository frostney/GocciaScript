/*---
description: Temporal.ZonedDateTime.prototype.toLocaleString
features: [Temporal, Intl.DateTimeFormat]
---*/

const hasTemporalIntl = typeof Temporal !== "undefined" && typeof Intl !== "undefined";

describe.runIf(hasTemporalIntl)("Temporal.ZonedDateTime.prototype.toLocaleString", () => {
  test("uses the ZonedDateTime time zone", () => {
    const zdt = new Temporal.ZonedDateTime(0n, "America/New_York");
    const options = { hour: "numeric", minute: "numeric", timeZoneName: "short" };

    const actual = zdt.toLocaleString("en-US", options);
    const expected = new Intl.DateTimeFormat("en-US", {
      hour: "numeric",
      minute: "numeric",
      timeZoneName: "short",
      timeZone: "America/New_York",
    }).format(0);
    const utc = new Intl.DateTimeFormat("en-US", {
      hour: "numeric",
      minute: "numeric",
      timeZoneName: "short",
      timeZone: "UTC",
    }).format(0);

    expect(actual).toBe(expected);
    expect(actual === utc).toBe(false);
  });

  test("rejects an explicit timeZone option", () => {
    const zdt = new Temporal.ZonedDateTime(0n, "UTC");

    expect(() => zdt.toLocaleString("en-US", { timeZone: "UTC" })).toThrow(
      TypeError
    );
  });
});
