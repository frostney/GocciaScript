/*---
description: Intl.DateTimeFormat.prototype.resolvedOptions
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.DateTimeFormat.prototype.resolvedOptions", () => {
  test("normalizes offset time zones to HH:MM form", () => {
    const cases = [
      ["+03", "+03:00"],
      ["+13", "+13:00"],
      ["+23", "+23:00"],
      ["-07", "-07:00"],
      ["-14", "-14:00"],
      ["-21", "-21:00"],
      ["+01:03", "+01:03"],
      ["+15:59", "+15:59"],
      ["+22:27", "+22:27"],
      ["-02:32", "-02:32"],
      ["-17:01", "-17:01"],
      ["-22:23", "-22:23"],
    ];

    for (const [timeZone, expected] of cases) {
      const options = new Intl.DateTimeFormat("en", { timeZone }).resolvedOptions();
      expect(options.timeZone).toBe(expected);
    }
  });

  test("rejects invalid offset time zones", () => {
    expect(() => new Intl.DateTimeFormat("en", { timeZone: "+24" })).toThrow(RangeError);
    expect(() => new Intl.DateTimeFormat("en", { timeZone: "+01:60" })).toThrow(RangeError);
    expect(() => new Intl.DateTimeFormat("en", { timeZone: "+01:02:03" })).toThrow(RangeError);
  });
});
