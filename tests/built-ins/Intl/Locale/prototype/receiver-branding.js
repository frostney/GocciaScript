/*---
description: Intl.Locale prototype information method receiver branding
features: [Intl]
---*/

const isSupported = typeof Intl !== "undefined" && typeof Intl.Locale !== "undefined";

describe.runIf(isSupported)("Intl.Locale information method receiver branding", () => {
  test("methods reject receivers without Intl.Locale internal slots", () => {
    for (const method of [
      "getCalendars",
      "getCollations",
      "getHourCycles",
      "getNumberingSystems",
      "getTimeZones",
      "getTextInfo",
      "getWeekInfo",
    ]) {
      const functionValue = Intl.Locale.prototype[method];
      for (const receiver of [{}, Intl.Locale.prototype, null, undefined]) {
        expect(() => functionValue.call(receiver)).toThrow(TypeError);
      }
    }
  });
});
