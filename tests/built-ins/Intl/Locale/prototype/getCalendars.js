/*---
description: Intl.Locale.prototype.getCalendars
features: [Intl]
---*/

const isSupported = typeof Intl !== "undefined" && typeof Intl.Locale !== "undefined";

describe.runIf(isSupported)("Intl.Locale.prototype.getCalendars", () => {
  test("returns the preferred calendar", () => {
    expect(new Intl.Locale("en").getCalendars()).toEqual(["gregory"]);
    expect(new Intl.Locale("en-u-ca-buddhist").getCalendars()).toEqual(["buddhist"]);
  });
});
