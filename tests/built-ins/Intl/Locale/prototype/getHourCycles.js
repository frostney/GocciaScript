/*---
description: Intl.Locale.prototype.getHourCycles
features: [Intl]
---*/

const isSupported = typeof Intl !== "undefined" && typeof Intl.Locale !== "undefined";

describe.runIf(isSupported)("Intl.Locale.prototype.getHourCycles", () => {
  test("returns locale preferences and honors an explicit hour cycle", () => {
    expect(new Intl.Locale("en").getHourCycles()).toEqual(["h12"]);
    expect(new Intl.Locale("en-GB").getHourCycles()).toEqual(["h23"]);
    expect(new Intl.Locale("en-u-hc-h24").getHourCycles()).toEqual(["h24"]);
  });
});
