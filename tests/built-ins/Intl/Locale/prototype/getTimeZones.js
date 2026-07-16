/*---
description: Intl.Locale.prototype.getTimeZones
features: [Intl]
---*/

const isSupported = typeof Intl !== "undefined" && typeof Intl.Locale !== "undefined";

describe.runIf(isSupported)("Intl.Locale.prototype.getTimeZones", () => {
  test("returns region time zones", () => {
    const zones = new Intl.Locale("en-US").getTimeZones();
    expect(Array.isArray(zones)).toBe(true);
    expect(zones[0]).toBe("America/Adak");
    expect(zones.includes("America/New_York")).toBe(true);
  });

  test("returns undefined when the locale has no region", () => {
    expect(new Intl.Locale("en").getTimeZones()).toBeUndefined();
  });

  test("returns an empty array when the region has no commonly used time zones", () => {
    expect(new Intl.Locale("en-BV").getTimeZones()).toEqual([]);
  });
});
