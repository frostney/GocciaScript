/*---
description: Intl.Locale.prototype.getWeekInfo
features: [Intl]
---*/

const isSupported = typeof Intl !== "undefined" && typeof Intl.Locale !== "undefined";

describe.runIf(isSupported)("Intl.Locale.prototype.getWeekInfo", () => {
  test("returns first-day and weekend data", () => {
    expect(new Intl.Locale("en-US").getWeekInfo()).toEqual({ firstDay: 7, weekend: [6, 7] });
    expect(new Intl.Locale("en-GB").getWeekInfo()).toEqual({ firstDay: 1, weekend: [6, 7] });
  });

  test("honors extension and constructor overrides", () => {
    expect(new Intl.Locale("en-US-u-fw-tue").getWeekInfo()).toEqual({ firstDay: 2, weekend: [6, 7] });
    expect(new Intl.Locale("en-US", { firstDayOfWeek: "mon" }).getWeekInfo()).toEqual({
      firstDay: 1,
      weekend: [6, 7],
    });
    expect(new Intl.Locale("en", { language: "ar", region: "EG" }).getWeekInfo()).toEqual({
      firstDay: 6,
      weekend: [5, 6],
    });
  });
});
