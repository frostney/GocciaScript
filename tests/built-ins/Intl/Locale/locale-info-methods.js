/*---
description: Intl.Locale locale information methods
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl && typeof Intl.Locale !== "undefined")("Intl.Locale locale information methods", () => {
  test("getCalendars returns preferred calendars", () => {
    expect(new Intl.Locale("en").getCalendars()).toEqual(["gregory"]);
    expect(new Intl.Locale("en-u-ca-buddhist").getCalendars()).toEqual(["buddhist"]);
  });

  test("getCollations returns supported collations", () => {
    expect(new Intl.Locale("en").getCollations()).toEqual(["emoji", "eor"]);
    expect(new Intl.Locale("en-u-co-phonebk").getCollations()).toEqual(["phonebk"]);
  });

  test("getHourCycles returns preferred hour cycles", () => {
    expect(new Intl.Locale("en").getHourCycles()).toEqual(["h12"]);
    expect(new Intl.Locale("en-GB").getHourCycles()).toEqual(["h23"]);
    expect(new Intl.Locale("en-u-hc-h24").getHourCycles()).toEqual(["h24"]);
  });

  test("getNumberingSystems returns preferred numbering systems", () => {
    expect(new Intl.Locale("en").getNumberingSystems()).toEqual(["latn"]);
    expect(new Intl.Locale("ar").getNumberingSystems()).toEqual(["arab"]);
    expect(new Intl.Locale("ar-u-nu-latn").getNumberingSystems()).toEqual(["latn"]);
  });

  test("getTimeZones returns region time zones or undefined", () => {
    const usZones = new Intl.Locale("en-US").getTimeZones();
    expect(Array.isArray(usZones)).toBe(true);
    expect(usZones[0]).toBe("America/Adak");
    expect(usZones.includes("America/New_York")).toBe(true);
    expect(new Intl.Locale("en").getTimeZones()).toBeUndefined();
  });

  test("getTextInfo returns text direction", () => {
    expect(new Intl.Locale("en").getTextInfo()).toEqual({ direction: "ltr" });
    expect(new Intl.Locale("ar").getTextInfo()).toEqual({ direction: "rtl" });
  });

  test("getWeekInfo returns first day and weekend data", () => {
    expect(new Intl.Locale("en-US").getWeekInfo()).toEqual({ firstDay: 7, weekend: [6, 7] });
    expect(new Intl.Locale("en-GB").getWeekInfo()).toEqual({ firstDay: 1, weekend: [6, 7] });
    expect(new Intl.Locale("en-US-u-fw-tue").getWeekInfo()).toEqual({ firstDay: 2, weekend: [6, 7] });
    expect(new Intl.Locale("en-US", { firstDayOfWeek: "mon" }).getWeekInfo()).toEqual({
      firstDay: 1,
      weekend: [6, 7],
    });
  });
});
