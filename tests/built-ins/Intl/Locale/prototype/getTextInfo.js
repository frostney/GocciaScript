/*---
description: Intl.Locale.prototype.getTextInfo
features: [Intl]
---*/

const isSupported = typeof Intl !== "undefined" && typeof Intl.Locale !== "undefined";

describe.runIf(isSupported)("Intl.Locale.prototype.getTextInfo", () => {
  test("returns direction from the locale script", () => {
    expect(new Intl.Locale("en").getTextInfo()).toEqual({ direction: "ltr" });
    expect(new Intl.Locale("ar").getTextInfo()).toEqual({ direction: "rtl" });
    expect(new Intl.Locale("ar-Latn").getTextInfo()).toEqual({ direction: "ltr" });
    expect(new Intl.Locale("en-Hatr").getTextInfo()).toEqual({ direction: "rtl" });
  });

  test("uses constructor language and script overrides", () => {
    expect(new Intl.Locale("en", { language: "ar", region: "EG" }).getTextInfo()).toEqual({ direction: "rtl" });
    expect(new Intl.Locale("az", { script: "Arab" }).getTextInfo()).toEqual({ direction: "rtl" });
  });
});
