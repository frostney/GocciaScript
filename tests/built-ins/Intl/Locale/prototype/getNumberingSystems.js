/*---
description: Intl.Locale.prototype.getNumberingSystems
features: [Intl]
---*/

const isSupported = typeof Intl !== "undefined" && typeof Intl.Locale !== "undefined";

describe.runIf(isSupported)("Intl.Locale.prototype.getNumberingSystems", () => {
  test("returns locale preferences and honors an explicit numbering system", () => {
    expect(new Intl.Locale("en").getNumberingSystems()).toEqual(["latn"]);
    expect(new Intl.Locale("ar").getNumberingSystems()).toEqual(["arab"]);
    expect(new Intl.Locale("ar-u-nu-latn").getNumberingSystems()).toEqual(["latn"]);
  });

  test("uses constructor language and region overrides", () => {
    expect(new Intl.Locale("en", { language: "ar", region: "EG" }).getNumberingSystems()).toEqual(["arab"]);
  });
});
