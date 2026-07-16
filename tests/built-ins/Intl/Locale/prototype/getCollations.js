/*---
description: Intl.Locale.prototype.getCollations
features: [Intl]
---*/

const isSupported = typeof Intl !== "undefined" && typeof Intl.Locale !== "undefined";
const hasFullICU = isSupported && new Intl.NumberFormat("en-US").format(NaN) === "NaN";

describe.runIf(isSupported)("Intl.Locale.prototype.getCollations", () => {
  test("returns supported collations and honors an explicit collation", () => {
    const englishCollations = new Intl.Locale("en").getCollations();
    if (hasFullICU) {
      expect(englishCollations).toEqual(["emoji", "eor"]);
    } else {
      expect(Array.isArray(englishCollations)).toBe(true);
      expect(englishCollations.includes("search")).toBe(false);
    }
    expect(new Intl.Locale("en-u-co-phonebk").getCollations()).toEqual(["phonebk"]);
  });

  test("returns canonical collation identifiers", () => {
    if (!hasFullICU) {
      expect(Array.isArray(new Intl.Locale("de").getCollations())).toBe(true);
      return;
    }

    const germanCollations = new Intl.Locale("de").getCollations();
    expect(germanCollations.includes("phonebk")).toBe(true);
    expect(germanCollations.includes("phonebook")).toBe(false);

    const sinhalaCollations = new Intl.Locale("si").getCollations();
    expect(sinhalaCollations.includes("dict")).toBe(true);
    expect(sinhalaCollations.includes("dictionary")).toBe(false);
  });
});
