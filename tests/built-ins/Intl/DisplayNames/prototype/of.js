/*---
description: Intl.DisplayNames.prototype.of
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const hasFullICU = isIntl && new Intl.NumberFormat("en-US").format(NaN) === "NaN";

describe.runIf(isIntl && typeof Intl.DisplayNames !== "undefined")("Intl.DisplayNames.prototype.of", () => {
  test("returns localized currency display names", () => {
    const dn = new Intl.DisplayNames("en", { type: "currency" });

    expect(dn.of("USD")).toBe(hasFullICU ? "US Dollar" : "USD");
  });

  test("returns current currency display names missing from older ICU data", () => {
    const dn = new Intl.DisplayNames("en", { type: "currency", fallback: "none" });

    expect(dn.of("XCG")).toBe("Caribbean guilder");
    expect(dn.of("ZWG")).toBe("Zimbabwean Gold");
  });

  test("returns localized script display names", () => {
    const dn = new Intl.DisplayNames("en", { type: "script" });

    expect(dn.of("Latn")).toBe("Latin");
  });

  test("returns localized region display names", () => {
    const dn = new Intl.DisplayNames("en", { type: "region" });

    expect(dn.of("US")).toBe("United States");
  });
});
