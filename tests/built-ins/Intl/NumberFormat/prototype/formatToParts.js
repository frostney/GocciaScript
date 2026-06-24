/*---
description: Intl.NumberFormat.prototype.formatToParts
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const hasFullICU = isIntl && new Intl.NumberFormat("en-US").format(NaN) === "NaN";

const partsString = (parts) => {
  return parts.map((part) => part.value).join("");
};

describe.runIf(isIntl)("Intl.NumberFormat.prototype.formatToParts", () => {
  test("formatToParts converts omitted and undefined values to NaN", () => {
    const nf = new Intl.NumberFormat("en-US");
    if (!hasFullICU) {
      expect(typeof partsString(nf.formatToParts())).toBe("string");
      expect(typeof partsString(nf.formatToParts(undefined))).toBe("string");
      return;
    }
    expect(partsString(nf.formatToParts())).toBe("NaN");
    expect(partsString(nf.formatToParts(undefined))).toBe("NaN");
  });
});
