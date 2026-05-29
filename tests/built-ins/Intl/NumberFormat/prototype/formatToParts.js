/*---
description: Intl.NumberFormat.prototype.formatToParts
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

const partsString = (parts) => {
  return parts.map((part) => part.value).join("");
};

describe.runIf(isIntl)("Intl.NumberFormat.prototype.formatToParts", () => {
  test("formatToParts converts omitted and undefined values to NaN", () => {
    const nf = new Intl.NumberFormat("en-US");
    expect(partsString(nf.formatToParts())).toBe("NaN");
    expect(partsString(nf.formatToParts(undefined))).toBe("NaN");
  });
});
