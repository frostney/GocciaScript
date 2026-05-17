/*---
description: Intl.NumberFormat.prototype.formatRange
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.NumberFormat.prototype.formatRange", () => {
  test("formatRange is exposed", () => {
    const nf = new Intl.NumberFormat("en-US");
    expect(typeof nf.formatRange).toBe("function");
  });

  test("formats a currency range with locale range separator", () => {
    const nf = new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: "USD",
      maximumFractionDigits: 0
    });
    expect(nf.formatRange(3, 5)).toBe("$3 – $5");
  });

  test("uses approximate formatting for values equal after rounding", () => {
    const nf = new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: "USD",
      maximumFractionDigits: 0
    });
    expect(nf.formatRange(2.9, 3.1)).toBe("~$3");
  });

  test("throws RangeError for NaN endpoints", () => {
    const nf = new Intl.NumberFormat("en-US");
    expect(() => nf.formatRange(NaN, 1)).toThrow(RangeError);
    expect(() => nf.formatRange(1, NaN)).toThrow(RangeError);
  });
});
