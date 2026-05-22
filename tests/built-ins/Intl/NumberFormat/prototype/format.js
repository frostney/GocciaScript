/*---
description: Intl.NumberFormat.prototype.format
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.NumberFormat.prototype.format", () => {
  test("format returns a string", () => {
    const nf = new Intl.NumberFormat("en-US");
    const result = nf.format(1234.56);
    expect(typeof result).toBe("string");
  });

  test("format includes grouping separator for large numbers", () => {
    const nf = new Intl.NumberFormat("en-US");
    const result = nf.format(1234567);
    expect(result.includes(",")).toBe(true);
  });

  test("currency format produces a string containing the currency symbol", () => {
    const nf = new Intl.NumberFormat("en-US", { style: "currency", currency: "USD" });
    const result = nf.format(9.99);
    expect(result.includes("$")).toBe(true);
  });

  test("percent format produces a string containing the percent sign", () => {
    const nf = new Intl.NumberFormat("en-US", { style: "percent" });
    const result = nf.format(0.75);
    expect(result.includes("%")).toBe(true);
  });

  test("format preserves negative zero", () => {
    expect(new Intl.NumberFormat("en-US").format(-0)).toBe("-0");
    expect(new Intl.NumberFormat("en-US", {
      minimumFractionDigits: 2,
      maximumFractionDigits: 2,
    }).format(-0)).toBe("-0.00");
    expect(new Intl.NumberFormat("en-US", {
      maximumFractionDigits: 1,
      signDisplay: "exceptZero",
    }).format(-0.01)).toBe("0");
  });

  test("significant digit formatting keeps plain decimal precision", () => {
    expect(new Intl.NumberFormat("en-US", {
      useGrouping: false,
      minimumSignificantDigits: 1,
    }).format(0.00000123)).toBe("0.00000123");

    expect(new Intl.NumberFormat("en-US", {
      useGrouping: false,
      minimumSignificantDigits: 1,
    }).format(1234567890123456)).toBe("1234567890123456");
  });

  test("lessPrecision chooses the less precise mixed digit result", () => {
    const nf = new Intl.NumberFormat("en-US", {
      useGrouping: false,
      roundingPriority: "lessPrecision",
      minimumFractionDigits: 1,
      maximumFractionDigits: 3,
      minimumSignificantDigits: 3,
      maximumSignificantDigits: 3,
    });
    expect(nf.format(1)).toBe("1.00");
    expect(nf.format(1.23456)).toBe("1.23");
  });

  test("morePrecision chooses the more precise mixed digit result", () => {
    const nf = new Intl.NumberFormat("en-US", {
      useGrouping: false,
      roundingPriority: "morePrecision",
      minimumFractionDigits: 1,
      maximumFractionDigits: 3,
      minimumSignificantDigits: 3,
      maximumSignificantDigits: 3,
    });
    expect(nf.format(1)).toBe("1.0");
    expect(nf.format(1.23456)).toBe("1.235");
  });

  test("large roundingIncrement values round on the fraction digit grid", () => {
    expect(new Intl.NumberFormat("en-US", {
      useGrouping: false,
      roundingIncrement: 1000,
      minimumFractionDigits: 3,
      maximumFractionDigits: 3,
    }).format(1.0005)).toBe("1.000");

    expect(new Intl.NumberFormat("en-US", {
      useGrouping: false,
      roundingIncrement: 2500,
      minimumFractionDigits: 3,
      maximumFractionDigits: 3,
    }).format(1.5)).toBe("2.500");
  });

  test("notation options use ICU skeleton formatting", () => {
    expect(new Intl.NumberFormat("en-US", {
      notation: "scientific",
      maximumFractionDigits: 2,
    }).format(12345)).toBe("1.23E4");

    expect(new Intl.NumberFormat("en-US", {
      notation: "engineering",
      maximumFractionDigits: 2,
    }).format(12345)).toBe("12.35E3");

    expect(new Intl.NumberFormat("en-US", {
      notation: "compact",
      useGrouping: false,
    }).format(1234)).toBe("1.2K");

    expect(new Intl.NumberFormat("en-US", {
      notation: "compact",
      compactDisplay: "long",
      useGrouping: false,
    }).format(1234)).toBe("1.2 thousand");
  });
});
