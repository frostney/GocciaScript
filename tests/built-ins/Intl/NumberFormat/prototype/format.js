/*---
description: Intl.NumberFormat.prototype.format
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.NumberFormat.prototype.format", () => {
  test("format is an accessor property on the prototype", () => {
    const desc = Object.getOwnPropertyDescriptor(Intl.NumberFormat.prototype, "format");
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.value).toBe(undefined);
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
  });

  test("format getter returns a cached bound function", () => {
    const nf = new Intl.NumberFormat("en-US");
    const first = nf.format;
    const second = nf.format;

    expect(typeof first).toBe("function");
    expect(first).toBe(second);
    expect(first(1234)).toBe(nf.format(1234));
    expect(Object.prototype.hasOwnProperty.call(nf, "format")).toBe(false);
  });

  test("format getter creates distinct functions for distinct instances", () => {
    const first = new Intl.NumberFormat("en-US").format;
    const second = new Intl.NumberFormat("en-US").format;

    expect(first === second).toBe(false);
  });

  test("format returns a string", () => {
    const nf = new Intl.NumberFormat("en-US");
    const result = nf.format(1234.56);
    expect(typeof result).toBe("string");
  });

  test("format converts omitted and undefined values to NaN", () => {
    const nf = new Intl.NumberFormat("en-US");
    expect(nf.format()).toBe("NaN");
    expect(nf.format(undefined)).toBe("NaN");
  });

  test("format includes grouping separator for large numbers", () => {
    const nf = new Intl.NumberFormat("en-US");
    const result = nf.format(1234567);
    expect(result.includes(",")).toBe(true);
  });

  test("format applies numberingSystem through ICU", () => {
    expect(new Intl.NumberFormat("en-US", { numberingSystem: "arab" }).format(12345)).toBe("\u0661\u0662\u066c\u0663\u0664\u0665");
    expect(new Intl.NumberFormat("en-US", { numberingSystem: "arabext" }).format(12345)).toBe("\u06f1\u06f2\u066c\u06f3\u06f4\u06f5");
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

  test("format treats NaN as unsigned for signDisplay", () => {
    expect(new Intl.NumberFormat("en-US", { signDisplay: "auto" }).format(NaN)).toBe("NaN");
    expect(new Intl.NumberFormat("en-US", { signDisplay: "always" }).format(NaN)).toBe("+NaN");
    expect(new Intl.NumberFormat("en-US", { signDisplay: "never" }).format(NaN)).toBe("NaN");
    expect(new Intl.NumberFormat("en-US", { signDisplay: "exceptZero" }).format(NaN)).toBe("NaN");
    expect(new Intl.NumberFormat("en-US", { notation: "scientific" }).format(NaN)).toBe("NaN");
    expect(new Intl.NumberFormat("en-US", { notation: "engineering" }).format(NaN)).toBe("NaN");
    expect(new Intl.NumberFormat("en-US", { notation: "compact" }).format(NaN)).toBe("NaN");
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
