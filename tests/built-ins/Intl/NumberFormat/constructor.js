/*---
description: Intl.NumberFormat constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.NumberFormat constructor", () => {
  test("creates an instance with no arguments", () => {
    const nf = new Intl.NumberFormat();
    expect(nf).toBeInstanceOf(Intl.NumberFormat);
  });

  test("creates an instance with a locale argument", () => {
    const nf = new Intl.NumberFormat("en-US");
    expect(nf).toBeInstanceOf(Intl.NumberFormat);
  });

  test("format property is a function", () => {
    const nf = new Intl.NumberFormat("en-US");
    expect(typeof nf.format).toBe("function");
  });

  test("resolvedOptions returns an object with locale", () => {
    const nf = new Intl.NumberFormat("en-US");
    const options = nf.resolvedOptions();
    expect(typeof options.locale).toBe("string");
  });
});

describe("Intl.NumberFormat non-finite digit options", () => {
  test("maximumFractionDigits Infinity throws RangeError", () => {
    expect(() => new Intl.NumberFormat("en", { maximumFractionDigits: Infinity })).toThrow(RangeError);
  });

  test("maximumFractionDigits NaN throws RangeError", () => {
    expect(() => new Intl.NumberFormat("en", { maximumFractionDigits: NaN })).toThrow(RangeError);
  });

  test("minimumIntegerDigits -Infinity throws RangeError", () => {
    expect(() => new Intl.NumberFormat("en", { minimumIntegerDigits: -Infinity })).toThrow(RangeError);
  });
});
