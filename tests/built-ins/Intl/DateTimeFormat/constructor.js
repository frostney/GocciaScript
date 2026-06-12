/*---
description: Intl.DateTimeFormat constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.DateTimeFormat constructor", () => {
  test("creates an instance with no arguments", () => {
    const dtf = new Intl.DateTimeFormat();
    expect(dtf).toBeInstanceOf(Intl.DateTimeFormat);
  });

  test("creates an instance with a locale argument", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    expect(dtf).toBeInstanceOf(Intl.DateTimeFormat);
  });

  test("format property is a function", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    expect(typeof dtf.format).toBe("function");
  });

  test("format returns a string for a Date object", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    const result = dtf.format(new Date(2025, 0, 1));
    expect(typeof result).toBe("string");
    expect(result.length > 0).toBe(true);
  });

  test("resolvedOptions returns an object with locale and timeZone", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    const options = dtf.resolvedOptions();
    expect(typeof options.locale).toBe("string");
    expect(typeof options.timeZone).toBe("string");
  });
});

describe.runIf(isIntl)("Intl.DateTimeFormat non-finite fractionalSecondDigits", () => {
  test("Infinity throws RangeError", () => {
    expect(() => new Intl.DateTimeFormat("en", { fractionalSecondDigits: Infinity })).toThrow(RangeError);
  });

  test("NaN throws RangeError", () => {
    expect(() => new Intl.DateTimeFormat("en", { fractionalSecondDigits: NaN })).toThrow(RangeError);
  });
});
