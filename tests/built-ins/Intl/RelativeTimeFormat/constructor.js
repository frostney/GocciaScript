/*---
description: Intl.RelativeTimeFormat constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl && typeof Intl.RelativeTimeFormat !== "undefined")("Intl.RelativeTimeFormat constructor", () => {
  test("creates an instance with a locale argument", () => {
    const rtf = new Intl.RelativeTimeFormat("en");
    expect(rtf).toBeInstanceOf(Intl.RelativeTimeFormat);
  });

  test("creates an instance with no arguments", () => {
    const rtf = new Intl.RelativeTimeFormat();
    expect(rtf).toBeInstanceOf(Intl.RelativeTimeFormat);
  });

  test("format property is a function", () => {
    const rtf = new Intl.RelativeTimeFormat("en");
    expect(typeof rtf.format).toBe("function");
  });

  test("format returns a string for a relative day value", () => {
    const rtf = new Intl.RelativeTimeFormat("en", { numeric: "always" });
    const result = rtf.format(-1, "day");
    expect(typeof result).toBe("string");
    expect(result.length > 0).toBe(true);
  });

  test("resolvedOptions returns an object with locale and numeric", () => {
    const rtf = new Intl.RelativeTimeFormat("en");
    const options = rtf.resolvedOptions();
    expect(typeof options.locale).toBe("string");
    expect(typeof options.numeric).toBe("string");
  });
});
