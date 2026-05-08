/*---
description: Intl.ListFormat constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl && typeof Intl.ListFormat !== "undefined")("Intl.ListFormat constructor", () => {
  test("creates an instance with a locale argument", () => {
    const lf = new Intl.ListFormat("en");
    expect(lf).toBeInstanceOf(Intl.ListFormat);
  });

  test("creates an instance with no arguments", () => {
    const lf = new Intl.ListFormat();
    expect(lf).toBeInstanceOf(Intl.ListFormat);
  });

  test("format property is a function", () => {
    const lf = new Intl.ListFormat("en");
    expect(typeof lf.format).toBe("function");
  });

  test("resolvedOptions returns an object with locale and type", () => {
    const lf = new Intl.ListFormat("en");
    const options = lf.resolvedOptions();
    expect(typeof options.locale).toBe("string");
    expect(typeof options.type).toBe("string");
  });
});
