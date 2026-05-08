/*---
description: Intl.DurationFormat constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl && typeof Intl.DurationFormat !== "undefined")("Intl.DurationFormat constructor", () => {
  test("creates an instance with a locale argument", () => {
    const df = new Intl.DurationFormat("en");
    expect(df).toBeInstanceOf(Intl.DurationFormat);
  });

  test("creates an instance with no arguments", () => {
    const df = new Intl.DurationFormat();
    expect(df).toBeInstanceOf(Intl.DurationFormat);
  });

  test("format property is a function", () => {
    const df = new Intl.DurationFormat("en");
    expect(typeof df.format).toBe("function");
  });

  test("format returns a string for a duration object", () => {
    const df = new Intl.DurationFormat("en");
    const result = df.format({ hours: 1, minutes: 30 });
    expect(typeof result).toBe("string");
    expect(result.length > 0).toBe(true);
  });

  test("resolvedOptions returns an object with locale", () => {
    const df = new Intl.DurationFormat("en");
    const options = df.resolvedOptions();
    expect(typeof options.locale).toBe("string");
  });
});
