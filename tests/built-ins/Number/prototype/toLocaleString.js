/*---
description: Number.prototype.toLocaleString delegates to Intl.NumberFormat
features: [Intl, Number.prototype.toLocaleString]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Number.prototype.toLocaleString", () => {
  test("formats with the requested locale", () => {
    const value = 1234567.89;

    expect(value.toLocaleString("de-DE")).toBe(new Intl.NumberFormat("de-DE").format(value));
    expect(value.toLocaleString("en-US")).toBe(new Intl.NumberFormat("en-US").format(value));
  });

  test("passes formatting options to Intl.NumberFormat", () => {
    const options = { style: "currency", currency: "EUR" };
    const value = 1234.5;

    expect(value.toLocaleString("de-DE", options)).toBe(new Intl.NumberFormat("de-DE", options).format(value));
  });

  test("works with Number objects", () => {
    const value = new Number(42);

    expect(value.toLocaleString("en-US")).toBe(new Intl.NumberFormat("en-US").format(42));
  });
});
