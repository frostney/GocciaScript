/*---
description: TypedArray.prototype.toLocaleString delegates to each element
features: [Intl, TypedArray.prototype.toLocaleString]
---*/

const isIntl = typeof Intl !== "undefined";

describe("%TypedArray%.prototype.toLocaleString", () => {
  test("invokes number element toLocaleString with locales and options", () => {
    const original = Number.prototype.toLocaleString;
    const options = { marker: "yes" };

    try {
      Number.prototype.toLocaleString = {
        method(locales, receivedOptions) {
          return "number:" + locales + ":" + receivedOptions.marker;
        },
      }.method;

      expect(new Uint8Array([0]).toLocaleString("xx", options)).toBe("number:xx:yes");
    } finally {
      Number.prototype.toLocaleString = original;
    }
  });
});

describe.runIf(isIntl)("%TypedArray%.prototype.toLocaleString Intl elements", () => {
  test("formats number elements through Number.prototype.toLocaleString", () => {
    const options = { minimumFractionDigits: 3 };
    const expected = new Intl.NumberFormat("th-u-nu-thai", options).format(0);

    expect(new Uint8Array([0]).toLocaleString("th-u-nu-thai", options)).toBe(expected);
  });
});
