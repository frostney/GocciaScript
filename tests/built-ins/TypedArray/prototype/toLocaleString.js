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

  test("invokes BigInt element toLocaleString with locales and options", () => {
    const original = BigInt.prototype.toLocaleString;
    const options = { marker: "yes" };

    try {
      BigInt.prototype.toLocaleString = {
        method(locales, receivedOptions) {
          return "bigint:" + locales + ":" + receivedOptions.marker;
        },
      }.method;

      expect(new BigInt64Array([0n]).toLocaleString("xx", options)).toBe("bigint:xx:yes");
    } finally {
      BigInt.prototype.toLocaleString = original;
    }
  });
});

describe.runIf(isIntl)("%TypedArray%.prototype.toLocaleString Intl elements", () => {
  test("formats number elements through Number.prototype.toLocaleString", () => {
    const options = { minimumFractionDigits: 3 };
    const expected = new Intl.NumberFormat("th-u-nu-thai", options).format(0);

    expect(new Uint8Array([0]).toLocaleString("th-u-nu-thai", options)).toBe(expected);
  });

  test("formats BigInt elements through BigInt.prototype.toLocaleString", () => {
    const options = { useGrouping: false };
    const expected = new Intl.NumberFormat("de-DE", options).format(1234n);

    expect(new BigInt64Array([1234n]).toLocaleString("de-DE", options)).toBe(expected);
  });
});
