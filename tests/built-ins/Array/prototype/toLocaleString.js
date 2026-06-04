/*---
description: Array.prototype.toLocaleString delegates to each element
features: [Intl, Array.prototype.toLocaleString]
---*/

const isIntl = typeof Intl !== "undefined";

describe("Array.prototype.toLocaleString", () => {
  test("exists on Array.prototype", () => {
    expect(typeof Array.prototype.toLocaleString).toBe("function");
  });

  test("invokes each element toLocaleString with locales and options", () => {
    const calls = [];
    const options = { style: "currency", currency: "EUR" };
    const item = {
      toLocaleString(locales, receivedOptions) {
        calls.push([locales, receivedOptions]);
        return "item";
      },
    };

    expect([item, null, undefined, item].toLocaleString("de-DE", options)).toBe("item,,,item");
    expect(calls.length).toBe(2);
    expect(calls[0][0]).toBe("de-DE");
    expect(calls[0][1]).toBe(options);
    expect(calls[1][0]).toBe("de-DE");
    expect(calls[1][1]).toBe(options);
  });

  test("throws when an element toLocaleString is not callable", () => {
    expect(() => [{ toLocaleString: 1 }].toLocaleString()).toThrow(TypeError);
  });

  test("does not delegate to mutable Array helpers", () => {
    const originalFrom = Array.from;
    const originalJoin = Array.prototype.join;

    try {
      Array.from = () => ["tainted"];
      Array.prototype.join = () => "tainted";

      expect([1, 2].toLocaleString()).toBe("1,2");
    } finally {
      Array.from = originalFrom;
      Array.prototype.join = originalJoin;
    }
  });
});

describe.runIf(isIntl)("Array.prototype.toLocaleString Intl elements", () => {
  test("formats number elements through Number.prototype.toLocaleString", () => {
    const options = { style: "currency", currency: "EUR" };
    const expected = new Intl.NumberFormat("de-DE", options).format(1234.5);

    expect([1234.5].toLocaleString("de-DE", options)).toBe(expected);
  });

  test("formats BigInt elements through BigInt.prototype.toLocaleString", () => {
    const options = { style: "currency", currency: "EUR" };
    const expected = new Intl.NumberFormat("de-DE", options).format(1234n);

    expect([1234n].toLocaleString("de-DE", options)).toBe(expected);
  });
});
