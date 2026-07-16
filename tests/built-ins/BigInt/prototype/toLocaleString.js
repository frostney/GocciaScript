/*---
description: BigInt.prototype.toLocaleString
features: [bigint, Intl]
---*/

test("formats primitive and boxed BigInts", () => {
  expect((42n).toLocaleString()).toBe("42");
  expect(Object(42n).toLocaleString()).toBe("42");
});

test("forwards locales and options to Intl.NumberFormat", () => {
  const options = { style: "currency", currency: "EUR" };
  const value = 1234n;
  const expected = new Intl.NumberFormat("de-DE", options).format(value);

  expect(value.toLocaleString("de-DE", options)).toBe(expected);
  expect(BigInt.prototype.toLocaleString.call(Object(value), "de-DE", options)).toBe(expected);
  expect(value.toLocaleString(["de-DE", "en-US"], options)).toBe(
    new Intl.NumberFormat(["de-DE", "en-US"], options).format(value),
  );
});

test("rejects null options", () => {
  expect(() => (1234n).toLocaleString("en-US", null)).toThrow(TypeError);
});
