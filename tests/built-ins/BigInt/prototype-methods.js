/*---
description: BigInt prototype methods
features: [bigint]
---*/

test("toString()", () => {
  expect((42n).toString()).toBe("42");
  expect((-42n).toString()).toBe("-42");
  expect((0n).toString()).toBe("0");
  expect(Object(42n).toString()).toBe("42");
});

test("toString(radix)", () => {
  expect((255n).toString(16)).toBe("ff");
  expect((10n).toString(2)).toBe("1010");
  expect((8n).toString(8)).toBe("10");
});

test("valueOf()", () => {
  expect((42n).valueOf()).toBe(42n);
  expect(Object(42n).valueOf()).toBe(42n);
});

test("BigInt.prototype inherits from Object.prototype", () => {
  expect(Object.getPrototypeOf(BigInt.prototype)).toBe(Object.prototype);
});

test("toLocaleString()", () => {
  expect((42n).toLocaleString()).toBe("42");
  expect(typeof Object(42n).toLocaleString).toBe("function");
  expect(Object(42n).toLocaleString()).toBe("42");
});

test("toLocaleString(locales, options)", () => {
  const options = { style: "currency", currency: "EUR" };
  const value = 1234n;

  expect(value.toLocaleString("de-DE", options)).toBe(
    new Intl.NumberFormat("de-DE", options).format(value)
  );
  expect(BigInt.prototype.toLocaleString.call(Object(value), "de-DE", options)).toBe(
    new Intl.NumberFormat("de-DE", options).format(value)
  );
  expect(value.toLocaleString(["de-DE", "en-US"], options)).toBe(
    new Intl.NumberFormat(["de-DE", "en-US"], options).format(value)
  );
});

test("toLocaleString rejects null options", () => {
  expect(() => (1234n).toLocaleString("en-US", null)).toThrow(TypeError);
});

describe("BigInt.prototype.toString non-finite radix", () => {
  test("Infinity radix throws RangeError", () => {
    expect(() => (5n).toString(Infinity)).toThrow(RangeError);
  });

  test("NaN radix throws RangeError", () => {
    expect(() => (5n).toString(NaN)).toThrow(RangeError);
  });
});

test("boxed BigInt ordinary ToPrimitive observes prototype accessors once", () => {
  const bigIntValueOf = BigInt.prototype.valueOf;
  let toStringGets = 0;
  let valueOfGets = 0;
  let valueOfCalls = 0;
  const valueOfFunction = {
    valueOfReplacement() {
      valueOfCalls++;
      return bigIntValueOf.call(this) * 2n;
    },
  }.valueOfReplacement;

  Object.defineProperty(BigInt.prototype, "toString", {
    get() {
      toStringGets++;
      return undefined;
    },
  });
  Object.defineProperty(BigInt.prototype, "valueOf", {
    get() {
      valueOfGets++;
      return valueOfFunction;
    },
  });

  expect("".concat(Object(1n))).toBe("2");
  expect(toStringGets).toBe(1);
  expect(valueOfGets).toBe(1);
  expect(valueOfCalls).toBe(1);
});
