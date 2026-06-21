/*---
description: Shared Intl constructor and prototype substrate
features: [Intl, Reflect, Symbol]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl constructor/prototype substrate", () => {
  test("Intl prototypes inherit from Object.prototype", () => {
    expect(Object.getPrototypeOf(Intl.Collator.prototype)).toBe(Object.prototype);
    expect(Object.getPrototypeOf(Intl.NumberFormat.prototype)).toBe(Object.prototype);
    expect(Object.getPrototypeOf(Intl.DateTimeFormat.prototype)).toBe(Object.prototype);
    expect(Object.getPrototypeOf(Intl.PluralRules.prototype)).toBe(Object.prototype);
  });

  test("construct-only Intl constructors reject function calls", () => {
    expect(() => Intl.PluralRules()).toThrow(TypeError);
    expect(() => Intl.RelativeTimeFormat()).toThrow(TypeError);
    expect(() => Intl.Segmenter()).toThrow(TypeError);
  });

  test("Reflect.construct honors newTarget.prototype", () => {
    const NewTarget = class {};
    const proto = NewTarget.prototype;

    const collator = Reflect.construct(Intl.Collator, [], NewTarget);
    expect(Object.getPrototypeOf(collator)).toBe(proto);
  });

  test("legacy NumberFormat calls define a non-enumerable fallback symbol", () => {
    const receiver = new Intl.NumberFormat();
    const chained = Intl.NumberFormat.call(receiver);
    const fallbackSymbol = Object.getOwnPropertySymbols(chained)
      .find((symbol) => symbol.description === "IntlLegacyConstructedSymbol");
    const descriptor = Object.getOwnPropertyDescriptor(chained, fallbackSymbol);

    expect(chained).toBe(receiver);
    expect(typeof fallbackSymbol).toBe("symbol");
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(descriptor.writable).toBe(false);
  });
});
