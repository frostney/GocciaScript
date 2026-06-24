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

  test("unsupported requested locales resolve to each constructor default", () => {
    const constructors = [
      [Intl.Collator],
      [Intl.NumberFormat],
      [Intl.DateTimeFormat],
      [Intl.PluralRules],
      [Intl.RelativeTimeFormat],
      [Intl.ListFormat],
      [Intl.DisplayNames, { type: "language" }],
      [Intl.Segmenter],
      [Intl.DurationFormat],
    ];

    expect(Intl.DateTimeFormat.supportedLocalesOf(["zz-ZZ"]).length).toBe(0);

    for (const [Ctor, options] of constructors) {
      const defaultLocale = new Ctor(undefined, options).resolvedOptions().locale;
      const unsupportedLocale = new Ctor("zz-ZZ", options).resolvedOptions().locale;
      const unsupportedExtensionLocale = new Ctor("zz-ZZ-u-ca-gregory", options)
        .resolvedOptions().locale;

      expect(unsupportedLocale).toBe(defaultLocale);
      expect(unsupportedExtensionLocale).toBe(defaultLocale);
      expect(unsupportedLocale.includes("zz")).toBe(false);
      expect(unsupportedExtensionLocale.includes("zz")).toBe(false);
    }
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
