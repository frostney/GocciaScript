/*---
description: Intl.NumberFormat.prototype.resolvedOptions
features: [Intl]
---*/

describe("Intl.NumberFormat.prototype.resolvedOptions", () => {
  test("decimal style includes all spec-required fields", () => {
    const opts = new Intl.NumberFormat("en-US").resolvedOptions();
    expect(opts.locale).toBe("en-US");
    expect(opts.numberingSystem).toBe("latn");
    expect(opts.style).toBe("decimal");
    expect(opts.minimumIntegerDigits).toBe(1);
    expect(opts.minimumFractionDigits).toBe(0);
    expect(opts.maximumFractionDigits).toBe(3);
    expect(opts.useGrouping).toBe("auto");
    expect(opts.notation).toBe("standard");
    expect(opts.signDisplay).toBe("auto");
    expect(opts.roundingMode).toBe("halfExpand");
    expect(opts.roundingIncrement).toBe(1);
    expect(opts.roundingPriority).toBe("auto");
    expect(opts.trailingZeroDisplay).toBe("auto");
  });

  test("currency style includes currency-specific fields", () => {
    const opts = new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: "USD",
    }).resolvedOptions();
    expect(opts.style).toBe("currency");
    expect(opts.currency).toBe("USD");
    expect(opts.currencyDisplay).toBe("symbol");
    expect(opts.currencySign).toBe("standard");
    expect(opts.minimumFractionDigits).toBe(2);
    expect(opts.maximumFractionDigits).toBe(2);
  });

  test("currency style omits unit fields", () => {
    const opts = new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: "USD",
    }).resolvedOptions();
    expect(opts.unit).toBe(undefined);
    expect(opts.unitDisplay).toBe(undefined);
  });

  test("percent style resolves fraction digits to 0", () => {
    const opts = new Intl.NumberFormat("en-US", {
      style: "percent",
    }).resolvedOptions();
    expect(opts.minimumFractionDigits).toBe(0);
    expect(opts.maximumFractionDigits).toBe(0);
  });

  test("compact notation includes compactDisplay", () => {
    const opts = new Intl.NumberFormat("en-US", {
      notation: "compact",
    }).resolvedOptions();
    expect(opts.notation).toBe("compact");
    expect(opts.compactDisplay).toBe("short");
  });

  test("standard notation omits compactDisplay", () => {
    const opts = new Intl.NumberFormat("en-US").resolvedOptions();
    expect(opts.compactDisplay).toBe(undefined);
  });

  test("significantDigits rounding omits fraction digit fields", () => {
    const opts = new Intl.NumberFormat("en-US", {
      minimumSignificantDigits: 1,
      maximumSignificantDigits: 5,
    }).resolvedOptions();
    expect(opts.minimumSignificantDigits).toBe(1);
    expect(opts.maximumSignificantDigits).toBe(5);
    expect(opts.minimumFractionDigits).toBe(undefined);
    expect(opts.maximumFractionDigits).toBe(undefined);
  });
});
