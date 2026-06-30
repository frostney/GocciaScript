/*---
description: >
  Lazily-materialized Intl is robust to userland tainting: its construction does
  not read or write through Object.prototype accessors, and Number/Date
  toLocaleString use the intrinsic NumberFormat/DateTimeFormat rather than the
  replaceable global. Regression for the lazy-built-in materialization work,
  which made Intl materialize after user code can run.
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("lazy Intl is taint-robust", () => {
  test("Intl constructors materialize after Object.prototype is tainted", () => {
    // Getter-only accessors on Object.prototype: option-record construction must
    // not [[Set]] through them, and undefined-options handling must not [[Get]]
    // through them. This is the first touch of Intl in this file, so the
    // namespace materializes only now — after the taint. A throw here fails the
    // test.
    const tainted = ["type", "style", "numeric", "localeMatcher"];
    for (const name of tainted) {
      Object.defineProperty(Object.prototype, name, {
        configurable: true,
        get() { throw new Error(`Object.prototype.${name} getter must not run`); },
      });
    }
    try {
      expect(new Intl.PluralRules().select(9)).toBe("other");
      expect(typeof new Intl.RelativeTimeFormat().format(1, "day")).toBe("string");
      expect(new Intl.ListFormat([], undefined).resolvedOptions().type).toBe("conjunction");
    } finally {
      for (const name of tainted) delete Object.prototype[name];
    }
  });

  test("Number/Array/Date toLocaleString use the intrinsic, not the global", () => {
    const originalNumberFormat = Intl.NumberFormat;
    const originalDateTimeFormat = Intl.DateTimeFormat;
    Object.defineProperty(Intl, "NumberFormat", {
      configurable: true,
      get() { throw new Error("Intl.NumberFormat global must not be read"); },
    });
    Object.defineProperty(Intl, "DateTimeFormat", {
      configurable: true,
      get() { throw new Error("Intl.DateTimeFormat global must not be read"); },
    });
    try {
      // A throw (from reading the tainted global) fails the test.
      expect(typeof (0).toLocaleString()).toBe("string");
      expect(typeof [1, 2].toLocaleString()).toBe("string");
      expect(typeof new Date(0).toLocaleString()).toBe("string");
      expect(typeof new Date(0).toLocaleDateString()).toBe("string");
      expect(typeof new Date(0).toLocaleTimeString()).toBe("string");
    } finally {
      Object.defineProperty(Intl, "NumberFormat", {
        configurable: true, writable: true, value: originalNumberFormat,
      });
      Object.defineProperty(Intl, "DateTimeFormat", {
        configurable: true, writable: true, value: originalDateTimeFormat,
      });
    }
  });
});
