/*---
description: >
  Number/Date toLocaleString construct the intrinsic NumberFormat/DateTimeFormat
  rather than the replaceable Intl globals, even after user code taints those
  globals. Regression for the lazy-built-in materialization work.

  The companion "Intl construction is robust to a tainted Object.prototype" path
  only reproduces when Intl materializes *after* user code runs; the first-party
  runner materializes Intl eagerly (its harness touches Intl before any test),
  so that path is covered by the bare-loader scripts/test-cli-intl-taint.ts and
  the pinned test262 intl402 taint suite instead.
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("lazy Intl is taint-robust", () => {
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
