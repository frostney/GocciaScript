/*---
description: Intl.RelativeTimeFormat.prototype.resolvedOptions
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.RelativeTimeFormat.prototype.resolvedOptions", () => {
  test("default includes all spec-required fields", () => {
    const opts = new Intl.RelativeTimeFormat("en-US").resolvedOptions();
    expect(opts.locale).toBe("en-US");
    expect(opts.numberingSystem).toBe("latn");
    expect(opts.style).toBe("long");
    expect(opts.numeric).toBe("always");
  });

  test("explicit options are preserved", () => {
    const opts = new Intl.RelativeTimeFormat("en-US", {
      style: "short",
      numeric: "auto",
    }).resolvedOptions();
    expect(opts.style).toBe("short");
    expect(opts.numeric).toBe("auto");
  });
});
