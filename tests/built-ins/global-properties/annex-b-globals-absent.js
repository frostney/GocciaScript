/*---
description: >
  The recommended profile leaves out the Annex B globals escape and unescape
  (ADR 0177); compat-annex-b turns them on.
features: [global-properties, escape, unescape]
---*/

describe("escape and unescape are absent by default", () => {
  test("typeof escape and unescape are 'undefined'", () => {
    expect(typeof escape).toBe("undefined");
    expect(typeof unescape).toBe("undefined");
  });

  test("globalThis has no escape or unescape property", () => {
    expect("escape" in globalThis).toBe(false);
    expect("unescape" in globalThis).toBe(false);
  });
});
