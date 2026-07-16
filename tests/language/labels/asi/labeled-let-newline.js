/*---
description: Labeled contextual let identifiers follow ASI in non-strict mode
features: [compat-label, automatic-semicolon-insertion, compat-non-strict-mode]
---*/

describe("labeled contextual let", () => {
  test("let followed by a newline parses as a labeled expression statement", () => {
    if (false) {
      label: let
      x = 1
    }

    expect(true).toBe(true);
  });

  test("object methods do not force strict mode", () => {
    new Function("({ m(){ let + 1; } });");
    expect(true).toBe(true);
  });
});
