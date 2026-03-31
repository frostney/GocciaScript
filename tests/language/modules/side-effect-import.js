/*---
description: >
  Side-effect imports are parsed as no-ops with a warning. Code after the
  skipped import must still run.
features: [parser-warnings, unsupported-features]
---*/

import "./helpers/math-utils.js";

const x = 1;

describe.runIf(typeof GocciaScript !== "undefined")("side-effect import is skipped", () => {
  test("code after skipped side-effect import executes", () => {
    expect(x).toBe(1);
  });
});
