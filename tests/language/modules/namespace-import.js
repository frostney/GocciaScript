/*---
description: >
  Namespace imports are parsed as no-ops with a warning. Code after the skipped
  import must still execute correctly, and no binding is created.
features: [parser-warnings, unsupported-features]
---*/

import * as math from "./helpers/math-utils.js";

const x = 1;

describe.runIf(typeof GocciaScript !== "undefined")("namespace import is skipped", () => {
  test("code after skipped namespace import executes", () => {
    expect(x).toBe(1);
    expect(typeof math).toBe("undefined");
  });
});
