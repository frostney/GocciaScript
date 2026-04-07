/*---
description: >
  Default imports are parsed as no-ops with a warning. Code after the skipped
  import must still execute correctly, and no binding is created.
features: [parser-warnings, unsupported-features]
---*/

import addDefault from "./helpers/math-utils.js";

const x = 1;

describe.runIf(typeof Goccia !== "undefined")("default import is skipped", () => {
  test("code after skipped default import executes", () => {
    expect(x).toBe(1);
    expect(typeof addDefault).toBe("undefined");
  });
});
