/*---
description: >
  Wildcard re-exports (export * from ...) are parsed as no-ops with a warning.
  Code after the skipped export must still execute correctly.
features: [parser-warnings, unsupported-features]
---*/

export * from "./helpers/math-utils.js";

const x = 1;

describe.runIf(typeof GocciaScript !== "undefined")("wildcard re-export is skipped", () => {
  test("code after skipped wildcard re-export executes", () => {
    expect(x).toBe(1);
  });
});
