/*---
description: >
  Side-effect imports evaluate their dependency and code after the import runs.
features: [modules]
---*/

import "./helpers/side-effect-marker.js";

const x = 1;

describe.runIf(typeof Goccia !== "undefined")("side-effect import", () => {
  test("side-effect dependency evaluates", () => {
    expect(globalThis.moduleSideEffectImportCount).toBe(1);
  });

  test("code after side-effect import executes", () => {
    expect(x).toBe(1);
  });
});
