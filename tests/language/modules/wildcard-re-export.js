/*---
description: >
  Wildcard re-exports (export * from ...) forward named exports and code after
  the export still executes.
features: [modules]
---*/

export * from "./helpers/math-utils.js";

import { add, multiply, PI } from "./helpers/wildcard-re-exporter.js";

const x = 1;

describe.runIf(typeof Goccia !== "undefined")("wildcard re-export", () => {
  test("re-exported names are importable", () => {
    expect(add(2, 3)).toBe(5);
    expect(multiply(4, 5)).toBe(20);
    expect(PI).toBe(3.14159);
  });

  test("code after wildcard re-export executes", () => {
    expect(x).toBe(1);
  });
});
