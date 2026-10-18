/*---
description: >
  A dynamic import() is an expression that Script code may contain (ES2026
  §13.3.10), so an entry whose only import is dynamic stays a Script
  (ADR 0140): its top-level var is a global property and top-level this is
  the global object.
features: [compat-var, dynamic-import]
---*/

var dynamicImportScriptVar = "global";
const topLevelThis = this;
const loadedNamespace = await import("../../../../fixtures/modules/detected-dynamic-target.js");

describe("an entry whose only import is dynamic", () => {
  test("the dynamic import loads the module", () => {
    expect(loadedNamespace.dynamicValue).toBe("loaded");
  });

  test("a top-level var is a global property", () => {
    expect(globalThis.dynamicImportScriptVar).toBe("global");
  });

  test("top-level this is the global object", () => {
    expect(topLevelThis).toBe(globalThis);
  });
});
