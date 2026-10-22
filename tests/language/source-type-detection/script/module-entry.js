/*---
description: >
  An export declaration makes an entry with no explicit source type a Module
  (ADR 0140), even with --compat-var: its top-level var lives in the module
  environment, not on the global object (ES2026 §16.2.1.7.3.1
  InitializeEnvironment), and top-level this is undefined (§16.2.1.6.4).
features: [modules, compat-var]
---*/

var detectedModuleVar = "module";
const topLevelThis = this;

export {};

describe("an entry with an export declaration", () => {
  test("a top-level var is not a global property", () => {
    expect(globalThis.detectedModuleVar).toBeUndefined();
    expect(detectedModuleVar).toBe("module");
  });

  test("top-level this is undefined", () => {
    expect(topLevelThis).toBeUndefined();
  });
});
