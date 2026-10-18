/*---
description: >
  An entry with no explicit source type and no import or export declaration
  stays a Script (ADR 0140): its top-level var is a property of the global
  object (ES2026 §16.1.7 GlobalDeclarationInstantiation) and top-level this
  is the global object.
features: [compat-var]
---*/

var detectedScriptVar = "global";
const topLevelThis = this;

describe("an entry without module syntax", () => {
  test("a top-level var is a global property", () => {
    expect(globalThis.detectedScriptVar).toBe("global");
  });

  test("top-level this is the global object", () => {
    expect(topLevelThis).toBe(globalThis);
  });
});
