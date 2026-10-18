/*---
description: >
  An explicit "source-type": "script" keeps an entry a Script whatever it
  contains (ADR 0140): GocciaScript accepts import and export declarations
  in Script source, its top-level var stays a global property, and top-level
  this stays the global object.
features: [compat-var]
---*/

var explicitScriptVar = "global";
const topLevelThis = this;

export const explicitScriptExport = "exported";

describe("an explicit Script entry with an export declaration", () => {
  test("a top-level var is a global property", () => {
    expect(globalThis.explicitScriptVar).toBe("global");
  });

  test("top-level this is the global object", () => {
    expect(topLevelThis).toBe(globalThis);
  });

  test("the exported binding is usable in the file", () => {
    expect(explicitScriptExport).toBe("exported");
  });
});
