/*---
description: >
  The names a module's imports bind are among its LexicallyDeclaredNames, so
  a module that also declares one of them is a SyntaxError before it runs
  (ES2026 §16.2.1.1 Static Semantics: Early Errors). Node.js rejects the
  same module.
features: [modules, dynamic-import]
---*/

describe("an imported binding redeclared in the module", () => {
  test("loading the module rejects with a SyntaxError", async () => {
    let caught;
    try {
      await import("../../../fixtures/modules/duplicate-imported-binding.js");
    } catch (error) {
      caught = error;
    }
    expect(caught).toBeInstanceOf(SyntaxError);
    expect(caught.message).toBe('Identifier "dynamicValue" has already been declared');
  });
});
