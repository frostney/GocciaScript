/*---
description: Module lexical names include imports and named default declarations, with var names collected through nested statements
features: [modules, dynamic-import]
---*/

describe("module declaration early errors", () => {
  for (const name of ["import-nested-var", "nested-var-import", "default-import-nested-var", "namespace-import-nested-var", "import-using", "default-class-lexical", "lexical-default-class", "default-function-var", "var-default-function", "default-generator-lexical", "default-async-function-lexical", "default-async-generator-lexical"]) {
    test(`rejects ${name} before evaluation`, async () => {
      let caught;
      try {
        await import(`../../../../fixtures/modules/declaration-early-errors/${name}.js`);
      } catch (error) {
        caught = error;
      }
      expect(caught).toBeInstanceOf(SyntaxError);
      expect(caught.message).toContain("already been declared");
    });
  }
  for (const name of ["import-inner-shadow", "parenthesized-default-function", "parenthesized-default-class", "anonymous-default-function", "anonymous-default-class"]) {
    test(`accepts ${name}`, async () => {
      const module = await import(`../../../../fixtures/modules/declaration-early-errors/${name}.js`);
      expect(module.value).toBe(1);
    });
  }
});
