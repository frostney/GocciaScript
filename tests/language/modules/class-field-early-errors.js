/*---
description: yield in a class field initializer, directly or in a class nested in it, is a SyntaxError inside a generator
features: [class-fields-public, class-static-fields-public, generators]
---*/

// ES2026 §15.7 passes [?Yield] to a field's Initializer, but an initializer is
// evaluated as a function body of its own, so suspending the enclosing
// generator from it cannot work; tc39/ecma262#3333 records the spec bug, and
// V8, SpiderMonkey and JavaScriptCore reject `yield` there. A computed key of
// the class itself still yields in the enclosing generator
// (tests/language/function-keyword/class-computed-field-yield.js).
describe("yield in class field initializers", () => {
  test("directly in an initializer", async () => {
    await expect(
      import("../../../fixtures/modules/class-field-early-errors/yield-in-field-initializer.js")
    ).rejects.toThrow(SyntaxError);
  });

  test("in the computed key of a class inside a static initializer", async () => {
    await expect(
      import("../../../fixtures/modules/class-field-early-errors/yield-in-nested-class-key.js")
    ).rejects.toThrow(SyntaxError);
  });
});
