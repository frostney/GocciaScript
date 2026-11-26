/*---
description: An arrow function is an operand of an operator only when parenthesized
features: [modules]
---*/

describe("operand early errors", () => {
  test("an arrow function as a binary operand is a SyntaxError", async () => {
    await expect(import("../../../fixtures/modules/operand-early-errors/arrow-binary-operand.js")).rejects.toThrow(SyntaxError);
  });

  test("an arrow function as a unary operand is a SyntaxError", async () => {
    await expect(import("../../../fixtures/modules/operand-early-errors/arrow-unary-operand.js")).rejects.toThrow(SyntaxError);
  });

  test("a parenthesized arrow function is an operand", async () => {
    const module = await import("../../../fixtures/modules/operand-early-errors/parenthesized-arrow-operand.js");
    expect(module.value).toBe("3function");
  });
});
