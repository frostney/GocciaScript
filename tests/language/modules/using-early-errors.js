/*---
description: A using declaration directly in a case or default clause's statement list is an early SyntaxError
features: [explicit-resource-management]
---*/

// ES2027 draft §14.3.1.1: "It is a Syntax Error if UsingDeclaration is
// contained directly within the StatementList of either a CaseClause or
// DefaultClause" (test262 using-invalid-switchstatement-caseclause.js).
describe("using declarations in switch clauses", () => {
  test("directly in a case clause", async () => {
    await expect(
      import("../../../fixtures/modules/using-early-errors/using-in-case-clause.js")
    ).rejects.toThrow(SyntaxError);
  });

  test("directly in a default clause", async () => {
    await expect(
      import("../../../fixtures/modules/using-early-errors/using-in-default-clause.js")
    ).rejects.toThrow(SyntaxError);
  });
});
