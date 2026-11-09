/*---
description: Module export early errors reject the import with a SyntaxError
features: [modules]
---*/

describe("export early errors", () => {
  test("an export list may not repeat an exported name", async () => {
    await expect(import("../../../fixtures/modules/duplicate-export-names.js")).rejects.toThrow(SyntaxError);
  });

  test("an escaped async after export default is no async function declaration", async () => {
    await expect(import("../../../fixtures/modules/escaped-async-default.js")).rejects.toThrow(SyntaxError);
  });
});
