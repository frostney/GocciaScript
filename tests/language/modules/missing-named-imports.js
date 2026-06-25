const expectMissingExportRejection = async (specifier) => {
  let error;
  try {
    await import(specifier);
  } catch (e) {
    error = e;
  }
  expect(error).toBeDefined();
  expect(String(error && error.message)).toContain("has no export named");
};

describe("missing named imports reject identically in both engines", () => {
  test("rejects a missing named import from a JavaScript module", async () => {
    await expectMissingExportRejection(
      "../../../fixtures/modules/missing-named-import-js.js",
    );
  });

  test("rejects a missing named import from a JSON module", async () => {
    await expectMissingExportRejection(
      "../../../fixtures/modules/missing-named-import-json.js",
    );
  });

  test("rejects a missing named import from a text module", async () => {
    await expectMissingExportRejection(
      "../../../fixtures/modules/missing-named-import-text.js",
    );
  });
});
