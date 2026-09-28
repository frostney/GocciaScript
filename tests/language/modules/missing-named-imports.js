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

  test("rejects a missing named import from a bytes module", async () => {
    await expectMissingExportRejection(
      "../../../fixtures/modules/missing-named-import-bytes.js",
    );
  });

  test("a missing import from a module still evaluating names the specifier, not the host path", async () => {
    let message;
    try {
      await import("../../../fixtures/modules/cyclic-missing-export-a.js");
    } catch (e) {
      message = e.message;
    }
    // The importing module wrote "./cyclic-missing-export-a.js"; its expanded
    // host path stays host-side (ADR 0108).
    expect(message).toBe(
      'Module "./cyclic-missing-export-a.js" has no export named "thisExportDoesNotExist"',
    );
  });
});
