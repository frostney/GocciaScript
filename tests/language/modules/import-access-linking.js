/*---
description: Import binding fast paths preserve linking and namespace semantics
features: [modules]
---*/

describe("import binding linking semantics", () => {
  test("ambiguous named imports reject before module evaluation", async () => {
    globalThis.__importAccessEvaluationCount = 0;
    let error;

    try {
      await import(
        "../../../fixtures/modules/import-access-ambiguous-importer.js"
      );
    } catch (caught) {
      error = caught;
    }

    expect(error instanceof SyntaxError).toBe(true);
    expect(globalThis.__importAccessEvaluationCount).toBe(0);
  });

  test("module namespaces omit ambiguous star exports", async () => {
    globalThis.__importAccessEvaluationCount = 0;
    const namespace = await import(
      "../../../fixtures/modules/import-access-ambiguous-barrel.js"
    );

    expect("duplicate" in namespace).toBe(false);
    expect(namespace.onlyA).toBe("only-a");
    expect(namespace.onlyB).toBe("only-b");
    expect(globalThis.__importAccessEvaluationCount).toBe(2);
  });

  test("missing named imports reject before module evaluation", async () => {
    globalThis.__importAccessMissingEvaluationCount = 0;
    let error;

    try {
      await import(
        "../../../fixtures/modules/import-access-missing-importer.js"
      );
    } catch (caught) {
      error = caught;
    }

    expect(error instanceof SyntaxError).toBe(true);
    expect(globalThis.__importAccessMissingEvaluationCount).toBe(0);
  });
});
