describe("experimental dynamic source-phase imports", () => {
  test("returns cached ModuleSource objects without evaluating modules", async () => {
    globalThis.__gocciaSourceDynamicImportEvaluated = false;
    const first = await import.source("../helpers/source-dynamic-import-side-effect.js");
    const second = await import.source("../helpers/source-dynamic-import-side-effect.js");
    expect(Object.prototype.toString.call(first)).toBe("[object ModuleSource]");
    expect(second).toBe(first);
    expect(globalThis.__gocciaSourceDynamicImportEvaluated).toBe(false);
  });
});
