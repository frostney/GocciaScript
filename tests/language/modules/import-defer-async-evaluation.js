import { done } from "../../../fixtures/modules/deferred-async-cycle-promises.js";
import "../../../fixtures/modules/deferred-async-cycle-target.js";

describe("import defer async evaluation", () => {
  test("deferred namespace readiness errors do not poison later access", async () => {
    await done;
    expect(globalThis.__gocciaImportDeferErrorWhileEvaluating instanceof TypeError).toBe(true);
    expect(globalThis.__gocciaImportDeferErrorWhileEvaluatingAsync instanceof TypeError).toBe(true);
    expect(globalThis.__gocciaImportDeferValueAfterEvaluated).toBe(1);
  });
});
