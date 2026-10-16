// The entry and a helper with top-level await import each other. The helper
// evaluates first and is asynchronous, so the entry waits for it (ES2026
// §16.2.1.6.1.3.1 InnerModuleEvaluation step 11): when the helper resumes
// after its await, the entry's body has still not run, and the entry's
// exports are still in their temporal dead zone.
import { afterAwait, beforeAwait } from "../../../fixtures/modules/entry-tla-cycle-peer.js";

export const entryValue = "entry value";

const helperResumedBeforeEntry = afterAwait;

describe("an entry in an import cycle with a module that has top-level await", () => {
  test("the helper resumed before the entry's body ran", () => {
    expect(beforeAwait).toBe("ReferenceError");
    expect(helperResumedBeforeEntry).toBe("ReferenceError");
  });

  test("the entry's binding is initialized once its body has run", () => {
    expect(entryValue).toBe("entry value");
  });
});
