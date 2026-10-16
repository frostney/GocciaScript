// The entry imports a module with top-level await. That module is one of the
// entry's pending asynchronous dependencies (ES2026 §16.2.1.6.1.3.1
// InnerModuleEvaluation step 11), so the entry's body runs only once the
// module has finished evaluating (§16.2.1.6.1.3.4
// AsyncModuleExecutionFulfilled), not at its first await.
import { later } from "../../../fixtures/modules/entry-tla-dependency.js";

const laterWhenEntryRan = later;
globalThis.entryTlaOrder = [...(globalThis.entryTlaOrder ?? []), "entry"];

describe("an entry that imports a module with top-level await", () => {
  test("its body runs after the module finished evaluating", () => {
    expect(globalThis.entryTlaOrder).toEqual([
      "dependency start",
      "dependency end",
      "entry",
    ]);
  });

  test("its body reads the module's bindings as the module left them", () => {
    expect(laterWhenEntryRan).toBe("after await");
  });
});
