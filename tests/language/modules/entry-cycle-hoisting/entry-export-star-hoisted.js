// A helper in an import cycle with the entry re-exports it with
// `export * from`. A `var` export and a function declaration export are
// bound when the entry's environment is initialized (ES2026 §16.2.1.7.3.1
// InitializeEnvironment), before the helper links, so the helper forwards
// them (§16.2.1.7.2.1 GetExportedNames).
import * as helper from "../../../../fixtures/modules/entry-export-star-hoisting-peer.js";

export var entryVar = "entry var";
export function entryFunction() {
  return "entry function";
}

describe("export * from an entry with hoisted exports in an import cycle", () => {
  test("the helper's namespace lists the var and function exports", () => {
    expect(Object.keys(helper)).toEqual([
      "entryFunction",
      "entryVar",
      "helperOwn",
    ]);
  });

  test("the forwarded bindings read the entry's values", () => {
    expect(helper.entryVar).toBe("entry var");
    expect(helper.entryFunction).toBe(entryFunction);
    expect(helper.entryFunction()).toBe("entry function");
  });
});
