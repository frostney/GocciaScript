// The entry and a helper import each other, and the helper re-exports the
// entry with `export * from`. The entry's local exports exist once its
// environment is initialized (ES2026 §16.2.1.7.3.1 InitializeEnvironment),
// before the helper links, so the helper forwards every one of them but
// `default` (§16.2.1.7.2.1 GetExportedNames), and reads them live.
import * as helper from "../../../fixtures/modules/entry-export-star-peer.js";

export const entryConst = "entry const";
export let entryCounter = 1;
export class EntryClass {}
export default "entry default";

entryCounter = 2;

describe("export * from the entry in an import cycle", () => {
  test("the helper's namespace lists the entry's local exports", () => {
    expect(Object.keys(helper)).toEqual([
      "EntryClass",
      "entryConst",
      "entryCounter",
      "helperOwn",
    ]);
  });

  test("the forwarded bindings read the entry's values live", () => {
    expect(helper.entryConst).toBe("entry const");
    expect(helper.entryCounter).toBe(2);
    expect(helper.EntryClass).toBe(EntryClass);
    expect(helper.helperOwn).toBe("helper own");
  });

  test("the entry's default export is not forwarded", () => {
    expect("default" in helper).toBe(false);
  });
});
