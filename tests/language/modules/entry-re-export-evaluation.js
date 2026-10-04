/*---
description: >
  A module the entry file only re-exports from is still a requested module, so
  it evaluates before the entry body, once, in source order with the entry's
  imports.
features: [modules]
---*/

export { namedValue } from "./helpers/entry-re-export-named.js";
import { importedValue } from "./helpers/entry-re-export-import.js";
export * from "./helpers/entry-re-export-star.js";
export * as namespaceExport from "./helpers/entry-re-export-namespace.js";

const orderAtEntryStart = [...globalThis.entryReExportOrder];

describe("entry re-export evaluation", () => {
  test("every re-exported module evaluates before the entry body", () => {
    expect(orderAtEntryStart).toEqual(["named", "import", "star", "namespace"]);
  });

  test("each re-exported module evaluates once", () => {
    expect(globalThis.entryReExportOrder).toEqual([
      "named",
      "import",
      "star",
      "namespace",
    ]);
  });

  test("an import between re-exports still binds", () => {
    expect(importedValue).toBe("import");
  });
});
