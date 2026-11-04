/*---
description: An export list before a module's top-level using declarations reads no binding, so the module evaluates (ES2027 §16.2.3.7)
features: [explicit-resource-management, modules, top-level-await]
---*/

import { asyncResource, constant, resource } from "../../../fixtures/modules/top-level-using-export-list.js";

describe("an export list before the declarations it exports", () => {
  test("the imported module evaluates", () => {
    expect(globalThis.topLevelUsingExportListEvaluated).toBe(true);
  });

  test("its exports read the initialized bindings", () => {
    expect(resource).toBe(null);
    expect(asyncResource).toBe(null);
    expect(constant).toBe("constant");
  });
});
