/*---
description: >
  Default imports bind the module's "default" export. They can be combined with
  named imports or namespace imports.
features: [modules, default-imports]
---*/

import importedDefault, { if as importedIf } from "./helpers/identifier-name-export-source.js";
import importedDefaultAgain, * as keywordModule from "./helpers/identifier-name-export-source.js";
import scalarDefault from "./helpers/scalar.yaml";

describe("default import", () => {
  test("imports the default export binding", () => {
    expect(importedDefault).toBe("default export");
    expect(importedDefaultAgain).toBe("default export");
  });

  test("can be combined with named imports", () => {
    expect(importedIf).toBe("if export");
  });

  test("can be combined with namespace imports", () => {
    expect(keywordModule.default).toBe("default export");
    expect(keywordModule.if).toBe("if export");
  });

  test("imports a scalar structured-data module default export", () => {
    expect(scalarDefault).toBe(42);
  });
});
