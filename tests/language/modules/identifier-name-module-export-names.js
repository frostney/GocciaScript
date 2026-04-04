import { if as importedIf, class as importedClass, default as importedDefault } from "./helpers/identifier-name-export-source.js";
import { while as reExportedIf, switch as reExportedClass, break as reExportedDefault } from "./helpers/identifier-name-re-exporter.js";

describe("IdentifierName module export names", () => {
  test("imports keyword export names from JavaScript modules", () => {
    expect(importedIf).toBe("if export");
    expect(importedClass).toBe("class export");
    expect(importedDefault).toBe("default export");
  });

  test("re-exports keyword export names through JavaScript modules", () => {
    expect(reExportedIf).toBe("if export");
    expect(reExportedClass).toBe("class export");
    expect(reExportedDefault).toBe("default export");
  });
});
