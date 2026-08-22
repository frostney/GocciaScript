// Differential suite N — the node_modules behaviours that are goccia's own.
//
// Both are deliberate deviations recorded in docs/module-resolution.md, so
// neither bun nor vitest can act as an oracle for them and this file is
// classified `skip` for both. Mode parity is still checked.
import {
  importCommonJSOnlyPackage,
  importEsbuildBundledPackage,
  modfieldLabel,
} from "./mods/nodemods/goccia-entry.js";

describe("node_modules resolution: goccia-specific behaviour", () => {
  test('the "module" field is preferred over "main" without an exports map', () => {
    // Node ignores "module" entirely and bun resolves this package to its
    // CommonJS "main". GocciaScript loads only ES modules, so the ES module
    // build behind "module" is the only usable entry.
    expect(modfieldLabel).toBe("modfield-esm");
  });

  test("a CommonJS-only package fails with a named error, not a SyntaxError", async () => {
    let message = "";
    try {
      await importCommonJSOnlyPackage();
    } catch (error) {
      message = error.message;
    }

    expect(message).toBe(
      'Package "cjsonly" resolved to a CommonJS file (index.js); GocciaScript loads only ES modules',
    );
  });

  test("an esbuild CommonJS bundle is refused by name despite its banner", async () => {
    // esbuild ends every CommonJS output with "// Annotate the CommonJS export
    // names for ESM import in node:". The source scan strips comments before
    // looking for module markers, so those two words no longer make the bundle
    // look like an ES module — without the strip this failed at `require`
    // instead, with a ReferenceError naming an undefined variable.
    let message = "";
    try {
      await importEsbuildBundledPackage();
    } catch (error) {
      message = error.message;
    }

    expect(message).toBe(
      'Package "esbuildcjs" resolved to a CommonJS file (index.js); GocciaScript loads only ES modules',
    );
  });
});
