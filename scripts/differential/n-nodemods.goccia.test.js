// Differential suite M — the node_modules behaviours that are goccia's own.
//
// Both are deliberate deviations recorded in docs/module-resolution.md, so
// neither bun nor vitest can act as an oracle for them and this file is
// classified `skip` for both. Mode parity is still checked.
import {
  importCommonJSOnlyPackage,
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
});
