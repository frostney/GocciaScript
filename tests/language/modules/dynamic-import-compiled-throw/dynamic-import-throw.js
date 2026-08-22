/*---
description: dynamic import() evaluated by the interpreter of a module whose compiled top-level throws rejects with the thrown value itself
features: [modules, compat-function]
---*/

// importThrowing is an imported module-level async function, so under the
// bytecode executor it runs on the interpreter and evaluates import() through
// the interpreter's TGocciaImportExpression ladder. The imported module's
// top-level code is VM-compiled, so its throw crosses that boundary as the
// engine's bytecode-throw exception. The rejection must carry the thrown
// value's identity, not a fresh Error synthesized from "Name: message".
import { importThrowing } from "./helpers/importer.js";

describe("interpreter dynamic import() of a module with a compiled top-level throw", () => {
  test("rejects with the thrown value, identity preserved", async () => {
    // Arm the imported module's top-level throw for this evaluation only.
    globalThis.__gocciaArmTopLevelThrow = true;

    let caught = null;
    try {
      await importThrowing();
    } catch (error) {
      caught = error;
    } finally {
      globalThis.__gocciaArmTopLevelThrow = false;
    }

    // A mangled rejection would be a plain Error named "Error" with message
    // "Boom: boom"; the identity-preserving path keeps the module's own Boom.
    expect(caught).not.toBe(null);
    expect(caught instanceof Error).toBe(true);
    expect(caught.name).toBe("Boom");
    expect(caught.message).toBe("boom");
  });
});
