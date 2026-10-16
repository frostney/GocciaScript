/*---
description: dynamic import() from an imported module function of a module whose top level throws rejects with the thrown value itself
features: [modules, compat-function]
---*/

// importThrowing is an imported module-level async function that evaluates
// import(). The imported module's top-level throw reaches the VM's dynamic
// import handling as the engine's bytecode-throw exception. The rejection must
// carry the thrown value's identity, not a fresh Error synthesized from
// "Name: message".
import { importThrowing } from "./helpers/importer.js";

describe("dynamic import() of a module with a throwing top level", () => {
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
    // Assert by reference: a fresh Boom with the same fields would still be a
    // boundary-contract violation, and only an identity check catches it.
    expect(caught).not.toBe(null);
    expect(caught).toBe(globalThis.__gocciaTopLevelThrowValue);
    expect(caught instanceof Error).toBe(true);
    expect(caught.name).toBe("Boom");
    expect(caught.message).toBe("boom");
  });
});
