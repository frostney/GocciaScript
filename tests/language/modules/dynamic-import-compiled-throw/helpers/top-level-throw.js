// Top-level code that throws during module evaluation, but only when the
// importing test has armed the flag. The test runner also evaluates this file
// standalone (it discovers every .js); leaving the flag unset there lets that
// run complete cleanly instead of reporting a spurious failure. Under the
// bytecode executor the armed throw is VM-compiled, so it crosses the
// dynamic-import boundary as the engine's bytecode-throw exception.
class Boom extends Error {
  constructor(message) {
    super(message);
    this.name = "Boom";
  }
}

if (globalThis.__gocciaArmTopLevelThrow) {
  // Expose the exact instance so the importing test can assert the rejection
  // preserves its identity by reference, not merely its name and message.
  const boom = new Boom("boom");
  globalThis.__gocciaTopLevelThrowValue = boom;
  throw boom;
}

export const unreached = true;
