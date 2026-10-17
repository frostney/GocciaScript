// Throws during module evaluation, but only when the importing test has armed
// the flag. The test runner also evaluates this file standalone (it discovers
// every .js); leaving the flag unset there lets that run complete cleanly.
if (globalThis.__gocciaArmDeferredThrow) {
  throw new Error("deferred module failed");
}

export const unreached = true;
