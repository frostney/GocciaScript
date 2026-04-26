/*---
description: >
  At the top level of a Module, `this` is undefined per §16.2.1.6.4
  InitializeEnvironment, which sets the Module Environment Record's
  [[ThisValue]] to undefined. ResolveThisBinding (§9.4.3) reads that
  value via GetThisBinding.
---*/

import {
  thisAtModuleTopLevel,
  typeofThisAtModuleTopLevel,
} from "./helpers/this-in-module.js";

test("module-level this is undefined", () => {
  expect(thisAtModuleTopLevel).toBeUndefined();
});

test("module-level typeof this is undefined", () => {
  expect(typeofThisAtModuleTopLevel).toBe("undefined");
});

test("module-level this is not globalThis", () => {
  expect(thisAtModuleTopLevel === globalThis).toBe(false);
});
