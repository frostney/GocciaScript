/*---
description: A deferred module's source phase import neither evaluates its module nor makes it an asynchronous dependency
features: [import-defer, source-phase-imports]
---*/

import defer * as library from "./helpers/deferred-source-library.js";

describe("import defer with a source phase dependency", () => {
  test("the module a source phase import names is not evaluated before the deferred module is used", () => {
    expect(globalThis.__gocciaDeferredSourceTargetRan).toBeUndefined();
  });

  test("using the deferred module evaluates it synchronously, still without the source phase module", () => {
    expect(library.kind).toBe("[object ModuleSource]");
    expect(globalThis.__gocciaDeferredSourceTargetRan).toBeUndefined();
  });
});
