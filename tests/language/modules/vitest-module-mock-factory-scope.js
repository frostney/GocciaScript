/*---
description: >
  A hoisted vi.mock factory is relocated into the generated module's own scope,
  so a reference to a binding of the test file is a real ReferenceError rather
  than an emulated rule — matching Vitest's "factory may not reference outer
  variables" restriction.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi } from "vitest";

const outerBinding = "VISIBLE-ONLY-IN-THE-TEST-FILE";

// The factory below reads `outerBinding`, which does not exist in the module
// the shim generates. Imported dynamically so the failure lands in the test
// rather than while the entry module is still linking.
vi.mock("./helpers/mock-error-automock.js", () => ({ label: outerBinding }));

describe("vi.mock factory scope", () => {
  test("the factory cannot reference a binding of the test file", async () => {
    let error;
    try {
      await import("./helpers/mock-error-automock.js");
    } catch (caught) {
      error = caught;
    }

    expect(error instanceof ReferenceError).toBe(true);
    expect(String(error)).toContain("outerBinding");
    // The binding really does exist here — it is the factory's new home that
    // cannot see it.
    expect(outerBinding).toBe("VISIBLE-ONLY-IN-THE-TEST-FILE");
  });
});
