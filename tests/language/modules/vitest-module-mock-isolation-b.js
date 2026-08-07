/*---
description: >
  The other half of the symmetric vi.mock per-file isolation pair. This file
  mocks helpers/mock-isolation-target-b.js and must see the mocked value, while
  importing helpers/mock-isolation-target-a.js — which its partner
  vitest-module-mock-isolation-a.js mocks — and must still see the real value.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi } from "vitest";

import { label as ownLabel } from "./helpers/mock-isolation-target-b.js";
import { label as foreignLabel } from "./helpers/mock-isolation-target-a.js";

// See vitest-module-mock-isolation-a.js for why this pair is symmetric: each
// file registers its own mock before importing the other file's target, which
// makes a shared or process-global virtual-module registry impossible to hide
// behind load order. Do not turn this file into a pure importer — that would
// reintroduce the ordering hole the symmetry exists to close.
vi.mock("./helpers/mock-isolation-target-b.js", () => ({
  label: "MOCKED-IN-FILE-B",
}));

describe("vi.mock isolation (file B)", () => {
  test("the mock applies in the file that declared it", () => {
    expect(ownLabel).toBe("MOCKED-IN-FILE-B");
  });

  test("a mock declared by the other test file does not leak into this one", () => {
    expect(foreignLabel).toBe("REAL-ISOLATION-TARGET-A");
  });
});
