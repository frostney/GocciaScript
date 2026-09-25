/*---
description: >
  One half of the symmetric vi.mock per-file isolation pair. This file mocks
  helpers/mock-isolation-target-a.js and must see the mocked value, while
  importing helpers/mock-isolation-target-b.js — which its partner
  vitest-module-mock-isolation-b.js mocks — and must still see the real value.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi } from "vitest";

import { label as ownLabel } from "./helpers/mock-isolation-target-a.js";
import { label as foreignLabel } from "./helpers/mock-isolation-target-b.js";

// DO NOT "simplify" either half of this pair away, and do not make one half
// stop mocking. The pair is deliberately SYMMETRIC, and the symmetry is what
// makes it order-independent.
//
// GocciaTestRunner executes the whole tests/ tree in one process with parallel
// worker threads. Mock isolation holds only because every test file gets its
// own engine, module loader and virtual-module registry. If that registry ever
// became shared or process-global, this pair is the only place in the suite
// that would notice.
//
// An asymmetric pair — one file mocking, one file only importing — would not
// be enough: under a shared registry the non-mocking file could still pass
// simply by having been loaded before the mocking file registered anything.
// Symmetry removes that escape. Each file registers its own mock BEFORE it
// imports the other file's target, so under a shared registry at least one
// half must fail for EVERY possible interleaving:
//
//   Suppose both halves passed. A passing means A imported target-B before B
//   registered mock-B; B passing means B imported target-A before A registered
//   mock-A. Each file also registers its own mock before it imports, so:
//     register(mock-A) < import(target-B) < register(mock-B)
//                      < import(target-A) < register(mock-A)
//   which is a cycle, and therefore impossible. A shared registry is caught
//   regardless of which file runs first, or whether they run concurrently.
vi.mock("./helpers/mock-isolation-target-a.js", () => ({
  label: "MOCKED-IN-FILE-A",
}));

describe("vi.mock isolation (file A)", () => {
  test("the mock applies in the file that declared it", () => {
    expect(ownLabel).toBe("MOCKED-IN-FILE-A");
  });

  test("a mock declared by the other test file does not leak into this one", () => {
    expect(foreignLabel).toBe("REAL-ISOLATION-TARGET-B");
  });
});
