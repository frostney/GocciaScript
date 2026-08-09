/*---
description: >
  Second half of the vi.mock per-file isolation pair. This file mocks nothing
  and must see the real helpers/mock-isolation-target.js, even though
  vitest-module-mock-isolation-a-mocked.js mocked that exact module in the same
  test-runner process.
features: [modules, runtime-modules]
---*/

import { describe, expect, test } from "vitest";

import { label } from "./helpers/mock-isolation-target.js";

// DO NOT "simplify" this file away as a test that asserts nothing interesting.
// It is the negative half of an isolation pair, and its assertion is
// load-bearing: GocciaTestRunner executes the whole tests/ tree in one process
// (with parallel worker threads), and mock isolation holds only because every
// test file gets its own engine, module loader and virtual-module registry. If
// that registry ever became shared or process-global, the mock declared by
// vitest-module-mock-isolation-a-mocked.js would leak into this file and this
// assertion is the only place in the suite that would notice. A single-file
// test cannot catch it, and neither can the differential harness, which runs
// one process per differential suite.
describe("vi.mock isolation (unmocked half)", () => {
  test("a mock declared by another test file does not leak into this one", () => {
    expect(label).toBe("REAL-ISOLATION-TARGET");
  });
});
