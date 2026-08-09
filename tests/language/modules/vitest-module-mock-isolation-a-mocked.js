/*---
description: >
  First half of the vi.mock per-file isolation pair. This file mocks
  helpers/mock-isolation-target.js and must see the mocked value; its partner
  vitest-module-mock-isolation-b-real.js imports the same module without
  mocking it and must still see the real value.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi } from "vitest";

import { label } from "./helpers/mock-isolation-target.js";

vi.mock("./helpers/mock-isolation-target.js", () => ({
  label: "MOCKED-IN-FILE-A",
}));

describe("vi.mock isolation (mocking half)", () => {
  test("the mock applies in the file that declared it", () => {
    expect(label).toBe("MOCKED-IN-FILE-A");
  });
});
