/*---
description: >
  A vi.mock written inside a block or inside a test callback is still hoisted
  and still applies, matching Vitest. Each nested call also prints Vitest's
  "not at the top level" warning to stderr, which is why this file is expected
  to be noisy on a normal run.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi } from "vitest";

import { label as blockMockedLabel } from "./helpers/mock-nested-block-target.js";
import { label as callbackMockedLabel } from "./helpers/mock-nested-callback-target.js";

if (true) {
  vi.mock("./helpers/mock-nested-block-target.js", () => ({
    label: "MOCKED-FROM-A-BLOCK",
  }));
}

describe("nested vi.mock", () => {
  test("a vi.mock inside a block is hoisted and applies", () => {
    expect(blockMockedLabel).toBe("MOCKED-FROM-A-BLOCK");
  });

  test("a vi.mock inside a test callback is hoisted and applies", () => {
    // Hoisted out of this callback: by the time the callback runs, the import
    // at the top of the file has already been linked against the mock.
    vi.mock("./helpers/mock-nested-callback-target.js", () => ({
      label: "MOCKED-FROM-A-CALLBACK",
    }));

    expect(callbackMockedLabel).toBe("MOCKED-FROM-A-CALLBACK");
  });
});
