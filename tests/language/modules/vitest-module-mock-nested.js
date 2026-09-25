/*---
description: >
  A vi.mock written anywhere but the top level — inside a block, a test
  callback, a call callee, or a class static block — is still hoisted and still
  applies, matching Vitest, whose hoisting transform walks the whole AST. Each
  nested call also prints Vitest's "not at the top level" warning to stderr,
  which is why this file is expected to be noisy on a normal run.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi } from "vitest";

import { label as blockMockedLabel } from "./helpers/mock-nested-block-target.js";
import { label as callbackMockedLabel } from "./helpers/mock-nested-callback-target.js";
import { label as calleeMockedLabel } from "./helpers/mock-nested-callee-target.js";
import { label as classMockedLabel } from "./helpers/mock-nested-class-target.js";
import { label as computedKeyMockedLabel } from "./helpers/mock-nested-computed-key-target.js";
import { label as computedBodyMockedLabel } from "./helpers/mock-nested-computed-body-target.js";

if (true) {
  vi.mock("./helpers/mock-nested-block-target.js", () => ({
    label: "MOCKED-FROM-A-BLOCK",
  }));
}

// The two cases below sit in positions the hoister's walk only reaches because
// it descends through arbitrary expressions, not just statements and call
// arguments. Vitest walks the whole AST, so a directive it would have hoisted
// must be hoisted here too — otherwise the mock silently never applies.
const wrap = (fn) => fn;

// Inside a call CALLEE: the arrow holding the directive is an argument of the
// *callee* sub-call `wrap(...)`, not of the outer call.
wrap(() => {
  vi.mock("./helpers/mock-nested-callee-target.js", () => ({
    label: "MOCKED-FROM-A-CALLEE",
  }));
})();

// Inside a class static block.
class MockDeclarations {
  static {
    vi.mock("./helpers/mock-nested-class-target.js", () => ({
      label: "MOCKED-FROM-A-CLASS-STATIC-BLOCK",
    }));
  }
}

// Inside a COMPUTED class key, and inside the body of a method whose key is
// computed. Both positions are unreachable through a class's name-keyed
// member maps — a computed element has no static name to be keyed by — so the
// hoister has to read them off the class element list instead. Both were
// silently dropped once: the mock never registered and nothing reported it.
const computedMethodName = "run";

class ComputedMockDeclarations {
  [vi.mock("./helpers/mock-nested-computed-key-target.js", () => ({
    label: "MOCKED-FROM-A-COMPUTED-KEY",
  })) ?? "declaredByComputedKey"]() {
    return "computed-key-method";
  }

  [computedMethodName]() {
    vi.mock("./helpers/mock-nested-computed-body-target.js", () => ({
      label: "MOCKED-FROM-A-COMPUTED-METHOD-BODY",
    }));
  }
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

  test("a vi.mock inside a call callee is hoisted and applies", () => {
    expect(calleeMockedLabel).toBe("MOCKED-FROM-A-CALLEE");
  });

  test("a vi.mock inside a class static block is hoisted and applies", () => {
    expect(classMockedLabel).toBe("MOCKED-FROM-A-CLASS-STATIC-BLOCK");
    // The class is only referenced so it is not dead code.
    expect(typeof MockDeclarations).toBe("function");
  });

  test("a vi.mock inside a computed class key is hoisted and applies", () => {
    expect(computedKeyMockedLabel).toBe("MOCKED-FROM-A-COMPUTED-KEY");
    expect(typeof ComputedMockDeclarations).toBe("function");
  });

  test("a vi.mock inside a computed-key method body is hoisted and applies", () => {
    expect(computedBodyMockedLabel).toBe("MOCKED-FROM-A-COMPUTED-METHOD-BODY");
  });
});
