/*---
description: Non-strict compatibility flags do not change module strict-this semantics
features: [compat-non-strict-mode]
---*/

function standaloneThis() {
  return this;
}

function argumentsType() {
  return typeof arguments;
}

describe("module non-strict compatibility", () => {
  test("ordinary function calls keep strict module this", () => {
    expect(standaloneThis()).toBeUndefined();
  });

  test("nullish call receivers are not coerced to globalThis", () => {
    expect(standaloneThis.call(null)).toBeNull();
    expect(standaloneThis.call(undefined)).toBeUndefined();
  });

  test("module functions do not receive compatibility arguments objects", () => {
    expect(argumentsType(1, 2)).toBe("undefined");
  });
});
