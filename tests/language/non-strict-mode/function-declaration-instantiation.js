/*---
description: Non-strict function declarations replace same-named parameter bindings
features: [compat-function, compat-non-strict-mode]
---*/

test("function declarations replace same-named parameters during instantiation", () => {
  function f(x) {
    return x;

    function x() {
      return 7;
    }
  }

  expect(f()).toBeInstanceOf(Function);
  expect(f()()).toBe(7);
});

test("default parameter self-reference observes parameter TDZ", () => {
  let bodyRan = false;

  function f(x = x) {
    bodyRan = true;
  }

  expect(() => f()).toThrow(ReferenceError);
  expect(bodyRan).toBe(false);
});

test("parameter default closures do not see body function declarations", () => {
  let x = "outside";
  let probeParams;
  let probeBody;

  function f(_ = probeParams = () => x) {
    probeBody = () => x();

    function x() {
      return "inside";
    }
  }

  f();
  expect(probeParams()).toBe("outside");
  expect(probeBody()).toBe("inside");
});
