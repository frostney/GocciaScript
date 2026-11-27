/*---
description: Annex B block function copies are checked when the outer var binding has an enforced type
features: [compat-function, compat-non-strict-mode, compat-var, strict-type-enforcement]
---*/

describe("Annex B stores into typed var bindings", () => {
  test("an incompatible function copy leaves the outer var unchanged", () => {
    function run() {
      var value: string = "original";
      try {
        {
          function value() {}
        }
      } catch (error) {
        return [error.name, value];
      }
      return [typeof value, value];
    }

    expect(run()).toEqual(["TypeError", "original"]);
  });
});
