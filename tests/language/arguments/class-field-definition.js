/*---
description: A public class field defined on a mapped arguments object updates the parameter its index aliases
features: [arguments-object, compat-non-strict-mode, function-declarations, class-fields]
---*/

describe("public class fields on a mapped arguments object", () => {
  test("a named numeric field updates the parameter and the index", () => {
    function make(value) {
      return [arguments, () => value];
    }
    class ReturnObject {
      constructor(target) { return target; }
    }
    class WithField extends ReturnObject {
      0 = "accepted";
    }

    const [alias, read] = make("original");
    new WithField(alias);
    expect([read(), alias[0]]).toEqual(["accepted", "accepted"]);
  });

  test("a computed field updates the parameter and the index", () => {
    function make(value) {
      return [arguments, () => value];
    }
    class ReturnObject {
      constructor(target) { return target; }
    }
    class WithComputed extends ReturnObject {
      ["0"] = 5;
    }

    const [alias, read] = make("original");
    new WithComputed(alias);
    expect([read(), alias[0]]).toEqual([5, 5]);
  });
});
