/*---
description: Under strict types, a var initializer that updates a binding typed other than number throws before either binding changes
features: [types-as-comments, strict-type-enforcement, compat-var]
---*/

describe("an update of a typed binding in a var initializer", () => {

test("the var keeps its value when the update throws", () => {
  const f = (s: string) => {
    var y = "keep";
    try { var y = s++; } catch (e) {}
    return [y, s];
  };
  expect(f("1")).toEqual(["keep", "1"]);
});

test("a typed binding from an enclosing function is checked the same way", () => {
  const outer = (s: string) => {
    const inner = () => {
      var y = "keep";
      try { var y = s--; } catch (e) {}
      return [y, s];
    };
    return inner();
  };
  expect(outer("1")).toEqual(["keep", "1"]);
});

test("a var that updates its own typed binding keeps the binding's value when the update throws", () => {
  const f = (x: string) => {
    try { var x = x++; } catch (e) {}
    return [x, typeof x];
  };
  expect(f("1")).toEqual(["1", "string"]);
});

test("a number binding stores the old value", () => {
  const f = (n: number) => { var y = n++; return [y, n]; };
  expect(f(1)).toEqual([1, 2]);
});

}); // describe
