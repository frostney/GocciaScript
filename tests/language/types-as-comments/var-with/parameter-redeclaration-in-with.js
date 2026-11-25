/*---
description: Under strict types, a var in a with block whose object has the name stores into the object property, so a typed parameter of the same name is neither checked nor changed
features: [types-as-comments, strict-type-enforcement, compat-var, compat-non-strict-mode]
---*/

describe("a var redeclaring a typed parameter inside with", () => {

test("the store goes to the object's property", () => {
  const box = { x: 0 };
  const m = (x: number) => { with (box) { var x = "s"; } return [x, box.x].join(","); };
  expect(m(1)).toBe("1,s");
});

test("without the property, the store reaches the parameter and is checked", () => {
  const box = {};
  const m = (x: number) => { with (box) { var x = "s"; } return x; };
  expect(() => m(1)).toThrow(TypeError);
});

}); // describe
