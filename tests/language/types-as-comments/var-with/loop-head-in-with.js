/*---
description: Under strict types, a for-of head inside with stores into the with object's property without checking it against a typed binding of the same name, and checks the value only when it falls back to that binding
features: [types-as-comments, strict-type-enforcement, compat-var, compat-non-strict-mode]
---*/

describe("a for-of head naming a typed parameter inside with", () => {

test("a var head stores into the object's property unchecked", () => {
  const box = { x: 0 };
  const m = (x: number) => { with (box) { for (var x of ["s"]) {} } return [x, box.x]; };
  expect(m(1)).toEqual([1, "s"]);
});

test("a var head without the property stores into the parameter and is checked", () => {
  const box = {};
  const m = (x: number) => { with (box) { for (var x of ["s"]) {} } return x; };
  expect(() => m(1)).toThrow(TypeError);
});

test("a var head without the property stores a matching value into the parameter", () => {
  const box = {};
  const m = (x: number) => { with (box) { for (var x of [2]) {} } return x; };
  expect(m(1)).toBe(2);
});

test("an assignment head stores into the object's property unchecked", () => {
  const box = { x: 0 };
  const m = (x: number) => { with (box) { for (x of ["s"]) {} } return [x, box.x]; };
  expect(m(1)).toEqual([1, "s"]);
});

test("an assignment head without the property is checked", () => {
  const box = {};
  const m = (x: number) => { with (box) { for (x of ["s"]) {} } return x; };
  expect(() => m(1)).toThrow(TypeError);
});

test("a pattern var head stores into the object's property unchecked", () => {
  const box = { x: 0 };
  const m = (x: number) => { with (box) { for (var [x] of [["s"]]) {} } return [x, box.x]; };
  expect(m(1)).toEqual([1, "s"]);
});

test("a pattern var head without the property is checked", () => {
  const box = {};
  const m = (x: number) => { with (box) { for (var [x] of [["s"]]) {} } return x; };
  expect(() => m(1)).toThrow(TypeError);
});

}); // describe
