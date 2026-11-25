/*---
description: A destructuring var declaration inside with stores each name into the with object's property of that name, and into the var only when no with object exposes the name as a binding
features: [compat-var, compat-non-strict-mode]
---*/

describe("destructuring var declaration inside with", () => {

test("an object pattern stores into the object's property", () => {
  const box = { x: 0 };
  const f = () => { var x; with (box) { var { x } = { x: "d" }; } return [x, box.x]; };
  expect(f()).toEqual([undefined, "d"]);
});

test("an array pattern stores into the object's property", () => {
  const box = { x: 0 };
  const f = () => { var x; with (box) { var [x] = ["e"]; } return [x, box.x]; };
  expect(f()).toEqual([undefined, "e"]);
});

test("a name no with object exposes is stored into the var", () => {
  const box = { x: 0 };
  const f = () => { with (box) { var { x, y } = { x: "a", y: "b" }; } return [x, y, box.x, "y" in box]; };
  expect(f()).toEqual([undefined, "b", "a", false]);
});

test("a default applies before the store into the property", () => {
  const box = { x: 0 };
  const f = () => { var x; with (box) { var { x = "dflt" } = {}; } return [x, box.x]; };
  expect(f()).toEqual([undefined, "dflt"]);
});

test("the name is resolved before its value is read", () => {
  const box = {};
  const source = { get x() { box.x = "added"; return "v"; } };
  const f = () => { var x; with (box) { var { x } = source; } return [x, box.x]; };
  expect(f()).toEqual(["v", "added"]);
});

test("a rest element stores into the object's property", () => {
  const box = { rest: 0 };
  const f = () => { var rest; with (box) { var { ...rest } = { a: 1 }; } return [rest, box.rest]; };
  expect(f()).toEqual([undefined, { a: 1 }]);
});

test("a rest element's name is resolved before the rest object is built", () => {
  const box = {};
  const source = { get a() { box.rest = "added"; return 1; } };
  const f = () => { var rest; with (box) { var { ...rest } = source; } return [rest, box.rest]; };
  expect(f()).toEqual([{ a: 1 }, "added"]);
});

}); // describe
