/*---
description: A for-in head inside with stores each key into the with object's property of that name, and into the binding only when no object has it
features: [compat-var, compat-non-strict-mode, compat-for-in-loop]
---*/

describe("for-in head inside with", () => {

test("a var head stores into the object's property", () => {
  const box = { x: 0 };
  const f = () => { var x; with (box) { for (var x in { k: 1 }) {} } return [x, box.x]; };
  expect(f()).toEqual([undefined, "k"]);
});

test("a var head whose name no object has stores into the var", () => {
  const box = {};
  const f = () => { var x; with (box) { for (var x in { k: 1 }) {} } return [x, "x" in box]; };
  expect(f()).toEqual(["k", false]);
});

test("an array pattern var head stores into the object's property", () => {
  const box = { x: 0 };
  const f = () => { var x; with (box) { for (var [x] in { k: 1 }) {} } return [x, box.x]; };
  expect(f()).toEqual([undefined, "k"]);
});

test("an assignment head stores into the object's property", () => {
  const box = { x: 0 };
  const f = () => { var x; with (box) { for (x in { k: 1 }) {} } return [x, box.x]; };
  expect(f()).toEqual([undefined, "k"]);
});

test("a let head declares its own binding", () => {
  const box = { x: 0 };
  const f = () => { var x = "outer"; with (box) { for (let x in { k: 1 }) {} } return [x, box.x]; };
  expect(f()).toEqual(["outer", 0]);
});

}); // describe
