/*---
description: >
  In non-strict code, storing to a property named like the engine's private
  storage keys creates an ordinary property and leaves private fields alone (#1326)
features: [compat-non-strict-mode]
---*/

test("a non-strict store creates an ordinary property", () => {
  const obj = {};
  obj["#slot:0$x"] = 1;
  obj["#brand:y"] = 2;
  expect(Object.keys(obj)).toEqual(["#slot:0$x", "#brand:y"]);
});

test("a non-strict store to a class instance does not reach its private field", () => {
  class Box {
    #x = "private";
    read() {
      return this.#x;
    }
  }
  const box = new Box();
  box["#slot:0$x"] = "public";
  expect(box.read()).toBe("private");
  expect(Object.keys(box)).toEqual(["#slot:0$x"]);
});
