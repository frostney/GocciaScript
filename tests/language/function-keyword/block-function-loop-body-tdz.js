/*---
description: A function declared in a loop body reads the body's let and const bindings in their TDZ when called before them
features: [compat-function, let, const]
---*/

test("a hoisted function called from a leading initializer throws on every iteration", () => {
  const seen = [];
  for (const step of [1, 2]) {
    const first = (() => {
      try {
        return readSecond();
      } catch (error) {
        return error.constructor.name;
      }
    })();
    const second = step;
    seen.push(first, second);
    function readSecond() {
      return second;
    }
  }
  expect(seen).toEqual(["ReferenceError", 1, "ReferenceError", 2]);
});
