/*---
description: delete on primitive receivers coerces the computed property key before yielding true
features: [delete]
---*/

test("delete on a number receiver runs the key's toString coercion", () => {
  let calls = 0;
  const key = {
    toString() {
      calls += 1;
      return "x";
    },
  };
  const result = delete (5)[key];
  expect(result).toBe(true);
  expect(calls).toBe(1);
});

test("delete on a string receiver runs the key's toString coercion", () => {
  let calls = 0;
  const key = {
    toString() {
      calls += 1;
      return "y";
    },
  };
  const result = delete "abc"[key];
  expect(result).toBe(true);
  expect(calls).toBe(1);
});

test("delete on a boolean receiver with a plain string key returns true", () => {
  expect(delete true["whatever"]).toBe(true);
});
