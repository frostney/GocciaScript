/*---
description: Non-strict for-in heads may give their var binding an initializer (Annex B.3.5)
features: [compat-for-in-loop, compat-var, compat-non-strict-mode, compat-traditional-for-loop]
---*/

test("the initializer runs once, before the object expression", () => {
  let effects = 0;
  let stored;
  let iterations = 0;
  for (var a = (++effects, -1) in stored = a, { x: 0, y: 1 }) {
    iterations += 1;
  }
  expect(effects).toBe(1);
  expect(stored).toBe(-1);
  expect(iterations).toBe(2);
  expect(a).toBe("y");
});

test("an empty object leaves the initialized value", () => {
  for (var b = 7 in {});
  expect(b).toBe(7);
});

test("an anonymous arrow initializer takes the binding's name", () => {
  for (var f = () => {} in {});
  expect(f.name).toBe("f");
});

test("a traditional for loop with a var initializer still parses", () => {
  let total = 0;
  for (var i = 0; i < 3; i++) total += i;
  expect(total).toBe(3);
});
