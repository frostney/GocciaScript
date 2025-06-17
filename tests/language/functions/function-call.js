/*---
description: Call function programatically
features: [function-call]
---*/

test("call function programatically", () => {
  const add = (a, b) => a + b;
  const result = add.call(null, 1, 2);
  expect(result).toBe(3);
});

test("call function on object", () => {
  const obj = {
    value: 1,
    add: (a, b) => a + b,
  };
  const result = obj.add.call(obj, 2, 3);
  expect(result).toBe(5);
});
