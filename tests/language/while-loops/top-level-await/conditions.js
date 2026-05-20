/*---
description: Top-level await operands in while-loop conditions
features: [top-level-await, compat-while-loops]
---*/

let objectConditionIterations = 0;
while (await { function() {} }) {
  objectConditionIterations++;
  break;
}

let regexpConditionIterations = 0;
while (await /1/) {
  regexpConditionIterations++;
  break;
}

test("while condition accepts top-level await of object literal with function property name", () => {
  expect(objectConditionIterations).toBe(1);
});

test("while condition accepts top-level await of regular expression literal", () => {
  expect(regexpConditionIterations).toBe(1);
});
