/*---
description: for-in assignment targets disable counted-loop fast paths
features: [compat-for-in-loop, compat-traditional-for-loop]
---*/

test("for-in assignment target mutates the surrounding counter", () => {
  const values = [];
  const obj = { first: 1, second: 2 };

  for (let i = 0; i < 3; i++) {
    for (i in obj) {
      break;
    }
    values.push(i);
  }

  expect(values).toEqual(["first"]);
});
