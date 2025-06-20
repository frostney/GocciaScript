/*---
description: Nested destructuring
features: [destructuring]
---*/

test("nested array destructuring", () => {
  let colors = ["red", ["green", "blue"]];
  let [primary, [secondary, tertiary]] = colors;

  expect(primary).toBe("red");
  expect(secondary).toBe("green");
  expect(tertiary).toBe("blue");
});

test("nested object destructuring", () => {
  let person = {
    name: "John",
    age: 30,
    address: { city: "New York", state: "NY" },
  };
  let {
    name,
    address: { city, state },
  } = person;

  expect(name).toBe("John");
  expect(city).toBe("New York");
  expect(state).toBe("NY");
});
