/*---
description: Basic destructuring
features: [destructuring]
---*/

test("single item in array destructuring", () => {
  let colors = ["red", "green", "blue"];
  let [primary] = colors;

  let secondary;

  [secondary] = colors;

  const differentColors = ["white", "black", "gray"];
  const [primary2] = differentColors;

  expect(primary).toBe("red");
  expect(primary2).toBe("white");
  expect(secondary).toBe("red");
});

test("multiple items in array destructuring", () => {
  let colors = ["red", "green", "blue"];
  const differentColors = ["white", "black", "gray"];

  let [primary, secondary, tertiary] = colors;
  const [primary2, secondary2, tertiary2] = differentColors;

  expect(primary).toBe("red");
  expect(secondary).toBe("green");
  expect(tertiary).toBe("blue");

  expect(primary2).toBe("white");
  expect(secondary2).toBe("black");
  expect(tertiary2).toBe("gray");
});

test("multiple items in array destructuring with pre-declared variables", () => {
  let colors = ["red", "green", "blue"];
  let primary, secondary, tertiary;
  [primary, secondary, tertiary] = colors;
  expect(primary).toBe("red");
  expect(secondary).toBe("green");
  expect(tertiary).toBe("blue");
});

test("multiple items in array on empty array", () => {
  let colors = [];
  let [primary, secondary, tertiary] = colors;
  expect(primary).toBeUndefined();
  expect(secondary).toBeUndefined();
  expect(tertiary).toBeUndefined();
});

test("single item in object destructuring", () => {
  let person = { name: "John", age: 30, city: "New York" };
  const differentPerson = { name: "Jane", age: 25, city: "Los Angeles" };

  let city;
  let { name } = person;
  const { age } = differentPerson;

  ({ city } = person);

  expect(name).toBe("John");
  expect(age).toBe(25);
  expect(city).toBe("New York");
});

test("multiple items in object destructuring", () => {
  let person = { name: "John", age: 30, city: "New York" };
  const differentPerson = { name: "Jane", age: 25, city: "Los Angeles" };

  let { name, age, city } = person;
  const { name: name2, age: age2, city: city2 } = differentPerson;

  expect(name).toBe("John");
  expect(age).toBe(30);
  expect(city).toBe("New York");

  expect(name2).toBe("Jane");
  expect(age2).toBe(25);
  expect(city2).toBe("Los Angeles");
});

test("multiple items in object destructuring with pre-declared variables", () => {
  let person = { name: "John", age: 30, city: "New York" };
  let name, age, city;
  ({ name, age, city } = person);
  expect(name).toBe("John");
  expect(age).toBe(30);
  expect(city).toBe("New York");
});

test("multiple items in object on empty object", () => {
  let person = {};
  let { name, age, city } = person;
  expect(name).toBeUndefined();
  expect(age).toBeUndefined();
  expect(city).toBeUndefined();
});
