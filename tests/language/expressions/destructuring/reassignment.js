/*---
description: Reassignment of destructured variables
features: [destructuring]
---*/

test("swap values in array destructuring", () => {
  let a = 1;
  let b = 2;

  expect(a).toBe(1);
  expect(b).toBe(2);

  [a, b] = [b, a];

  expect(a).toBe(2);
  expect(b).toBe(1);
});

test("reassignment in object destructuring", () => {
  let person = { name: "John", age: 30, city: "New York" };
  const differentPerson = { name: "Jane", age: 25, city: "Los Angeles" };

  let { name: firstName, age: firstAge, city: firstCity } = person;
  const {
    name: secondName,
    age: secondAge,
    city: secondCity,
  } = differentPerson;

  expect(firstName).toBe("John");
  expect(firstAge).toBe(30);
  expect(firstCity).toBe("New York");

  expect(secondName).toBe("Jane");
  expect(secondAge).toBe(25);
  expect(secondCity).toBe("Los Angeles");
});

test("const object destructuring throws on reassignment", () => {
  const { x, y } = { x: 1, y: 2 };
  expect(x).toBe(1);
  expect(y).toBe(2);
  expect(() => { x = 10; }).toThrow(TypeError);
  expect(() => { y = 20; }).toThrow(TypeError);
});

test("const array destructuring throws on reassignment", () => {
  const [a, b] = [3, 4];
  expect(a).toBe(3);
  expect(b).toBe(4);
  expect(() => { a = 30; }).toThrow(TypeError);
  expect(() => { b = 40; }).toThrow(TypeError);
});

test("const nested destructuring throws on reassignment", () => {
  const { nested: { deep } } = { nested: { deep: 42 } };
  expect(deep).toBe(42);
  expect(() => { deep = 0; }).toThrow(TypeError);
});
