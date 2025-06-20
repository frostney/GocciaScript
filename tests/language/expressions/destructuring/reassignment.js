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
