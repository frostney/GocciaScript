/*---
description: Shorthand properties
features: [shorthand-properties]
---*/

test("shorthand properties", () => {
  let name = "John";
  let age = 30;
  let person = { name, age };
  expect(person.name).toBe("John");
  expect(person.age).toBe(30);
});
