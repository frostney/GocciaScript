/*---
description: Basic class declaration and instantiation works correctly
features: [class-declaration]
---*/

test("simple class creation", () => {
  class Person {
    constructor(name) {
      this.name = name;
    }
  }

  const person = new Person("Alice");
  expect(person.name).toBe("Alice");
  expect(person instanceof Person).toBeTruthy();
});
