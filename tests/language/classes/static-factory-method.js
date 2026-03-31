/*---
description: Static factory methods can construct class instances
features: [static-methods]
---*/

test("static factory methods create class instances", () => {
  class Person {
    constructor(name, age) {
      this.name = name;
      this.age = age;
    }

    static fromString(str) {
      const [name, age] = str.split(",");
      return new Person(name, Number.parseInt(age));
    }

    static adult(name) {
      return new Person(name, 18);
    }
  }

  const person1 = Person.fromString("Alice,25");
  const person2 = Person.adult("Bob");

  expect(person1.name).toBe("Alice");
  expect(person1.age).toBe(25);
  expect(person2.name).toBe("Bob");
  expect(person2.age).toBe(18);
});
