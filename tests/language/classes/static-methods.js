/*---
description: Static methods and properties work correctly
features: [static-methods, static-properties]
---*/

test("static methods", () => {
  class MathUtils {
    static add(a, b) {
      return a + b;
    }

    static multiply(a, b) {
      return a * b;
    }

    static PI = 3.14159;
  }

  expect(MathUtils.add(2, 3)).toBe(5);
  expect(MathUtils.multiply(4, 5)).toBe(20);
  expect(MathUtils.PI).toBe(3.14159);

  // Static methods are not available on instances
  const instance = new MathUtils();
  expect(instance.add).toBeUndefined();
});

test("static methods in inheritance", () => {
  class Parent {
    static parentMethod() {
      return "parent";
    }
  }

  class Child extends Parent {
    static childMethod() {
      return "child";
    }

    static parentMethod() {
      return super.parentMethod() + " overridden";
    }
  }

  expect(Child.parentMethod()).toBe("parent overridden");
  expect(Child.childMethod()).toBe("child");
});

test("static factory methods", () => {
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
  expect(person1.name).toBe("Alice");
  expect(person1.age).toBe(25);

  const person2 = Person.adult("Bob");
  expect(person2.name).toBe("Bob");
  expect(person2.age).toBe(18);
});
