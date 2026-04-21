/*---
description: Basic class declaration and instantiation works correctly
features: [class-declaration]
---*/

test("class declaration has .name property", () => {
  class Person {
    constructor(name) {
      this.name = name;
    }
  }
  expect(Person.name).toBe("Person");

  const desc = Object.getOwnPropertyDescriptor(Person, "name");
  expect(desc.writable).toBe(false);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(true);
});

test("named class expression .name uses the class name", () => {
  const C = class MyClass {};
  expect(C.name).toBe("MyClass");
});

test("anonymous class expression infers name from binding", () => {
  const C = class {};
  expect(C.name).toBe("C");

  let D = class {};
  expect(D.name).toBe("D");
});

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

test("instance.constructor points to class", () => {
  class Foo {
    constructor() {
      this.name = "foo";
    }
  }

  const f = new Foo();
  expect(f.constructor === Foo).toBe(true);
});

test("instance.constructor follows inheritance chain", () => {
  class Base {
    constructor() {
      this.base = true;
    }
  }

  class Child extends Base {
    constructor() {
      super();
      this.child = true;
    }
  }

  const b = new Base();
  const c = new Child();
  expect(b.constructor === Base).toBe(true);
  expect(c.constructor === Child).toBe(true);
  expect(c.constructor === Base).toBe(false);
});

test("class reference preserves instanceof", () => {
  class MyClass {
    constructor(x) {
      this.x = x;
    }
  }

  const ClassRef = MyClass;
  const obj = new ClassRef(42);
  expect(obj instanceof MyClass).toBe(true);
  expect(obj instanceof ClassRef).toBe(true);
  expect(obj.x).toBe(42);
});
