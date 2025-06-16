/*---
description: Object creation, property access, and object operations work correctly
features: [Object, property-access, object-literals]
---*/

test("basic object creation and property access", () => {
  const obj = { name: "Alice", age: 30, active: true };
  expect(obj.name).toBe("Alice");
  expect(obj.age).toBe(30);
  expect(obj.active).toBeTruthy();
  expect(obj.nonExistent).toBeUndefined();
});

test("object creation with various property types", () => {
  const complexObj = {
    string: "text",
    number: 42,
    boolean: true,
    null: null,
    undefined: undefined,
    array: [1, 2, 3],
    nested: { inner: "value" },
    method: function () {
      return "method result";
    },
    arrow: () => "arrow result",
    computed: 10 + 20,
  };

  expect(complexObj.string).toBe("text");
  expect(complexObj.number).toBe(42);
  expect(complexObj.boolean).toBe(true);
  expect(complexObj.null).toBe(null);
  expect(complexObj.undefined).toBe(undefined);
  expect(complexObj.array).toEqual([1, 2, 3]);
  expect(complexObj.nested.inner).toBe("value");
  expect(complexObj.method()).toBe("method result");
  expect(complexObj.arrow()).toBe("arrow result");
  expect(complexObj.computed).toBe(30);
});

test("object property enumeration and inspection", () => {
  const obj = { a: 1, b: 2, c: 3 };

  const keys = Object.keys(obj);
  expect(keys).toEqual(["a", "b", "c"]);

  const values = Object.values(obj);
  expect(values).toEqual([1, 2, 3]);

  const entries = Object.entries(obj);
  expect(entries).toEqual([
    ["a", 1],
    ["b", 2],
    ["c", 3],
  ]);

  expect(Object.hasOwnProperty.call(obj, "a")).toBe(true);
  expect(Object.hasOwnProperty.call(obj, "d")).toBe(false);
});

test("object creation with constructors and prototypes", () => {
  function Person(name, age) {
    this.name = name;
    this.age = age;
  }

  Person.prototype.greet = function () {
    return `Hello, I'm ${this.name}`;
  };

  Person.prototype.getAge = function () {
    return this.age;
  };

  const person1 = new Person("Alice", 30);
  const person2 = new Person("Bob", 25);

  expect(person1.name).toBe("Alice");
  expect(person1.age).toBe(30);
  expect(person1.greet()).toBe("Hello, I'm Alice");
  expect(person1.getAge()).toBe(30);

  expect(person2.name).toBe("Bob");
  expect(person2.age).toBe(25);
  expect(person2.greet()).toBe("Hello, I'm Bob");
  expect(person2.getAge()).toBe(25);
});

test("object property descriptors and configuration", () => {
  const obj = {};

  Object.defineProperty(obj, "readOnly", {
    value: "cannot change",
    writable: false,
    enumerable: true,
    configurable: false,
  });

  Object.defineProperty(obj, "hidden", {
    value: "not enumerable",
    writable: true,
    enumerable: false,
    configurable: true,
  });

  expect(obj.readOnly).toBe("cannot change");
  expect(obj.hidden).toBe("not enumerable");

  // Test that readOnly property cannot be changed
  obj.readOnly = "try to change";
  expect(obj.readOnly).toBe("cannot change");

  // Test enumeration
  const keys = Object.keys(obj);
  expect(keys).toContain("readOnly");
  expect(keys).not.toContain("hidden");
});
