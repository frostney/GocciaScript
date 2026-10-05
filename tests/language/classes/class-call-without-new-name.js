/*---
description: Calling a class without new depends on the constructor, not on its name property
features: [classes, Function.name]
---*/

const renamed = (C, name, body) => {
  const original = Object.getOwnPropertyDescriptor(C, "name");
  Object.defineProperty(C, "name", { writable: true });
  C.name = name;
  try {
    return body(C);
  } finally {
    Object.defineProperty(C, "name", original);
  }
};

describe("built-in constructors called without new", () => {
  test("keep converting after their name is assigned", () => {
    expect(renamed(String, "Str", (S) => S(1))).toBe("1");
    expect(renamed(String, "Str", (S) => S(Symbol("d")))).toBe("Symbol(d)");
    expect(renamed(Number, "N", (N) => N("3"))).toBe(3);
    expect(renamed(Boolean, "B", (B) => B(1))).toBe(true);
    expect(renamed(Array, "A", (A) => A(1, 2))).toEqual([1, 2]);
    expect(renamed(Array, "A", (A) => A(3).length)).toBe(3);
    expect(renamed(Object, "O", (O) => typeof O())).toBe("object");
    expect(renamed(Object, "O", (O) => typeof O(1))).toBe("object");
  });

  test("Object keeps returning an object argument after its name is assigned", () => {
    const value = {};
    expect(renamed(Object, "O", (O) => O(value))).toBe(value);
    expect(renamed(Object, "O", (O) => new O(value))).toBe(value);
    expect(renamed(Object, "O", (O) => typeof new O(1))).toBe("object");
  });

  test("the assigned name is what the name property reports", () => {
    expect(renamed(String, "Str", (S) => S.name)).toBe("Str");
    expect(String.name).toBe("String");
  });
});

describe("user classes called without new", () => {
  test("throw when their name is assigned a built-in constructor's name", () => {
    for (const name of ["String", "Number", "Boolean", "Array", "Object"]) {
      class U {}
      Object.defineProperty(U, "name", { writable: true });
      U.name = name;
      expect(U.name).toBe(name);
      expect(() => U(5)).toThrow(TypeError);
    }
  });

  test("throw when declared or bound with a built-in constructor's name", () => {
    const classes = [
      class String {},
      class Number {},
      class Boolean {},
      class Array {},
      class Object {},
    ];
    for (const C of classes) {
      expect(() => C(1)).toThrow(TypeError);
    }

    const Array2 = (() => {
      const Array = class {};
      return Array;
    })();
    expect(Array2.name).toBe("Array");
    expect(() => Array2(2)).toThrow(TypeError);
  });

  test("name the class they were created as in the error after a name assignment", () => {
    class U {}
    Object.defineProperty(U, "name", { writable: true });
    U.name = "Foo";
    let message = "";
    try {
      U(5);
    } catch (error) {
      message = error.message;
    }
    expect(message).toBe("Class constructor U cannot be invoked without 'new'");
  });

  test("a user class named Object constructs an instance of itself", () => {
    class Object {}
    const value = {};
    const fromPrimitive = new Object(5);
    expect(fromPrimitive instanceof Object).toBe(true);
    expect(fromPrimitive.constructor).toBe(Object);
    expect(new Object(value) === value).toBe(false);
  });
});
