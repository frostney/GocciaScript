/*---
description: Reflect.construct
features: [Reflect]
---*/

describe("Reflect.construct", () => {
  test("creates a new instance of a class", () => {
    class Foo {
      constructor(x) {
        this.x = x;
      }
    }
    const obj = Reflect.construct(Foo, [42]);
    expect(obj.x).toBe(42);
    expect(obj instanceof Foo).toBe(true);
  });

  test("works with empty arguments list", () => {
    class Bar {
      constructor() {
        this.value = "default";
      }
    }
    const obj = Reflect.construct(Bar, []);
    expect(obj.value).toBe("default");
  });

  test("uses newTarget to set prototype", () => {
    class Foo {
      constructor(x) {
        this.x = x;
      }
    }
    class Bar {
      getX() {
        return this.x;
      }
    }
    const obj = Reflect.construct(Foo, [10], Bar);
    expect(obj.x).toBe(10);
    expect(obj instanceof Bar).toBe(true);
  });

  test("throws TypeError if target is not a constructor", () => {
    expect(() => Reflect.construct(() => {}, [])).toThrow(TypeError);
    expect(() => Reflect.construct(42, [])).toThrow(TypeError);
  });

  test("throws TypeError if newTarget is not a constructor", () => {
    class Foo {}
    expect(() => Reflect.construct(Foo, [], 42)).toThrow(TypeError);
    expect(() => Reflect.construct(Foo, [], () => {})).toThrow(TypeError);
  });

  test("throws TypeError if argumentsList is not an array", () => {
    class Foo {}
    expect(() => Reflect.construct(Foo, "not-array")).toThrow(TypeError);
  });

  test("newTarget prototype is visible during constructor execution", () => {
    class Bar {}
    class Foo {
      constructor() {
        this.protoWasCorrect = (Object.getPrototypeOf(this) === Bar.prototype);
      }
    }

    const obj = Reflect.construct(Foo, [], Bar);
    expect(obj instanceof Bar).toBe(true);
    expect(obj.protoWasCorrect).toBe(true);
  });

  test("newTarget prototype is visible for instanceof during construction", () => {
    class Bar {}
    class Base {
      constructor() {
        this.isBar = (this instanceof Bar);
      }
    }

    const obj = Reflect.construct(Base, [], Bar);
    expect(obj.isBar).toBe(true);
  });

  test("constructor can call inherited methods from newTarget prototype", () => {
    class Target {
      constructor() {
        this.greeting = this.greet();
      }
    }
    class NewTarget {
      greet() {
        return "hello from NewTarget";
      }
    }

    const obj = Reflect.construct(Target, [], NewTarget);
    expect(obj.greeting).toBe("hello from NewTarget");
  });

  test("newTarget same as target preserves default behavior", () => {
    class Foo {
      constructor() {
        this.isFoo = (this instanceof Foo);
      }
    }

    const obj = Reflect.construct(Foo, [], Foo);
    expect(obj instanceof Foo).toBe(true);
    expect(obj.isFoo).toBe(true);
  });
});
