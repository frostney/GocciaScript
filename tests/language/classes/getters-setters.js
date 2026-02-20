/*---
description: Class getters and setters work correctly (instance and static)
features: [class-getters, class-setters, static-getters, static-setters]
---*/

test("basic getters and setters", () => {
  class Rectangle {
    constructor(width, height) {
      this._width = width;
      this._height = height;
    }

    get width() {
      return this._width;
    }

    set width(value) {
      if (value > 0) {
        this._width = value;
      }
    }

    get height() {
      return this._height;
    }

    set height(value) {
      if (value > 0) {
        this._height = value;
      }
    }

    get area() {
      return this._width * this._height;
    }
  }

  const rect = new Rectangle(5, 10);
  expect(rect.width).toBe(5);
  expect(rect.height).toBe(10);
  expect(rect.area).toBe(50);

  rect.width = 8;
  expect(rect.width).toBe(8);
  expect(rect.area).toBe(80);

  // Invalid values are ignored
  rect.width = -5;
  expect(rect.width).toBe(8); // Unchanged
});

test("computed property getters", () => {
  class Circle {
    constructor(radius) {
      this.radius = radius;
    }

    get diameter() {
      return this.radius * 2;
    }

    get circumference() {
      return 2 * Math.PI * this.radius;
    }

    get area() {
      return Math.PI * this.radius * this.radius;
    }
  }

  const circle = new Circle(3);
  expect(circle.diameter).toBe(6);
  expect(circle.circumference).toBeCloseTo(18.85, 2);
  expect(circle.area).toBeCloseTo(28.27, 2);
});

test("complex example", () => {
  class Calculator {
    constructor() {
      this._value = 0;
      this._operations = 0;
    }

    get value() {
      return this._value;
    }

    set value(newValue) {
      this._value = newValue;
      this._operations = this._operations + 1;
    }

    get doubled() {
      return this._value * 2;
    }

    get squared() {
      return this._value * this._value;
    }

    get operations() {
      return this._operations;
    }

    set reset(ignored) {
      this._operations = this._operations + 1;
      this._value = 0;
    }

    add(n) {
      this._value = this._value + n;
      this._operations = this._operations + 1;
    }

    multiply(n) {
      this._value = this._value * n;
      this._operations = this._operations + 1;
    }
  }

  const calculator = new Calculator();
  expect(calculator.value).toBe(0);
  expect(calculator.operations).toBe(0);

  calculator.value = 5;
  expect(calculator.value).toBe(5);

  expect(calculator.doubled).toBe(10);
  expect(calculator.squared).toBe(25);

  calculator.add(3);
  expect(calculator.value).toBe(8);
  expect(calculator.operations).toBe(2);

  calculator.multiply(2);
  expect(calculator.value).toBe(16);
  expect(calculator.operations).toBe(3);

  calculator.reset = null;
  expect(calculator.value).toBe(0);
  expect(calculator.operations).toBe(4);
});

test("static getter", () => {
  class Config {
    static get defaultPort() {
      return 8080;
    }
  }
  expect(Config.defaultPort).toBe(8080);
});

test("static getter and setter", () => {
  class Registry {
    static _count = 0;

    static get count() {
      return Registry._count;
    }

    static set count(value) {
      Registry._count = value;
    }
  }
  expect(Registry.count).toBe(0);
  Registry.count = 5;
  expect(Registry.count).toBe(5);
});

test("static getter is not on instances", () => {
  class Foo {
    static get bar() {
      return 42;
    }
  }
  expect(Foo.bar).toBe(42);
  const f = new Foo();
  expect(f.bar).toBeUndefined();
});

test("static getter with this refers to the class", () => {
  class Base {
    static _name = "Base";

    static get label() {
      return this._name;
    }
  }
  expect(Base.label).toBe("Base");
});

test("static getter inheritance", () => {
  class Base {
    static get type() {
      return "base";
    }
  }
  class Derived extends Base {}
  expect(Derived.type).toBe("base");
});

test("static getter override in subclass", () => {
  class Base {
    static get type() {
      return "base";
    }
  }
  class Derived extends Base {
    static get type() {
      return "derived";
    }
  }
  expect(Base.type).toBe("base");
  expect(Derived.type).toBe("derived");
});

test("static setter with validation", () => {
  class Settings {
    static _volume = 50;

    static get volume() {
      return Settings._volume;
    }

    static set volume(v) {
      if (v >= 0 && v <= 100) {
        Settings._volume = v;
      }
    }
  }
  Settings.volume = 75;
  expect(Settings.volume).toBe(75);
  Settings.volume = -10;
  expect(Settings.volume).toBe(75);
  Settings.volume = 200;
  expect(Settings.volume).toBe(75);
});

test("computed static getter with symbol key", () => {
  const key = Symbol("info");
  class MyClass {
    static get [key]() {
      return "computed symbol getter";
    }
  }
  expect(MyClass[key]).toBe("computed symbol getter");
});

test("computed static getter with string key", () => {
  const prop = "dynamicProp";
  class MyClass {
    static get [prop]() {
      return 123;
    }
  }
  expect(MyClass.dynamicProp).toBe(123);
});

test("computed static getter with Symbol.species pattern", () => {
  class MyCollection {
    static get [Symbol.species]() {
      return Array;
    }
  }
  expect(MyCollection[Symbol.species]).toBe(Array);
});

test("computed static getter this context", () => {
  class Base {
    static get [Symbol.species]() {
      return this;
    }
  }
  class Derived extends Base {}
  expect(Base[Symbol.species]).toBe(Base);
  expect(Derived[Symbol.species]).toBe(Derived);
});
